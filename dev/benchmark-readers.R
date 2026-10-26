# Run from the package root with tidyr, Biostrings and pkgload installed:
# Rscript dev/benchmark-readers.R /tmp/geneviewer-reader-benchmark.csv
# Optionally filter case names with GENEVIEWER_BENCHMARK_CASES (a regex).
# The baseline uses the same reader code, replacing only the metadata helper
# with the previous tidyr operation. Timings exclude package loading and use
# warmed filesystem caches. No network access or timing assertions are used.
pkgload::load_all(quiet = TRUE)
stopifnot(requireNamespace("tidyr", quietly = TRUE),
          requireNamespace("Biostrings", quietly = TRUE))

ns <- asNamespace("geneviewer")
old <- new.env(parent = ns)
old$widen_attributes <- function(data) {
  tidyr::pivot_wider(data, names_from = "key", values_from = "value")
}
for (name in c("read_fasta", "read_gff", "process_gff")) {
  fun <- get(name, envir = ns)
  environment(fun) <- old
  assign(name, fun, envir = old)
}

workspace <- tempfile("geneviewer-reader-benchmark-")
dir.create(workspace)
results <- list()
measure <- function(label, before, after, batch = 1L) {
  selected <- Sys.getenv("GENEVIEWER_BENCHMARK_CASES")
  if (nzchar(selected) && !grepl(selected, label)) return(invisible())
  # Check complete output equality, including classes and list columns.
  stopifnot(identical(suppressWarnings(before()), suppressWarnings(after())))
  elapsed <- function(fun) {
    unname(system.time(for (i in seq_len(batch)) suppressWarnings(fun()))[["elapsed"]]) / batch
  }
  times <- matrix(NA_real_, nrow = 7L, ncol = 2L)
  for (i in seq_len(nrow(times))) {
    # Alternate order to reduce systematic bias from CPU/cache conditions.
    for (j in if (i %% 2L) 1:2 else 2:1) {
      gc()
      times[i, j] <- elapsed(if (j == 1L) before else after)
    }
  }
  row <- data.frame(
    case = label, baseline_ms = median(times[, 1]) * 1000,
    helper_ms = median(times[, 2]) * 1000,
    baseline_min_ms = min(times[, 1]) * 1000,
    baseline_max_ms = max(times[, 1]) * 1000,
    helper_min_ms = min(times[, 2]) * 1000,
    helper_max_ms = max(times[, 2]) * 1000
  )
  row$change_percent <- (row$helper_ms / row$baseline_ms - 1) * 100
  results[[length(results) + 1L]] <<- row
  print(row, row.names = FALSE)
}

for (n in c(1000L, 10000L, 100000L)) {
  metadata <- data.frame(
    id = as.character(rep(seq_len(n), each = 8L)),
    key = rep(paste0("field", 1:8), n),
    value = rep(c("gene", "protein", "100..200", NA, "", "x y", "a=b", "NA"), n)
  )
  # Sparse records, preserving at least one field for each record.
  metadata <- metadata[seq_len(nrow(metadata)) %% 7L != 0L, ]
  measure(paste("Metadata", n, "records"),
          function() old$widen_attributes(metadata),
          function() geneviewer:::widen_attributes(metadata),
          batch = if (n == 1000L) 20L else 1L)
}

gff <- "inst/extdata/HQ113105.1.gff3"
measure("GFF packaged example", function() old$read_gff(gff), function() read_gff(gff))
if (file.exists("gff_files/genomic.gff")) {
  gff <- "gff_files/genomic.gff"
  measure("GFF genomic.gff", function() old$read_gff(gff), function() read_gff(gff))
}
gff <- file.path(workspace, "sparse.gff3")
n <- 10000L
attributes <- ifelse(seq_len(n) %% 7L == 0L, ".", paste0(
  "ID=g", seq_len(n), ";Name=gene", seq_len(n),
  ";Note=protein%20with%20spaces;Dbxref=GeneID:", seq_len(n)))
writeLines(paste("chr1", "benchmark", "CDS", seq_len(n) * 100L,
                 seq_len(n) * 100L + 80L, ".", "+", "0", attributes, sep = "\t"), gff)
measure("GFF 10000 sparse records", function() old$read_gff(gff), function() read_gff(gff))

fasta <- "inst/extdata/BGC0000108.fasta"
measure("FASTA packaged example", function() old$read_fasta(fasta),
        function() read_fasta(fasta), batch = 10L)
for (n in c(10000L, 100000L)) {
  fasta <- file.path(workspace, "proteins.fasta")
  headers <- paste0(
    ">s", seq_len(n), " [gene=g", seq_len(n), "] [protein=putative protein ",
    seq_len(n), "] [protein_id=P", seq_len(n), "] [location=",
    seq_len(n) * 100L, "..", seq_len(n) * 100L + 80L, "]")
  headers[seq_len(n) %% 7L == 0L] <- paste0(
    ">s", which(seq_len(n) %% 7L == 0L), " [gene=sparse]")
  sequence <- paste(rep("MKTAYIAKQR", 20L), collapse = "")
  writeLines(as.vector(rbind(headers, rep(sequence, n))), fasta)
  measure(paste("FASTA", n, "proteins"),
          function() old$read_fasta(fasta), function() read_fasta(fasta))
}

results <- do.call(rbind, results)
output <- commandArgs(trailingOnly = TRUE)
if (length(output)) write.csv(results, output[[1]], row.names = FALSE)
unlink(workspace, recursive = TRUE)
cat("\nAll benchmark outputs were identical.\n")
print(results, row.names = FALSE)
