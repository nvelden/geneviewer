write_fasta <- function(lines, fileext = ".fasta", eol = "\n") {
  file <- tempfile(fileext = fileext)
  con <- if (endsWith(fileext, ".gz")) gzfile(file, "wb") else file(file, "wb")
  writeBin(charToRaw(paste0(lines, eol, collapse = "")), con)
  close(con)
  file
}

# FASTA files in the formats of different sources
fasta_fixtures <- function() {
  list(
    ncbi_cds = c(
      ">lcl|NC_000913.3_prot_NP_414542.1_1 [gene=thrL] [locus_tag=b0001] [protein=thr operon leader peptide] [protein_id=NP_414542.1] [location=190..255] [gbkey=CDS]",
      "MKRISTTITTTITITTGNGAG",
      ">lcl|NC_045512.2_prot_YP_009724389.1_1 [gene=ORF1ab] [protein=ORF1ab polyprotein] [exception=ribosomal slippage] [location=join(266..13468,13468..21555)] [gbkey=CDS]",
      "MESLVPGFNEKTHVQLSLPVLQVRDVLVRGFGDSVEEVLSEARQHLKDGTCGLVEVEKGVLPQLEQPYVF",
      "IKRSDARTAPHGHVMVELVAELEGIQYGRSGETLGVLVPHVGEIPVAYRKVLLRKNGNKG",
      ">lcl|NC_001133.9_prot_NP_009332.1_1 [gene=PAU8] [protein=seripauperin PAU8] [location=complement(1807..2169)] [gbkey=CDS]",
      "MVKLTSIAAGVAAIAATASATTTLAQSDERVNLVELGVYVSDIR"
    ),
    ncbi_unbracketed = c(
      ">lcl|HQ3864.1_prot_ADX475.1_1 protein=putative phosphoenolpyruvate synthase protein_id=ADX66475.1 location=complement(<1..2247) gbkey=CDS",
      "MRATGLVRGRAAKRFGRAGGAGDAIGQCRATGHDCLGGSAMIEQYVWDLHEVDETQVAVVGGKGAHLGGL",
      "SRIEGIRVPAG"
    ),
    uniprot = c(
      ">sp|P04637|P53_HUMAN Cellular tumor antigen p53 OS=Homo sapiens OX=9606 GN=TP53 PE=1 SV=4",
      "MEEPQSDPSVEPPLSQETFSDLWKLLPENNVLSPLPSQAMDDLMLSPDDIEQWFTEDPGP",
      "DEAPRMPEAAPPVAPAPAAPTPAAPAPAPSWPLSSSVPSQKTYQGSYGFRLGFLHSGTAK",
      ">sp|P04637-2|P53_HUMAN Isoform 2 of Cellular tumor antigen p53 OS=Homo sapiens OX=9606 GN=TP53",
      "MEEPQSDPSVEPPLSQETFSDLWKLLPENNVLSPLPSQAMDDLMLSPDDIEQWFTEDPGP"
    ),
    ncbi_protein = c(
      ">WP_000003984.1 HD domain-containing protein [Vibrio sp. RC586]",
      "MSERLAQQMTLLMELDRLKSVLRRTRVKSAEGRLENSAEHSWHVALMAILLQEHANEAVDMQKVLTMLLL",
      "",
      ">NP_414542.1 thr operon leader peptide [Escherichia coli str. K-12 substr. MG1655]",
      "MKRISTTITTTITITTGNGAG"
    ),
    pipes = c(
      ">BGC0000001.5|c1|1-1083|+|AEK75490.1|protein_methyltransferase|AEK75490.1",
      "MTELDRAFDAVPAPIYTHHERHGETVHRSAPESIRRELAALQVRAGDRVLEIGTGSGYSGALLAHL",
      ">4HHB_1|Chains A, C|Hemoglobin subunit alpha|Homo sapiens (9606)",
      "VLSPADKTNVKAAWGKVGAHAGEYGAEALERMFLSFPTTKTYFPHFDLSHGSAQVKGHGKKVADALTNAV"
    ),
    nucleotide = c(
      ">lcl|NC_045512.2_cds_YP_009725255.1_12 [gene=ORF10] [protein=ORF10 protein] [location=29558..29674] [gbkey=CDS]",
      "ATGGGCTATATAAACGTTTTCGCTTTTCCGTTTACGATATATAGTCTACTCTTGTGCAGAATGAATTCTC",
      "GTAACTACATAGCACAAGTAGATGTAGTTAACTTTAATCTCACATAG"
    ),
    alignment = c(
      ">seq1 [gene=a] [location=1..30]", "MKT-AY..IAKQR--",
      ">seq2 [gene=b] [location=40..70]", "mkt-ay..iakqr--"
    ),
    messy = c(
      ";comment before the first record",
      "",
      ">a [gene=x] [location=1..9]  ",
      "mkta yi",
      ";comment inside a sequence",
      "AKQR*",
      "",
      ">b [gene=y] [location=20..29]",
      ">c [gene=z] [location=complement(40..49)]",
      "      1 mktayiakqr qisfvkshfs",
      "     21 rqleer"
    )
  )
}

test_that("read_fasta_records joins, upper cases and cleans the sequences", {
  file <- write_fasta(fasta_fixtures()$messy)
  expect_warning(records <- read_fasta_records(file), "ignored 18 invalid one-letter sequence codes")
  expect_identical(records$headers, c("a [gene=x] [location=1..9]  ", "b [gene=y] [location=20..29]",
                                      "c [gene=z] [location=complement(40..49)]"))
  expect_identical(records$sequences, c("MKTAYIAKQR*", "", "MKTAYIAKQRQISFVKSHFSRQLEER"))

  # Alignment gaps and stops are kept
  file <- write_fasta(fasta_fixtures()$alignment)
  expect_identical(read_fasta_records(file)$sequences, c("MKT-AY..IAKQR--", "MKT-AY..IAKQR--"))
})

test_that("read_fasta_records reads Windows and old Mac line endings and gzip files", {
  lines <- fasta_fixtures()$ncbi_cds
  expected <- read_fasta_records(write_fasta(lines))
  expect_identical(read_fasta_records(write_fasta(lines, eol = "\r\n")), expected)
  expect_identical(read_fasta_records(write_fasta(lines, eol = "\r")), expected)
  expect_identical(read_fasta_records(write_fasta(lines, fileext = ".fasta.gz")), expected)
  expect_identical(expected$sequences[1], "MKRISTTITTTITITTGNGAG")
})

test_that("read_fasta_records handles empty files and files without a header", {
  empty <- read_fasta_records(write_fasta(character()))
  expect_identical(empty, list(headers = character(), sequences = character()))
  expect_error(read_fasta_records(write_fasta(c(";comment", "", "MKT", ">a", "MKT"))),
               '">" expected at beginning of line 3')

  # read_fasta skips such a file with a warning
  expect_warning(result <- read_fasta(write_fasta(c("MKT", ">a [gene=x]", "MKT"))),
                 '">" expected at beginning of line 1')
  expect_identical(nrow(result), 0L)
})

test_that("read_fasta reads FASTA files of different sources", {
  fixtures <- fasta_fixtures()

  ncbi <- read_fasta(write_fasta(fixtures$ncbi_cds))
  expect_identical(ncbi$gene, c("thrL", "ORF1ab", "PAU8"))
  expect_identical(ncbi$start, c(190, 266, 1807))
  expect_identical(ncbi$end, c(255, 21555, 2169))
  expect_identical(ncbi$strand, c("forward", "forward", "complement"))
  expect_identical(nchar(ncbi$sequence), c(21L, 130L, 44L))

  uniprot <- read_fasta(write_fasta(fixtures$uniprot), sequence = FALSE)
  expect_identical(uniprot$GN, c("TP53", "TP53"))
  expect_identical(uniprot$OS, c("Homo sapiens", "Homo sapiens"))

  nucleotide <- read_fasta(write_fasta(fixtures$nucleotide))
  expect_identical(nucleotide$sequence, paste0(fixtures$nucleotide[2:3], collapse = ""))

  # Headers without key=value pairs give no rows
  expect_warning(pipes <- read_fasta(write_fasta(fixtures$pipes)), "No keys found")
  expect_identical(nrow(pipes), 0L)
})

test_that("read_fasta gives the same result as Biostrings", {
  skip_if_not_installed("Biostrings")

  read_biostrings <- function(file) {
    set <- suppressWarnings(Biostrings::readAAStringSet(file))
    list(headers = names(set), sequences = unname(as.character(set)))
  }

  files <- c(
    lapply(fasta_fixtures(), write_fasta),
    list(
      crlf = write_fasta(fasta_fixtures()$ncbi_cds, eol = "\r\n"),
      gzip = write_fasta(fasta_fixtures()$uniprot, fileext = ".fasta.gz"),
      no_final_newline = write_fasta(">a [gene=x]\nMKT\n>b [gene=y]\nMAA", eol = "")
    ),
    as.list(list.files(system.file("extdata", package = "geneviewer"),
                       pattern = "\\.fasta$", full.names = TRUE))
  )

  for (file in files) {
    expect_identical(suppressWarnings(read_fasta_records(file)), read_biostrings(file),
                     info = basename(file))
  }
})

test_that("read_fasta skips an empty file with the warning of a file without keys (B66)", {
  file <- write_fasta(character())
  expect_warning(result <- read_fasta(file), "No keys found in")
  expect_identical(nrow(result), 0L)
})

test_that("read_fasta gives every file of a folder a strand, also with only forward genes (B73)", {
  dir <- tempfile()
  dir.create(dir)
  writeLines(c(">a [protein_id=p1] [location=complement(1..90)]", "MKK",
               ">b [protein_id=p2] [location=100..190]", "MAA"), file.path(dir, "A.fasta"))
  writeLines(c(">a [protein_id=p3] [location=1..90]", "MKK",
               ">b [protein_id=p4] [location=100..190]", "MAA",
               ">c [protein_id=p5]", "MCC"), file.path(dir, "B.fasta"))

  genes <- read_fasta(dir)
  expect_identical(genes$strand, c("complement", "forward", "forward", "forward", NA))
  # A row without a location has no start, end or strand; GC_chart() leaves it out
  expect_warning(chart <- GC_chart(genes, cluster = "cluster", strand = "strand"), "filtered out")
  expect_s3_class(chart, "geneviewer")

  # One file with only forward genes
  forward <- read_fasta(file.path(dir, "B.fasta"))
  expect_identical(forward$strand, c("forward", "forward", NA))
})
