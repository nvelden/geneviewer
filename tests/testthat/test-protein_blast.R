test_that("protein_blast works with a cluster column of any name (B06)", {
  skip_if_not_installed("Biostrings")
  skip_if_not_installed("pwalign")
  requireNamespace("Biostrings", quietly = TRUE)
  requireNamespace("pwalign", quietly = TRUE)

  genes <- data.frame(
    protein_id = c("A1", "A2", "B1", "B2"),
    cluster = c("A", "A", "B", "B"),
    start = c(1, 200, 1, 200),
    end = c(150, 350, 150, 350),
    translation = c("MKTAYIAKQRQISFVKSHFSRQ", "MSDNELLKRLGEAVLAHPTTG",
                    "MKTAYIAKQRQISFVKSHFSRQ", "MLPGGRTSWVQEHCDKAYNIF")
  )
  expected <- protein_blast(genes, query = "A", parallel = FALSE)

  genes_bgc <- genes
  names(genes_bgc)[names(genes_bgc) == "cluster"] <- "bgc"
  result <- protein_blast(genes_bgc, query = "A", cluster = "bgc", parallel = FALSE)

  expect_equal(result$bgc, expected$cluster)
  expect_equal(result$BlastP, expected$BlastP)
  expect_equal(result$BlastP[result$protein_id == "B1"], "A1")
})

test_that("protein_blast ignores the deprecated parallel argument (B21)", {
  skip_on_cran()
  skip_if_not_installed("Biostrings")
  skip_if_not_installed("pwalign")
  requireNamespace("Biostrings", quietly = TRUE)
  requireNamespace("pwalign", quietly = TRUE)

  # 40 x 40 proteins: more than 1000 combinations, which used the parallel path
  set.seed(1)
  aa <- strsplit("ACDEFGHIKLMNPQRSTVWY", "")[[1]]
  proteins <- vapply(1:40, function(i) paste(sample(aa, 30, TRUE), collapse = ""), "")
  genes <- data.frame(
    protein_id = c(paste0("A", 1:40), paste0("B", 1:40)),
    cluster = rep(c("A", "B"), each = 40),
    start = rep(seq(1, by = 100, length.out = 40), 2),
    end = rep(seq(90, by = 100, length.out = 40), 2),
    translation = c(proteins, rev(proteins))
  )
  expect_identical(protein_blast(genes, query = "A", parallel = TRUE),
                   protein_blast(genes, query = "A", parallel = FALSE))
})

blast_genes <- data.frame(
  protein_id = c("A1", "A2", "B1", "B2"),
  cluster = c("A", "A", "B", "B"),
  start = c(1, 200, 1, 200),
  end = c(150, 350, 150, 350),
  BlastP = c("A1", "No Hit", "A1", "No Hit"),
  identity = c(100, 100, 55.4, NA),
  similarity = c(100, 100, 68.2, NA)
)

test_that("the query cluster of BlastP results gets its own tooltip", {
  chart <- GC_chart(blast_genes, cluster = "cluster", group = "BlastP")
  model <- compile_chart(chart)
  expect_match(model$series$A$tooltip$formatter, "Query gene")
  expect_no_match(model$series$A$tooltip$formatter, "Identity")
  expect_match(model$series$B$tooltip$formatter, "Best hit")
  expect_match(model$series$B$tooltip$formatter, "Identity")
})

test_that("protein_blast works with only the query cluster (B67)", {
  skip_if_not_installed("Biostrings")
  skip_if_not_installed("pwalign")

  # pwalign crashed R when there was nothing to align
  genes <- data.frame(protein_id = c("a", "b"), cluster = "A", start = c(1, 10),
                      end = c(5, 15), translation = c("MKV", "MKL"))
  result <- protein_blast(genes, query = "A")
  expect_equal(result$identity, c(100, 100))
  expect_equal(result$BlastP, c("No Hit", "No Hit"))

  gbk <- protein_blast(system.file("extdata", "BGC0000001.gbk", package = "geneviewer"),
                       query = "BGC0000001")
  expect_true(all(gbk$identity == 100))
})

test_that("protein_blast can be run on its own result", {
  skip_if_not_installed("Biostrings")
  skip_if_not_installed("pwalign")

  genes <- data.frame(
    protein_id = c("A1", "A2", "B1", "B2"), cluster = c("A", "A", "B", "B"),
    start = c(1, 200, 1, 200), end = c(150, 350, 150, 350),
    translation = c("MKTAYIAKQRQISFVKSHFSRQ", "MSDNELLKRLGEAVLAHPTTG",
                    "MKTAYIAKQRQISFVKSHFSRQ", "MLPGGRTSWVQEHCDKAYNIF")
  )
  first <- protein_blast(genes, query = "A")
  again <- protein_blast(first, query = "A")
  expect_setequal(names(again), names(first))
  expect_equal(again[names(first)], first)

  # Another query replaces the results of the first
  by_b <- protein_blast(first, query = "B")
  expect_equal(by_b$cluster[1], "B")
  expect_equal(by_b$BlastP[by_b$protein_id == "A1"], "B1")
})
