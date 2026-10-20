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

test_that("protein_blast gives the same result in parallel (B21)", {
  skip_on_cran()
  skip_if_not_installed("Biostrings")
  skip_if_not_installed("pwalign")
  requireNamespace("Biostrings", quietly = TRUE)
  requireNamespace("pwalign", quietly = TRUE)

  # 40 x 40 proteins: more than 1000 combinations, so the parallel path is used
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
