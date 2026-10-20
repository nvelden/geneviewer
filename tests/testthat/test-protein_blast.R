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
