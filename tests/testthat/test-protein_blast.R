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
  expect_match(chart$x$series$A$tooltip$formatter, "Query gene")
  expect_no_match(chart$x$series$A$tooltip$formatter, "Identity")
  expect_match(chart$x$series$B$tooltip$formatter, "BlastP hit with")
  expect_match(chart$x$series$B$tooltip$formatter, "Identity")
})

test_that("tooltips leave out lines without values", {
  chart <- GC_chart(blast_genes, cluster = "cluster", group = "BlastP",
                    width = "600px", height = "300px") %>%
    GC_links(group = "BlastP")
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file)
  result <- session$Runtime$evaluate("
    (() => {
      const tips = {};
      d3.selectAll('[data-component=genes] *').filter(d => d && d.protein_id).each(function (d) {
        this.dispatchEvent(new MouseEvent('mouseover', { bubbles: true }));
        tips[d.cluster + d.protein_id] = Array.from(document.querySelectorAll('.cluster-tooltip'))
          .map(t => t.innerText).find(t => t.includes(d.protein_id));
        this.dispatchEvent(new MouseEvent('mouseout', { bubbles: true }));
      });
      return JSON.stringify(tips);
    })()
  ")
  expect_null(result$exceptionDetails)
  tips <- jsonlite::fromJSON(result$result$value)
  expect_match(tips$AA1, "Query gene")
  expect_match(tips$AA2, "Query gene")
  expect_no_match(tips$AA2, "No Hit|Identity")
  expect_match(tips$BB1, "BlastP hit with: A1")
  expect_match(tips$BB1, "Identity: 55.4%")
  expect_match(tips$BB1, "Similarity: 68.2%")
  expect_match(tips$BB2, "BlastP hit with: No Hit")
  expect_no_match(tips$BB2, "Identity|Similarity")
  expect_match(tips$BB2, "Location: 200 - 350")
})
