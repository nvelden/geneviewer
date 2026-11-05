track_genes <- data.frame(
  name = c("A", "B", "B2", "B3", "C"),
  start = c(1, 120, 300, 320, 400),
  end = c(125, 400, 380, 350, 600)
)

# The track of each gene, as the library places them for
# GC_cluster(prevent_gene_overlap = )
tracks_of <- function(data, nested = FALSE, overlap_min = NULL) {
  chart <- GC_chart(data, height = "150px") %>%
    GC_cluster(prevent_gene_overlap = if (nested) "nested" else TRUE,
               overlap_min = overlap_min)
  tracked <- compile_chart(chart)$series[["cluster"]]$data
  tracks <- stats::setNames(tracked$geneTrack, tracked$name)
  tracks[data$name]
}

test_that("overlapping genes go on separate tracks by default", {
  # A and B share 6 bases, B and C one; B2 and B3 lie inside B and overlap
  expect_equal(tracks_of(track_genes), c(A = 1, B = 2, B2 = 1, B3 = 3, C = 1))
})

test_that("small overlaps stay on one track with overlap_min", {
  # Overlaps of up to 6 bases stay on a track: only the nested genes move
  expect_equal(tracks_of(track_genes, overlap_min = 6), c(A = 1, B = 1, B2 = 2, B3 = 3, C = 1))
  # As a share of the shorter gene: 6 bases are 5% of A
  expect_equal(tracks_of(track_genes, overlap_min = "5%"), c(A = 1, B = 1, B2 = 2, B3 = 3, C = 1))
  # 4% is less: A and B go on different tracks (in a chart, B is placed
  # first, so A goes below, as in R 0.2.0)
  expect_equal(tracks_of(track_genes, overlap_min = "4%")[c("A", "B")], c(A = 2, B = 1))
})

test_that("nested tracks move only genes inside another gene", {
  # The nested genes go below, apart from each other; the others stay on top,
  # also where they overlap
  expect_equal(tracks_of(track_genes, nested = TRUE), c(A = 1, B = 1, B2 = 2, B3 = 3, C = 1))
  # Of two equal genes, the second goes below
  same <- data.frame(name = c("X", "Y"), start = c(1, 1), end = c(50, 50))
  expect_equal(tracks_of(same, nested = TRUE), c(X = 1, Y = 2))
  # Genes on the reverse strand (start > end) count by their extent
  reverse <- data.frame(name = c("P", "Q"), start = c(100, 60), end = c(1, 40))
  expect_equal(tracks_of(reverse, nested = TRUE), c(P = 1, Q = 2))
})

test_that("GC_cluster() checks prevent_gene_overlap and overlap_min", {
  chart <- GC_chart(track_genes, height = "150px")
  expect_error(GC_cluster(chart, prevent_gene_overlap = "inside"),
               "'prevent_gene_overlap' must be TRUE, FALSE or \"nested\"")
  expect_error(GC_cluster(chart, prevent_gene_overlap = TRUE, overlap_min = -1),
               "'overlap_min' must be")
  expect_error(GC_cluster(chart, prevent_gene_overlap = TRUE, overlap_min = "ten%"),
               "'overlap_min' must be")
  for (value in c("1.2.3%", ".%", "5.%")) {
    expect_error(GC_cluster(chart, prevent_gene_overlap = TRUE, overlap_min = value),
                 "'overlap_min' must be")
  }
  nested <- GC_cluster(chart, prevent_gene_overlap = "nested")
  nested_model <- compile_chart(nested)
  data <- nested_model$series[[1]]$data
  expect_equal(stats::setNames(data$geneTrack, data$name)[track_genes$name],
               c(A = 1, B = 1, B2 = 2, B3 = 3, C = 1))
  expect_true(nested_model$series[[1]]$cluster$preventGeneOverlap)
  small <- GC_cluster(chart, prevent_gene_overlap = TRUE, overlap_min = "5%")
  expect_equal(max(compile_chart(small)$series[[1]]$data$geneTrack), 3)
  # Both strands and overlap tracks cannot be combined
  expect_warning(GC_cluster(chart, separate_strands = TRUE, prevent_gene_overlap = "nested"),
                 "not supported")
})

test_that("genes and labels can be styled by their track", {
  chart <- GC_chart(track_genes, height = "150px") %>%
    GC_cluster(prevent_gene_overlap = "nested") %>%
    GC_genes(style_by = "geneTrack",
             groupStyle = list("2" = list(markerHeight = 8, arrowheadHeight = 8))) %>%
    GC_labels("name", style_by = "geneTrack", groupStyle = list("2" = list(fontSize = "8px")))
  expect_identical(compile_chart(chart)$series[[1]]$genes$styleBy, "geneTrack")
  expect_error(GC_genes(chart, style_by = "track"), "style_by must be")

  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file, width = 800, height = 300)
  result <- session$Runtime$evaluate("
    (() => {
      const genes = Array.from(document.querySelectorAll('.gene'));
      const labels = Array.from(document.querySelectorAll('text.label'));
      const height = name => genes.find(g => g.__data__.name === name).getBBox().height;
      const font = name => labels.find(t => t.textContent === name).style.fontSize;
      return JSON.stringify({ B: height('B'), B2: height('B2'), B3: height('B3'),
                              fontB: font('B'), fontB2: font('B2') });
    })()
  ")
  expect_null(result$exceptionDetails)
  value <- jsonlite::fromJSON(result$result$value)
  # The genes of the second track are drawn smaller, the others as before
  expect_lt(value$B2, value$B)
  expect_equal(value$B3, value$B)
  expect_equal(value$fontB2, "8px")
  expect_false(identical(value$fontB, "8px"))
})
