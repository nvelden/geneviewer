test_that("Read .gbk file from file path", {
  # Use the actual GBK file
  gbk_file <- system.file("extdata", "BGC0000001.gbk", package = "geneviewer")

  # Run the GC_chart function
  chart <- GC_chart(gbk_file)

  # Assertions
  expect_s3_class(chart, "htmlwidget")
  expect_true("strand" %in% names(chart$x$data))
  expect_true("cluster" %in% names(chart$x$data))
})

test_that("Read FASTA from file path", {

  # Use the actual FASTA file
  file_path <- system.file("extdata", "BGC0000108.fasta", package = "geneviewer")
  fasta_df <- geneviewer::read_fasta(file_path, sequence = FALSE)

  # Run the GC_chart function
  chart <- GC_chart(fasta_df)

  # Assertions
  expect_s3_class(chart, "htmlwidget")
  expect_true("strand" %in% names(chart$x$data))
  expect_true("cluster" %in% names(chart$x$data))
})

test_that("Read .gff3 from file path", {
  # Use the actual .gff3 file
  file_path <- system.file("extdata", "HQ113105.1.gff3", package = "geneviewer")

  # Read the .gff3 file with all specified fields
  gff_df <- geneviewer::read_gff(
    file_path,
    fields = c("source", "type", "start", "end", "strand", "Name")
  )

  # Run the GC_chart function
  chart <- GC_chart(gff_df)

  # Assertions for the chart object
  expect_s3_class(chart, "htmlwidget")

  # Assertions for all required fields in the processed data
  expected_fields <- c("source", "type", "start", "end", "strand", "Name", "rowID")
  missing_fields <- setdiff(expected_fields, names(chart$x$data))
  expect_equal(length(missing_fields), 0, info = paste("Missing fields:", paste(missing_fields, collapse = ", ")))
})
