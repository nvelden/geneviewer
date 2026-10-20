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

test_that("read_gbk only starts features on feature key lines (B03)", {
  gbk_lines <- c(
    "LOCUS       DEMO01                   600 bp    DNA     linear   BCT 01-JAN-2024",
    "FEATURES             Location/Qualifiers",
    "     source          1..600",
    "     gene            10..300",
    "                     /gene=\"abcA\"",
    "     CDS             10..300",
    "                     /gene=\"abcA\"",
    "                     /note=\"part of a biosynthetic",
    "                     gene cluster for a polyketide\"",
    "                     /locus_tag=\"DEMO_0001\"",
    "     CDS_motif       20..80",
    "                     /note=\"motif\"",
    "     gene            350..580",
    "                     /gene=\"abcB\"",
    "     CDS             350..580",
    "                     /gene=\"abcB\"",
    "ORIGIN",
    "        1 atgaaacgca",
    "//"
  )
  gbk_file <- tempfile(fileext = ".gbk")
  writeLines(gbk_lines, gbk_file)

  features <- read_gbk(gbk_file)[[1]]$FEATURES
  expect_length(features$gene, 2)
  expect_length(features$CDS, 2)
  expect_equal(unname(sapply(features$gene, `[[`, "region")), c("10..300", "350..580"))
  expect_equal(unname(features$CDS[[1]]["locus_tag"]), "DEMO_0001")

  # Packaged example: 28 CDS and 27 gene features (also has CDS_motif features)
  gbk_file <- system.file("extdata", "BGC0000001.gbk", package = "geneviewer")
  features <- read_gbk(gbk_file)[[1]]$FEATURES
  expect_length(features$CDS, 28)
  expect_length(features$gene, 27)
})

test_that("read_gbk keeps qualifier values intact (B04)", {
  gbk_lines <- c(
    "LOCUS       DEMO01                   600 bp    DNA     linear   BCT 01-JAN-2024",
    "FEATURES             Location/Qualifiers",
    "     CDS             10..300",
    "                     /locus_tag=\"DEMO_0001\"",
    "                     /product=\"4'-phosphopantetheinyl transferase (PPTase)\"",
    "                     /note=\"[pseudo] see ref. 12.\"",
    "                     /codon_start=1",
    "     CDS             350..580",
    "                     /locus_tag=\"DEMO_0002\"",
    "                     /note=\"similar to /gene=abcX in other",
    "                     strains\"",
    "                     /pseudo=\"\"",
    "                     /function=\"says \"\"hello\"\"\"",
    "                     /product=\"(R)-3-hydroxybutyryl-CoA dehydrogenase\"",
    "ORIGIN",
    "        1 atgaaacgca",
    "//"
  )
  gbk_file <- tempfile(fileext = ".gbk")
  writeLines(gbk_lines, gbk_file)

  cds <- read_gbk(gbk_file)[[1]]$FEATURES$CDS
  expect_equal(unname(cds[[1]]["product"]), "4'-phosphopantetheinyl transferase (PPTase)")
  expect_equal(unname(cds[[1]]["note"]), "[pseudo] see ref. 12.")
  expect_equal(unname(cds[[1]]["codon_start"]), "1")
  expect_equal(unname(cds[[2]]["note"]), "similar to /gene=abcX in other strains")
  expect_false("gene" %in% names(cds[[2]]))
  expect_equal(unname(cds[[2]]["pseudo"]), "")
  expect_equal(unname(cds[[2]]["function"]), "says \"hello\"")
  expect_equal(unname(cds[[2]]["product"]), "(R)-3-hydroxybutyryl-CoA dehydrogenase")
})

test_that("read_bed converts BED coordinates like rtracklayer (B05)", {
  bed_file <- tempfile(fileext = ".bed")
  writeLines(c(
    "chr1\t1000\t5000\ttx1\t0\t+\t1200\t4800\t0\t3\t500,400,600\t0,2000,3400",
    "chr1\t6000\t7000\ttx2\t0\t-\t6000\t7000\t0\t3\t300,200,100\t0,500,900"
  ), bed_file)

  bed <- read_bed(bed_file)
  expect_equal(bed$chromStart, c(1001, 6001))
  expect_equal(bed$chromEnd, c(5000, 7000))
  expect_equal(bed$thickStart, c(1201, 6001))
  expect_equal(bed$thickEnd, c(4800, 7000))
  # Same number of blocks in every row: still one list element per row
  expect_type(bed$blockSizes, "list")
  expect_equal(bed$blockSizes[[1]], c(500, 400, 600))
  expect_equal(bed$blockStarts[[1]], c(1, 2001, 3401))
  expect_equal(bed$blockSizes[[2]], c(300, 200, 100))

  # A single BED12 row
  writeLines("chr1\t0\t100\ta\t0\t+\t0\t100\t0\t2\t10,20\t0,80", bed_file)
  bed <- read_bed(bed_file)
  expect_equal(bed$chromEnd, 100)
  expect_equal(bed$blockSizes[[1]], c(10, 20))
  expect_equal(bed$blockStarts[[1]], c(1, 81))
})

test_that("read_gff parses attributes robustly (B07)", {
  row <- function(attr, start = 100, end = 500) {
    paste("chr1", "demo", "gene", start, end, ".", "+", ".", attr, sep = "\t")
  }
  gff_file <- tempfile(fileext = ".gff3")
  writeLines(c(
    "##gff-version 3",
    row("ID=g1;Name=abc%2Cdef;Note=ratio a=b"),
    row("", 600, 900),
    row(".", 1000, 1200),
    row("ID=g4; Name=x;description=WD40 %26#946%3B propellers;gene_id=G4", 1300, 1500),
    row("ID=g5;Note=50% GC", 1600, 1700),
    "##FASTA",
    ">chr1",
    "ACGTACGT"
  ), gff_file)

  gff <- read_gff(gff_file)
  expect_equal(nrow(gff), 5)
  expect_equal(gff$start, c(100, 600, 1000, 1300, 1600))
  expect_setequal(names(gff), c("seqid", "source", "type", "start", "end", "score",
                                "strand", "phase", "ID", "Name", "Note", "description", "gene_id"))
  expect_equal(gff$ID, c("g1", NA, NA, "g4", "g5"))
  expect_equal(gff$Name, c("abc,def", NA, NA, "x", NA))
  expect_equal(gff$Note, c("ratio a=b", NA, NA, NA, "50% GC"))
  expect_equal(gff$description[4], "WD40 &#946; propellers")
  expect_equal(gff$gene_id[4], "G4")
})
