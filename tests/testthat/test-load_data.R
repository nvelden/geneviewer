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

test_that("read_fasta reads single sequences and values with spaces (B08)", {
  skip_if_not_installed("Biostrings")
  fasta_file <- tempfile(fileext = ".fasta")

  # A single sequence with a location
  writeLines(c(">seq1 [protein_id=ABC1.1] [location=complement(100..400)] [gbkey=CDS]", "MKTAYIAKQR"), fasta_file)
  fasta <- read_fasta(fasta_file, sequence = FALSE)
  expect_equal(nrow(fasta), 1)
  expect_equal(fasta$start, 100)
  expect_equal(fasta$end, 400)
  expect_equal(fasta$strand, "complement")

  # Values with spaces: NCBI brackets, no brackets and UniProt
  writeLines(c(
    ">lcl|X_1 [gene=sigD] [protein=putative RNA polymerase sigma factor] [protein_id=ADX66476.1] [location=3301..3870]", "MKTAYIAKQR",
    ">lcl|X_2 [protein=ScnRII] [protein_id=ADX66474.1] [location=complement(5576..6154)]", "MSDNELLKRL"
  ), fasta_file)
  fasta <- read_fasta(fasta_file, sequence = FALSE)
  expect_equal(fasta$protein, c("putative RNA polymerase sigma factor", "ScnRII"))
  expect_equal(fasta$gene, c("sigD", NA))

  writeLines(c(">X_1 protein=putative phosphoenolpyruvate synthase protein_id=ADX66475.1 location=complement(<1..2247)", "MKTAYIAKQR"), fasta_file)
  fasta <- read_fasta(fasta_file, sequence = FALSE)
  expect_equal(fasta$protein, "putative phosphoenolpyruvate synthase")
  expect_equal(fasta$protein_id, "ADX66475.1")

  writeLines(c(">sp|Q9NWW9|PLAT2_HUMAN Phospholipase A and acyltransferase 2 OS=Homo sapiens OX=9606 GN=PLAAT2 PE=1 SV=1", "MKTAYIAKQR"), fasta_file)
  fasta <- read_fasta(fasta_file, sequence = FALSE)
  expect_equal(fasta$OS, "Homo sapiens")
  expect_equal(fasta$GN, "PLAAT2")
})

test_that("read_fasta keeps square brackets inside values (B08)", {
  skip_if_not_installed("Biostrings")
  fasta_file <- tempfile(fileext = ".fasta")
  writeLines(c(
    ">no keys in this header", "MKTAYIAKQR",
    ">X_1 [protein=3-oxoacyl-[acyl-carrier-protein] reductase FabG] [protein_id=NP_1.1] [location=1..300]", "MKTAYIAKQR",
    ">X_2 [protein=autoinhibited H[+]-ATPase isoform 10] [protein_id=NP_2.1] [location=400..900]", "MSDNELLKRL",
    ">sp|Q1|FIP1_ARATH FIP1 OS=Arabidopsis thaliana OX=3702 GN=FIP1[V] PE=1 SV=1", "MSDNELLKRL"
  ), fasta_file)
  fasta <- suppressWarnings(read_fasta(fasta_file, sequence = FALSE))
  expect_equal(fasta$protein[1:2], c("3-oxoacyl-[acyl-carrier-protein] reductase FabG", "autoinhibited H[+]-ATPase isoform 10"))
  expect_equal(fasta$protein_id[1:2], c("NP_1.1", "NP_2.1"))
  expect_equal(fasta$GN[3], "FIP1[V]")
})

test_that("read_fasta uses the full span of join() locations (B08)", {
  skip_if_not_installed("Biostrings")
  fasta_file <- tempfile(fileext = ".fasta")
  writeLines(c(
    ">X_1 [gene=ORF1ab] [location=join(266..13468,13468..21555)]", "MKTAYIAKQR",
    ">X_2 [gene=ARV1] [location=complement(join(6915..7069,7157..7232,8571..8666))]", "MSDNELLKRL",
    ">X_3 [gene=abc] [location=complement(<1..2247)]", "MSDNELLKRL",
    ">X_4 [gene=ori] [location=join(30883..31317,1..108)]", "MSDNELLKRL"
  ), fasta_file)
  fasta <- read_fasta(fasta_file, sequence = FALSE)
  expect_equal(fasta$start, c(266, 6915, 1, 30883))
  expect_equal(fasta$end, c(21555, 8666, 2247, 31317))
  expect_equal(fasta$strand, c("forward", "complement", "complement", "forward"))
})

test_that("genbank_to_fasta writes one FASTA record per GenBank record (B19)", {
  record <- function(id, seq) c(
    sprintf("LOCUS       %s                 20 bp    DNA     linear   BCT 01-JAN-2024", id),
    sprintf("DEFINITION  Record %s.", id),
    sprintf("VERSION     %s.1", id),
    "FEATURES             Location/Qualifiers",
    "ORIGIN",
    sprintf("        1 %s", seq),
    "//")
  gbk_file <- tempfile(fileext = ".gbk")
  writeLines(c(record("AAA", "acgtacgtac gtacgtacgt"), record("BBB", "ttttgggggc ccccaaaaat")), gbk_file)

  out_dir <- tempfile(); dir.create(out_dir)
  genbank_to_fasta(gbk_file, output_dir = out_dir)
  fasta <- readLines(file.path(out_dir, sub("\\.gbk$", ".fasta", basename(gbk_file))))
  expect_equal(fasta, c(">AAA.1 Record AAA", "ACGTACGTACGTACGTACGT", ">BBB.1 Record BBB", "TTTTGGGGGCCCCCAAAAAT"))

  # Single record: unchanged format
  writeLines(record("AAA", "acgtacgtac gtacgtacgt"), gbk_file)
  genbank_to_fasta(gbk_file, output_dir = out_dir)
  expect_equal(readLines(file.path(out_dir, sub("\\.gbk$", ".fasta", basename(gbk_file)))),
               c(">AAA.1 Record AAA", "ACGTACGTACGTACGTACGT"))
})

test_that("mummer_alignment matches files exactly and only removes its own files (B20)", {
  skip_if(Sys.which("nucmer") == "", "MUMmer is not installed")
  set.seed(1)
  seq1 <- paste(sample(c("A", "C", "G", "T"), 3000, TRUE), collapse = "")
  seq2 <- paste0(substr(seq1, 1, 1500), paste(sample(c("A", "C", "G", "T"), 200, TRUE), collapse = ""), substr(seq1, 1501, 3000))
  dir <- tempfile("mummer"); dir.create(dir)
  writeLines(c(">a", seq1), file.path(dir, "BGC1+2.fasta"))
  writeLines(c(">b", seq2), file.path(dir, "BGC2.fasta"))
  writeLines("my notes", file.path(dir, "BGC1+2_BGC2_notes.txt"))

  links <- suppressWarnings(mummer_alignment(dir, cluster = c("BGC1+2", "BGC2"), seqtype = "nucleotide", output_dir = dir))
  expect_true(is.data.frame(links) && nrow(links) > 0)
  expect_true(file.exists(file.path(dir, "BGC1+2_BGC2_notes.txt")))
  expect_false(any(grepl("\\.(delta|coords)$", list.files(dir))))

  expect_error(mummer_alignment(dir, cluster = c("BGC 1", "BGC2"), output_dir = dir), "without spaces")
})

test_that("read_gbk only adds translation to features that have one (B22)", {
  features <- read_gbk(system.file("extdata", "BGC0000001.gbk", package = "geneviewer"))[[1]]$FEATURES
  expect_false(any(vapply(features$gene, function(x) "translation" %in% names(x), logical(1))))
  expect_true(all(vapply(features$CDS, function(x) !is.na(x[["translation"]]), logical(1))))
  expect_false(grepl("[^A-Za-z]", features$CDS[[1]][["translation"]]))
})
