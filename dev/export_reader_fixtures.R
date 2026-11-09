# Export the reader fixtures for the Python package (geneviewer-py)
#
# Usage, from the package root:
#
#   Rscript dev/export_reader_fixtures.R <geneviewer-py>/tests/readers
#
# For each case below, writes its input to <folder>/inputs/ (the files of
# inst/extdata, and the inputs the tests in tests/testthat write inline) and
# runs the R reader on it from <folder>, so file paths in messages are the
# same relative paths Python's tests use. The result goes to
# <folder>/expected/<case>.json:
#
#   calls     the functions and their arguments, applied in turn; the first
#             gets `input` (a path, relative to <folder>) or `data` (a table)
#   result    a table (columns with their R type, and values by column),
#             the list of read_gbk() (features as [name, value] pairs), or
#             the lines of the file genbank_to_fasta() writes
#   warnings  the warning messages, in order
#   error     the error message, or null
#
# Types: character, integer, numeric, logical, factor (with levels) and
# list (of numeric vectors). NA and NaN are null.
#
# Also protein_blast() and mummer_alignment(), for the alignment of the
# Python package (the mummer-* cases need MUMmer in the PATH).

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1 || !dir.exists(args[[1]])) {
  stop("Usage: Rscript dev/export_reader_fixtures.R <geneviewer-py>/tests/readers", call. = FALSE)
}
out <- normalizePath(args[[1]])
# Messages in English, as Python writes them
Sys.setenv(LANGUAGE = "en")
pkg <- normalizePath(".")
suppressMessages(pkgload::load_all(pkg, quiet = TRUE))

inputs <- file.path(out, "inputs")
expected <- file.path(out, "expected")
unlink(c(inputs, expected), recursive = TRUE)
dir.create(inputs)
dir.create(expected)

# Inputs ----------------------------------------------------------------------

put <- function(name, lines, eol = "\n") {
  file <- file.path(inputs, name)
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  con <- if (endsWith(name, ".gz")) gzfile(file, "wb") else file(file, "wb")
  writeBin(charToRaw(paste0(lines, eol, collapse = "")), con)
  close(con)
  invisible(file.path("inputs", name))
}
extdata <- function(name) {
  file.copy(system.file("extdata", name, package = "geneviewer"), file.path(inputs, name))
  invisible(file.path("inputs", name))
}

gbk_record <- function(id, seq) c(
  sprintf("LOCUS       %s                 20 bp    DNA     linear   BCT 01-JAN-2024", id),
  sprintf("DEFINITION  Record %s.", id),
  sprintf("VERSION     %s.1", id),
  "FEATURES             Location/Qualifiers",
  "ORIGIN",
  sprintf("        1 %s", seq),
  "//")

gbk_b03 <- c(
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
  "//")
gbk_b04 <- c(
  "LOCUS       DEMO01                   600 bp    DNA     linear   BCT 01-JAN-2024",
  "FEATURES             Location/Qualifiers",
  "     CDS             10..300",
  "                     /locus_tag=\"DEMO_0001\"",
  "                     /product=\"4'-phosphopantetheinyl transferase (PPTase)\"",
  "                     /note=\"[pseudo] see ref. 12.\"",
  "                     /codon_start=1",
  "                     /db_xref=\"GI:1\"",
  "                     /db_xref=\"UniProtKB:Q1\"",
  "     CDS             350..580",
  "                     /locus_tag=\"DEMO_0002\"",
  "                     /note=\"similar to /gene=abcX in other",
  "                     strains\"",
  "                     /pseudo=\"\"",
  "                     /function=\"says \"\"hello\"\"\"",
  "                     /product=\"(R)-3-hydroxybutyryl-CoA dehydrogenase\"",
  "ORIGIN",
  "        1 atgaaacgca",
  "//")
gbk_b57 <- c(
  "LOCUS       DEMO01                 32000 bp    DNA     linear   PLN 01-JAN-2024",
  "FEATURES             Location/Qualifiers",
  "     CDS             join(complement(5441..5600),complement(5095..5373),",
  "                     complement(5024..5040))",
  "                     /locus_tag=\"DEMO_0001\"",
  "     CDS             complement(join(6915..7069,7157..7232))",
  "                     /locus_tag=\"DEMO_0002\"",
  "     CDS             join(<10..120,200..>300)",
  "                     /locus_tag=\"DEMO_0003\"",
  "     CDS             join(30883..31317,1..108)",
  "                     /locus_tag=\"DEMO_0004\"",
  "     CDS             complement(120..920)",
  "                     /locus_tag=\"DEMO_0005\"",
  "ORIGIN",
  "        1 atgaaacgca",
  "//")

gff_row <- function(attr, start = 100, end = 500, type = "gene", score = ".", phase = ".") {
  paste("chr1", "demo", type, start, end, score, "+", phase, attr, sep = "\t")
}
gtf_lines <- c(
  "chr1\tdemo\tgene\t100\t900\t.\t+\t.\tgene_id \"g1\"; gene_name \"abcA\";",
  "chr1\tdemo\texon\t100\t400\t.\t+\t0\tgene_id \"g1\"; transcript_id \"t1\"; tag \"basic\"; tag \"CCDS\";",
  "chr1\tdemo\tgene\t1000\t1900\t.\t-\t.\tgene_id \"g2\";  gene_name \"a b\"",
  "chr1\tdemo\tgene\t2000\t2600\t.\t+\t.\tgene_id g3; level 2; flag"
)

fasta <- list(
  ncbi_cds = c(
    ">lcl|NC_000913.3_prot_NP_414542.1_1 [gene=thrL] [locus_tag=b0001] [protein=thr operon leader peptide] [protein_id=NP_414542.1] [location=190..255] [gbkey=CDS]",
    "MKRISTTITTTITITTGNGAG",
    ">lcl|NC_045512.2_prot_YP_009724389.1_1 [gene=ORF1ab] [protein=ORF1ab polyprotein] [exception=ribosomal slippage] [location=join(266..13468,13468..21555)] [gbkey=CDS]",
    "MESLVPGFNEKTHVQLSLPVLQVRDVLVRGFGDSVEEVLSEARQHLKDGTCGLVEVEKGVLPQLEQPYVF",
    "IKRSDARTAPHGHVMVELVAELEGIQYGRSGETLGVLVPHVGEIPVAYRKVLLRKNGNKG",
    ">lcl|NC_001133.9_prot_NP_009332.1_1 [gene=PAU8] [protein=seripauperin PAU8] [location=complement(1807..2169)] [gbkey=CDS]",
    "MVKLTSIAAGVAAIAATASATTTLAQSDERVNLVELGVYVSDIR"),
  ncbi_unbracketed = c(
    ">lcl|HQ3864.1_prot_ADX475.1_1 protein=putative phosphoenolpyruvate synthase protein_id=ADX66475.1 location=complement(<1..2247) gbkey=CDS",
    "MRATGLVRGRAAKRFGRAGGAGDAIGQCRATGHDCLGGSAMIEQYVWDLHEVDETQVAVVGGKGAHLGGL",
    "SRIEGIRVPAG"),
  uniprot = c(
    ">sp|P04637|P53_HUMAN Cellular tumor antigen p53 OS=Homo sapiens OX=9606 GN=TP53 PE=1 SV=4",
    "MEEPQSDPSVEPPLSQETFSDLWKLLPENNVLSPLPSQAMDDLMLSPDDIEQWFTEDPGP",
    ">sp|P04637-2|P53_HUMAN Isoform 2 of Cellular tumor antigen p53 OS=Homo sapiens OX=9606 GN=TP53",
    "MEEPQSDPSVEPPLSQETFSDLWKLLPENNVLSPLPSQAMDDLMLSPDDIEQWFTEDPGP"),
  ncbi_protein = c(
    ">WP_000003984.1 HD domain-containing protein [Vibrio sp. RC586]",
    "MSERLAQQMTLLMELDRLKSVLRRTRVKSAEGRLENSAEHSWHVALMAILLQEHANEAVDMQKVLTMLLL",
    "",
    ">NP_414542.1 thr operon leader peptide [Escherichia coli str. K-12 substr. MG1655]",
    "MKRISTTITTTITITTGNGAG"),
  pipes = c(
    ">BGC0000001.5|c1|1-1083|+|AEK75490.1|protein_methyltransferase|AEK75490.1",
    "MTELDRAFDAVPAPIYTHHERHGETVHRSAPESIRRELAALQVRAGDRVLEIGTGSGYSGALLAHL"),
  nucleotide = c(
    ">lcl|NC_045512.2_cds_YP_009725255.1_12 [gene=ORF10] [protein=ORF10 protein] [location=29558..29674] [gbkey=CDS]",
    "ATGGGCTATATAAACGTTTTCGCTTTTCCGTTTACGATATATAGTCTACTCTTGTGCAGAATGAATTCTC",
    "GTAACTACATAGCACAAGTAGATGTAGTTAACTTTAATCTCACATAG"),
  alignment = c(
    ">seq1 [gene=a] [location=1..30]", "MKT-AY..IAKQR--",
    ">seq2 [gene=b] [location=40..70]", "mkt-ay..iakqr--"),
  messy = c(
    ";comment before the first record", "",
    ">a [gene=x] [location=1..9]  ", "mkta yi", ";comment inside a sequence", "AKQR*", "",
    ">b [gene=y] [location=20..29]",
    ">c [gene=z] [location=complement(40..49)]",
    "      1 mktayiakqr qisfvkshfs", "     21 rqleer"),
  b08_single = c(">seq1 [protein_id=ABC1.1] [location=complement(100..400)] [gbkey=CDS]", "MKTAYIAKQR"),
  b08_spaces = c(
    ">lcl|X_1 [gene=sigD] [protein=putative RNA polymerase sigma factor] [protein_id=ADX66476.1] [location=3301..3870]", "MKTAYIAKQR",
    ">lcl|X_2 [protein=ScnRII] [protein_id=ADX66474.1] [location=complement(5576..6154)]", "MSDNELLKRL"),
  b08_brackets = c(
    ">no keys in this header", "MKTAYIAKQR",
    ">X_1 [protein=3-oxoacyl-[acyl-carrier-protein] reductase FabG] [protein_id=NP_1.1] [location=1..300]", "MKTAYIAKQR",
    ">X_2 [protein=autoinhibited H[+]-ATPase isoform 10] [protein_id=NP_2.1] [location=400..900]", "MSDNELLKRL",
    ">sp|Q1|FIP1_ARATH FIP1 OS=Arabidopsis thaliana OX=3702 GN=FIP1[V] PE=1 SV=1", "MSDNELLKRL"),
  b08_join = c(
    ">X_1 [gene=ORF1ab] [location=join(266..13468,13468..21555)]", "MKTAYIAKQR",
    ">X_2 [gene=ARV1] [location=complement(join(6915..7069,7157..7232,8571..8666))]", "MSDNELLKRL",
    ">X_3 [gene=abc] [location=complement(<1..2247)]", "MSDNELLKRL",
    ">X_4 [gene=ori] [location=join(30883..31317,1..108)]", "MSDNELLKRL",
    ">X_5 [gene=BDH2] [location=join(complement(5441..5600),complement(5095..5373))]", "MSDNELLKRL"),
  repeated = c(">s1 [gene=a] [note=first] [note=second]", "MKT", ">s2 [gene=b] [note=NA]", "MAA"),
  id_key = c(">s1 [id=custom1] [gene=a]", "MKT", ">s2 [gene=b]", "MAA", ">s3 [id=custom3] [gene=c]", "MAG"),
  no_location = c(">s1 [gene=a] [location=1..9]", "MKT", ">s2 [gene=b]", "MAA"),
  no_header = c("MKT", ">a [gene=x]", "MKT")
)

bed12 <- c(
  "chr1\t1000\t5000\ttx1\t0\t+\t1200\t4800\t0\t3\t500,400,600\t0,2000,3400",
  "chr1\t6000\t7000\ttx2\t0\t-\t6000\t7000\t0\t3\t300,200,100\t0,500,900")

# Cases -----------------------------------------------------------------------

table_json <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, optional = TRUE)
  columns <- lapply(names(df), function(name) {
    x <- df[[name]]
    type <- if (is.factor(x)) "factor" else if (is.list(x)) "list" else class(x)[1]
    column <- list(name = name, type = type)
    if (is.factor(x)) column$levels <- I(levels(x))
    column
  })
  values <- lapply(df, function(x) {
    if (is.factor(x)) x <- as.character(x)
    if (is.list(x)) lapply(x, function(v) if (is.null(v)) NULL else I(v)) else I(x)
  })
  list(columns = columns, data = stats::setNames(unname(values), names(df)))
}

gbk_json <- function(gbk) {
  lapply(gbk, function(record) {
    lapply(stats::setNames(names(record), names(record)), function(section) {
      value <- record[[section]]
      if (section != "FEATURES") return(value)
      lapply(value, function(features) {
        if (is.null(features)) return(NULL)
        lapply(features, function(x) lapply(seq_along(x), function(i) list(names(x)[i], unname(x[[i]]))))
      })
    })
  })
}

cases <- list()
add <- function(name, calls, input = NULL, data = NULL) {
  cases[[name]] <<- list(calls = calls, input = input, data = data)
}
call <- function(fun, ...) list(fun = fun, args = list(...))

gbk1 <- extdata("BGC0000001.gbk")
add("gbk-BGC0000001", list(call("read_gbk")), gbk1)
add("gbk-BGC0000001-cds", list(call("read_gbk", sections = c("LOCUS", "DEFINITION", "FEATURES"), features = "CDS", origin = FALSE)), gbk1)
add("gbk-BGC0000001-missing", list(call("read_gbk", sections = c("LOCUS", "NOPE"), features = c("CDS", "nope"))), gbk1)
add("gbk-no-features", list(call("read_gbk", sections = "LOCUS", features = "CDS")), gbk1)
add("gbk-B03", list(call("read_gbk")), put("B03.gbk", gbk_b03))
add("gbk-B04", list(call("read_gbk")), put("B04.gbk", gbk_b04))
add("gbk-multi", list(call("read_gbk")), put("multi.gbk", c(gbk_record("AAA", "acgtacgtac gtacgtacgt"), gbk_record("BBB", "ttttgggggc ccccaaaaat"))))
put("gbk-folder/B03.gbk", gbk_b03)
add("gbk-folder", list(call("read_gbk")), put("gbk-folder/B04.gb", gbk_b04) |> dirname())
add("gbkdf-BGC0000001", list(call("read_gbk"), call("gbk_features_to_df")), gbk1)
add("gbkdf-BGC0000001-gene", list(call("read_gbk"), call("gbk_features_to_df", feature = "gene", keys = c("gene", "locus_tag", "region", "nokey"))), gbk1)
add("gbkdf-BGC0000001-noregion", list(call("read_gbk"), call("gbk_features_to_df", keys = c("protein_id", "region", "translation"), process_region = FALSE)), gbk1)
add("gbkdf-B57", list(call("read_gbk"), call("gbk_features_to_df", keys = c("region", "locus_tag"))), put("B57.gbk", gbk_b57))
add("gbkdf-B04", list(call("read_gbk"), call("gbk_features_to_df")), "inputs/B04.gbk")
add("gbkdf-B03-all", list(call("read_gbk"), call("gbk_features_to_df", feature = "CDS_motif")), "inputs/B03.gbk")
add("gbkdf-folder-missing", list(call("read_gbk"), call("gbk_features_to_df", feature = "tRNA")), "inputs/gbk-folder")
add("gbkdf-nokeys", list(call("read_gbk"), call("gbk_features_to_df", keys = "nokey")), "inputs/B04.gbk")
add("g2f-BGC0000001", list(call("genbank_to_fasta")), gbk1)
add("g2f-multi", list(call("genbank_to_fasta")), "inputs/multi.gbk")
add("g2f-single", list(call("genbank_to_fasta")), put("single.gbk", gbk_record("AAA", "acgtacgtac gtacgtacgt")))

gff1 <- extdata("HQ113105.1.gff3")
add("gff-HQ113105", list(call("read_gff")), gff1)
add("gff-HQ113105-fields", list(call("read_gff", fields = c("source", "type", "start", "end", "strand", "Name"))), gff1)
add("gff-B07", list(call("read_gff")), put("B07.gff3", c(
  "##gff-version 3",
  gff_row("ID=g1;Name=abc%2Cdef;Note=ratio a=b"),
  gff_row("", 600, 900),
  gff_row(".", 1000, 1200),
  gff_row("ID=g4; Name=x;description=WD40 %26#946%3B propellers;gene_id=G4", 1300, 1500),
  gff_row("ID=g5;Note=50% GC", 1600, 1700),
  "##FASTA", ">chr1", "ACGTACGT")))
add("gff-repeated", list(call("read_gff")), put("repeated.gff3", c(
  "##gff-version 3",
  "chr1\tdemo\tgene\t1\t20\t.\t+\t.\tID=g1;Note=a;Note=b",
  "chr1\tdemo\tgene\t30\t40\t.\t+\t.\t.",
  "chr1\tdemo\tgene\t50\t60\t.\t-\t.\tID=g3;Note=")))
add("gff-id", list(call("read_gff")), put("id.gff3", c(
  "chr1\tdemo\tgene\t1\t20\t.\t+\t.\tid=custom1;ID=standard1",
  "chr1\tdemo\tgene\t30\t40\t.\t+\t.\t.",
  "chr1\tdemo\tgene\t50\t60\t.\t-\t.\tid=custom3")))
add("gff-id-fields", list(call("read_gff", fields = "id")), "inputs/id.gff3")
add("gff-B48-repeat", list(call("read_gff")), put("B48.gff3", c(
  "##gff-version 3",
  "chr1\tdemo\tgene\t100\t900\t.\t+\t.\tID=g1;Name=abcA;Dbxref=GeneID:1;Dbxref=UniProt:P1",
  "chr1\tdemo\tgene\t1000\t1900\t.\t-\t.\tID=g2;Name=abcB;Dbxref=GeneID:2",
  "chr1\tdemo\tgene\t2000\t2600\t.\t+\t.\tID=g3;Name=abcC")))
add("gff-B48-gtf", list(call("read_gff")), put("B48.gtf", gtf_lines))
add("gff-numbers", list(call("read_gff")), put("numbers.gff3", c(
  gff_row("ID=a", score = "0.5", phase = "0"),
  gff_row("ID=b", 600, 900, score = "12", phase = "2"),
  "chr1\tdemo\tgene\t1000\t1200\t1e3\t-",
  "  ")))
put("gff-folder/genes.gtf", gtf_lines)
put("gff-folder/more.gff2", gtf_lines)
add("gff-folder", list(call("read_gff")), put("gff-folder/B48.gff3", c("##gff-version 3", gff_row("ID=g1;Name=abcA"))) |> dirname())
add("gff-folder-fields", list(call("read_gff", fields = c("type", "gene_id"))), "inputs/gff-folder")
add("gff-invalid-field", list(call("read_gff", fields = c("type", "nope", "nope2"))), gff1)
add("gff-empty-folder", list(call("read_gff")), put("no-gff/readme.txt", "no GFF files here") |> dirname())
add("gff-missing", list(call("read_gff")), "inputs/does-not-exist.gff3")

fasta1 <- extdata("BGC0000108.fasta")
add("fasta-BGC0000108", list(call("read_fasta")), fasta1)
add("fasta-BGC0000108-keys", list(call("read_fasta", sequence = FALSE, keys = c("protein_id", "start", "end", "strand", "nokey"))), fasta1)
for (name in names(fasta)) {
  add(paste0("fasta-", gsub("_", "-", name)), list(call("read_fasta")), put(paste0(name, ".fasta"), fasta[[name]]))
}
add("fasta-crlf", list(call("read_fasta")), put("crlf.fasta", fasta$ncbi_cds, eol = "\r\n"))
add("fasta-cr", list(call("read_fasta")), put("cr.fasta", fasta$ncbi_cds, eol = "\r"))
add("fasta-gzip", list(call("read_fasta")), put("uniprot.fasta.gz", fasta$uniprot))
add("fasta-no-final-newline", list(call("read_fasta")), put("no_final_newline.fasta", ">a [gene=x]\nMKT\n>b [gene=y]\nMAA", eol = ""))
add("fasta-empty", list(call("read_fasta")), put("empty.fasta", character()))
put("fasta-folder/b.fasta", fasta$alignment)
put("fasta-folder/c.fa", fasta$uniprot)
add("fasta-folder", list(call("read_fasta", sequence = FALSE)), put("fasta-folder/a.fasta", fasta$ncbi_cds) |> dirname())
add("fasta-folder-fa", list(call("read_fasta", file_extension = "fa")), "inputs/fasta-folder")
add("fasta-missing", list(call("read_fasta")), "inputs/does-not-exist.fasta")

add("bed-B05", list(call("read_bed")), put("B05.bed", bed12))
add("bed-B05-single", list(call("read_bed")), put("B05-single.bed", "chr1\t0\t100\ta\t0\t+\t0\t100\t0\t2\t10,20\t0,80"))
add("bed-track-names", list(call("read_bed")), put("track-names.bed", c(
  "chr1\t0\t10\tfirst", "chr1\t20\t30\ttrack_gene",
  "chr1\t40\t50\tlast#1", "track_chr\t60\t70\tbrowser_gene")))
add("bed-directives", list(call("read_bed")), put("directives.bed", c(
  "# BED example", "browser position chr1:1-100", "",
  "track name=first", "chr1\t0\t10\tfirst",
  "  # another track", "track name=second",
  "chr1\t20\t30\tsecond", "browser hide all")))
add("bed-6", list(call("read_bed")), put("six.bed", c("chr1\t100\t500\tgeneA\t0\t+", "chr1\t700\t1200\tgeneB\t0\t-")))
add("bed-single-blocks", list(call("read_bed")), put("single-blocks.bed", c(
  "chr1\t0\t100\ta\t0\t+\t0\t100\t0\t1\t100\t0",
  "chr1\t200\t300\tb\t0\t-\t200\t300\t0\t1\t100\t0")))
add("bed-trailing-commas", list(call("read_bed")), put("trailing-commas.bed", c(
  "chr1\t0\t100\ta\t0\t+\t0\t100\t0\t2\t10,20,\t0,80,")))
put("bed-folder/a.bed", bed12)
add("bed-folder", list(call("read_bed")), put("bed-folder/b.bed6", c("chr2\t5\t50\tx\t0\t+", "chr2\t60\t90\ty\t0\t-")) |> dirname())

introns <- list(
  example = data.frame(transcript = "tx1", type = "exon", start = c(100, 400, 900),
                       end = c(250, 600, 1100), strand = "forward"),
  adjacent = data.frame(start = c(1, 11, 15), end = c(10, 20, 30)),
  nested = data.frame(start = c(201, 20, 1, 90, 40), end = c(210, 30, 100, 120, 50),
                      type = c("exon", "UTR", "exon", "exon", "UTR")),
  strands = data.frame(start = c(90, 1, 10, 31), end = c(80, 10, 1, 40),
                       transcript = c("reverse", "forward", "reverse", "forward"),
                       strand = c("-", "+", "-", "+")),
  one_base = data.frame(start = c(1, 12), end = c(10, 20)),
  single = data.frame(start = 1, end = 10),
  integers = data.frame(start = c(1L, 30L, 60L), end = c(20L, 50L, 80L),
                        name = c("e1", "e2", "e3"), type = c("5'UTR", "CDS exon", "gene")),
  transcripts = data.frame(transcript = c("b", "a", "b", "a", "b"), start = c(300, 10, 100, 50, 500),
                           end = c(400, 20, 200, 60, 600), type = c("exon", "exon", "exon", "utr", "EXON"))
)
for (name in names(introns)) {
  add(paste0("introns-", gsub("_", "-", name)), list(call("get_introns")), data = introns[[name]])
}

# Alignment (protein_blast, mummer_alignment) ----------------------------------

blast_genes <- data.frame(
  protein_id = c("A1", "A2", "B1", "B2"), cluster = c("A", "A", "B", "B"),
  start = c(1, 200, 1, 200), end = c(150, 350, 150, 350),
  translation = c("MKTAYIAKQRQISFVKSHFSRQ", "MSDNELLKRLGEAVLAHPTTG",
                  "MKTAYIAKQRQISFVKSHFSRQ", "MLPGGRTSWVQEHCDKAYNIF")
)
add("blast-example", list(call("protein_blast", query = "A")), data = blast_genes)
add("blast-genes", list(call("protein_blast", query = "A", genes = "A1")), data = blast_genes)
add("blast-sequence", list(call("protein_blast", query = 1)), data = data.frame(
  protein_id = c("a", "b", "c", "d"), cluster = c(1, 1, 2, 2), start = c(1, 10, 1, 10),
  end = c(5, 15, 5, 15), sequence = c("MKVLLAW", "MKLGGTT", "MKVLLAW", "PPPPKLGG")))
add("blast-one-cluster", list(call("protein_blast", query = "BGC0000001")), gbk1)
add("blast-missing-query", list(call("protein_blast", query = "Z")), data = blast_genes)
add("blast-no-sequence", list(call("protein_blast", query = "A")), data = blast_genes[, 1:4])
add("blast-erythromycin", list(call("protein_blast", query = "BGC0000055")),
    data = erythromycin_BlastP[, c("protein_id", "region", "translation", "cluster",
                                   "strand", "start", "end")])

# Two clusters made from BGC0000001: its GenBank file, and its sequence
# rearranged (the second half first) with every 50th base changed
mm_fasta <- readLines(genbank_to_fasta(system.file("extdata", "BGC0000001.gbk", package = "geneviewer"),
                                       output_dir = tempdir()))
mm_seq <- strsplit(paste(mm_fasta[-1], collapse = ""), "")[[1]]
mm_n <- length(mm_seq)
mm_b <- c(mm_seq[(mm_n %/% 2 + 1):mm_n], mm_seq[1:(mm_n %/% 2)])
mm_at <- seq(25, mm_n, by = 50)
mm_b[mm_at] <- c(A = "C", C = "G", G = "T", T = "A")[mm_b[mm_at]]
dir.create(file.path(inputs, "mummer"))
file.copy(system.file("extdata", "BGC0000001.gbk", package = "geneviewer"), file.path(inputs, "mummer", "A.gbk"))
put("mummer/B.fasta", c(">B rearranged", paste(mm_b, collapse = "")))
put("mummer/C.fasta", c(">C", paste(mm_b[1:(mm_n %/% 3)], collapse = "")))
for (seqtype in c("nucleotide", "protein")) {
  for (maptype in c("many-to-many", "one-to-one")) {
    add(paste("mummer", seqtype, maptype, sep = "-"),
        list(call("mummer_alignment", cluster = c("A", "B", "C"), seqtype = seqtype, maptype = maptype)),
        "inputs/mummer")
  }
}
add("mummer-missing", list(call("mummer_alignment", cluster = c("A", "Z"))), "inputs/mummer")
add("mummer-one-cluster", list(call("mummer_alignment", cluster = "A")), "inputs/mummer")

# Run ---------------------------------------------------------------------------

setwd(out)
for (name in names(cases)) {
  case <- cases[[name]]
  warnings <- character()
  error <- NULL
  result <- tryCatch(withCallingHandlers({
    value <- if (is.null(case$data)) case$input else case$data
    for (step in case$calls) {
      args <- step$args
      if (step$fun == "genbank_to_fasta") args$output_dir <- tempdir()
      value <- do.call(step$fun, c(list(value), args))
    }
    value
  }, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(e) {
    error <<- conditionMessage(e)
    NULL
  })
  last <- case$calls[[length(case$calls)]]$fun
  if (!is.null(error)) {
    result <- NULL
  } else if (last == "read_gbk") {
    result <- gbk_json(result)
  } else if (last == "genbank_to_fasta") {
    result <- I(readLines(result))
  } else {
    result <- table_json(result)
  }
  calls <- lapply(case$calls, function(step) {
    vectors <- names(step$args) %in% c("keys", "fields", "sections", "features", "cluster", "genes")
    args <- step$args
    args[vectors] <- lapply(args[vectors], I)
    list(fun = step$fun, args = if (length(args)) args else structure(list(), names = character()))
  })
  json <- list(
    calls = calls,
    input = case$input,
    data = if (!is.null(case$data)) table_json(case$data),
    result = result,
    warnings = I(warnings),
    error = error
  )
  writeLines(jsonlite::toJSON(json, auto_unbox = TRUE, null = "null", na = "null",
                              digits = NA, pretty = TRUE),
             file.path(expected, paste0(name, ".json")))
  cat(sprintf("%-32s %s\n", name, if (!is.null(error)) paste("error:", error)
              else if (length(warnings)) paste(length(warnings), "warning(s)") else "ok"))
}
