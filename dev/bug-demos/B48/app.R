# B48: read_gff() and repeated keys / GTF (GFF2) files
# Run from the package folder: shiny::runApp("dev/bug-demos/B48")
suppressMessages(devtools::load_all(
  rprojroot::find_package_root_file(path = getwd()), quiet = TRUE))
library(shiny)

dir <- file.path(tempdir(), "b48")
dir.create(dir, showWarnings = FALSE)

# 1. GFF3 with one key repeated in a row -----------------------------------
gff3_lines <- c(
  "##gff-version 3",
  "chr1\tdemo\tgene\t100\t900\t.\t+\t.\tID=g1;Name=abcA;Dbxref=GeneID:1;Dbxref=UniProt:P1",
  "chr1\tdemo\tgene\t1000\t1900\t.\t-\t.\tID=g2;Name=abcB;Dbxref=GeneID:2",
  "chr1\tdemo\tgene\t2000\t2600\t.\t+\t.\tID=g3;Name=abcC"
)
gff3_file <- file.path(dir, "repeat.gff3")
writeLines(gff3_lines, gff3_file)

warn1 <- NULL
gff3 <- withCallingHandlers(read_gff(gff3_file), warning = function(w) {
  warn1 <<- conditionMessage(w); invokeRestart("muffleWarning")
})
csv1 <- tryCatch({ write.csv(gff3, tempfile()); "written" },
                 error = function(e) paste("Error:", conditionMessage(e)))

# 2. GTF (GFF2) attributes: key "value"; ----------------------------------
gtf_lines <- c(
  "chr1\tdemo\tgene\t100\t900\t.\t+\t.\tgene_id \"g1\"; gene_name \"abcA\";",
  "chr1\tdemo\tgene\t1000\t1900\t.\t-\t.\tgene_id \"g2\"; gene_name \"abcB\";",
  "chr1\tdemo\tgene\t2000\t2600\t.\t+\t.\tgene_id \"g3\"; gene_name \"abcC\"; tag \"basic\"; tag \"CCDS\";"
)
gtf_file <- file.path(dir, "genes.gtf")
writeLines(gtf_lines, gtf_file)
gtf <- read_gff(gtf_file)
chart2_error <- tryCatch({ GC_chart(gtf, group = "gene_name"); NULL },
                         error = function(e) conditionMessage(e))

# 3. A folder with the GTF file: not found ---------------------------------
gtf_dir <- file.path(dir, "gtf_only")
dir.create(gtf_dir, showWarnings = FALSE)
file.copy(gtf_file, gtf_dir, overwrite = TRUE)
dir_result <- tryCatch(capture.output(str(read_gff(gtf_dir))),
                       error = function(e) paste("Error:", conditionMessage(e)))

source_lines <- function(file, from, to) {
  lines <- readLines(file.path(pkg_root, file))[from:to]
  paste(sprintf("%s:%d  %s", file, from:to, lines), collapse = "\n")
}
pkg_root <- rprojroot::find_package_root_file(path = getwd())

code <- function(...) pre(paste(c(...), collapse = "\n"))
print_of <- function(x) paste(capture.output(x), collapse = "\n")

ui <- fluidPage(
  tags$style("pre { font-size: 12px; } h3 { margin-top: 30px; }"),
  titlePanel("B48: read_gff() with repeated keys and GTF files"),
  p("geneviewer on branch m6-js-library, loaded with devtools::load_all(). ",
    "Nothing in R/ is changed."),

  h3("1. A key repeated in one GFF3 row"),
  p("The file (gene g1 has Dbxref twice):"), code(gff3_lines),
  code('gff3 <- read_gff("repeat.gff3")'),
  p(strong("Warning: "), code(if (is.null(warn1)) "(none)" else warn1)),
  p("Every attribute column becomes a list column, ID and Name too, ",
    "not only Dbxref. A gene without Dbxref gets NULL:"),
  code(print_of(str(gff3))),
  p("A list column cannot be written to CSV:"),
  code('write.csv(gff3, "genes.csv")', csv1),
  p("The chart still draws. The tooltip shows both values for g1 and an ",
    "empty value for g3:"),
  code('GC_chart(gff3, group = "Name", height = "160px") %>%',
       '  GC_labels("Name") %>%',
       '  GC_tooltip(formatter = "<b>{Name}</b><br>Dbxref: {Dbxref}")'),
  GC_chartOutput("chart1", height = "160px"),

  h3("2. A GTF (GFF2) file"),
  p("GTF writes attributes as key \"value\"; pairs:"), code(gtf_lines),
  code('gtf <- read_gff("genes.gtf")'),
  p("read_gff() only splits pairs at '=': each whole pair becomes a column ",
    "name, and every value is NA:"),
  code(print_of(str(gtf))),
  code('GC_chart(gtf, group = "gene_name")',
       if (is.null(chart2_error)) "(no error)" else paste("Error:", chart2_error)),
  p("Without the attributes, the chart has no names or groups:"),
  code('GC_chart(gtf, height = "160px")'),
  GC_chartOutput("chart2", height = "160px"),

  h3("3. A folder of GTF files"),
  code('read_gff("gtf_only/")   # holds genes.gtf', dir_result),

  h3("The source lines"),
  p("Repeated keys: widen_attributes() makes every column a list when any ",
    "cell has two values:"),
  code(source_lines("R/widen_attributes.R", 18, 30)),
  p("Attributes are split at '=' only (GFF3):"),
  code(source_lines("R/read_gff.R", 105, 113)),
  p("Folders are searched for .gff and .gff3 only:"),
  code(source_lines("R/read_gff.R", 37, 37)),

  h3("Fix options (for Niels)"),
  tags$ol(
    tags$li(strong("Repeated keys: "),
            "(a) join the values with ',' into one text column, as GFF3 writes ",
            "several values of one key (Dbxref=GeneID:1,UniProt:P1 already reads as ",
            "\"GeneID:1,UniProt:P1\"); other columns stay text; no warning. ",
            "(b) keep the first value. (c) keep a list column for the repeated key only. ",
            "Recommended: (a)."),
    tags$li(strong("GTF/GFF2: "),
            "parse key \"value\" pairs when a row's attributes have no '=' ",
            "(quotes removed, a repeated key such as tag handled as in 1), and ",
            "read .gtf and .gff2 files from folders too."),
    tags$li(strong("Scope: "),
            "read_fasta() shares widen_attributes() and keeps list columns for ",
            "repeated [key=value] fields. Fix it the same way, or leave it?")
  )
)

server <- function(input, output, session) {
  output$chart1 <- renderGC_chart(
    GC_chart(gff3, group = "Name", height = "160px") %>%
      GC_labels("Name") %>%
      GC_tooltip(formatter = "<b>{Name}</b><br>Dbxref: {Dbxref}")
  )
  output$chart2 <- renderGC_chart(
    if (is.null(chart2_error)) {
      GC_chart(gtf, group = "gene_name", height = "160px") %>% GC_labels("gene_name")
    } else {
      GC_chart(gtf, height = "160px")
    }
  )
}

shinyApp(ui, server)
