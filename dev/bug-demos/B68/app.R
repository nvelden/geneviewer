# B68: GC_normalize() shrinks the cluster instead of spacing genes over its range
# Run from the package folder: shiny::runApp("dev/bug-demos/B68")
suppressMessages(devtools::load_all(
  rprojroot::find_package_root_file(path = getwd()), quiet = TRUE))
library(shiny)

pkg_root <- rprojroot::find_package_root_file(path = getwd())

# 1. Genes that are already evenly spaced: normalizing should change nothing
even <- data.frame(
  start = c(10, 50, 90), end = c(40, 80, 120),
  name = c("A", "B", "C"), cluster = "even"
)
# 2. The example of ?GC_normalize: gaps of 50 and 10
uneven <- data.frame(
  start = c(10, 90, 130), end = c(40, 120, 160),
  name = c("A", "B", "C"), cluster = "uneven"
)
genes <- rbind(even, uneven)

positions <- function(chart) {
  chart$x$spec$data[, c("cluster", "name", "start", "end")]
}

# What the docs describe: the same genes, the total gap shared by the gaps
# between them (n - 1), so the cluster keeps its range
expected_positions <- function(data, gap = NULL) {
  do.call(rbind, lapply(split(data, data$cluster), function(d) {
    d <- d[order(d$start), ]
    n <- nrow(d)
    length <- d$end - d$start
    range <- max(d$end) - min(d$start)
    each <- if (is.null(gap)) (range - sum(length)) / (n - 1) else range * gap
    d$start <- round(min(d$start) + cumsum(c(0, head(length + each, -1))))
    d$end <- d$start + length
    d[, c("cluster", "name", "start", "end")]
  }))
}

chart <- function(data, title) {
  GC_chart(data, cluster = "cluster", height = "220px") %>%
    GC_labels("name") %>%
    GC_scale(start = 0, end = 170) %>%
    GC_clusterLabel(title = unique(data$cluster)) %>%
    GC_title(title, height = "30px")
}

before <- chart(genes, "Not normalized")
now <- chart(genes, "GC_normalize() now") %>% GC_normalize()
now_gap <- GC_chart(genes, cluster = "cluster") %>% GC_normalize(gap = 0.1)

expected <- expected_positions(genes)
expected_data <- merge(genes[, c("name", "cluster")], expected, by = c("cluster", "name"))
expected_chart <- chart(expected_data, "Expected: the total gap shared by the n - 1 gaps")

gaps <- function(positions) {
  do.call(rbind, lapply(split(positions, positions$cluster), function(d) {
    d <- d[order(d$start), ]
    data.frame(cluster = d$cluster[1],
               gaps = paste(d$start[-1] - head(d$end, -1), collapse = ", "),
               range = paste(min(d$start), "to", max(d$end)))
  }))
}

source_lines <- function(file, from, to) {
  lines <- readLines(file.path(pkg_root, file))[from:to]
  paste(sprintf("%s:%d  %s", file, from:to, lines), collapse = "\n")
}
code <- function(...) pre(paste(c(...), collapse = "\n"))
print_of <- function(x) paste(capture.output(print(x, row.names = FALSE)), collapse = "\n")

ui <- fluidPage(
  tags$style("pre { font-size: 12px; } h3 { margin-top: 30px; }"),
  titlePanel("B68: GC_normalize() shrinks the cluster"),
  p("geneviewer on branch bugfixes, loaded with devtools::load_all(). ",
    "Nothing in R/ is changed."),
  p("?GC_normalize: \"ensuring that the genes are evenly spaced along the ",
    "entire range of the cluster\". The axis of every chart runs from 0 to 170."),

  h3("The data"),
  p("Cluster \"even\" already has gaps of 10 and 10. Cluster \"uneven\" is the ",
    "example of the help page, with gaps of 50 and 10."),
  code(print_of(gaps(genes[, c("cluster", "name", "start", "end")]))),
  GC_chartOutput("before", height = "220px"),

  h3("GC_normalize() now"),
  code('GC_chart(genes, cluster = "cluster") %>% GC_normalize()'),
  p("The even cluster is changed (gaps 7 and 6), and both clusters end before ",
    "their last gene did: 113 instead of 120, and 140 instead of 160."),
  code(print_of(gaps(positions(now)))),
  GC_chartOutput("now", height = "220px"),

  h3("Expected"),
  p("The even cluster stays as it is; the uneven one gets gaps of 30 and 30 ",
    "and still runs from 10 to 160."),
  code(print_of(gaps(expected))),
  GC_chartOutput("expected", height = "220px"),

  h3("With gap ="),
  p("?GC_normalize: gap is \"the proportion of the total length to be used as ",
    "the gap between genes\". With gap = 0.1 the range of \"even\" is 110, so ",
    "each gap should be 11; of \"uneven\" 150, so 15. Now they are 7 and 10, ",
    "two thirds of that:"),
  code('GC_chart(genes, cluster = "cluster") %>% GC_normalize(gap = 0.1)'),
  code(print_of(gaps(positions(now_gap)))),

  h3("The source lines"),
  p("The total gap is divided over nrow(data) genes; there are nrow(data) - 1 ",
    "gaps between them. The same in both branches:"),
  code(source_lines("R/gene_normalization.R", 155, 160)),

  h3("Fix options (for Niels)"),
  tags$ol(
    tags$li(strong("Fix: "), "divide by the number of gaps (n - 1). Clusters ",
            "keep their range and a given gap is the gap drawn. Genes move in ",
            "existing charts that use GC_normalize(), by at most one gap. ",
            "With gap = and preserve_gene_length = TRUE the cluster then ",
            "grows or shrinks to fit the gaps asked for, as it must. ",
            "Recommended, with a line in NEWS; the Python port needs the same change."),
    tags$li(strong("Keep: "), "leave the positions as they are and change the ",
            "docs to say what happens."),
    tags$li(strong("Scope: "), "a cluster of one gene has no gap (n - 1 = 0): ",
            "it stays as it is, as now.")
  )
)

server <- function(input, output, session) {
  output$before <- renderGC_chart(before)
  output$now <- renderGC_chart(now)
  output$expected <- renderGC_chart(expected_chart)
}

shinyApp(ui, server)
