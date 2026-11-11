# B70: GC_links(group = ) when the data has a column named "cluster" and the
# chart's clusters are another column
# Run from the package folder: shiny::runApp("dev/bug-demos/B70")
suppressMessages(devtools::load_all(
  rprojroot::find_package_root_file(path = getwd()), quiet = TRUE))
library(shiny)

pkg_root <- rprojroot::find_package_root_file(path = getwd())

# Two genomes with the same three genes. "cluster" is a column of the data
# with another meaning (e.g. a cluster of orthologs, or a BGC number)
genes <- data.frame(
  start = c(10, 100, 190, 10, 100, 190),
  end = c(80, 170, 260, 80, 170, 260),
  name = c("abcA", "abcB", "abcC", "abcA", "abcB", "abcC"),
  genome = c("genome1", "genome1", "genome1", "genome2", "genome2", "genome2"),
  stringsAsFactors = FALSE
)
# 1. Every gene has the same value: no links at all
# The column comes before "genome" in the data. (After it, the links are
# made, by chance: data$cluster reads the first column of that name.)
same <- cbind(cluster = "BGC1", genes, stringsAsFactors = FALSE)
# 2. The values cross the genomes: links between the wrong rows are left out
mixed <- cbind(cluster = c("genome1", "genome2", "genome2", "genome1", "genome1", "genome2"),
               genes, stringsAsFactors = FALSE)

chart <- function(data) {
  GC_chart(data, cluster = "genome", group = "name", height = "250px") %>%
    GC_labels("name") %>%
    GC_legend(FALSE)
}
with_links <- function(data) chart(data) %>% GC_links(group = "name", label = FALSE)

attempt <- function(data) {
  tryCatch(with_links(data), error = function(e) conditionMessage(e))
}
link_table <- function(result) {
  if (is.character(result)) return(paste("Error:", result))
  links <- result$x$spec$links
  if (is.null(links)) return("(no links in the chart)")
  columns <- intersect(c("name1", "cluster1", "cluster2", "genome1", "genome2",
                         "start1", "start2"), names(links$data))
  paste(capture.output(print(links$data[, columns], row.names = FALSE)), collapse = "\n")
}

result_plain <- attempt(genes)
result_same <- attempt(same)
result_mixed <- attempt(mixed)

source_lines <- function(file, from, to) {
  lines <- readLines(file.path(pkg_root, file))[from:to]
  paste(sprintf("%s:%d  %s", file, from:to, lines), collapse = "\n")
}
code <- function(...) pre(paste(c(...), collapse = "\n"))
print_of <- function(x) paste(capture.output(print(x, row.names = FALSE)), collapse = "\n")
shown <- function(result, fallback) if (is.character(result)) fallback else result

ui <- fluidPage(
  tags$style("pre { font-size: 12px; } h3 { margin-top: 30px; }"),
  titlePanel("B70: GC_links(group = ) with a column named \"cluster\""),
  p("geneviewer on branch bugfixes, loaded with devtools::load_all(). ",
    "Nothing in R/ is changed."),
  code('GC_chart(data, cluster = "genome", group = "name") %>%',
       '  GC_labels("name") %>%',
       '  GC_links(group = "name", label = FALSE)'),

  h3("Without a column named \"cluster\": three links, as expected"),
  code(print_of(genes)),
  code(link_table(result_plain)),
  GC_chartOutput("plain", height = "250px"),

  h3("1. A column \"cluster\" with one value for all genes"),
  code(print_of(same)),
  p("The same call stops:"),
  code(link_table(result_same)),
  p("The chart without GC_links():"),
  GC_chartOutput("same", height = "250px"),

  h3("2. A column \"cluster\" whose values are also names of the genomes"),
  code(print_of(mixed)),
  code(link_table(result_mixed)),
  p("No error and no warning: only abcB gets its link, abcA and abcC get none:"),
  GC_chartOutput("mixed", height = "250px"),

  h3("The source lines"),
  p("get_links() renames the cluster column of the chart (\"genome\") to ",
    "\"cluster\". The data then has two columns of that name, and ",
    "data$cluster reads the first one: the column of the user, when it ",
    "comes before the cluster column of the chart. When it comes after it, ",
    "the links are right."),
  code(source_lines("R/chart_links.R", 56, 61), "...",
       source_lines("R/chart_links.R", 67, 68)),

  h3("Fix options (for Niels)"),
  tags$ol(
    tags$li(strong("Fix: "), "read the clusters from the chart's cluster column ",
            "by its own name, and write them to the link table as cluster1 and ",
            "cluster2, which the library reads. The user's column \"cluster\" ",
            "is left out of the link table (its values are still in the chart ",
            "data, by rowID1 and rowID2). Recommended; charts that work now ",
            "do not change."),
    tags$li(strong("Scope: "), "GC_links(group = \"cluster\") on such a chart ",
            "(links by the user's column itself) then also works: the group ",
            "values go to the link table as the library expects them."),
    tags$li(strong("Python: "), "geneviewer-py get_links() copies R; the same ",
            "fix there, if it has the same bug.")
  )
)

server <- function(input, output, session) {
  output$plain <- renderGC_chart(shown(result_plain, chart(genes)))
  output$same <- renderGC_chart(shown(result_same, chart(same)))
  output$mixed <- renderGC_chart(shown(result_mixed, chart(mixed)))
}

shinyApp(ui, server)
