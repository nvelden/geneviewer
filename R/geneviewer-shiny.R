#' Shiny bindings for geneviewer
#'
#' Output and render functions for using geneviewer within Shiny applications and
#' interactive Rmd documents.
#'
#' @param outputId Output variable to read from.
#' @param width, height Must be a valid CSS unit (like '100%', '400px', 'auto')
#'   or a number, which will be coerced to a string and have 'px' appended.
#' @param height Height of the output widget, must be a valid CSS unit (like
#'   '100%', '400px', 'auto') or a number, which will be coerced to a string and
#'   have 'px' appended.
#' @param expr An expression that generates a GC chart.
#' @param env The environment in which to evaluate expr.
#' @param quoted Is expr a quoted expression (with quote())? This is useful if
#'   you want to save an expression in a variable.
#' @return GC_chart widget that can be placed in the UI.
#' @name geneviewer-shiny
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(geneviewer)
#'
#'   ui <- fluidPage(
#'     titlePanel("Omphalotin Gene Cluster Visualization"),
#'     mainPanel(
#'       GC_chartOutput("gcChart", width = "100%", height = "500px")
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'     output$gcChart <- renderGC_chart({
#'       GC_chart(
#'         ophA_clusters, # Ensure 'ophA_clusters' data is defined or available
#'         cluster = "cluster",
#'         group = "class"
#'       ) %>%
#'       GC_title(title = c("<i>O. olearius</i>", "<i>D. bispora</i>")) %>%
#'       GC_labels("name") %>%
#'       GC_legend(position = "bottom") %>%
#'       GC_scaleBar() %>%
#'       GC_clusterLabel(title = "ophA")
#'     })
#'   }
#'
#'   shinyApp(ui = ui, server = server)
#' }
#' @seealso [GC_chart()]
#' @importFrom htmlwidgets shinyWidgetOutput shinyRenderWidget
#' @export
GC_chartOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'geneviewer', width, height, package = 'geneviewer')
}

#' @rdname geneviewer-shiny
#' @export
renderGC_chart <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, GC_chartOutput, env, quoted = TRUE)
}
