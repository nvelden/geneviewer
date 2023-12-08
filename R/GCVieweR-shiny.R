#' Shiny bindings for GCVieweR
#'
#' Output and render functions for using GCVieweR within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId Output variable to read from.
#' @param width, height Must be a valid CSS unit (like '100%', '400px', 'auto')
#'   or a number, which will be coerced to a string and have 'px' appended.
#' @param expr An expression that generates a GC chart.
#' @param env The environment in which to evaluate expr.
#' @param quoted Is expr a quoted expression (with quote())? This
#'   is useful if you want to save an expression in a variable.
#' @param session The Shiny session object to which the map belongs; usually
#'   the default value will suffice.
#' @return GC_chart widget that can be placed in the UI.
#' @name GCVieweR-shiny
#'
#' @examples
#' if (interactive()) {
#' # Load necessary libraries
#' library(shiny)
#' library(GCVieweR)
#'
#' # Define UI
#' ui <- fluidPage(titlePanel("Omphalotin Gene Cluster Visualization"),
#'                 mainPanel(# Output for GC_chart
#'                   GC_chartOutput(
#'                     "gcChart", width = "100%", height = "500px"
#'                   )))
#'
#' # Define server logic
#' server <- function(input, output) {
#'   output$gcChart <- renderGC_chart({
#'
#'     GC_chart(
#'       ophA_clusters,
#'       cluster = "cluster",
#'       group = "class"
#'     ) %>%
#'       GC_title(title = c("<i>O. olearius</i>", "<i>D. bispora</i>")) %>%
#'       GC_labels("name") %>%
#'       GC_legend(position = "bottom") %>%
#'       GC_scaleBar() %>%
#'       GC_clusterLabel(title = "ophA")
#'
#'   })
#' }
#' # Run the application
#' shinyApp(ui = ui, server = server)
#'}
#' @seealso
#' [GC_chart()]
#' @importFrom htmlwidgets shinyWidgetOutput shinyRenderWidget
#' @export
GC_chartOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'GCVieweR', width, height, package = 'GCVieweR')
}

#' @rdname GCVieweR-shiny
#' @export
renderGC_chart <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, GC_chartOutput, env, quoted = TRUE)
}
