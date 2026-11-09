color_bar_genes <- data.frame(
  start = c(10, 90, 130, 170, 240, 250),
  end = c(40, 120, 160, 200, 210, 270),
  group = c("A", "B", "C", "A", "B", "C"),
  identity = c(NA, NA, NA, 50, 40, 100),
  cluster = c(1, 1, 1, 2, 2, 2)
)

color_bar_chart <- function(colorBarOptions = list(), legend = TRUE, theme = "default") {
  chart <- GC_chart(color_bar_genes, cluster = "cluster", group = "group",
                    height = "240px", width = "700px") %>%
    GC_links(group = "group", measure = "identity", label = FALSE,
             colorBarOptions = colorBarOptions) %>%
    GC_theme(theme)
  if (legend) GC_legend(chart, position = "top") else GC_legend(chart, FALSE)
}

test_that("GC_links() checks and keeps where the color bar goes", {
  chart <- color_bar_chart(list(position = "topleft", orientation = "horizontal"))
  expect_identical(compile_chart(chart)$links[[1]]$options$colorBarOptions,
                   list(position = "topleft", orientation = "horizontal"))
  expect_error(color_bar_chart(list(position = "left")),
               "'colorBarOptions\\$position' must be one of")
  expect_error(color_bar_chart(list(orientation = "diagonal")),
               "'colorBarOptions\\$orientation' must be one of")
  expect_error(color_bar_chart(list(position = c("legend", "topleft"))),
               "'colorBarOptions\\$position' must be one of")
  expect_error(color_bar_chart(list(labels = "above")),
               "'colorBarOptions\\$labels' must be one of")
  expect_error(color_bar_chart(list(position = "legend", legendSide = "top")),
               "'colorBarOptions\\$legendSide' must be one of")
})
