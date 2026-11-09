test_that("element overrides target row IDs and merge without replacing neighbours", {
  data <- data.frame(start = c(90, 10, 50), end = c(130, 40, 80),
                     name = c("same", "same", "other"), cluster = c("A", "A", "B"))
  base <- GC_chart(data, cluster = "cluster") %>% GC_labels("name")
  chart <- base %>%
    GC_element_style("genes", 1, fill = "red") %>%
    GC_element_style("genes", 2, fill = "blue") %>%
    GC_element_style("genes", 1, marker = "box") %>%
    GC_element_style("labels", 1, text = "Changed", fontSize = "18px")
  model <- compile_chart(chart)
  items <- model$series$A$genes$itemStyle
  expect_length(items, 2)
  expect_equal(items[[2]], list(fill = "red", rowID = 1, marker = "box"))
  expect_null(model$series$B$genes$itemStyle)
  expect_null(compile_chart(base)$series$A$genes$itemStyle)
  expect_equal(model$series$A$labels$itemStyle[[1]]$text, "Changed")
  reset <- GC_element_style(chart, "genes", 1, reset = TRUE)
  reset_model <- compile_chart(reset)
  expect_equal(reset_model$series$A$genes$itemStyle, list(list(fill = "blue", rowID = 2)))
  expect_identical(reset_model$series$A$labels, model$series$A$labels)
  expect_error(GC_element_style(base, "genes", 99, fill = "red"), "row_id")
  expect_error(GC_element_style(base, "genes", 1, "red"), "names")
  expect_error(GC_element_style(base, "genes", 1, index = 0), "row_id")
  expect_error(GC_element_style(base, "genes", 1, fill = "red", reset = TRUE), "reset")
  expect_error(GC_element_style(base, "genes", 1, reset = NA), "TRUE or FALSE")
})

test_that("editor events are independent and validate their flags", {
  chart <- GC_chart(data.frame(start = 1, end = 20))
  events <- compile_chart(GC_events(chart, click = FALSE, dblclick = TRUE, select = TRUE,
                      legend_click = FALSE))$events
  expect_false(events$click)
  expect_true(events$dblclick)
  expect_true(events$select)
  expect_false(events$legendClick)
  for (argument in c("dblclick", "select", "legend_click")) {
    for (value in list(NA, 1, "yes", logical(), c(TRUE, FALSE))) {
      expect_error(do.call(GC_events, c(list(chart), setNames(list(value), argument))),
                   "TRUE or FALSE")
    }
  }
})

test_that("cluster titles, footers, labels and coordinates are styled one by one", {
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class", height = "500px") %>%
    GC_clusterTitle(title = c("Title A", "Title B"), subtitle = "Sub") %>%
    GC_clusterFooter(title = "Foot", subtitle = "Foot sub") %>%
    GC_clusterLabel(title = c("Label A", "Label B"), subtitle = "Species") %>%
    GC_coordinates() %>%
    GC_element_style("clusterTitle", cluster = 1, part = "subtitle", text = "Edited",
                     fill = "red", fontStyle = "italic") %>%
    GC_element_style("clusterFooter", cluster = "ophA", part = "title", text = "") %>%
    GC_element_style("clusterLabel", cluster = 2, part = "title", fontSize = "20px") %>%
    GC_element_style("clusterLabel", cluster = 2, part = "subtitle", fill = "blue") %>%
    GC_element_style("coordinates", row_id = 1, end = "start", fill = "red") %>%
    GC_element_style("coordinates", row_id = 2, show = FALSE) %>%
    GC_events(click = FALSE, select = TRUE)
  model <- compile_chart(chart)
  expect_equal(model$series$ophA$footer$itemStyle, list(list(text = "", part = "title")))
  expect_equal(model$series$ophA$coordinates$itemStyle[[1]],
               list(fill = "red", rowID = 1, end = "start"))
  expect_error(GC_element_style(chart, "clusterTitle", cluster = 1, part = "header"), "part")
  expect_error(GC_element_style(chart, "clusterTitle", cluster = "nope", part = "title"), "cluster")
  expect_error(GC_element_style(chart, "coordinates", row_id = 1, end = "middle"), "end")
  expect_error(GC_element_style(chart, "genes", row_id = 1, cluster = 1), "row_id")
  reset <- GC_element_style(chart, "clusterLabel", cluster = 2, part = "title", reset = TRUE)
  expect_length(compile_chart(reset)$series$dbophA$clusterLabel$itemStyle, 1)
})

test_that("legend overrides merge and reset each part independently", {
  base <- GC_chart(data.frame(start = c(10, 60), end = c(40, 90), group = c("A", "B")),
                   group = "group") %>%
    GC_legend(groupStyle = list(A = list(fill = "grey", marker = list(fill = "green"))))
  chart <- base %>%
    GC_element_style("legend", group = "A", part = "label", text = "Edited", fill = "red") %>%
    GC_element_style("legend", group = "A", part = "marker", fill = "blue") %>%
    GC_element_style("legend", group = "B", part = "label", fontStyle = "italic") %>%
    GC_element_style("legend", group = "A", part = "label", fontSize = "20px")
  model <- compile_chart(chart)
  expect_length(model$legend$itemStyle, 3)
  label <- model$legend$itemStyle[[3]]
  expect_equal(label$text, "Edited")
  expect_equal(label$fontSize, "20px")
  reset <- GC_element_style(chart, "legend", group = "A", part = "label", reset = TRUE)
  reset_model <- compile_chart(reset)
  expect_length(reset_model$legend$itemStyle, 2)
  base_model <- compile_chart(base)
  expect_equal(reset_model$legend$groupStyle, base_model$legend$groupStyle)
  expect_null(base_model$legend$itemStyle)
  expect_error(GC_element_style(base, "legend", group = "absent", part = "label"), "group")
  expect_error(GC_element_style(base, "legend", group = "A", part = "title"), "part")
  expect_error(GC_element_style(base, "legend", row_id = 1, group = "A", part = "label"), "group")
})

test_that("text drag events are opt-in and chart titles support individual offsets", {
  chart <- GC_chart(data.frame(start = 1, end = 20)) %>% GC_title("Chart", subtitle = "Sub")
  expect_null(compile_chart(GC_events(chart))$events$drag)
  expect_true(compile_chart(GC_events(chart, drag = TRUE))$events$drag)
  expect_error(GC_events(chart, drag = NA), "TRUE or FALSE")
  edited <- chart %>%
    GC_element_style("title", part = "title", moveX = 25, moveY = 10, fill = "red") %>%
    GC_element_style("title", part = "subtitle", moveX = -5) %>%
    GC_element_style("title", part = "title", moveY = 20)
  expect_equal(compile_chart(edited)$title$itemStyle[[2]], list(moveX = 25, moveY = 20, fill = "red", part = "title"))
  reset <- GC_element_style(edited, "title", part = "title", reset = TRUE)
  expect_equal(compile_chart(reset)$title$itemStyle, list(list(moveX = -5, part = "subtitle")))
  expect_error(GC_element_style(chart, "title", part = "title", cluster = 1), "part")
  expect_error(GC_element_style(chart, "title", part = "other"), "part")
})

for (component in c("scaleBar", "scale")) test_that(paste(component, "is moved as a whole, per cluster"), {
  chart <- GC_chart(data.frame(start = c(10, 20), end = c(50, 60), cluster = c("A", "B")),
                    cluster = "cluster", height = "400px") %>%
    GC_scale(hidden = FALSE) %>%
    GC_scaleBar(scaleBarUnit = 10) %>%
    GC_element_style(component, cluster = "A", moveX = 5, moveY = -5) %>%
    GC_events(click = FALSE, select = TRUE, drag = TRUE)
  expect_error(GC_element_style(chart, component, cluster = "A", part = "label"), "whole")
  reset <- GC_element_style(chart, component, cluster = "A", reset = TRUE)
  expect_length(compile_chart(reset)$series$A[[component]]$itemStyle, 0)
  expect_null(compile_chart(chart)$series$B[[component]]$itemStyle)
})

test_that("moves add Font Awesome for the toolbar icons", {
  chart <- GC_chart(data.frame(start = 10, end = 50), height = "250px") %>%
    GC_events(click = FALSE, select = TRUE, drag = TRUE)
  expect_true("font-awesome" %in% vapply(chart$dependencies, `[[`, "", "name"))
})

test_that("legend parts keep their offsets per group", {
  data <- data.frame(start = c(10, 60), end = c(40, 90), group = c("A", "B"))
  chart <- GC_chart(data, group = "group", height = "250px") %>%
    GC_legend(TRUE) %>%
    GC_element_style("legend", group = "B", part = "marker", moveX = 4, moveY = -3) %>%
    GC_events(click = FALSE, drag = TRUE)
  expect_mapequal(compile_chart(chart)$legend$itemStyle[[1]], list(group = "B", part = "marker", moveX = 4, moveY = -3))
})

test_that("genes and coordinates cannot be moved, and Font Awesome is added once", {
  chart <- GC_chart(data.frame(start = 10, end = 50), height = "200px")
  expect_error(GC_element_style(chart, "genes", 1, moveX = 5), "cannot be moved")
  expect_error(GC_element_style(chart, "coordinates", 1, moveY = 5), "cannot be moved")
  chart <- GC_chart(data.frame(start = 10, end = 50), height = "200px") %>%
    GC_events(drag = TRUE) %>%
    GC_toolbar(size = 16) %>%
    GC_toolbar(color = "grey")
  names <- vapply(chart$dependencies, function(dep) dep$name, character(1))
  expect_equal(sum(names == fontawesome::fa_html_dependency()$name), 1)
})

test_that("lists of selected elements reach Shiny as lists", {
  skip_if_not_installed("shiny")
  loadNamespace("shiny")
  inputs <- shiny:::applyInputHandlers(list(
    "chart_selection:geneviewer.list" = list(list(component = "labels", data = list(rowID = 1)),
                                            list(component = "labels", data = list(rowID = 2))),
    "empty_selection:geneviewer.list" = NULL))
  expect_equal(names(inputs), c("chart_selection", "empty_selection"))
  expect_length(inputs$chart_selection, 2)
  expect_equal(inputs$chart_selection[[2]]$data$rowID, 2)
  expect_identical(inputs$empty_selection, list())
})

test_that("GC_toolbar() checks and keeps its options", {
  chart <- GC_chart(data.frame(start = 10, end = 50), height = "200px")
  expect_error(GC_toolbar(chart, show = "sometimes"), "'show' must be one of")
  expect_error(GC_toolbar(chart, position = "top"), "'position' must be one of")
  expect_error(GC_toolbar(chart, buttons = c("undo", "print")), "'buttons' must be")
  expect_error(GC_toolbar(chart, size = -1), "positive number")
  expect_error(GC_toolbar(chart, color = c("red", "blue")), "single colour")
  expect_error(GC_toolbar(chart, icons = list(print = "print")), "'icons' must name")
  expect_error(GC_toolbar(chart, icons = "camera"), "'icons' must name")
  expect_error(GC_theme(chart, toolbar = list(show = "sometimes")), "'show' must be one of")
  chart <- chart %>%
    GC_toolbar(show = "always", buttons = "save", icons = c(save = "camera")) %>%
    GC_toolbar(size = 16)
  expect_equal(chart$x$spec$toolbar, list(show = "always", buttons = list("save"), icons = list(save = "camera"), size = 16))
  expect_equal(compile_chart(chart, simplify = FALSE)$toolbar,
               list(show = "always", buttons = list("save"), size = 16, icons = list(save = "camera")))
  expect_true("font-awesome" %in% vapply(chart$dependencies, `[[`, "", "name"))
  # The built-in themes keep the icons visible on their background
  expect_equal(geneviewer_theme("dark")$toolbar$hoverColor, "#eeeeee")
  theme_js <- function(name) compile_chart(GC_theme(chart, name))$theme
  expect_equal(theme_js("dark")$toolbarOptions$color, "#999999")
  # In the style of each theme's tooltip, and with its font
  publication <- theme_js("publication")$toolbarOptions
  expect_equal(publication[c("tooltipBorder", "tooltipRadius", "fontFamily")],
               list(tooltipBorder = "1px solid black", tooltipRadius = "0",
                    fontFamily = "Arial, Helvetica, sans-serif"))
  for (theme in c("classic", "dark", "vintage", "infographic", "publication", "bw")) {
    expect_named(geneviewer_theme(theme)$toolbar[c("color", "hoverColor", "tooltipBackground")])
  }
  expect_error(GC_toolbar(chart, tooltipRadius = -2), "tooltipRadius")
})
