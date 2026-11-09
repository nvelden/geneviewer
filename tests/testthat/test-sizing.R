test_that("rendered sizing uses the current widget width without changing the chart", {
  for (width in list("100%", "75%", "600px", 600, NULL)) {
    chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class", width = width)
    # The library sizes the chart as the widget. Without a width it gives
    # "100%" (R 0.2.0 sent none); only pixel widths are applied in Shiny.
    expect_equal(compile_chart(chart)$size$width, if (is.null(width)) "100%" else chart$width)
    expect_null(chart$x$spec$size)
    # Sending the chart leaves the widget and its spec as they are
    sent <- send_spec(chart)
    expect_identical(sent$width, chart$width)
    expect_identical(sent$height, chart$height)
    expect_identical(sent$x$spec, chart$x$spec)
  }

  chart <- GC_chart(ophA_clusters, cluster = "cluster") %>% GC_grid(width = 700)
  expect_equal(chart$width, 700)
  expect_equal(compile_chart(chart)$size$width, 700)
  # The construction arguments are kept as given; GC_grid() sets the width.
  expect_null(chart$x$spec$width)
  expect_equal(chart$x$spec$grid$width, 700)
})
