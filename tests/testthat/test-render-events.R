test_that("render events are opt-in and validate identifiers", {
  chart <- GC_chart(data.frame(start = 1, end = 10))
  expect_identical(compile_chart(GC_events(chart))$events, list(click = TRUE, highlight = FALSE))
  # Without rendered, render_id is still sent: it marks a new load for GC_zoom()
  expect_identical(compile_chart(GC_events(chart, rendered = FALSE, render_id = "load-1"))$events,
                   list(click = TRUE, highlight = FALSE, renderId = "load-1"))
  for (id in list(NULL, "load-1", "", 0, 2L, 1.5)) {
    chart_id <- GC_events(chart, click = FALSE, rendered = TRUE, render_id = id)
    events <- compile_chart(chart_id)$events
    expect_true(events$rendered)
    expect_false(events$click)
    expect_equal(events$renderId, id)
    expect_identical(chart_id$x$spec$events$renderId, id)
  }
  for (id in list(NA, Inf, NaN, TRUE, character(), c("one", "two"), list(1), 1i)) {
    expect_error(GC_events(chart, rendered = TRUE, render_id = id), "render_id")
  }
  for (value in list(NA, "yes", logical(), c(TRUE, FALSE))) {
    expect_error(GC_events(chart, rendered = value), "TRUE or FALSE")
  }
})
