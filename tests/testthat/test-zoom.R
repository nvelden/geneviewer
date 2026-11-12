test_that("GC_zoom() checks and keeps its options", {
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class")
  expect_null(compile_chart(chart)$zoom)

  zoomed <- GC_zoom(chart)
  expect_identical(compile_chart(zoomed)$zoom, list(show = TRUE))
  expect_true(any(vapply(zoomed$dependencies, function(dep) dep$name == "font-awesome", logical(1))))

  zoomed <- chart %>%
    GC_zoom(range = c(0.6, 0.2), height = 30) %>%
    GC_zoom(update = "release")
  zoomed_model <- compile_chart(zoomed)
  expect_identical(zoomed_model$zoom$range, c(0.2, 0.6))
  expect_equal(zoomed_model$zoom$height, 30)
  expect_identical(zoomed_model$zoom$update, "release")

  # A new mode drops the range of the old one
  absolute <- GC_zoom(zoomed, mode = "absolute")
  expect_null(compile_chart(absolute)$zoom$range)
  expect_equal(compile_chart(GC_zoom(zoomed, mode = "absolute", range = c(100, 5000)))$zoom$range, c(100, 5000))
  expect_false(compile_chart(GC_zoom(zoomed, show = FALSE))$zoom$show)

  expect_error(GC_zoom(chart, range = c(100, 5000)), "between 0 and 1")
  expect_error(GC_zoom(chart, range = 0.5), "two different numbers")
  expect_error(GC_zoom(chart, range = c(0.5, 0.5)), "two different numbers")
  expect_error(GC_zoom(chart, mode = "both"), "'mode' must be one of")
  expect_error(GC_zoom(chart, update = "never"), "'update' must be one of")
  expect_error(GC_zoom(chart, min = 2), "between 0 and 1")
  expect_error(GC_zoom(chart, height = -1), "positive number")
  expect_error(GC_zoom(chart, show = NA), "TRUE or FALSE")
  expect_error(GC_zoom(chart, style = "red"), "named list")

  expect_true(compile_chart(GC_events(chart, zoom = TRUE))$events$zoom)
  expect_null(compile_chart(GC_events(chart))$events$zoom)
  expect_error(GC_events(chart, zoom = "yes"), "TRUE or FALSE")
  expect_error(GC_zoom_to("chart", range = "a"), "two numbers")
})

test_that("a range as Shiny gives it goes back into GC_zoom()", {
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class") %>%
    GC_zoom(range = c(0.25, 0.5))
  # A range as Shiny gives it, a list, goes back into GC_zoom(), also after
  # an earlier range
  expect_identical(compile_chart(GC_zoom(chart, range = list(0.4, 0.2)))$zoom$range, c(0.2, 0.4))
  expect_identical(GC_zoom(chart, range = list(0.4, 0.2))$x$spec$zoom$range, list(0.4, 0.2))
})

test_that("render_id is sent without rendered = TRUE", {
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class") %>%
    GC_events(zoom = TRUE, render_id = "load-1")
  expect_null(compile_chart(chart)$events$rendered)
  expect_identical(chart$x$spec$events$renderId, "load-1")
})

test_that("GC_zoom_to() reaches the chart of its output, found with the real htmlwidgets", {
  chart <- GC_chart(ophA_clusters, cluster = "cluster", elementId = "first") %>% GC_zoom()
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file, width = 1000, height = 600)
  result <- session$Runtime$evaluate("
    (() => {
      const handlers = {};
      window.Shiny = { addCustomMessageHandler: (name, handler) => { handlers[name] = handler; },
                       setInputValue: () => {} };
      HTMLWidgets.shinyMode = true;
      // A second output, drawn in Shiny mode, which adds the message handler
      const data = document.querySelector('script[data-for=first]').textContent;
      const el = document.createElement('div');
      el.id = 'second';
      el.className = 'geneviewer html-widget';
      el.style.width = '800px';
      el.style.height = '300px';
      const script = document.createElement('script');
      script.type = 'application/json';
      script.setAttribute('data-for', 'second');
      script.textContent = data;
      document.body.append(el, script);
      HTMLWidgets.staticRender();
      const start = (id) => document.querySelector('#' + id + ' [data-zoom-part=window]').style.left;
      handlers['geneviewer-zoom']({ id: 'second', range: [0.25, 0.5] });
      handlers['geneviewer-zoom']({ id: 'missing', range: [0.25, 0.5] });
      return JSON.stringify({ second: start('second'), first: start('first') });
    })()")
  expect_null(result$exceptionDetails)
  value <- jsonlite::fromJSON(result$result$value)
  expect_identical(value$second, "25%")
  expect_identical(value$first, "0%")
})

test_that("range and min are in the mode of an earlier GC_zoom() call (B77)", {
  genes <- data.frame(start = c(100, 5000), end = c(900, 9000))
  chart <- GC_chart(genes) %>% GC_zoom(mode = "absolute") %>% GC_zoom(range = c(100, 2000), min = 50)
  expect_identical(chart$x$spec$zoom, list(mode = "absolute", range = list(100, 2000), min = 50))
  expect_error(GC_chart(genes) %>% GC_zoom(mode = "relative") %>% GC_zoom(range = c(100, 2000)),
               "relative 'range'")
  # A new mode in the same call is checked against itself
  expect_error(GC_chart(genes) %>% GC_zoom(mode = "absolute") %>%
                 GC_zoom(mode = "relative", range = c(100, 2000)), "relative 'range'")
})
