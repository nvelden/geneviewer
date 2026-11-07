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

test_that("render events follow successful drawing and are independent of clicks and resize", {
  genes <- data.frame(cluster = rep(c("A", "B"), each = 2),
                      start = c(1, 201, 1, 201), end = c(150, 350, 150, 350),
                      name = rep(c("a", "b"), 2), identity = c(NA, NA, 90, 70))
  chart <- GC_chart(genes, cluster = "cluster", group = "name",
                    width = "600px", height = "400px") %>%
    GC_title("Render completion") %>% GC_labels("name") %>% GC_links("name") %>%
    GC_events(rendered = TRUE)
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file)
  result <- session$Runtime$evaluate("
    (() => {
      const el = document.querySelector('.geneviewer');
      const widget = HTMLWidgets.find('#' + el.id);
      const x = JSON.parse(document.querySelector('script[data-for=\"' + el.id + '\"]').textContent).x;
      const dom = [], shiny = [];
      window.Shiny = { setInputValue: (name, value, options) => shiny.push({ name, value, options }) };
      document.addEventListener('geneviewer:rendered', event => {
        dom.push({ target: event.target.id, ...event.detail,
          links: el.querySelectorAll('path.GeneLink').length,
          legend: el.querySelectorAll('.legend').length,
          save: !!el.querySelector('.save-option'),
          highlighted: el.classList.contains('geneviewer-highlight') });
      });
      x.spec.events = { click: false, highlight: false };
      widget.renderValue(x);
      const defaultCount = dom.length;
      x.spec.events.rendered = true;
      x.spec.events.renderId = 'standalone';
      widget.renderValue(x);
      const standaloneShiny = shiny.length;
      HTMLWidgets.shinyMode = true;
      x.spec.events = { click: true, highlight: true, rendered: true, renderId: 'load-1' };
      widget.renderValue(x);
      widget.renderValue(x);
      x.spec.events.renderId = 2;
      widget.renderValue(x);
      const beforeResize = dom.length;
      widget.resize(800, 400);
      const afterResize = dom.length;
      el.querySelector('.gene').dispatchEvent(new MouseEvent('click', { bubbles: true }));
      x.spec.events.rendered = false;
      widget.renderValue(x);
      const disabledCount = dom.length;
      x.spec.events.rendered = true;
      x.spec.events.renderId = 'broken';
      // Links are drawn with d3.path() (the page's D3), and only links
      const originalPath = d3.path;
      d3.path = () => { throw new Error('Simulated link drawing failure'); };
      let failed = false;
      try { widget.renderValue(x); } catch (e) { failed = true; }
      d3.path = originalPath;
      const failedCount = dom.length;
      x.spec.events.renderId = 'recovered';
      widget.renderValue(x);
      return JSON.stringify({ id: el.id, dom, shiny, defaultCount, standaloneShiny,
        beforeResize, afterResize, disabledCount, failed, failedCount });
    })()
  ")
  expect_null(result$exceptionDetails)
  res <- jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
  expect_equal(res$defaultCount, 0)
  expect_equal(res$standaloneShiny, 0)
  expect_equal(vapply(res$dom, function(e) as.character(e$id), ""),
               c("standalone", "load-1", "load-1", "2", "recovered"))
  expect_true(all(vapply(res$dom, function(e) e$target == res$id, logical(1))))
  expect_true(all(vapply(res$dom, function(e) e$chartId == res$id, logical(1))))
  expect_true(all(vapply(res$dom, function(e) e$links > 0 && e$legend > 0 && e$save,
                         logical(1))))
  expect_true(res$dom[[2]]$highlighted)
  expect_equal(res$dom[[2]]$width, 600)
  expect_gt(res$dom[[2]]$height, 0)
  expect_equal(res$beforeResize, res$afterResize)
  expect_equal(res$disabledCount, res$afterResize)
  expect_true(res$failed)
  expect_equal(res$failedCount, res$disabledCount)
  rendered <- Filter(function(e) endsWith(e$name, "_rendered"), res$shiny)
  expect_length(rendered, 4)
  expect_true(all(vapply(rendered, function(e) e$name == paste0(res$id, "_rendered") &&
                           e$options$priority == "event", logical(1))))
  expect_equal(rendered[[3]]$value$id, 2)
  expect_true(any(vapply(res$shiny, function(e) e$name == paste0(res$id, "_click"), logical(1))))
})

test_that("omitted render IDs are unique across charts and repeated updates", {
  chart <- GC_chart(data.frame(start = c(1, 50), end = c(40, 90)), height = "200px") %>%
    GC_events(click = FALSE, rendered = TRUE)
  file <- tempfile(fileext = ".html")
  # Reusing the same R object must still give two distinct chart instances.
  htmltools::save_html(htmltools::tagList(chart, chart), file)
  session <- chrome_page(file)
  result <- session$Runtime$evaluate("
    (() => {
      const elements = [...document.querySelectorAll('.geneviewer')];
      const dom = [], shiny = [];
      document.addEventListener('geneviewer:rendered', event => dom.push({
        ...event.detail, target: event.target.id
      }));
      window.Shiny = { setInputValue: (name, value) => shiny.push({ name, value }) };
      HTMLWidgets.shinyMode = true;
      const render = (el, id) => {
        const x = JSON.parse(document.querySelector('script[data-for=\"' + el.id + '\"]').textContent).x;
        if (id !== undefined) x.spec.events.renderId = id;
        HTMLWidgets.find('#' + el.id).renderValue(x);
      };
      for (let i = 0; i < 3; i++) elements.forEach(el => render(el));
      render(elements[0], 0);
      render(elements[1], '');
      render(elements[0], 'explicit');
      render(elements[1], 'explicit');
      // Returning to automatic IDs must not reuse an earlier identifier.
      elements.forEach(el => render(el, null));
      return JSON.stringify({ elements: elements.map(el => el.id), dom, shiny });
    })()
  ")
  expect_null(result$exceptionDetails)
  res <- jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
  expect_length(res$elements, 2)
  expect_false(identical(res$elements[[1]], res$elements[[2]]))
  expect_length(res$dom, 12)
  automatic <- res$dom[c(1:6, 11:12)]
  ids <- vapply(automatic, `[[`, "", "id")
  expect_true(all(nzchar(ids)))
  expect_length(unique(ids), 8)
  expect_true(all(vapply(automatic, function(e) startsWith(e$id, paste0(e$chartId, "-render-")),
                         logical(1))))
  expect_equal(vapply(res$dom, `[[`, "", "chartId"), rep(unlist(res$elements), 6))
  expect_true(all(vapply(res$dom, function(e) e$target == e$chartId, logical(1))))
  expect_identical(res$dom[[7]]$id, 0L)
  expect_identical(res$dom[[8]]$id, "")
  expect_identical(res$dom[[9]]$id, "explicit")
  expect_identical(res$dom[[10]]$id, "explicit")
  expect_equal(lapply(res$shiny, `[[`, "value"),
               lapply(res$dom, function(e) e[names(e) != "target"]))
  expect_equal(vapply(res$shiny, `[[`, "", "name"),
               paste0(rep(unlist(res$elements), 6), "_rendered"))
})
