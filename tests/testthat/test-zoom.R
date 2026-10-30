test_that("GC_zoom() checks and keeps its options", {
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class")
  expect_null(chart$x$zoom)

  zoomed <- GC_zoom(chart)
  expect_identical(zoomed$x$zoom, list(show = TRUE))
  expect_true(any(vapply(zoomed$dependencies, function(dep) dep$name == "font-awesome", logical(1))))

  zoomed <- chart %>%
    GC_zoom(range = c(0.6, 0.2), height = 30) %>%
    GC_zoom(update = "release")
  expect_identical(zoomed$x$zoom$range, c(0.2, 0.6))
  expect_identical(zoomed$x$zoom$height, 30)
  expect_identical(zoomed$x$zoom$update, "release")

  # A new mode drops the range of the old one
  absolute <- GC_zoom(zoomed, mode = "absolute")
  expect_null(absolute$x$zoom$range)
  expect_identical(GC_zoom(zoomed, mode = "absolute", range = c(100, 5000))$x$zoom$range, c(100, 5000))
  expect_false(GC_zoom(zoomed, show = FALSE)$x$zoom$show)

  expect_error(GC_zoom(chart, range = c(100, 5000)), "between 0 and 1")
  expect_error(GC_zoom(chart, range = 0.5), "two different numbers")
  expect_error(GC_zoom(chart, range = c(0.5, 0.5)), "two different numbers")
  expect_error(GC_zoom(chart, mode = "both"), "'mode' must be one of")
  expect_error(GC_zoom(chart, update = "never"), "'update' must be one of")
  expect_error(GC_zoom(chart, min = 2), "between 0 and 1")
  expect_error(GC_zoom(chart, height = -1), "positive number")
  expect_error(GC_zoom(chart, show = NA), "TRUE or FALSE")
  expect_error(GC_zoom(chart, style = "red"), "named list")

  expect_true(GC_events(chart, zoom = TRUE)$x$events$zoom)
  expect_null(GC_events(chart)$x$events$zoom)
  expect_error(GC_events(chart, zoom = "yes"), "TRUE or FALSE")
  expect_error(GC_zoom_to("chart", range = "a"), "two numbers")
})

test_that("the zoom slider zooms, clips, reports and is left out of saved images", {
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class",
                    height = "400px", width = "800px") %>%
    GC_labels("name") %>%
    GC_links("class") %>%
    GC_scale(hidden = FALSE) %>%
    GC_events(zoom = TRUE) %>%
    GC_zoom(range = c(0.25, 0.5))
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file, width = 1000, height = 600)
  result <- session$Runtime$evaluate("
    (() => {
      const el = document.querySelector('.geneviewer');
      const slider = el.querySelector('.geneviewer-zoom-slider');
      const windowNode = slider.querySelector('[data-zoom-part=window]');
      const before = {
        clips: el.querySelectorAll('.geneviewer-zoom-clip').length,
        left: windowNode.style.left,
        width: windowNode.style.width,
        overview: slider.querySelectorAll('.geneviewer-zoom-overview rect').length,
        linksClipped: !!el.querySelector('svg.GeneLink g[clip-path]'),
        resetHidden: el.querySelector('.geneviewer-zoom-buttons').hidden,
        exported: chartToSVG(el).includes('geneviewer-zoom-overview')
      };
      const reports = [];
      el.addEventListener('geneviewer:zoom', event => reports.push(event.detail));
      const box = windowNode.getBoundingClientRect();
      const x = box.left + box.width / 2, y = box.top + box.height / 2;
      windowNode.dispatchEvent(new PointerEvent('pointerdown', { bubbles: true, clientX: x, clientY: y, pointerId: 1, button: 0 }));
      window.dispatchEvent(new PointerEvent('pointermove', { clientX: x + box.width, clientY: y, pointerId: 1 }));
      window.dispatchEvent(new PointerEvent('pointerup', { clientX: x + box.width, clientY: y, pointerId: 1 }));
      const moved = reports[0];
      el.querySelector('.geneviewer-zoom-slider').dispatchEvent(new MouseEvent('dblclick', { bubbles: true }));
      return JSON.stringify({ before, moved, reset: reports[1],
        clipsAfterReset: el.querySelectorAll('.geneviewer-zoom-clip').length });
    })()
  ")
  expect_null(result$exceptionDetails)
  value <- jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
  expect_equal(value$before$clips, 2)
  expect_identical(value$before$left, "25%")
  expect_identical(value$before$width, "25%")
  expect_gt(value$before$overview, 10)
  expect_true(value$before$linksClipped)
  expect_false(value$before$resetHidden)
  expect_false(value$before$exported)
  expect_identical(value$moved$mode, "relative")
  expect_equal(value$moved$start, 0.5)
  expect_equal(value$moved$end, 0.75)
  expect_true(value$moved$zoomed)
  expect_length(value$moved$clusters, 2)
  expect_false(value$reset$zoomed)
  expect_equal(value$clipsAfterReset, 0)
})

test_that("redraws keep the window, and every change of it is reported", {
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class",
                    height = "400px", width = "800px") %>%
    GC_events(zoom = TRUE) %>%
    GC_zoom(range = c(0.25, 0.5))
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file, width = 1000, height = 600)
  result <- session$Runtime$evaluate("
    (() => {
      const el = document.querySelector('.geneviewer');
      const widget = HTMLWidgets.find('#' + el.id);
      const x = JSON.parse(document.querySelector('script[data-for=\"' + el.id + '\"]').textContent).x;
      const copy = change => Object.assign(JSON.parse(JSON.stringify(x)), change);
      const reports = [];
      el.addEventListener('geneviewer:zoom', event => reports.push(event.detail));
      const width = () => el.querySelector('[data-zoom-part=window]')?.style.width ?? null;
      const steps = {};
      const drag = () => {
        const windowNode = el.querySelector('[data-zoom-part=window]');
        const box = windowNode.getBoundingClientRect();
        const x0 = box.left + box.width / 2, y = box.top + box.height / 2;
        windowNode.dispatchEvent(new PointerEvent('pointerdown', { bubbles: true, clientX: x0, clientY: y, pointerId: 1, button: 0 }));
        window.dispatchEvent(new PointerEvent('pointermove', { clientX: x0 + box.width / 2, clientY: y, pointerId: 1 }));
        window.dispatchEvent(new PointerEvent('pointerup', { clientX: x0 + box.width / 2, clientY: y, pointerId: 1 }));
      };
      drag();
      steps.dragged = { width: width(), reports: reports.splice(0) };
      // The same settings again, e.g. another option changed in an app
      widget.renderValue(copy({ title: { title: 'Again', show: true } }));
      steps.same = { width: width(), left: el.querySelector('[data-zoom-part=window]').style.left, reports: reports.splice(0) };
      // A new range from R
      widget.renderValue(copy({ zoom: { show: true, range: [0.5, 1] } }));
      steps.range = { width: width(), reports: reports.splice(0) };
      // The slider off and on again: the window is reported again
      widget.renderValue(copy({ zoom: null }));
      steps.off = { width: width(), reports: reports.splice(0) };
      widget.renderValue(copy({ zoom: { show: true, range: [0.5, 1] } }));
      steps.on = { width: width(), reports: reports.splice(0) };
      return JSON.stringify(steps);
    })()
  ")
  expect_null(result$exceptionDetails)
  steps <- jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
  expect_identical(steps$dragged$reports[[1]]$action, "drag")
  expect_equal(unlist(steps$dragged$reports[[1]]$range), c(0.375, 0.625))
  expect_identical(steps$same$width, "25%")
  expect_identical(steps$same$left, "37.5%")
  expect_length(steps$same$reports, 0)
  expect_identical(steps$range$width, "50%")
  expect_identical(steps$range$reports[[1]]$action, "render")
  expect_equal(unlist(steps$range$reports[[1]]$range), c(0.5, 1))
  expect_null(steps$off$width)
  expect_length(steps$off$reports, 0)
  expect_length(steps$on$reports, 1)

  # A range as Shiny gives it, a list, goes back into GC_zoom()
  expect_identical(GC_zoom(chart, range = list(0.4, 0.2))$x$zoom$range, c(0.2, 0.4))
})

test_that("a new render_id starts at the range again, also without rendered = TRUE", {
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class",
                    height = "400px", width = "800px") %>%
    GC_events(zoom = TRUE, render_id = "load-1") %>%
    GC_zoom(range = c(0.25, 0.5))
  expect_null(chart$x$events$rendered)
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file, width = 1000, height = 600)
  result <- session$Runtime$evaluate("
    (() => {
      const el = document.querySelector('.geneviewer');
      const widget = HTMLWidgets.find('#' + el.id);
      const x = JSON.parse(document.querySelector('script[data-for=\"' + el.id + '\"]').textContent).x;
      const copy = renderId => {
        const next = JSON.parse(JSON.stringify(x));
        next.events.renderId = renderId;
        return next;
      };
      const reports = [];
      el.addEventListener('geneviewer:zoom', event => reports.push(event.detail));
      const zoomWindow = () => {
        const node = el.querySelector('[data-zoom-part=window]');
        return { left: node.style.left, width: node.style.width };
      };
      const node = el.querySelector('[data-zoom-part=window]');
      const box = node.getBoundingClientRect();
      const x0 = box.left + box.width / 2, y = box.top + box.height / 2;
      node.dispatchEvent(new PointerEvent('pointerdown', { bubbles: true, clientX: x0, clientY: y, pointerId: 1, button: 0 }));
      window.dispatchEvent(new PointerEvent('pointermove', { clientX: x0 + box.width / 2, clientY: y, pointerId: 1 }));
      window.dispatchEvent(new PointerEvent('pointerup', { clientX: x0 + box.width / 2, clientY: y, pointerId: 1 }));
      const steps = { dragged: zoomWindow() };
      reports.splice(0);
      // The same load drawn again keeps the window the user picked
      widget.renderValue(copy('load-1'));
      steps.same = { ...zoomWindow(), reports: reports.splice(0) };
      // A new load of the same data and range starts at the range again
      widget.renderValue(copy('load-2'));
      steps.load = { ...zoomWindow(), reports: reports.splice(0) };
      return JSON.stringify(steps);
    })()
  ")
  expect_null(result$exceptionDetails)
  steps <- jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
  expect_identical(steps$dragged$left, "37.5%")
  expect_identical(steps$same$left, "37.5%")
  expect_length(steps$same$reports, 0)
  expect_identical(steps$load$left, "25%")
  expect_identical(steps$load$width, "25%")
  expect_identical(steps$load$reports[[1]]$action, "render")
  expect_equal(unlist(steps$load$reports[[1]]$range), c(0.25, 0.5))
})

test_that("saved images leave out the slider and its room", {
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class",
                    height = "400px", width = "800px") %>%
    GC_title("Zoomed") %>%
    GC_legend(position = "bottom") %>%
    # A moved legend label is drawn above the chart, below the slider
    GC_element_style("legend", group = "Monooxygenase", part = "label", moveY = 4) %>%
    GC_zoom(range = c(0.2, 0.6))
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file, width = 1000, height = 600)
  result <- session$Runtime$evaluate("
    (() => {
      const el = document.querySelector('.geneviewer');
      const box = el.getBoundingClientRect();
      const row = el.querySelector('.geneviewer-zoom-row');
      const style = getComputedStyle(row);
      const band = row.getBoundingClientRect().height + parseFloat(style.marginTop) + parseFloat(style.marginBottom);
      const top = node => node.getBoundingClientRect().top;
      const label = [...el.querySelectorAll('text')].find(n => n.textContent === 'Monooxygenase');
      const legendTop = top(el.querySelector('[id^=geneviewer-legend-container] svg')) - box.top;
      const titleTop = top(el.querySelector('[id^=geneviewer-title-container] svg')) - box.top;
      // Draw the saved SVG on the page to measure it
      const holder = document.createElement('div');
      holder.innerHTML = chartToSVG(el).replace(/^<\\?xml[^>]*>/, '');
      document.body.appendChild(holder);
      const saved = holder.querySelector('svg');
      const savedBox = saved.getBoundingClientRect();
      const parts = [...saved.children].filter(n => n.tagName === 'svg');
      const savedLabel = [...saved.querySelectorAll('text')].find(n => n.textContent === 'Monooxygenase');
      return JSON.stringify({
        band, elHeight: box.height, savedHeight: +saved.getAttribute('height'),
        legendShift: legendTop - Math.min(...parts.map(n => +n.getAttribute('y')).filter(y => Math.abs(y - (legendTop - band)) < 1).concat([1e9])),
        titleKept: parts.some(n => Math.abs(+n.getAttribute('y') - titleTop) < 0.5),
        labelShift: (top(label) - box.top) - (top(savedLabel) - savedBox.top),
        slider: holder.innerHTML.includes('geneviewer-zoom')
      });
    })()
  ")
  expect_null(result$exceptionDetails)
  value <- jsonlite::fromJSON(result$result$value)
  expect_gt(value$band, 20)
  expect_equal(value$savedHeight, ceiling(value$elHeight - value$band))
  expect_equal(value$legendShift, value$band, tolerance = 0.01)
  expect_true(value$titleKept)
  expect_equal(value$labelShift, value$band, tolerance = 0.5)
  expect_false(value$slider)
})
