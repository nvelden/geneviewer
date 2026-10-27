test_that("element overrides target row IDs and merge without replacing neighbours", {
  data <- data.frame(start = c(90, 10, 50), end = c(130, 40, 80),
                     name = c("same", "same", "other"), cluster = c("A", "A", "B"))
  base <- GC_chart(data, cluster = "cluster") %>% GC_labels("name")
  chart <- base %>%
    GC_element_style("genes", 1, fill = "red") %>%
    GC_element_style("genes", 2, fill = "blue") %>%
    GC_element_style("genes", 1, marker = "box") %>%
    GC_element_style("labels", 1, text = "Changed", fontSize = "18px")
  items <- chart$x$series$A$genes$itemStyle
  expect_length(items, 2)
  expect_equal(items[[2]], list(fill = "red", rowID = 1, marker = "box"))
  expect_null(chart$x$series$B$genes$itemStyle)
  expect_null(base$x$series$A$genes$itemStyle)
  expect_equal(chart$x$series$A$labels$itemStyle[[1]]$text, "Changed")
  reset <- GC_element_style(chart, "genes", 1, reset = TRUE)
  expect_equal(reset$x$series$A$genes$itemStyle, list(list(fill = "blue", rowID = 2)))
  expect_identical(reset$x$series$A$labels, chart$x$series$A$labels)
  expect_error(GC_element_style(base, "genes", 99, fill = "red"), "row_id")
  expect_error(GC_element_style(base, "genes", 1, "red"), "names")
  expect_error(GC_element_style(base, "genes", 1, index = 0), "row_id")
  expect_error(GC_element_style(base, "genes", 1, fill = "red", reset = TRUE), "reset")
  expect_error(GC_element_style(base, "genes", 1, reset = NA), "TRUE or FALSE")
})

test_that("editor events are independent and validate their flags", {
  chart <- GC_chart(data.frame(start = 1, end = 20))
  events <- GC_events(chart, click = FALSE, dblclick = TRUE, select = TRUE,
                      legend_click = FALSE)$x$events
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

# Exercise native click/double-click ordering independently of an app.
test_that("selection and legend toggles remain independent across redraws", {
  data <- data.frame(start = c(90, 10), end = c(130, 40),
                     name = c("same", "same"), group = "A")
  chart <- GC_chart(data, group = "group", height = "300px") %>%
    GC_labels("name") %>%
    GC_genes(groupStyle = list(A = list(fill = "green"))) %>%
    GC_element_style("genes", 1, fill = "red") %>%
    GC_element_style("labels", 1, text = "Edited") %>%
    GC_events(click = FALSE, select = TRUE, dblclick = TRUE, legend_click = FALSE)
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file)
  result <- session$Runtime$evaluate("
    (() => {
      const el = document.querySelector('.geneviewer');
      const widget = HTMLWidgets.find('#' + el.id);
      const x = JSON.parse(document.querySelector('script[data-for=\"' + el.id + '\"]').textContent).x;
      const messages = [], dom = [];
      window.Shiny = { setInputValue: (name, value) => messages.push({ name, value }) };
      HTMLWidgets.shinyMode = true;
      el.addEventListener('geneviewer:dblclick', e => dom.push(e.detail));
      const gene = () => el.querySelector('.gene[rowID=\"1\"]');
      const click = (node, detail = 1) => node.dispatchEvent(new MouseEvent('click', { bubbles: true, detail }));
      const double = node => {
        click(node); click(node, 2);
        node.dispatchEvent(new MouseEvent('dblclick', { bubbles: true, detail: 2 }));
      };
      const fill = getComputedStyle(gene()).fill;
      const text = el.querySelector('text.label[rowID=\"1\"]').textContent;
      double(el.querySelector('.legend-marker'));
      double(gene());
      widget.resize(700, 400);
      widget.renderValue(x);
      const hidden = gene().style.display;
      const selected = el.querySelector('.geneviewer-selected').getAttribute('rowID');
      click(el.querySelector('.legend-marker'));
      gene().dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter', bubbles: true }));
      gene().dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }));
      const cleared = el.querySelectorAll('.geneviewer-selected').length;
      // Another part of the chart ends the selection, as empty space does
      const others = ['.legend-marker', '[data-component=\"sequence\"] line'].map(part => {
        click(gene());
        click(el.querySelector(part));
        return el.querySelectorAll('.geneviewer-selected').length;
      });
      click(el.querySelector('.legend-marker'));
      click(gene());
      x.data.name[0] = 'Replacement';
      widget.renderValue(x);
      const replaced = el.querySelectorAll('.geneviewer-selected').length;
      x.events = { click: true };
      widget.renderValue(x);
      click(gene());
      return JSON.stringify({ fill, text, hidden, selected, cleared, others, replaced, dom,
        clicks: messages.filter(m => m.name.endsWith('_click')).length,
        doubles: messages.filter(m => m.name.endsWith('_dblclick')).length });
    })()
  ")
  expect_null(result$exceptionDetails)
  value <- jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
  expect_equal(value$fill, "rgb(255, 0, 0)")
  expect_equal(value$text, "Edited")
  expect_equal(value$hidden, "none")
  expect_equal(value$selected, "1")
  expect_equal(value$cleared, 0)
  expect_equal(value$others, list(0, 0))
  expect_equal(value$replaced, 0)
  expect_equal(value$clicks, 1)
  expect_equal(value$doubles, 2)
  expect_length(value$dom, 2)
  expect_equal(value$dom[[1]]$data$rowID, 1)
})

# Links are drawn in an SVG below the cluster SVGs, which take the mouse over
# their whole area: a real mouse click on a link must still report the link.
test_that("links below the cluster SVGs report clicks and are highlighted", {
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class", height = "400px") %>%
    GC_links("class", label = FALSE) %>%
    GC_trackMouse() %>%
    GC_events(click = TRUE, highlight = TRUE, select = TRUE)
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file)
  evaluate <- function(js) {
    result <- session$Runtime$evaluate(js, returnByValue = TRUE)
    expect_null(result$exceptionDetails)
    result$result$value
  }
  evaluate("
    window.messages = [];
    window.Shiny = { setInputValue: (name, value) => messages.push({ name, value }) };
    HTMLWidgets.shinyMode = true;
    const el = document.querySelector('.geneviewer');
    HTMLWidgets.find('#' + el.id).renderValue(
      JSON.parse(document.querySelector('script[data-for=\"' + el.id + '\"]').textContent).x);
  ")
  # A point where only the cluster SVG lies above a link
  point <- evaluate("
    (() => {
      for (const link of document.querySelectorAll('path[data-component=\"links\"]')) {
        const r = link.getBoundingClientRect();
        for (let fy = 0.3; fy < 0.8; fy += 0.1) {
          const x = Math.round(r.left + r.width / 2), y = Math.round(r.top + r.height * fy);
          const stack = document.elementsFromPoint(x, y);
          const top = stack.find(e => e.matches('path[data-component=\"links\"]'));
          if (stack[0].classList.contains('geneviewer-svg-content') && top)
            return { x, y, data: JSON.stringify(top.__data__) };
        }
      }
    })()
  ")
  expect_type(point, "list")
  mouse <- function(type, ...) {
    session$Input$dispatchMouseEvent(type = type, x = point$x, y = point$y, ...)
  }
  evaluate("document.querySelector('.gene').dispatchEvent(new MouseEvent('click', { bubbles: true, detail: 1 }))")
  expect_equal(evaluate("document.querySelectorAll('.geneviewer-selected').length"), 1)
  mouse("mouseMoved")
  mouse("mousePressed", button = "left", clickCount = 1)
  mouse("mouseReleased", button = "left", clickCount = 1)
  click <- evaluate("JSON.stringify(messages.filter(m => m.name.endsWith('_click')).at(-1)?.value)")
  click <- jsonlite::fromJSON(click, simplifyVector = FALSE)
  expect_equal(click$component, "links")
  expect_identical(jsonlite::toJSON(click$data, auto_unbox = TRUE),
                   jsonlite::toJSON(jsonlite::fromJSON(point$data, simplifyVector = FALSE),
                                    auto_unbox = TRUE))
  expect_equal(evaluate("document.querySelectorAll('.geneviewer-hover').length"), 1)
  # A click on a link ends the selection of a gene
  expect_equal(evaluate("document.querySelectorAll('.geneviewer-selected').length"), 0)
  # GC_trackMouse() still follows the mouse over the cluster SVG
  expect_equal(evaluate("document.querySelector('.coordinate-tooltip').style.visibility"), "visible")
  # Leaving the link, e.g. for the save button, removes the highlight
  session$Input$dispatchMouseEvent(type = "mouseMoved", x = 2, y = 2)
  expect_equal(evaluate("document.querySelectorAll('.geneviewer-hover').length"), 0)
})
