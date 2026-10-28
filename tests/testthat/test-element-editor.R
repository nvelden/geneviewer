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
      // A second click on the selected gene ends its selection
      click(gene());
      const toggled = [el.querySelectorAll('.geneviewer-selected').length];
      click(gene());
      toggled.push(el.querySelectorAll('.geneviewer-selected').length);
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
      return JSON.stringify({ fill, text, hidden, selected, cleared, toggled, others, replaced, dom,
        clicks: messages.filter(m => m.name.endsWith('_click')).length,
        doubles: messages.filter(m => m.name.endsWith('_dblclick')).length });
    })()
  ")
  expect_null(result$exceptionDetails)
  value <- jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
  expect_equal(value$fill, "rgb(255, 0, 0)")
  expect_equal(value$text, "Edited")
  expect_equal(value$hidden, "")
  expect_equal(value$selected, "1")
  expect_equal(value$cleared, 0)
  expect_equal(value$toggled, list(1, 0))
  expect_equal(value$others, list(1, 0))
  expect_equal(value$replaced, 0)
  expect_equal(value$clicks, 1)
  expect_equal(value$doubles, 3)
  expect_length(value$dom, 3)
  expect_equal(value$dom[[1]]$part, "marker")
  expect_equal(value$dom[[2]]$data$rowID, 1)
  # How the gene is drawn: its own fill, the default arrow
  style <- value$dom[[2]]$style
  expect_equal(style$fill, "#ff0000")
  expect_equal(style[c("marker", "markerSize", "markerHeight", "arrowheadWidth", "y")],
               list(marker = "arrow", markerSize = "medium", markerHeight = 15,
                    arrowheadWidth = 10, y = 50))
  expect_equal(style$stroke, "#000000")
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

# With a pattern for some groups, every gene is filled with a pattern: one of
# a single rect for the groups without stripes
test_that("a gene without stripes reports its colour when other groups have a pattern", {
  data <- data.frame(start = c(10, 60), end = c(40, 90), group = c("A", "B"))
  chart <- GC_chart(data, group = "group", height = "200px") %>%
    GC_color(customColors = list(A = "#c7e02c", B = "#1f77b4")) %>%
    # Patterns on the gene colours, as in the Tn7 example
    GC_pattern(patterns = list(B = "diagonal"), background = NULL) %>%
    GC_events(select = TRUE)
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file)
  result <- session$Runtime$evaluate("
    (() => {
      const el = document.querySelector('.geneviewer');
      const fills = [];
      el.addEventListener('geneviewer:select', e => fills.push(e.detail.style.fill));
      el.querySelectorAll('.gene').forEach(gene =>
        gene.dispatchEvent(new MouseEvent('click', { bubbles: true, detail: 1 })));
      return JSON.stringify(fills);
    })()
  ")
  expect_null(result$exceptionDetails)
  fills <- jsonlite::fromJSON(result$result$value)
  expect_equal(fills[1], "#c7e02c")
  expect_match(fills[2], "^url\\(")
})

test_that("cluster titles, footers, labels and coordinates are styled and selected one by one", {
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
  expect_equal(chart$x$series$ophA$footer$itemStyle, list(list(text = "", part = "title")))
  expect_equal(chart$x$series$ophA$coordinates$itemStyle[[1]],
               list(fill = "red", rowID = 1, end = "start"))
  expect_error(GC_element_style(chart, "clusterTitle", cluster = 1, part = "header"), "part")
  expect_error(GC_element_style(chart, "clusterTitle", cluster = "nope", part = "title"), "cluster")
  expect_error(GC_element_style(chart, "coordinates", row_id = 1, end = "middle"), "end")
  expect_error(GC_element_style(chart, "genes", row_id = 1, cluster = 1), "row_id")
  reset <- GC_element_style(chart, "clusterLabel", cluster = 2, part = "title", reset = TRUE)
  expect_length(reset$x$series$dbophA$clusterLabel$itemStyle, 1)

  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file)
  result <- session$Runtime$evaluate("
    (() => {
      const el = document.querySelector('.geneviewer');
      const [a, b] = el.querySelectorAll('.geneviewer-svg-content');
      const css = n => n && getComputedStyle(n);
      const part = (svg, component, p) => svg.querySelector(`[data-component=\"${component}\"][data-part=\"${p}\"], [data-component=\"${component}\"] [data-part=\"${p}\"]`);
      const subtitleA = part(a, 'clusterTitle', 'subtitle');
      const coordinates = [...a.querySelectorAll('text.coordinate')];
      const drawn = {
        subtitleA: [subtitleA.textContent, css(subtitleA).fill, css(subtitleA).fontStyle],
        subtitleB: part(b, 'clusterTitle', 'subtitle').textContent,
        footerTitleA: !!part(a, 'clusterFooter', 'title'),
        footerTitleB: part(b, 'clusterFooter', 'title').textContent,
        labelB: css(part(b, 'clusterLabel', 'title')).fontSize,
        labelA: css(part(a, 'clusterLabel', 'title')).fontSize,
        labelSubB: css(part(b, 'clusterLabel', 'subtitle')).fill,
        coordinateRed: coordinates.filter(t => css(t).fill === 'rgb(255, 0, 0)').map(t => [t.__data__.rowID, t.__data__.end]),
        coordinateRow2: coordinates.filter(t => t.__data__.rowID === 2).length
      };
      const selections = [];
      el.addEventListener('geneviewer:select', e => selections.push(e.detail));
      const click = n => n.dispatchEvent(new MouseEvent('click', { bubbles: true, detail: 1 }));
      click(subtitleA);
      click(part(b, 'clusterLabel', 'title'));
      click(coordinates.find(t => t.__data__.rowID === 1 && t.__data__.end === 'start'));
      const outlined = el.querySelectorAll('.geneviewer-selected').length;
      click(a.querySelector('[data-component=\"sequence\"] line'));
      return JSON.stringify({ drawn, outlined, selections: selections.map(s => s && {
        component: s.component, cluster: s.cluster, part: s.part ?? null,
        row: s.data?.rowID ?? null, end: s.data?.end ?? null, text: s.style?.text, fill: s.style?.fill }) });
    })()
  ")
  expect_null(result$exceptionDetails)
  value <- jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
  drawn <- value$drawn
  expect_equal(drawn$subtitleA, list("Edited", "rgb(255, 0, 0)", "italic"))
  expect_equal(drawn$subtitleB, "Sub")
  expect_false(drawn$footerTitleA)
  expect_equal(drawn$footerTitleB, "Foot")
  expect_equal(c(drawn$labelA, drawn$labelB), c("12px", "20px"))
  expect_equal(drawn$labelSubB, "rgb(0, 0, 255)")
  expect_equal(drawn$coordinateRed, list(list(1, "start")))
  expect_equal(drawn$coordinateRow2, 0)
  expect_equal(value$outlined, 1)
  s <- value$selections
  expect_length(s, 4)
  expect_equal(s[[1]][c("component", "cluster", "part", "text", "fill")],
               list(component = "clusterTitle", cluster = "ophA", part = "subtitle",
                    text = "Edited", fill = "#ff0000"))
  expect_equal(s[[2]][c("component", "cluster", "part", "text")],
               list(component = "clusterLabel", cluster = "dbophA", part = "title", text = "Label B"))
  expect_equal(s[[3]][c("component", "row", "end", "fill")],
               list(component = "coordinates", row = 1, end = "start", fill = "#ff0000"))
  expect_null(s[[4]])
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
  expect_length(chart$x$legend$itemStyle, 3)
  label <- chart$x$legend$itemStyle[[3]]
  expect_equal(label$text, "Edited")
  expect_equal(label$fontSize, "20px")
  reset <- GC_element_style(chart, "legend", group = "A", part = "label", reset = TRUE)
  expect_length(reset$x$legend$itemStyle, 2)
  expect_equal(reset$x$legend$groupStyle, base$x$legend$groupStyle)
  expect_null(base$x$legend$itemStyle)
  expect_error(GC_element_style(base, "legend", group = "absent", part = "label"), "group")
  expect_error(GC_element_style(base, "legend", group = "A", part = "title"), "part")
  expect_error(GC_element_style(base, "legend", row_id = 1, group = "A", part = "label"), "group")
})

test_that("legend parts report editing events and normal mode keeps visibility toggles", {
  chart <- GC_chart(data.frame(start = c(10, 60), end = c(40, 90), group = c("A", "B")),
                    group = "group", height = "300px") %>%
    GC_element_style("legend", group = "A", part = "label", text = "Edited", fill = "red") %>%
    GC_element_style("legend", group = "A", part = "marker", fill = "blue", strokeWidth = 3) %>%
    GC_events(select = TRUE, dblclick = TRUE)
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file)
  result <- session$Runtime$evaluate("
    (() => {
      const el = document.querySelector('.geneviewer');
      const widget = HTMLWidgets.find('#' + el.id);
      const x = JSON.parse(document.querySelector('script[data-for=\"' + el.id + '\"]').textContent).x;
      const messages = [], selections = [], doubles = [];
      window.Shiny = { setInputValue: (name, value) => messages.push({ name, value }) };
      HTMLWidgets.shinyMode = true;
      widget.renderValue(x);
      el.addEventListener('geneviewer:select', e => selections.push(e.detail));
      el.addEventListener('geneviewer:dblclick', e => doubles.push(e.detail));
      const click = node => node.dispatchEvent(new MouseEvent('click', { bubbles: true, detail: 1 }));
      const label = () => el.querySelector('.legend-label');
      const marker = () => el.querySelector('.legend-marker');
      const hidden = () => el.querySelector('.gene').style.display;
      click(label());
      click(marker());
      marker().dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter', bubbles: true }));
      const editingHidden = hidden();
      widget.resize(700, 400);
      const preserved = el.querySelector('.geneviewer-selected')?.getAttribute('data-part');
      label().dispatchEvent(new MouseEvent('dblclick', { bubbles: true, detail: 2 }));
      x.events = {};
      widget.renderValue(x);
      click(label());
      const normalHidden = hidden();
      widget.resize(650, 400);
      const hiddenAfterResize = hidden();
      click(marker());
      const shown = hidden();
      // Legacy click reporting alone also edits, without a selection outline.
      x.events = { click: true };
      widget.renderValue(x);
      click(label());
      const clickOnlyHidden = hidden();
      const lastClick = messages.filter(m => m.name.endsWith('_click')).at(-1).value;
      x.events = { click: true, legendClick: false };
      widget.renderValue(x);
      click(marker());
      return JSON.stringify({ selections, doubles, editingHidden, preserved,
        normalHidden, hiddenAfterResize, shown, clickOnlyHidden, lastClick,
        optedOutHidden: hidden(), label: label().textContent });
    })()
  ")
  expect_null(result$exceptionDetails)
  value <- jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
  expect_equal(value$selections[[1]]$part, "label")
  expect_equal(value$selections[[1]]$data, "A")
  expect_equal(value$selections[[1]]$style$text, "Edited")
  expect_equal(value$selections[[1]]$style$fill, "#ff0000")
  expect_equal(value$selections[[2]]$part, "marker")
  expect_equal(value$selections[[2]]$style$fill, "#0000ff")
  expect_equal(value$selections[[2]]$style$strokeWidth, 3)
  expect_null(value$selections[[2]]$style$text)
  expect_equal(value$doubles[[1]]$part, "marker")
  expect_equal(value$doubles[[2]]$part, "label")
  expect_equal(value$editingHidden, "")
  expect_equal(value$preserved, "marker")
  expect_equal(value$normalHidden, "none")
  expect_equal(value$hiddenAfterResize, "none")
  expect_equal(value$shown, "")
  expect_equal(value$clickOnlyHidden, "")
  expect_equal(value$lastClick$part, "label")
  expect_equal(value$lastClick$data, "A")
  expect_equal(value$optedOutHidden, "none")
  expect_equal(value$label, "Edited")
})
