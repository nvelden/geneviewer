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

test_that("text drag events are opt-in and chart titles support individual offsets", {
  chart <- GC_chart(data.frame(start = 1, end = 20)) %>% GC_title("Chart", subtitle = "Sub")
  expect_null(GC_events(chart)$x$events$drag)
  expect_true(GC_events(chart, drag = TRUE)$x$events$drag)
  expect_error(GC_events(chart, drag = NA), "TRUE or FALSE")
  edited <- chart %>%
    GC_element_style("title", part = "title", moveX = 25, moveY = 10, fill = "red") %>%
    GC_element_style("title", part = "subtitle", moveX = -5) %>%
    GC_element_style("title", part = "title", moveY = 20)
  expect_equal(edited$x$title$itemStyle[[2]], list(moveX = 25, moveY = 20, fill = "red", part = "title"))
  reset <- GC_element_style(edited, "title", part = "title", reset = TRUE)
  expect_equal(reset$x$title$itemStyle, list(list(moveX = -5, part = "subtitle")))
  expect_error(GC_element_style(chart, "title", part = "title", cluster = 1), "part")
  expect_error(GC_element_style(chart, "title", part = "other"), "part")
})

test_that("dragging works without selection and Escape cancels before reporting", {
  chart <- GC_chart(data.frame(start = 10, end = 50, name = "Gene"), height = "250px") %>%
    GC_labels("name", adjustLabels = FALSE, rotate = 30) %>%
    GC_events(click = FALSE, drag = TRUE)
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file)
  result <- session$Runtime$evaluate("
    (() => {
      const el = document.querySelector('.geneviewer');
      const node = el.querySelector('text.label');
      const drags = [];
      el.addEventListener('geneviewer:drag', event => drags.push(event.detail));
      el.style.transform = 'scale(0.5)'; el.style.transformOrigin = 'top left';
      const pointer = (target, type, x, y) => target.dispatchEvent(new PointerEvent(type, {
        pointerId: 1, isPrimary: true, button: 0, bubbles: true, clientX: x, clientY: y
      }));
      const r = node.getBoundingClientRect(), x = r.left, y = r.top;
      pointer(node, 'pointerdown', x, y);
      pointer(window, 'pointermove', x + 10, y + 5);
      pointer(window, 'pointerup', x + 10, y + 5);
      const committed = node.getAttribute('transform');
      pointer(node, 'pointerdown', x, y);
      pointer(window, 'pointermove', x + 20, y + 10);
      window.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }));
      pointer(window, 'pointerup', x + 20, y + 10);
      const cancelled = node.getAttribute('transform');
      pointer(el.querySelector('.gene'), 'pointerdown', x, y);
      pointer(window, 'pointermove', x + 20, y + 10);
      pointer(window, 'pointerup', x + 20, y + 10);
      const initialDrags = drags.slice();
      const widget = HTMLWidgets.find('#' + el.id);
      const input = JSON.parse(document.querySelector('script[data-for]').textContent).x;
      pointer(node, 'pointerdown', x, y);
      widget.renderValue(input);
      const deferredStart = el.contains(node);
      pointer(window, 'pointermove', x + 10, y + 5);
      widget.renderValue(input);
      const deferred = el.contains(node);
      pointer(window, 'pointerup', x + 10, y + 5);
      const flushed = !el.contains(node);
      return JSON.stringify({ drags: initialDrags, committed, cancelled, deferredStart, deferred, flushed,
        totalDrags: drags.length, dragging: el.classList.contains('geneviewer-dragging') });
    })()
  ")
  expect_null(result$exceptionDetails)
  value <- jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
  expect_length(value$drags, 1)
  expect_equal(value$drags[[1]]$component, "labels")
  expect_equal(value$drags[[1]]$data$rowID, 1)
  expect_equal(value$drags[[1]]$position, list(moveX = 20, moveY = 10))
  expect_identical(value$cancelled, value$committed)
  expect_false(value$dragging)
  expect_true(value$deferredStart)
  expect_true(value$deferred)
  expect_true(value$flushed)
  expect_equal(value$totalDrags, 2)
})

test_that("selection reports final label styles and clears when data changes during a drag", {
  chart <- GC_chart(data.frame(start = c(10, 30, 50), end = c(25, 45, 65),
                               name = c("long gene label alpha", "long gene label beta", "long gene label gamma")),
                    width = "450px", height = "300px") %>%
    GC_labels("name", fontSize = "30px") %>%
    GC_element_style("labels", 1, fill = "red", moveX = 25, moveY = 10) %>%
    GC_events(click = FALSE, select = TRUE, drag = TRUE)
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file)
  result <- session$Runtime$evaluate("
    (() => {
      const el = document.querySelector('.geneviewer');
      const widget = HTMLWidgets.find('#' + el.id);
      const input = JSON.parse(document.querySelector('script[data-for]').textContent).x;
      const selections = [], drags = [];
      el.addEventListener('geneviewer:select', e => selections.push(e.detail));
      el.addEventListener('geneviewer:drag', e => drags.push(e.detail));
      const label = () => el.querySelector('text.label[rowID=\"1\"]');
      label().dispatchEvent(new MouseEvent('click', { bubbles: true, detail: 1 }));
      const rotation = selections.at(-1).style.rotate;
      const transform = label().getAttribute('transform');
      Object.values(input.series)[0].labels.itemStyle = [];
      widget.renderValue(input);
      const reset = selections.at(-1).style;
      const pointer = (target, type, x, y) => target.dispatchEvent(new PointerEvent(type, {
        pointerId: 1, isPrimary: true, button: 0, bubbles: true, clientX: x, clientY: y
      }));
      pointer(label(), 'pointerdown', 100, 100);
      pointer(window, 'pointermove', 120, 110);
      // Drop any deferred settings redraw before replacing the dataset.
      widget.renderValue(input);
      input.data.name = ['new alpha', 'new beta', 'new gamma'];
      Object.values(input.series)[0].data.name = input.data.name;
      widget.renderValue(input);
      pointer(window, 'pointerup', 120, 110);
      return JSON.stringify({ rotation, transform, reset,
        selected: el.querySelectorAll('.geneviewer-selected').length,
        cleared: selections.at(-1) === null, drags: drags.length });
    })()
  ")
  expect_null(result$exceptionDetails)
  value <- jsonlite::fromJSON(result$result$value)
  expect_equal(value$rotation, -65)
  expect_match(value$transform, "rotate\\(-65,")
  expect_equal(value$reset$moveX, 0)
  expect_equal(value$reset$moveY, 0)
  expect_false(identical(value$reset$fill, "#ff0000"))
  expect_equal(value$selected, 0)
  expect_true(value$cleared)
  expect_equal(value$drags, 0)
})

for (component in c("scaleBar", "scale")) test_that(paste(component, "drags as a whole group and retains per-cluster offsets"), {
  chart <- GC_chart(data.frame(start = c(10, 20), end = c(50, 60), cluster = c("A", "B")),
                    cluster = "cluster", height = "400px") %>%
    GC_scale(hidden = FALSE) %>%
    GC_scaleBar(scaleBarUnit = 10) %>%
    GC_element_style(component, cluster = "A", moveX = 5, moveY = -5) %>%
    GC_events(click = FALSE, select = TRUE, drag = TRUE)
  expect_error(GC_element_style(chart, component, cluster = "A", part = "label"), "whole")
  reset <- GC_element_style(chart, component, cluster = "A", reset = TRUE)
  expect_length(reset$x$series$A[[component]]$itemStyle, 0)
  expect_null(chart$x$series$B[[component]]$itemStyle)
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file)
  result <- session$Runtime$evaluate(gsub("SCALE_COMPONENT", component, "
    (() => {
      const el = document.querySelector('.geneviewer');
      const widget = HTMLWidgets.find('#' + el.id);
      const input = JSON.parse(document.querySelector('script[data-for]').textContent).x;
      const bars = () => [...el.querySelectorAll('[data-component=\"SCALE_COMPONENT\"]')];
      const bar = () => bars().find(n => n.closest('[data-cluster]').dataset.cluster === 'A');
      const other = bars().find(n => n !== bar());
      const otherTransform = other.getAttribute('transform');
      const children = [...bar().querySelectorAll('line, text, path')];
      const geometry = children.map(n => n.outerHTML);
      const drags = [];
      el.addEventListener('geneviewer:drag', e => drags.push(e.detail));
      const pointer = (node, type, x, y) => node.dispatchEvent(new PointerEvent(type, {
        pointerId: 1, isPrimary: true, button: 0, bubbles: true, clientX: x, clientY: y
      }));
      // The main line, both ticks and the label all move the same group.
      children.forEach(node => {
        pointer(node, 'pointerdown', 100, 100);
        pointer(window, 'pointermove', 120, 110);
        pointer(window, 'pointerup', 120, 110);
      });
      const moved = bar().getAttribute('transform');
      pointer(children[0], 'pointerdown', 100, 100);
      pointer(window, 'pointermove', 130, 120);
      window.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }));
      const cancelled = bar().getAttribute('transform') === moved;
      const intact = children.every((n, i) => n.outerHTML === geometry[i] && bar().contains(n));
      const isolated = other.getAttribute('transform') === otherTransform;
      const selected = el.querySelectorAll('.geneviewer-selected').length;
      input.series.A.SCALE_COMPONENT.itemStyle = [drags.at(-1).position];
      widget.renderValue(input);
      widget.resize(700, 450);
      const restored = bar().__drawn__;
      input.events.drag = false;
      widget.renderValue(input);
      pointer(bar().querySelector('text'), 'pointerdown', 100, 100);
      pointer(window, 'pointermove', 140, 120);
      pointer(window, 'pointerup', 140, 120);
      return JSON.stringify({ drags, intact, isolated, cancelled, selected, restored });
    })()
  "))
  expect_null(result$exceptionDetails)
  value <- jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
  expect_gt(length(value$drags), 0)
  expect_true(value$intact)
  expect_true(value$isolated)
  expect_true(value$cancelled)
  expect_equal(value$selected, 1)
  expect_equal(value$restored, list(moveX = 5 + 20 * length(value$drags), moveY = -5 + 10 * length(value$drags)))
  for (i in seq_along(value$drags)) {
    expect_equal(value$drags[[i]]$component, component)
    expect_equal(value$drags[[i]]$cluster, "A")
    expect_null(value$drags[[i]]$part)
    expect_equal(value$drags[[i]]$position, list(moveX = 5 + 20 * i, moveY = -5 + 10 * i))
  }
})

test_that("moves are undone, redone and reset from the toolbar, keys and arrow keys", {
  chart <- GC_chart(data.frame(start = c(10, 100), end = c(50, 140), name = c("Gene", "Other")),
                    height = "250px") %>%
    GC_labels("name", adjustLabels = FALSE) %>%
    # Set in R: reset keeps it
    GC_element_style("labels", 2, moveX = 5, moveY = 5) %>%
    GC_events(click = FALSE, select = TRUE, drag = TRUE)
  expect_true("font-awesome" %in% vapply(chart$dependencies, `[[`, "", "name"))
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file)
  result <- session$Runtime$evaluate("
    (() => {
      const el = document.querySelector('.geneviewer');
      const label = (row = 1) => el.querySelector('text.label[rowID=\"' + row + '\"]');
      const at = (row = 1) => label(row).__drawn__.moveX + ',' + label(row).__drawn__.moveY;
      const drags = [], multi = [];
      el.addEventListener('geneviewer:drag', event => drags.push(event.detail));
      el.addEventListener('geneviewer:moves', event => multi.push(event.detail));
      const shown = name => {
        const b = el.querySelector('.geneviewer-' + name);
        return !b.closest('.geneviewer-history').hidden && !b.disabled;
      };
      const pointer = (target, type, x, y) => target.dispatchEvent(new PointerEvent(type, {
        pointerId: 1, isPrimary: true, button: 0, bubbles: true, clientX: x, clientY: y
      }));
      const drag = (dx, dy, row = 1) => {
        const r = label(row).getBoundingClientRect(), x = r.left + 2, y = r.top + 2;
        pointer(label(row), 'pointerdown', x, y);
        pointer(window, 'pointermove', x + dx, y + dy);
        pointer(window, 'pointerup', x + dx, y + dy);
      };
      const key = (k, mods = {}) => document.body.dispatchEvent(new KeyboardEvent('keydown',
        Object.assign({ key: k, bubbles: true, cancelable: true }, mods)));
      const order = Array.from(el.querySelectorAll('.geneviewer-toolbar button, .geneviewer-toolbar-divider, [id^=saveMenu]'),
        n => n.className || n.id);
      const before = { undo: shown('undo'), redo: shown('redo'), reset: shown('reset'),
        group: el.querySelector('.geneviewer-history').hidden };
      drag(10, 5); drag(20, 0);
      const dragged = at();
      el.querySelector('.geneviewer-undo').click();
      const undone = at(), afterUndo = { undo: shown('undo'), redo: shown('redo') };
      key('z', { ctrlKey: true });
      const undoneTwice = at(), empty = { undo: shown('undo'), redo: shown('redo') };
      key('z', { ctrlKey: true, shiftKey: true });
      key('y', { ctrlKey: true });
      const redone = at();
      // Arrow keys move the selected label; a run of them is one step
      key('ArrowRight'); key('ArrowRight'); key('ArrowDown', { shiftKey: true });
      const nudged = at();
      key('z', { metaKey: true });
      const nudgeUndone = at();
      // Reset moves everything back to where it was first drawn, in one step
      drag(10, 0, 2);
      const second = at(2);
      el.querySelector('.geneviewer-reset').click();
      const reset1 = at(), reset2 = at(2), afterReset = shown('reset');
      el.querySelector('.geneviewer-undo').click();
      const resetUndone = at() + ' ' + at(2);
      // Kept on resize outside Shiny
      const widget = HTMLWidgets.find('#' + el.id);
      widget.resize(el.clientWidth, el.clientHeight);
      const resized = at(), keptSteps = shown('undo');
      // Moved elsewhere by new settings: the steps no longer apply
      const input = JSON.parse(document.querySelector('script[data-for]').textContent).x;
      input.series[Object.keys(input.series)[0]].labels.itemStyle = [{ rowID: 1, moveX: 3, moveY: 3 }];
      widget.renderValue(input);
      const reset = { at: at(), undo: shown('undo'), redo: shown('redo') };
      return JSON.stringify({ order, before, dragged, undone, afterUndo, undoneTwice, empty, redone,
        nudged, nudgeUndone, second, reset1, reset2, afterReset, resetUndone, resized, keptSteps, reset,
        actions: drags.map(d => d.action), last: drags.at(-1).position,
        multi: multi.map(m => ({ action: m.action, n: m.moves.length, rows: m.moves.map(d => d.data.rowID).sort() })) });
    })()
  ")
  expect_null(result$exceptionDetails)
  value <- jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
  # Undo, redo and reset, then a divider before the save icon
  expect_equal(unlist(value$order)[1:4], c("geneviewer-toolbar-button geneviewer-undo",
                                           "geneviewer-toolbar-button geneviewer-redo",
                                           "geneviewer-toolbar-button geneviewer-reset",
                                           "geneviewer-toolbar-divider"))
  expect_match(value$order[[5]], "^saveMenu-")
  expect_equal(value$before, list(undo = FALSE, redo = FALSE, reset = FALSE, group = TRUE))
  expect_equal(value$dragged, "30,5")
  expect_equal(value$undone, "10,5")
  expect_equal(value$afterUndo, list(undo = TRUE, redo = TRUE))
  expect_equal(value$undoneTwice, "0,0")
  expect_equal(value$empty, list(undo = FALSE, redo = TRUE))
  expect_equal(value$redone, "30,5")
  expect_equal(value$nudged, "32,15")
  expect_equal(value$nudgeUndone, "30,5")
  expect_equal(value$second, "15,5")
  expect_equal(c(value$reset1, value$reset2), c("0,0", "5,5"))
  expect_false(value$afterReset)
  expect_equal(value$resetUndone, "30,5 15,5")
  expect_equal(value$resized, "30,5")
  expect_true(value$keptSteps)
  expect_equal(value$reset, list(at = "3,3", undo = FALSE, redo = FALSE))
  expect_equal(unlist(value$actions), c("drag", "drag", "undo", "undo", "redo", "redo", "undo", "drag"))
  expect_equal(value$last, list(moveX = 15, moveY = 5))
  # A step of more elements is reported once
  expect_equal(value$multi, list(list(action = "reset", n = 2L, rows = list(1L, 2L)),
                                 list(action = "undo", n = 2L, rows = list(1L, 2L))))
})

test_that("drags on a scaled chart move by whole pixels", {
  chart <- GC_chart(data.frame(start = 10, end = 50, name = "Gene"), height = "250px") %>%
    GC_labels("name", adjustLabels = FALSE) %>%
    GC_events(click = FALSE, drag = TRUE)
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file)
  result <- session$Runtime$evaluate("
    (() => {
      const el = document.querySelector('.geneviewer');
      const node = el.querySelector('text.label');
      const drags = [];
      el.addEventListener('geneviewer:drag', event => drags.push(event.detail.position));
      el.style.transform = 'scale(0.3)'; el.style.transformOrigin = 'top left';
      const pointer = (target, type, x, y) => target.dispatchEvent(new PointerEvent(type, {
        pointerId: 1, isPrimary: true, button: 0, bubbles: true, clientX: x, clientY: y
      }));
      const r = node.getBoundingClientRect(), x = r.left + 1, y = r.top + 1;
      pointer(node, 'pointerdown', x, y);
      pointer(window, 'pointermove', x + 10, y + 7);
      pointer(window, 'pointerup', x + 10, y + 7);
      return JSON.stringify({ drag: drags[0], drawn: node.__drawn__ });
    })()
  ")
  expect_null(result$exceptionDetails)
  value <- jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
  # 10 / 0.3 and 7 / 0.3 pixels
  expect_equal(value$drag, list(moveX = 33, moveY = 23))
  expect_equal(value$drawn[c("moveX", "moveY")], list(moveX = 33, moveY = 23))
})

test_that("legend labels and markers drag on their own and keep their offsets", {
  data <- data.frame(start = c(10, 60), end = c(40, 90), group = c("A", "B"))
  chart <- GC_chart(data, group = "group", height = "250px") %>%
    GC_legend(TRUE) %>%
    GC_element_style("legend", group = "B", part = "marker", moveX = 4, moveY = -3) %>%
    GC_events(click = FALSE, drag = TRUE)
  expect_mapequal(chart$x$legend$itemStyle[[1]], list(group = "B", part = "marker", moveX = 4, moveY = -3))
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file)
  result <- session$Runtime$evaluate("
    (() => {
      const el = document.querySelector('.geneviewer');
      const part = (group, name) => Array.from(el.querySelectorAll('.legend-' + name))
        .find(n => n.__data__ === group);
      const drags = [];
      el.addEventListener('geneviewer:drag', event => drags.push(event.detail));
      const pointer = (target, type, x, y) => target.dispatchEvent(new PointerEvent(type, {
        pointerId: 1, isPrimary: true, button: 0, bubbles: true, clientX: x, clientY: y
      }));
      const drag = (node, dx, dy) => {
        const r = node.getBoundingClientRect(), x = r.left + r.width / 2, y = r.top + r.height / 2;
        pointer(node, 'pointerdown', x, y);
        pointer(window, 'pointermove', x + dx, y + dy);
        pointer(window, 'pointerup', x + dx, y + dy);
        // The click a browser sends after the pointer is released
        node.dispatchEvent(new MouseEvent('click', { bubbles: true, detail: 1 }));
      };
      const label = part('A', 'label'), marker = part('A', 'marker');
      const labelBefore = label.getBoundingClientRect().toJSON();
      drag(marker, 30, 20);
      const markerMoved = marker.__drawn__, labelStays = label.getBoundingClientRect().toJSON();
      drag(label, -10, 15);
      const fromR = part('B', 'marker').__drawn__;
      const shown = el.querySelectorAll('.legend.unselected').length === 0 &&
        Array.from(el.querySelectorAll('.gene')).every(n => n.style.display !== 'none');
      // A click that is not the end of a drag still toggles the group, also
      // on a moved part, which is dimmed with the rest of its item
      part('A', 'label').dispatchEvent(new MouseEvent('click', { bubbles: true, detail: 1 }));
      const dimmed = getComputedStyle(part('A', 'marker')).opacity;
      return JSON.stringify({ dimmed, markerMoved, labelSame: labelBefore.x === labelStays.x && labelBefore.y === labelStays.y,
        labelMoved: label.__drawn__, fromR, shown, toggled: el.querySelectorAll('.legend.unselected').length,
        drags: drags.map(d => ({ part: d.part, group: d.data, position: d.position })) });
    })()
  ")
  expect_null(result$exceptionDetails)
  value <- jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
  expect_equal(value$markerMoved[c("moveX", "moveY")], list(moveX = 30, moveY = 20))
  expect_true(value$labelSame)
  expect_equal(value$labelMoved[c("moveX", "moveY")], list(moveX = -10, moveY = 15))
  expect_equal(value$fromR[c("moveX", "moveY")], list(moveX = 4, moveY = -3))
  expect_true(value$shown)
  expect_equal(value$toggled, 1)
  expect_equal(value$dimmed, "0.5")
  expect_equal(value$drags, list(
    list(part = "marker", group = "A", position = list(moveX = 30, moveY = 20)),
    list(part = "label", group = "A", position = list(moveX = -10, moveY = 15))))
})

test_that("several elements are selected, moved together and undone as one step", {
  data <- data.frame(start = c(10, 100), end = c(50, 140), name = c("Gene", "Other"), group = c("A", "B"))
  chart <- GC_chart(data, group = "group", height = "300px") %>%
    GC_labels("name", adjustLabels = FALSE) %>%
    GC_legend(TRUE) %>%
    GC_events(click = FALSE, select = TRUE, drag = TRUE)
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file)
  result <- session$Runtime$evaluate("
    (() => {
      const el = document.querySelector('.geneviewer');
      const label = row => el.querySelector('text.label[rowID=\"' + row + '\"]');
      const at = row => label(row).__drawn__.moveX + ',' + label(row).__drawn__.moveY;
      const selections = [], selects = [], moves = [];
      el.addEventListener('geneviewer:selection', e => selections.push(e.detail.map(d => d.component + ':' + (d.data?.rowID ?? d.data) + ':' + (d.part ?? ''))));
      el.addEventListener('geneviewer:select', e => selects.push(e.detail && (e.detail.data?.rowID ?? e.detail.data)));
      el.addEventListener('geneviewer:moves', e => moves.push({ action: e.detail.action, n: e.detail.moves.length }));
      const click = (node, mods = {}) => node.dispatchEvent(new MouseEvent('click', Object.assign({ bubbles: true, detail: 1 }, mods)));
      const pointer = (target, type, x, y, mods = {}) => target.dispatchEvent(new PointerEvent(type, Object.assign({
        pointerId: 1, isPrimary: true, button: 0, bubbles: true, clientX: x, clientY: y }, mods)));
      const key = (k, mods = {}) => document.body.dispatchEvent(new KeyboardEvent('keydown',
        Object.assign({ key: k, bubbles: true, cancelable: true }, mods)));
      el.dispatchEvent(new Event('focusin'));
      click(label(1));
      click(label(2), { shiftKey: true });
      const two = selections.at(-1), primary = selects.at(-1);
      const outlined = el.querySelectorAll('.geneviewer-selected').length;
      // Dragging one of them moves both
      const r = label(1).getBoundingClientRect(), x = r.left + 2, y = r.top + 2;
      pointer(label(1), 'pointerdown', x, y);
      pointer(window, 'pointermove', x + 10, y + 5);
      pointer(window, 'pointerup', x + 10, y + 5);
      click(label(1));
      const dragged = [at(1), at(2)], stillTwo = selections.at(-1).length;
      key('z', { ctrlKey: true });
      const undone = [at(1), at(2)];
      key('ArrowRight'); key('ArrowRight');
      const nudged = [at(1), at(2)];
      key('z', { ctrlKey: true });
      const nudgeUndone = [at(1), at(2)];
      // Shift-click takes one out again
      click(label(2), { shiftKey: true });
      const one = selections.at(-1);
      // A box around the legend selects its labels and markers, not the genes
      const legend = el.querySelector('[data-component=\"legend\"]').getBoundingClientRect();
      const x0 = legend.left - 6, y0 = legend.top - 6, x1 = legend.right + 6, y1 = legend.bottom + 6;
      const start = document.elementFromPoint(x0, y0);
      pointer(start, 'pointerdown', x0, y0);
      pointer(window, 'pointermove', (x0 + x1) / 2, (y0 + y1) / 2);
      const box = !!el.querySelector('.geneviewer-marquee');
      pointer(window, 'pointermove', x1, y1);
      pointer(window, 'pointerup', x1, y1);
      click(start);
      const boxed = selections.at(-1);
      const boxGone = !el.querySelector('.geneviewer-marquee');
      el.querySelector('.geneviewer-selected').dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }));
      return JSON.stringify({ two, primary, outlined, dragged, stillTwo, undone, nudged, nudgeUndone, one,
        box, boxed, boxGone, cleared: selections.at(-1), moves });
    })()
  ")
  expect_null(result$exceptionDetails)
  value <- jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
  expect_equal(unlist(value$two), c("labels:1:", "labels:2:"))
  expect_equal(value$primary, 2)
  expect_equal(value$outlined, 2)
  expect_equal(unlist(value$dragged), c("10,5", "10,5"))
  expect_equal(value$stillTwo, 2)
  expect_equal(unlist(value$undone), c("0,0", "0,0"))
  expect_equal(unlist(value$nudged), c("2,0", "2,0"))
  expect_equal(unlist(value$nudgeUndone), c("0,0", "0,0"))
  expect_equal(unlist(value$one), "labels:1:")
  expect_true(value$box)
  expect_setequal(unlist(value$boxed), c("legend:A:label", "legend:A:marker", "legend:B:label", "legend:B:marker"))
  expect_true(value$boxGone)
  expect_length(value$cleared, 0)
  expect_equal(value$moves[[1]], list(action = "drag", n = 2L))
  expect_equal(value$moves[[2]], list(action = "undo", n = 2L))
})

test_that("Escape cancels a selection box and a held-back redraw keeps a drop", {
  data <- data.frame(start = c(10, 100), end = c(50, 140), name = c("Gene", "Other"))
  chart <- GC_chart(data, height = "300px") %>%
    GC_labels("name", adjustLabels = FALSE) %>%
    GC_events(click = FALSE, select = TRUE, drag = TRUE)
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file)
  result <- session$Runtime$evaluate("
    (() => {
      const el = document.querySelector('.geneviewer');
      const widget = HTMLWidgets.find('#' + el.id);
      const input = JSON.parse(document.querySelector('script[data-for]').textContent).x;
      const label = row => el.querySelector('text.label[rowID=\"' + row + '\"]');
      const selects = [];
      el.addEventListener('geneviewer:select', e => selects.push(e.detail && e.detail.data.rowID));
      const pointer = (target, type, x, y, mods = {}) => target.dispatchEvent(new PointerEvent(type, Object.assign({
        pointerId: 1, isPrimary: true, button: 0, bubbles: true, clientX: x, clientY: y }, mods)));
      label(1).dispatchEvent(new MouseEvent('click', { bubbles: true, detail: 1 }));
      label(1).focus();
      // Escape on the focused element while a box adds to the selection: only
      // the box ends, and the selection before it stays
      const box = el.getBoundingClientRect();
      const x0 = box.left + 2, y0 = box.bottom - 2;
      pointer(document.elementFromPoint(x0, y0), 'pointerdown', x0, y0, { shiftKey: true });
      pointer(window, 'pointermove', x0 + 40, y0 - 40);
      const escape = new KeyboardEvent('keydown', { key: 'Escape', bubbles: true, cancelable: true });
      document.activeElement.dispatchEvent(escape);
      pointer(window, 'pointerup', x0 + 40, y0 - 40);
      const afterEscape = { selects: selects.slice(), outlined: el.querySelectorAll('.geneviewer-selected').length };
      // A settings redraw that arrives during a drag waits for the drop and
      // does not move the element back
      const r = label(2).getBoundingClientRect(), x = r.left + 2, y = r.top + 2;
      pointer(label(2), 'pointerdown', x, y);
      pointer(window, 'pointermove', x + 12, y + 7);
      widget.renderValue(input);
      pointer(window, 'pointerup', x + 12, y + 7);
      return JSON.stringify({ afterEscape, dropped: label(2).__drawn__,
        transform: label(2).getAttribute('transform'),
        undo: !el.querySelector('.geneviewer-undo').disabled });
    })()
  ")
  expect_null(result$exceptionDetails)
  value <- jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
  expect_equal(value$afterEscape$selects, list(1L))
  expect_equal(value$afterEscape$outlined, 1)
  expect_equal(value$dropped[c("moveX", "moveY")], list(moveX = 12, moveY = 7))
  expect_match(value$transform, "^translate\\(12, 7\\)")
  expect_true(value$undo)
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
  expect_error(GC_toolbar(chart, buttons = c("undo", "zoom")), "'buttons' must be")
  expect_error(GC_toolbar(chart, size = -1), "positive number")
  expect_error(GC_toolbar(chart, color = c("red", "blue")), "single colour")
  expect_error(GC_toolbar(chart, icons = list(zoom = "magnifying-glass")), "'icons' must name")
  expect_error(GC_toolbar(chart, icons = "camera"), "'icons' must name")
  expect_error(GC_theme(chart, toolbar = list(show = "sometimes")), "'show' must be one of")
  chart <- chart %>%
    GC_toolbar(show = "always", buttons = "save", icons = c(save = "camera")) %>%
    GC_toolbar(size = 16)
  expect_equal(chart$x$toolbar, list(show = "always", buttons = list("save"), icons = list(save = "camera"), size = 16))
  expect_true("font-awesome" %in% vapply(chart$dependencies, `[[`, "", "name"))
  # The built-in themes keep the icons visible on their background
  expect_equal(geneviewer_theme("dark")$toolbar$hoverColor, "#eeeeee")
  expect_equal(theme_to_js(geneviewer_theme("dark"))$toolbarOptions$color, "#999999")
  # In the style of each theme's tooltip, and with its font
  publication <- theme_to_js(geneviewer_theme("publication"))$toolbarOptions
  expect_equal(publication[c("tooltipBorder", "tooltipRadius", "fontFamily")],
               list(tooltipBorder = "1px solid black", tooltipRadius = "0",
                    fontFamily = "Arial, Helvetica, sans-serif"))
  for (theme in c("classic", "dark", "vintage", "infographic", "publication", "bw")) {
    expect_named(geneviewer_theme(theme)$toolbar[c("color", "hoverColor", "tooltipBackground")])
  }
  expect_error(GC_toolbar(chart, tooltipRadius = -2), "tooltipRadius")
})

test_that("the toolbar takes its buttons, icons, corner and colours from GC_toolbar() and the theme", {
  data <- data.frame(start = c(10, 60), end = c(40, 90), name = c("A", "B"))
  page <- function(chart) {
    file <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
    file
  }
  inspect <- "
    (() => {
      const el = document.querySelector('.geneviewer');
      const bar = el.querySelector('.geneviewer-toolbar');
      if (!bar) return JSON.stringify({ bar: false });
      const style = getComputedStyle(bar), box = bar.getBoundingClientRect(), chart = el.getBoundingClientRect();
      const save = el.querySelector('.geneviewer-save-icon');
      return JSON.stringify({ bar: true, classes: bar.className, opacity: style.opacity,
        buttons: Array.from(bar.querySelectorAll('button'), b => b.className.replace('geneviewer-toolbar-button ', '')),
        icons: Array.from(bar.querySelectorAll('button > i, .geneviewer-save-icon'), i => i.className.replace(' geneviewer-save-icon', '')),
        divider: !!bar.querySelector('.geneviewer-toolbar-divider'),
        saveColor: save && getComputedStyle(save).color, saveSize: save && getComputedStyle(save).fontSize,
        tip: getComputedStyle(bar).getPropertyValue('--gv-toolbar-tip-background').trim(),
        left: Math.round(box.left - chart.left), bottom: Math.round(chart.bottom - box.bottom) });
    })()"
  run <- function(chart) {
    session <- chrome_page(page(chart), env = parent.frame(2))
    result <- session$Runtime$evaluate(inspect)
    expect_null(result$exceptionDetails)
    jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
  }
  chart <- GC_chart(data, height = "200px") %>% GC_labels("name") %>% GC_events(select = TRUE, drag = TRUE)

  custom <- run(chart %>% GC_theme("dark") %>%
    GC_toolbar(show = "always", position = "bottomleft", buttons = c("undo", "save"), size = 16,
               color = "rgb(1, 2, 3)", tooltipBackground = "rgb(4, 5, 6)",
               icons = list(undo = "fa-regular fa-circle-left", save = "camera")))
  expect_match(custom$classes, "geneviewer-toolbar-bottomleft")
  expect_match(custom$classes, "geneviewer-toolbar-always")
  expect_equal(custom$opacity, "1")
  expect_equal(unlist(custom$buttons), "geneviewer-undo")
  expect_equal(unlist(custom$icons), c("fa-regular fa-circle-left", "fa-solid fa-camera"))
  expect_true(custom$divider)
  # GC_toolbar() over the dark theme
  expect_equal(custom$saveColor, "rgb(1, 2, 3)")
  expect_equal(custom$saveSize, "16px")
  expect_equal(custom$tip, "rgb(4, 5, 6)")
  expect_equal(c(custom$left, custom$bottom), c(10, 10))

  themed <- run(chart %>% GC_theme("dark"))
  expect_equal(unlist(themed$buttons), c("geneviewer-undo", "geneviewer-redo", "geneviewer-reset"))
  expect_equal(themed$icons[[4]], "fa-solid fa-download")
  expect_equal(themed$saveColor, "rgb(153, 153, 153)")
  expect_equal(themed$opacity, "0")

  expect_false(run(chart %>% GC_toolbar(show = "never"))$bar)
  # Only the save button, without moves: no undo, redo or reset
  plain <- run(GC_chart(data, height = "200px") %>% GC_toolbar(buttons = c("undo", "save")))
  expect_length(plain$buttons, 0)
  expect_false(plain$divider)
})
