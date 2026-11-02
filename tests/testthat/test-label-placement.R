# Genes of two clusters: wide genes, and narrow genes that crowd near the
# right end, with labels that overlap
placement_genes <- data.frame(
  cluster = rep(c("a", "b"), each = 6),
  start = rep(c(1, 400, 800, 830, 860, 890), 2),
  end = rep(c(380, 780, 828, 858, 888, 1000), 2),
  name = rep(c("first gene", "second gene", "x long name", "y long name",
               "z long name", "last long name"), 2),
  homolog = rep(paste0("h", 1:6), 2),
  identity = c(rep(NA, 6), 91.25, 64, 80, 55, 70, 99)
)

placement_chart <- function(...) {
  GC_chart(placement_genes, cluster = "cluster", height = "260px", width = "700px") %>%
    GC_labels("name", ...)
}

test_that("GC_links() and GC_labels() check where labels go", {
  expect_error(GC_links(placement_chart(), group = "homolog", label_position = "middle"),
               "'label_position' must be one of")
  expect_error(placement_chart(adjustLabels = "shrink"),
               "'adjustLabels' must be TRUE, FALSE, \"hide\" or \"spread\"")
  expect_error(placement_chart(adjustLabels = "spread", leaderLine = "grey"),
               "'leaderLine' must be FALSE or a named list")
  expect_error(placement_chart(adjustLabels = "spread", leaderLine = list(length = "30px")),
               "'leaderLine\\$length' must be a single number")
  chart <- GC_links(placement_chart(), group = "homolog", label_position = "link")
  expect_identical(chart$x$links[[1]]$options$labelPosition, "link")
  # Links from data keep their labels when they go on the links
  alignment <- data.frame(cluster1 = "a", cluster2 = "b", start1 = 1, end1 = 380,
                          start2 = 1, end2 = 380, identity = 90)
  expect_false(GC_links(placement_chart(), data = alignment)$x$links[[1]]$options$label)
  expect_true(GC_links(placement_chart(), data = alignment,
                       label_position = "link")$x$links[[1]]$options$label)
})

# Draws the chart in Chrome and describes its labels; Chrome closes on return
placement_drawn <- function(chart) {
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file, width = 800, height = 400, env = environment())
  result <- session$Runtime$evaluate("
    (() => {
      const el = document.querySelector('.geneviewer');
      const labels = Array.from(el.querySelectorAll('text.label'));
      const shown = labels.filter(t => t.style.display !== 'none');
      const outside = shown.filter(t => {
        const svg = t.ownerSVGElement.getBoundingClientRect(), b = t.getBoundingClientRect();
        return b.top < svg.top - 0.5 || b.bottom > svg.bottom + 0.5 ||
               b.left < svg.left - 0.5 || b.right > svg.right + 0.5;
      }).length;
      const boxes = shown.map(t => t.getBoundingClientRect());
      let overlapping = 0;
      boxes.forEach((a, i) => boxes.slice(i + 1).forEach(b => {
        if (a.left < b.right && b.left < a.right && a.top < b.bottom && b.top < a.bottom) overlapping++;
      }));
      const measures = Array.from(el.querySelectorAll('text.link-measure'), t => t.textContent);
      return JSON.stringify({
        total: labels.length,
        hidden: labels.length - shown.length,
        turned: shown.filter(t => /rotate\\((?!0,)/.test(t.getAttribute('transform') || '')).length,
        outside, overlapping, measures,
        geneMeasures: el.querySelectorAll('.link-text').length
      });
    })()
  ")
  expect_null(result$exceptionDetails)
  jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
}

# One cluster of narrow genes with short names, up to the right end
turning_chart <- function(...) {
  genes <- data.frame(start = c(1, 900, 930, 960, 985), end = c(880, 928, 958, 983, 1000),
                      name = c("first gene", "alpha", "beta", "gamma", "last label here"))
  GC_chart(genes, height = "300px", width = "700px") %>%
    GC_labels("name", ...)
}

test_that("turned labels stay inside the chart, hiding one that would run past the right end", {
  drawn <- placement_drawn(turning_chart())
  expect_gt(drawn$turned, 0)
  # The last label would run out of the plot area when turned
  expect_equal(drawn$hidden, 1)
  expect_equal(drawn$outside, 0)
})

test_that("labels too long to turn within their cluster are hidden, not cut off", {
  drawn <- placement_drawn(placement_chart())
  expect_gt(drawn$hidden, 0)
  expect_equal(drawn$outside, 0)
  expect_equal(drawn$overlapping, 0)
})

test_that("adjustLabels = 'hide' hides labels that overlap instead of turning them", {
  drawn <- placement_drawn(turning_chart(adjustLabels = "hide"))
  expect_gt(drawn$hidden, 0)
  expect_equal(drawn$turned, 0)
  expect_equal(drawn$overlapping, 0)
})

test_that("measures go on the links where they fit, and more of them when zoomed in", {
  chart <- placement_chart() %>%
    GC_links(group = "homolog", label_position = "link", label = TRUE)
  drawn <- placement_drawn(chart)
  # The wide links carry their measure, the narrow ones do not; no measures
  # are added to the gene labels
  expect_true(all(c("91.3%", "64%") %in% unlist(drawn$measures)))
  expect_lt(length(drawn$measures), 6)
  expect_equal(drawn$geneMeasures, 0)

  zoomed <- placement_drawn(chart %>% GC_zoom(range = c(0.78, 1)))
  expect_gt(length(zoomed$measures), length(drawn$measures))
})

# Draws the chart in Chrome and describes its spread labels and their lines,
# also after the first label is moved as a drag moves it
spread_drawn <- function(chart) {
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file, width = 800, height = 400, env = environment())
  result <- session$Runtime$evaluate("
    new Promise(resolve => {
      const el = document.querySelector('.geneviewer');
      const labels = Array.from(el.querySelectorAll('text.label'));
      const lines = Array.from(el.querySelectorAll('.leader-lines line'));
      const boxes = labels.map(t => t.getBoundingClientRect());
      let overlapping = 0;
      boxes.forEach((a, i) => boxes.slice(i + 1).forEach(b => {
        if (a.left < b.right && b.left < a.right && a.top < b.bottom && b.top < a.bottom) overlapping++;
      }));
      const genesBottom = Math.max(...Array.from(el.querySelectorAll('.gene'),
        g => g.getBoundingClientRect().bottom));
      const genesTop = Math.min(...Array.from(el.querySelectorAll('.gene'),
        g => g.getBoundingClientRect().top));
      const tops = boxes.map(b => Math.round(b.top));
      const svg = el.querySelector('svg').getBoundingClientRect();
      const result = {
        labels: labels.length, lines: lines.length, overlapping,
        turned: labels.filter(t => /rotate\\((?!0,)/.test(t.getAttribute('transform') || '')).length,
        oneLine: new Set(tops).size === 1,
        belowGenes: boxes.every(b => b.top > genesBottom),
        aboveGenes: boxes.every(b => b.bottom < genesTop),
        inside: boxes.every(b => b.left >= svg.left - 0.5 && b.right <= svg.right + 0.5),
        lineStyle: lines.length ? [getComputedStyle(lines[0]).stroke, getComputedStyle(lines[0]).strokeWidth] : null
      };
      if (!lines.length) return resolve(JSON.stringify(result));
      // Labels with a line, in the order of their lines
      const led = labels.filter(t => t.__leader__);
      result.unled = labels.filter(t => !t.__leader__ && t.textContent.trim() !== '').length;
      // The line ends at the side of the label that faces the genes, also
      // after the label is moved
      const ends = lines.map(l => [+l.getAttribute('x2'), +l.getAttribute('y2')]);
      const edges = (i) => {
        const b = led[i].getBBox(), m = lines[i].parentNode.getScreenCTM().inverse()
          .multiply(led[i].getScreenCTM());
        return [new DOMPoint(b.x + b.width / 2, b.y).matrixTransform(m),
                new DOMPoint(b.x + b.width / 2, b.y + b.height).matrixTransform(m)];
      };
      const near = (i) => {
        const [top, bottom] = edges(i);
        const y2 = +lines[i].getAttribute('y2');
        const edge = y2 < +lines[i].getAttribute('y1') ? bottom : top;
        return Math.abs(edge.x - +lines[i].getAttribute('x2')) < 1 && Math.abs(edge.y - y2) < 4;
      };
      result.atLabels = led.every((_, i) => near(i));
      const label = led[0];
      label.setAttribute('transform', 'translate(30, 10) ' + label.getAttribute('transform'));
      setTimeout(() => {
        const line = lines[0];
        result.followsX = +line.getAttribute('x2') - ends[0][0];
        result.followsY = +line.getAttribute('y2') - ends[0][1];
        // A label dragged to the other side of the genes takes the line from
        // the other side of its gene
        if (led.length < 2) return resolve(JSON.stringify(result));
        const other = led[1];
        const shift = result.aboveGenes ? 150 : -150;
        other.setAttribute('transform', `translate(0, ${shift}) ` + other.getAttribute('transform'));
        setTimeout(() => {
          const l = lines[1];
          result.flippedUp = +l.getAttribute('y2') < +l.getAttribute('y1');
          result.flippedAtLabel = near(1);
          resolve(JSON.stringify(result));
        }, 50);
      }, 50);
    })
  ", awaitPromise = TRUE)
  expect_null(result$exceptionDetails)
  jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
}

test_that("adjustLabels = 'spread' puts labels on one line below the genes, apart, with lines", {
  drawn <- spread_drawn(turning_chart(adjustLabels = "spread"))
  expect_equal(drawn$overlapping, 0)
  expect_equal(drawn$turned, 0)
  expect_true(drawn$oneLine)
  expect_true(drawn$belowGenes)
  expect_true(drawn$inside)
  expect_equal(drawn$lines, drawn$labels)
  expect_true(drawn$atLabels)
  # A dragged label takes its line along
  expect_equal(drawn$followsX, 30, tolerance = 0.01)
  expect_equal(drawn$followsY, 10, tolerance = 0.01)
  # Dragged above the genes, the line goes up from the top of the gene
  expect_true(drawn$flippedUp)
  expect_true(drawn$flippedAtLabel)
})

test_that("spreadPosition = 'top' puts spread labels on one line above the genes", {
  expect_error(placement_chart(adjustLabels = "spread", spreadPosition = "left"),
               "'spreadPosition' must be \"bottom\" or \"top\"")
  drawn <- spread_drawn(turning_chart(adjustLabels = "spread", spreadPosition = "top"))
  expect_equal(drawn$overlapping, 0)
  expect_true(drawn$oneLine)
  expect_true(drawn$aboveGenes)
  expect_equal(drawn$lines, drawn$labels)
  expect_true(drawn$atLabels)
  expect_equal(drawn$followsY, 10, tolerance = 0.01)
  # Dragged below the genes, the line goes down from the bottom of the gene
  expect_false(drawn$flippedUp)
  expect_true(drawn$flippedAtLabel)
})

test_that("genes without label text get no line when labels are spread", {
  genes <- data.frame(start = c(1, 300, 600, 800), end = c(250, 550, 750, 1000),
                      name = c("first", NA, "", "last"))
  chart <- GC_chart(genes, height = "300px", width = "700px") %>%
    GC_labels("name", adjustLabels = "spread")
  drawn <- spread_drawn(chart)
  expect_equal(drawn$lines, 2)
  expect_equal(drawn$unled, 0)
  expect_true(drawn$atLabels)
})

test_that("leaderLine = FALSE spreads the labels without lines", {
  drawn <- spread_drawn(turning_chart(adjustLabels = "spread", leaderLine = FALSE))
  expect_equal(drawn$lines, 0)
  expect_equal(drawn$overlapping, 0)
})

test_that("spread labels and their lines are only drawn for the genes in view when zoomed", {
  # The first gene, cut off by the left end of the window, gets its label too
  drawn <- spread_drawn(turning_chart(adjustLabels = "spread") %>% GC_zoom(range = c(0.85, 1)))
  expect_equal(drawn$lines, 5)
  expect_equal(drawn$overlapping, 0)
  expect_true(drawn$inside)
  expect_true(drawn$oneLine)
})

test_that("the lines of spread labels are drawn in the style of the theme, under that of GC_labels()", {
  expect_equal(spread_drawn(turning_chart(adjustLabels = "spread"))$lineStyle,
               list("rgb(153, 153, 153)", "0.75px"))
  themed <- turning_chart(adjustLabels = "spread") %>% GC_theme("vintage")
  expect_equal(spread_drawn(themed)$lineStyle, list("rgb(201, 194, 181)", "1px"))
  own <- turning_chart(adjustLabels = "spread", leaderLine = list(stroke = "red")) %>%
    GC_theme("vintage")
  expect_equal(spread_drawn(own)$lineStyle, list("rgb(255, 0, 0)", "1px"))
})

# Draws the page in Chrome, runs before (JavaScript) and describes the shown
# labels of the first cluster: how many are shown and hidden, whether they
# overlap, are turned, lie on one line, fit in the SVG of the cluster, and the
# style of the first line
spread_fit <- function(page, before = "") {
  file <- tempfile(fileext = ".html")
  if (inherits(page, "htmlwidget")) {
    htmlwidgets::saveWidget(page, file, selfcontained = FALSE)
  } else {
    htmltools::save_html(page, file)
  }
  session <- chrome_page(file, width = 800, height = 400, env = environment())
  result <- session$Runtime$evaluate(paste0("
    new Promise(resolve => {", before, "
      setTimeout(() => {
        const el = document.querySelector('.geneviewer');
        const all = Array.from(el.querySelectorAll('text.label'));
        const shown = all.filter(t => getComputedStyle(t).display !== 'none');
        const boxes = shown.map(t => t.getBoundingClientRect());
        let overlapping = 0;
        boxes.forEach((a, i) => boxes.slice(i + 1).forEach(b => {
          if (a.left < b.right && b.left < a.right && a.top < b.bottom && b.top < a.bottom) overlapping++;
        }));
        const area = all[0].ownerSVGElement.getBoundingClientRect();
        const line = el.querySelector('.leader-lines line');
        resolve(JSON.stringify({
          shown: shown.length, hidden: all.length - shown.length, overlapping,
          turned: shown.filter(t => /rotate\\((?!0,)/.test(t.getAttribute('transform') || '')).length,
          oneLine: new Set(boxes.map(b => Math.round(b.top))).size === 1,
          inside: boxes.every(b => b.left >= area.left - 0.5 && b.right <= area.right + 0.5 &&
                                   b.top >= area.top - 0.5 && b.bottom <= area.bottom + 0.5),
          lineWidth: line ? getComputedStyle(line).strokeWidth : null
        }));
      }, 200);
    })
  "), awaitPromise = TRUE)
  expect_null(result$exceptionDetails)
  jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
}

test_that("spread labels too many for the plot are hidden rather than cut off", {
  genes <- data.frame(start = seq(1, 5901, by = 100), end = seq(90, 5990, by = 100),
                      name = paste0("gene", 1:60))
  drawn <- spread_fit(GC_chart(genes, height = "300px", width = "700px") %>%
                        GC_labels("name", adjustLabels = "spread"))
  expect_gt(drawn$hidden, 0)
  expect_gt(drawn$shown, 0)
  expect_equal(drawn$overlapping, 0)
  expect_true(drawn$inside)
})

test_that("spread labels come closer to the genes when the cluster has no room below them", {
  chart <- turning_chart(adjustLabels = "spread", leaderLine = list(length = 200)) %>%
    GC_grid(margin = list(top = 0, bottom = 0))
  drawn <- spread_fit(chart)
  expect_true(drawn$inside)
  expect_true(drawn$oneLine)
  top <- spread_fit(turning_chart(adjustLabels = "spread", spreadPosition = "top",
                                  leaderLine = list(length = 200)) %>%
                      GC_grid(margin = list(top = 0, bottom = 0)))
  expect_true(top$inside)
})

test_that("labels that are not spread make room only for where spread labels end up", {
  # Gene a's label is first drawn where it would overlap that of gene b
  genes <- data.frame(start = c(1, 110, 800), end = c(100, 200, 1000), kind = c("a", "b", "b"),
                      name = c("a rather long gene name", "another long gene name", ""))
  chart <- GC_chart(genes, group = "kind", height = "300px", width = "700px") %>%
    GC_labels("name", groupStyle = list(a = list(adjustLabels = "spread")))
  drawn <- spread_fit(chart)
  expect_equal(drawn$turned, 0)
  expect_equal(drawn$hidden, 0)
})

test_that("labels of a chart drawn while hidden are spread once it is shown", {
  page <- htmltools::tagList(htmltools::div(
    id = "tab", style = "display: none;",
    turning_chart(adjustLabels = "spread")
  ))
  drawn <- spread_fit(page, before = "document.getElementById('tab').style.display = 'block';")
  expect_equal(drawn$overlapping, 0)
  expect_true(drawn$oneLine)
  expect_true(drawn$inside)
})

test_that("lines set again after a theme turns them off keep the default style", {
  chart <- turning_chart(adjustLabels = "spread", leaderLine = list(stroke = "red")) %>%
    GC_theme(labels = list(leaderLine = FALSE))
  expect_equal(spread_fit(chart)$lineWidth, "0.75px")
})
