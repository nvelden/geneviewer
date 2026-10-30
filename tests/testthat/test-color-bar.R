color_bar_genes <- data.frame(
  start = c(10, 90, 130, 170, 240, 250),
  end = c(40, 120, 160, 200, 210, 270),
  group = c("A", "B", "C", "A", "B", "C"),
  identity = c(NA, NA, NA, 50, 40, 100),
  cluster = c(1, 1, 1, 2, 2, 2)
)

color_bar_chart <- function(colorBarOptions = list(), legend = TRUE, theme = "default") {
  chart <- GC_chart(color_bar_genes, cluster = "cluster", group = "group",
                    height = "240px", width = "700px") %>%
    GC_links(group = "group", measure = "identity", label = FALSE,
             colorBarOptions = colorBarOptions) %>%
    GC_theme(theme)
  if (legend) GC_legend(chart, position = "top") else GC_legend(chart, FALSE)
}

test_that("GC_links() checks and keeps where the color bar goes", {
  chart <- color_bar_chart(list(position = "topleft", orientation = "horizontal"))
  expect_identical(chart$x$links[[1]]$options$colorBarOptions,
                   list(position = "topleft", orientation = "horizontal"))
  expect_error(color_bar_chart(list(position = "left")),
               "'colorBarOptions\\$position' must be one of")
  expect_error(color_bar_chart(list(orientation = "diagonal")),
               "'colorBarOptions\\$orientation' must be one of")
  expect_error(color_bar_chart(list(position = c("legend", "topleft"))),
               "'colorBarOptions\\$position' must be one of")
  expect_error(color_bar_chart(list(labels = "above")),
               "'colorBarOptions\\$labels' must be one of")
  expect_error(color_bar_chart(list(position = "legend", legendSide = "top")),
               "'colorBarOptions\\$legendSide' must be one of")
})

# Draws the chart in Chrome and describes its color bar; Chrome closes on return
color_bar_drawn <- function(chart) {
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file, width = 800, height = 400, env = environment())
  result <- session$Runtime$evaluate("
    (() => {
      const el = document.querySelector('.geneviewer');
      const overlay = el.querySelector('svg.color-bar');
      const inLegend = el.querySelector('.legend-color-bar');
      const bar = (overlay ?? inLegend)?.querySelector('rect');
      const box = node => { const b = node.getBoundingClientRect(); return [b.left, b.top, b.width, b.height]; };
      const texts = Array.from((overlay ?? inLegend)?.querySelectorAll('text') ?? []);
      const tickLabels = texts.filter(t => t.getAttribute('text-anchor') === 'middle').map(box);
      let hidden = null;
      if (inLegend) {
        inLegend.dispatchEvent(new MouseEvent('click', { bubbles: true }));
        hidden = el.querySelectorAll('.legend.unselected').length;
      }
      return JSON.stringify({
        overlay: !!overlay,
        inLegend: !!inLegend,
        barSize: bar ? [+bar.getAttribute('width'), +bar.getAttribute('height')] : null,
        bar: bar ? box(bar) : null,
        legendItem: el.querySelector('.legend rect') ? box(el.querySelector('.legend rect')) : null,
        lastLegendItem: (() => { const items = el.querySelectorAll('.legend rect');
          return items.length ? box(items[items.length - 1]) : null; })(),
        plot: box(el.querySelector('[id^=geneviewer-graph-container]')),
        texts: texts.map(t => t.textContent),
        titleWeight: texts[0] ? getComputedStyle(texts[0]).fontWeight : null,
        barStroke: bar ? getComputedStyle(bar).stroke : null,
        ticks: (overlay ?? inLegend)?.querySelectorAll('line').length ?? 0,
        tickLabels,
        hidden,
        exported: chartToSVG(el).includes('identity')
      });
    })()
  ")
  expect_null(result$exceptionDetails)
  jsonlite::fromJSON(result$result$value, simplifyVector = FALSE)
}

test_that("the color bar goes in the legend, lying down before the groups", {
  drawn <- color_bar_drawn(color_bar_chart(list(position = "legend")))
  expect_false(drawn$overlay)
  expect_true(drawn$inLegend)
  expect_identical(unlist(drawn$texts), c("identity", "0%", "100%"))
  # Wider than tall, as tall as the markers, and left of the first group of
  # the legend, with a gap wider than between the groups
  expect_gt(drawn$barSize[[1]], drawn$barSize[[2]])
  expect_equal(drawn$bar[[4]], drawn$legendItem[[4]], tolerance = 1)
  expect_gt(drawn$legendItem[[1]] - (drawn$bar[[1]] + drawn$bar[[3]]), 30)
  # A bold title
  expect_true(drawn$titleWeight %in% c("bold", "700"))
  # Clicking it hides no genes, and saved images keep it
  expect_equal(drawn$hidden, 0)
  expect_true(drawn$exported)
})

test_that("the color bar goes in each corner, standing or lying", {
  # By default it stands in the bottom right corner, as before
  drawn <- color_bar_drawn(color_bar_chart())
  expect_true(drawn$overlay)
  expect_equal(unlist(drawn$barSize), c(10, 60))
  expect_gt(drawn$bar[[1]], drawn$plot[[1]] + drawn$plot[[3]] / 2)
  expect_gt(drawn$bar[[2]], drawn$plot[[2]] + drawn$plot[[4]] / 2)

  drawn <- color_bar_drawn(color_bar_chart(list(position = "topleft", orientation = "horizontal")))
  expect_equal(unlist(drawn$barSize), c(80, 8))
  expect_lt(drawn$bar[[1]], drawn$plot[[1]] + drawn$plot[[3]] / 2)
  expect_lt(drawn$bar[[2]], drawn$plot[[2]] + drawn$plot[[4]] / 2)

  drawn <- color_bar_drawn(color_bar_chart(list(position = "bottomleft")))
  expect_lt(drawn$bar[[1]], drawn$plot[[1]] + drawn$plot[[3]] / 2)
  expect_gt(drawn$bar[[2]], drawn$plot[[2]] + drawn$plot[[4]] / 2)

  drawn <- color_bar_drawn(color_bar_chart(list(position = "topright")))
  expect_gt(drawn$bar[[1]], drawn$plot[[1]] + drawn$plot[[3]] / 2)
  expect_lt(drawn$bar[[2]], drawn$plot[[2]] + drawn$plot[[4]] / 2)
})

test_that("a color bar for the legend stands bottom right without a legend", {
  drawn <- color_bar_drawn(color_bar_chart(list(position = "legend"), legend = FALSE))
  expect_true(drawn$overlay)
  expect_false(drawn$inLegend)
  expect_equal(unlist(drawn$barSize), c(10, 60))
})

test_that("the legend outlines a color bar the theme leaves without one", {
  drawn <- color_bar_drawn(color_bar_chart(list(position = "legend"), theme = "publication"))
  expect_false(drawn$barStroke %in% c("none", ""))
})

test_that("a lying color bar takes its labels below, with as many ticks as fit", {
  drawn <- color_bar_drawn(color_bar_chart(list(orientation = "horizontal",
                                                position = "bottomright", labels = "below")))
  expect_equal(unlist(drawn$barSize), c(120, 8))
  expect_identical(drawn$texts[[1]], "identity")
  expect_gt(drawn$ticks, 2)
  expect_equal(length(drawn$tickLabels), drawn$ticks)
  # The measures stand side by side without overlapping
  ends <- vapply(drawn$tickLabels, function(b) b[[1]] + b[[3]], numeric(1))
  starts <- vapply(drawn$tickLabels, function(b) b[[1]], numeric(1))
  expect_true(all(starts[-1] > ends[-length(ends)]))
  # Inline is the default, also in a corner
  drawn <- color_bar_drawn(color_bar_chart(list(orientation = "horizontal")))
  expect_equal(drawn$ticks, 0)
  expect_equal(unlist(drawn$barSize), c(80, 8))
})

test_that("the color bar goes after the groups of the legend with legendSide = 'right'", {
  drawn <- color_bar_drawn(color_bar_chart(list(position = "legend", legendSide = "right")))
  expect_true(drawn$inLegend)
  # Right of the last group, on its row, with the same wider gap
  expect_gt(drawn$bar[[1]], drawn$lastLegendItem[[1]] + 30)
  expect_equal(drawn$bar[[2]] + drawn$bar[[4]] / 2,
               drawn$lastLegendItem[[2]] + drawn$lastLegendItem[[4]] / 2, tolerance = 1)

  # When it does not fit after the groups, it starts a new row
  wide <- GC_chart(pQEB1_plasmids, cluster = "cluster", group = "category",
                   height = "420px", width = "700px") %>%
    GC_links(data = pQEB1_alignment, label = FALSE,
             colorBarOptions = list(position = "legend", legendSide = "right")) %>%
    GC_legend(position = "top")
  drawn <- color_bar_drawn(wide)
  expect_gt(drawn$bar[[2]], drawn$lastLegendItem[[2]] + drawn$lastLegendItem[[4]] / 2)
})
