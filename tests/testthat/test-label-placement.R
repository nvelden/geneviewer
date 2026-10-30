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
               "'adjustLabels' must be TRUE, FALSE or \"hide\"")
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
