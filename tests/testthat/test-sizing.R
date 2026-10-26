test_that("rendered sizing uses the current widget width without changing the chart", {
  for (width in list("100%", "75%", "600px", 600, NULL)) {
    chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class", width = width)
    rendered <- render_theme(chart)
    expect_identical(rendered$x$size$width, chart$width)
    expect_null(chart$x$size)
    expect_identical(rendered$width, chart$width)
    expect_identical(rendered$height, chart$height)
    expect_identical(rendered$x$series, chart$x$series)
  }

  chart <- GC_chart(ophA_clusters, cluster = "cluster") %>% GC_grid(width = 700)
  expect_equal(render_theme(chart)$x$size$width, 700)
  # The original construction arguments are not the rendered dimensions.
  expect_equal(chart$x$params$width, "100%")
})

test_that("Shiny pixel widths size the whole widget and restore the output width", {
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class",
                    width = "600px", height = "400px") %>%
    GC_title("Sizing regression") %>% GC_labels("name") %>% GC_events(highlight = TRUE)
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file, width = 1200, height = 800)
  result <- session$Runtime$evaluate("
    (() => {
      const el = document.querySelector('.geneviewer');
      const widget = HTMLWidgets.find('#' + el.id);
      const original = JSON.parse(document.querySelector('script[data-for=\"' + el.id + '\"]').textContent).x;
      const clicks = [];
      window.Shiny = { setInputValue: (id, value) => clicks.push(value) };
      HTMLWidgets.shinyMode = true;
      el.parentElement.style.overflowX = 'auto';
      el.parentElement.style.width = '800px';
      const measure = () => ({
        width: el.getBoundingClientRect().width,
        height: el.getBoundingClientRect().height,
        svgs: [...el.querySelectorAll('svg')].map(svg => ({
          width: svg.getBoundingClientRect().width,
          scale: svg.getBoundingClientRect().width / svg.viewBox.baseVal.width
        })),
        genes: el.querySelectorAll('.gene').length,
        scrolls: el.parentElement.scrollWidth > el.parentElement.clientWidth
      });
      const render = (width) => {
        const x = JSON.parse(JSON.stringify(original));
        x.size.width = width;
        Object.values(x.series).forEach(s => { s.container.width = width; });
        widget.renderValue(x);
        return measure();
      };
      const wide = render('3000px');
      widget.resize(800, 400);
      const resized = measure();
      const numeric = render(500);
      const fractional = render('500.5px');
      const responsive = render('100%');
      const percent = render('75%');
      const legacy = JSON.parse(JSON.stringify(original));
      delete legacy.size;
      widget.renderValue(legacy);
      const oldPayload = measure();
      const styled = JSON.parse(JSON.stringify(original));
      styled.style = { width: '550px' };
      widget.renderValue(styled);
      const styleOverride = measure();
      const removedStyle = render('100%');
      el.querySelector('.gene').dispatchEvent(new MouseEvent('click', { bubbles: true }));
      const css = document.createElement('style');
      css.textContent = '.sizing-fill-row > div { flex: 1 1 0%; min-width: 0; }';
      document.head.appendChild(css);
      el.parentElement.classList.add('sizing-fill-row');
      el.parentElement.style.display = 'flex';
      el.parentElement.appendChild(document.createElement('div'));
      const fillRowFixed = render('3000px');
      const fillRowResponsive = render('100%');
      el.parentElement.style.display = 'block';
      el.style.width = '650px';
      el.style.flex = '2 1 auto';
      const hostChanged = render('100%');
      return JSON.stringify({ wide, resized, numeric, fractional, responsive,
        percent, oldPayload, styleOverride, removedStyle, clicks,
        fillRowFixed, fillRowResponsive, hostChanged, hostFlex: el.style.flex });
    })()
  ")
  expect_null(result$exceptionDetails)
  res <- jsonlite::fromJSON(result$result$value)
  expect_equal(res$wide$width, 3000)
  expect_true(res$wide$scrolls)
  expect_equal(res$wide$svgs$width, rep(3000, 4))
  expect_equal(res$wide$svgs$scale, rep(1, 4))
  expect_equal(res$resized, res$wide)
  expect_equal(res$numeric$width, 500)
  expect_equal(res$fractional$width, 500.5)
  expect_equal(res$responsive$width, 600)
  expect_false(res$responsive$scrolls)
  expect_equal(res$responsive$svgs$width, rep(600, 4))
  # Percentage widths retain their existing meaning relative to the output.
  expect_equal(res$percent$width, 600)
  expect_equal(res$percent$svgs$width, rep(600, 4))
  expect_equal(res$percent$svgs$scale, c(1, 600 / 450, 600 / 450, 1))
  expect_equal(res$oldPayload$width, 600)
  expect_equal(res$styleOverride$width, 550)
  expect_equal(res$removedStyle$width, 600)
  expect_equal(res$wide$height, res$responsive$height)
  expect_equal(res$wide$genes, nrow(ophA_clusters))
  expect_equal(res$responsive$genes, nrow(ophA_clusters))
  expect_equal(res$clicks$component, "genes")
  expect_equal(res$fillRowFixed$width, 3000)
  expect_true(res$fillRowFixed$scrolls)
  expect_equal(res$fillRowResponsive$width, 400)
  expect_equal(res$hostChanged$width, 650)
  expect_equal(res$hostFlex, "2 1 auto")
})
