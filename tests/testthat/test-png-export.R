test_that("PNG export works with modern host colors and retains pixel density and patterns", {
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class",
                    width = "600px", height = "300px") %>%
    GC_title("PNG export") %>% GC_theme("bw") %>% GC_labels("name")
  file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file)
  result <- session$Runtime$evaluate("
    (async () => {
      document.body.style.backgroundColor = 'oklch(0.95 0.02 240)';
      document.body.style.color = 'oklch(0.2 0.01 240)';
      const el = document.querySelector('.geneviewer');
      const originalRatio = window.devicePixelRatio;
      const results = [];
      for (const ratio of [1, 2]) {
        Object.defineProperty(window, 'devicePixelRatio', { configurable: true, value: ratio });
        const blob = await chartToPNG(el);
        const bitmap = await createImageBitmap(blob);
        const canvas = document.createElement('canvas');
        canvas.width = bitmap.width; canvas.height = bitmap.height;
        const context = canvas.getContext('2d');
        context.drawImage(bitmap, 0, 0);
        const pixels = context.getImageData(0, 0, canvas.width, canvas.height).data;
        let ink = 0;
        for (let i = 0; i < pixels.length; i += 4) {
          if (pixels[i + 3] > 0 && pixels[i] < 200 && pixels[i + 1] < 200 && pixels[i + 2] < 200) ink++;
        }
        results.push({ type: blob.type, width: bitmap.width, height: bitmap.height, ink });
        bitmap.close();
      }
      Object.defineProperty(window, 'devicePixelRatio', { configurable: true, value: originalRatio });
      return JSON.stringify({ results, width: Math.ceil(el.getBoundingClientRect().width),
        height: Math.ceil(el.getBoundingClientRect().height), patterns: el.querySelectorAll('pattern').length });
    })()
  ", awaitPromise = TRUE)
  expect_null(result$exceptionDetails)
  res <- jsonlite::fromJSON(result$result$value)
  expect_gt(res$patterns, 0)
  expect_equal(res$results$type, rep("image/png", 2))
  expect_equal(res$results$width, res$width * c(1, 2))
  expect_equal(res$results$height, res$height * c(1, 2))
  expect_true(all(res$results$ink > 0))
})
