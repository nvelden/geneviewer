
# GC_chart
test_that("GC_chart works with valid data", {
  # Run function
  chart <- GC_chart(geneviewer::ophA_clusters, group = "class", cluster = "cluster", height = "200px")

  # Check result
  expect_s3_class(chart, "htmlwidget")
  expect_true("x" %in% names(chart))
  expect_true("data" %in% names(chart$x))
  expect_equal(nrow(chart$x$data), nrow(geneviewer::ophA_clusters))
})

test_that("GC_chart throws error for missing columns", {
  # Create test data
  genes_data <- data.frame(
    start = c(10, 50, 90),
    end = c(40, 80, 120)
  )

  # Test for missing group column
  expect_error(
    GC_chart(genes_data, group = "group"),
    "group column not found in data"
  )

  # Test for missing cluster column
  expect_error(
    GC_chart(genes_data, cluster = "cluster"),
    "cluster column not found in data"
  )
})

test_that("GC_chart filters rows with NA in start or end columns", {

  genes_data <- data.frame(
    start = c(10, NA, 90, 130),
    end = c(40, 80, NA, 160),
    group = c('A', 'A', 'B', 'B')
  )

  expect_warning(
    chart <- GC_chart(genes_data, group = "group"),
    "Rows with NA values in 'start' or 'end' columns have been filtered out"
  )

  expect_equal(nrow(chart$x$data), 2)
})

# GC_title
test_that("GC_title updates cluster title and subtitle correctly with ophA_clusters data", {
  # Load ophA_clusters data
  data("ophA_clusters", package = "geneviewer")

  # Generate a GC chart
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class", height = "400px")

  # Add a title and subtitle to the chart
  updated_chart <- GC_title(
    GC_chart = chart,
    title = "OphA Cluster Analysis",
    subtitle = "Key Functional Clusters",
    show = TRUE,
    height = "50px",
    style = list(backgroundColor = "#e0f7fa"),
    titleFont = list(fontSize = "18px", fontWeight = "bold", fill = "#004d40"),
    subtitleFont = list(fontSize = "14px", fontStyle = "italic", fill = "#00796b")
  )

  # Assertions
  expect_s3_class(updated_chart, "htmlwidget")

  # Check if the title section is updated correctly
  expect_true("title" %in% names(updated_chart$x))
  expect_equal(updated_chart$x$title$title, "OphA Cluster Analysis")
  expect_equal(updated_chart$x$title$subtitle, "Key Functional Clusters")
  expect_equal(updated_chart$x$title$show, TRUE)
  expect_equal(updated_chart$x$title$height, "50px")

  # Check styles
  expect_equal(updated_chart$x$title$style$backgroundColor, "#e0f7fa")

  # Check title font
  expect_equal(updated_chart$x$title$titleFont$fontSize, "18px")
  expect_equal(updated_chart$x$title$titleFont$fontWeight, "bold")
  expect_equal(updated_chart$x$title$titleFont$fill, "#004d40")

  # Check subtitle font
  expect_equal(updated_chart$x$title$subtitleFont$fontSize, "14px")
  expect_equal(updated_chart$x$title$subtitleFont$fontStyle, "italic")
  expect_equal(updated_chart$x$title$subtitleFont$fill, "#00796b")

  # Ensure original chart is unmodified if no title or subtitle is provided
  no_title_chart <- GC_title(chart, show = TRUE)
  expect_equal(no_title_chart, chart)
})

#GC_title
test_that("GC_clusterTitle updates the title and subtitle of specified clusters correctly", {
  data("ophA_clusters", package = "geneviewer")

  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class", height = "400px")

  # Update titles and subtitles for specific clusters
  updated_chart <- GC_clusterTitle(
    GC_chart = chart,
    title = c("OphA Cluster", "dbOphA Cluster"),
    subtitle = c("Cluster 1 Details", "Cluster 2 Details"),
    cluster = c("ophA", "dbophA"),
    height = c("40px", "50px")
  )

  # Assertions for the two clusters
  expect_true("clusterTitle" %in% names(updated_chart$x$series$ophA))
  expect_true("clusterTitle" %in% names(updated_chart$x$series$dbophA))

  expect_equal(updated_chart$x$series$ophA$clusterTitle$title, "OphA Cluster")
  expect_equal(updated_chart$x$series$ophA$clusterTitle$subtitle, "Cluster 1 Details")
  expect_equal(updated_chart$x$series$dbophA$clusterTitle$title, "dbOphA Cluster")
  expect_equal(updated_chart$x$series$dbophA$clusterTitle$subtitle, "Cluster 2 Details")
})

# GC_sequence
test_that("GC_sequence updates sequence settings for specified clusters correctly", {
  # Load ophA_clusters data
  data("ophA_clusters", package = "geneviewer")

  # Generate a GC chart
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class", height = "400px")

  # Update sequence settings for specific clusters
  updated_chart <- GC_sequence(
    GC_chart = chart,
    show = TRUE,
    cluster = c("ophA", "dbophA"),
    y = c(60, 70),
    sequenceStyle = list(stroke = "blue", strokeWidth = 2),
    markerStyle = list(stroke = "red", strokeWidth = 1, gap = 5, tiltAmount = 10)
  )

  # Assertions
  expect_s3_class(updated_chart, "htmlwidget")

  # Check if sequence settings are applied to the specified clusters
  expect_true("sequence" %in% names(updated_chart$x$series$ophA))
  expect_true("sequence" %in% names(updated_chart$x$series$dbophA))

  # Verify sequence settings for `ophA` cluster
  ophA_sequence <- updated_chart$x$series$ophA$sequence
  expect_true(ophA_sequence$show)
  expect_equal(ophA_sequence$y, 60)
  expect_equal(ophA_sequence$sequenceStyle$stroke, "blue")
  expect_equal(ophA_sequence$sequenceStyle$strokeWidth, 2)
  expect_equal(ophA_sequence$markerStyle$stroke, "red")
  expect_equal(ophA_sequence$markerStyle$strokeWidth, 1)
  expect_equal(ophA_sequence$markerStyle$gap, 5)
  expect_equal(ophA_sequence$markerStyle$tiltAmount, 10)

  # Verify sequence settings for `dbophA` cluster
  dbophA_sequence <- updated_chart$x$series$dbophA$sequence
  expect_true(dbophA_sequence$show)
  expect_equal(dbophA_sequence$y, 70)
  expect_equal(dbophA_sequence$sequenceStyle$stroke, "blue")
  expect_equal(dbophA_sequence$sequenceStyle$strokeWidth, 2)
  expect_equal(dbophA_sequence$markerStyle$stroke, "red")
  expect_equal(dbophA_sequence$markerStyle$strokeWidth, 1)
  expect_equal(dbophA_sequence$markerStyle$gap, 5)
  expect_equal(dbophA_sequence$markerStyle$tiltAmount, 10)

})

test_that("GC_grid updates grid settings for specified clusters correctly", {

  # Load ophA_clusters data
  data("ophA_clusters", package = "geneviewer")

  # Generate a GC chart
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class", height = "400px")

  # Update grid settings for specific clusters
  updated_chart <- GC_grid(
    GC_chart = chart,
    margin = list(left = "20px", right = "30px"),
    width = c("50%", "50%"),
    height = c("150px", "200px"),
    direction = "row",
    cluster = c("ophA", "dbophA")
  )

  # Assertions
  expect_s3_class(updated_chart, "htmlwidget")

  # Check if grid settings are applied to the specified clusters
  expect_true("container" %in% names(updated_chart$x$series$ophA))
  expect_true("container" %in% names(updated_chart$x$series$dbophA))

  # Verify grid settings for `ophA` cluster
  ophA_grid <- updated_chart$x$series$ophA$container
  expect_equal(ophA_grid$margin$left, "20px")
  expect_equal(ophA_grid$margin$right, "30px")
  expect_equal(ophA_grid$style$width, "50%")
  expect_equal(ophA_grid$height, 150)

  # Verify grid settings for `dbophA` cluster
  dbophA_grid <- updated_chart$x$series$dbophA$container
  expect_equal(dbophA_grid$margin$left, "20px")
  expect_equal(dbophA_grid$margin$right, "30px")
  expect_equal(dbophA_grid$style$width, "50%")
  expect_equal(dbophA_grid$height, 200)

  # Check global settings
  expect_equal(updated_chart$x$graphContainer$direction, "row")

})

#GC_scale
test_that("GC_scale updates scale settings for specified clusters correctly", {

  # Load ophA_clusters data
  data("ophA_clusters", package = "geneviewer")

  # Generate a GC chart
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class", height = "400px")

  # Update scale settings for specific clusters
  updated_chart <- GC_scale(
    GC_chart = chart,
    cluster = c("ophA", "dbophA"),
    start = c(2500, 18000),
    end = c(22000, 44000),
    padding = 5,
    hidden = FALSE,
    reverse = c(FALSE, TRUE),
    axis_position = "top",
    scale_breaks = TRUE,
    scale_break_threshold = 30,
    ticksCount = 8,
    ticksFormat = ",.2f",
    tickStyle = list(stroke = "blue", strokeWidth = 2),
    textStyle = list(fill = "red", fontSize = "12px"),
    lineStyle = list(stroke = "black", strokeWidth = 2)
  )

  # Assertions
  expect_s3_class(updated_chart, "htmlwidget")

  # Check if scale settings are applied to the specified clusters
  expect_true("scale" %in% names(updated_chart$x$series$ophA))
  expect_true("scale" %in% names(updated_chart$x$series$dbophA))

  # Verify scale settings for `ophA` cluster
  ophA_scale <- updated_chart$x$series$ophA$scale
  expect_equal(ophA_scale$start, 2500)
  expect_equal(ophA_scale$end, 22000)
  expect_equal(ophA_scale$padding, 5)
  expect_false(ophA_scale$hidden)
  expect_false(ophA_scale$reverse)
  expect_equal(ophA_scale$axisPosition, "top")
  expect_true(ophA_scale$scale_breaks)
  expect_equal(ophA_scale$scale_break_threshold, 30)
  expect_equal(ophA_scale$ticksCount, 8)
  expect_equal(ophA_scale$ticksFormat, ",.2f")
  expect_equal(ophA_scale$tickStyle$stroke, "blue")
  expect_equal(ophA_scale$tickStyle$strokeWidth, 2)
  expect_equal(ophA_scale$textStyle$fill, "red")
  expect_equal(ophA_scale$textStyle$fontSize, "12px")
  expect_equal(ophA_scale$lineStyle$stroke, "black")
  expect_equal(ophA_scale$lineStyle$strokeWidth, 2)

  # Verify scale settings for `dbophA` cluster
  dbophA_scale <- updated_chart$x$series$dbophA$scale
  expect_equal(dbophA_scale$start, 18000)
  expect_equal(dbophA_scale$end, 44000)
  expect_equal(dbophA_scale$padding, 5)
  expect_false(dbophA_scale$hidden)
  expect_true(dbophA_scale$reverse)
  expect_equal(dbophA_scale$axisPosition, "top")
  expect_true(dbophA_scale$scale_breaks)
  expect_equal(dbophA_scale$scale_break_threshold, 30)
  expect_equal(dbophA_scale$ticksCount, 8)
  expect_equal(dbophA_scale$ticksFormat, ",.2f")
  expect_equal(dbophA_scale$tickStyle$stroke, "blue")
  expect_equal(dbophA_scale$tickStyle$strokeWidth, 2)
  expect_equal(dbophA_scale$textStyle$fill, "red")
  expect_equal(dbophA_scale$textStyle$fontSize, "12px")
  expect_equal(dbophA_scale$lineStyle$stroke, "black")
  expect_equal(dbophA_scale$lineStyle$strokeWidth, 2)

})

# GC_scaleBar
test_that("GC_scaleBar updates scale bar settings for specified clusters correctly", {

  # Load ophA_clusters data
  data("ophA_clusters", package = "geneviewer")

  # Generate a GC chart
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class", height = "400px")

  # Update scale bar settings for specific clusters
  updated_chart <- GC_scaleBar(
    GC_chart = chart,
    show = TRUE,
    cluster = c("ophA", "dbophA"),
    scaleBarLineStyle = list(stroke = "black", strokeWidth = 1),
    scaleBarTickStyle = list(stroke = "grey", strokeWidth = 2),
    labelStyle = list(
      labelPosition = "left",
      fontSize = "12px",
      fontFamily = "Arial",
      fill = "blue"
    ),
    title = c("1 kb", "2 kb"),
    scaleBarUnit = c(1000, 2000)
  )

  # Assertions
  expect_s3_class(updated_chart, "htmlwidget")

  # Check if scale bar settings are applied to the specified clusters
  expect_true("scaleBar" %in% names(updated_chart$x$series$ophA))
  expect_true("scaleBar" %in% names(updated_chart$x$series$dbophA))

  # Verify scale bar settings for `ophA` cluster
  ophA_scaleBar <- updated_chart$x$series$ophA$scaleBar
  expect_true(ophA_scaleBar$show)
  expect_equal(ophA_scaleBar$scaleBarLineStyle$stroke, "black")
  expect_equal(ophA_scaleBar$scaleBarLineStyle$strokeWidth, 1)
  expect_equal(ophA_scaleBar$scaleBarTickStyle$stroke, "grey")
  expect_equal(ophA_scaleBar$scaleBarTickStyle$strokeWidth, 2)
  expect_equal(ophA_scaleBar$labelStyle$labelPosition, "left")
  expect_equal(ophA_scaleBar$labelStyle$fontSize, "12px")
  expect_equal(ophA_scaleBar$labelStyle$fontFamily, "Arial")
  expect_equal(ophA_scaleBar$labelStyle$fill, "blue")
  expect_equal(ophA_scaleBar$title, "1 kb")
  expect_equal(ophA_scaleBar$scaleBarUnit, 1000)

  # Verify scale bar settings for `dbophA` cluster
  dbophA_scaleBar <- updated_chart$x$series$dbophA$scaleBar
  expect_true(dbophA_scaleBar$show)
  expect_equal(dbophA_scaleBar$scaleBarLineStyle$stroke, "black")
  expect_equal(dbophA_scaleBar$scaleBarLineStyle$strokeWidth, 1)
  expect_equal(dbophA_scaleBar$scaleBarTickStyle$stroke, "grey")
  expect_equal(dbophA_scaleBar$scaleBarTickStyle$strokeWidth, 2)
  expect_equal(dbophA_scaleBar$labelStyle$labelPosition, "left")
  expect_equal(dbophA_scaleBar$labelStyle$fontSize, "12px")
  expect_equal(dbophA_scaleBar$labelStyle$fontFamily, "Arial")
  expect_equal(dbophA_scaleBar$labelStyle$fill, "blue")
  expect_equal(dbophA_scaleBar$title, "2 kb")
  expect_equal(dbophA_scaleBar$scaleBarUnit, 2000)
})

# GC_clusterLabel
test_that("GC_clusterLabel updates cluster labels for specified clusters correctly", {

  # Load ophA_clusters data
  data("ophA_clusters", package = "geneviewer")

  # Generate a GC chart
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class", height = "400px")

  # Update cluster labels for specific clusters
  updated_chart <- GC_clusterLabel(
    GC_chart = chart,
    title = c("Cluster A", "Cluster B"),
    cluster = c("ophA", "dbophA"),
    position = c("left", "right"),
    width = "150px",
    wrapLabel = TRUE,
    wrapOptions = list(
      dyAdjust = 0,
      lineHeightEms = 1.1,
      splitOnHyphen = TRUE
    ),
    fontSize = "14px",
    fontStyle = "italic",
    fontWeight = "bold",
    fontFamily = "Arial",
    cursor = "pointer"
  )

  # Assertions
  expect_s3_class(updated_chart, "htmlwidget")

  # Check if cluster label settings are applied to the specified clusters
  expect_true("clusterLabel" %in% names(updated_chart$x$series$ophA))
  expect_true("clusterLabel" %in% names(updated_chart$x$series$dbophA))

  # Verify cluster label settings for `ophA` cluster
  ophA_label <- updated_chart$x$series$ophA$clusterLabel
  expect_equal(ophA_label$title, "Cluster A")
  expect_true(ophA_label$show)
  expect_equal(ophA_label$position, "left")
  expect_true(ophA_label$wrapLabel)
  expect_equal(ophA_label$wrapOptions$dyAdjust, 0)
  expect_equal(ophA_label$wrapOptions$lineHeightEms, 1.1)
  expect_true(ophA_label$wrapOptions$splitOnHyphen)
  expect_equal(ophA_label$fontSize, "14px")
  expect_equal(ophA_label$fontStyle, "italic")
  expect_equal(ophA_label$fontWeight, "bold")
  expect_equal(ophA_label$fontFamily, "Arial")
  expect_equal(ophA_label$cursor, "pointer")

  # Verify cluster label settings for `dbophA` cluster
  dbophA_label <- updated_chart$x$series$dbophA$clusterLabel
  expect_equal(dbophA_label$title, "Cluster B")
  expect_true(dbophA_label$show)
  expect_equal(dbophA_label$position, "right")
  expect_true(dbophA_label$wrapLabel)
  expect_equal(dbophA_label$wrapOptions$dyAdjust, 0)
  expect_equal(dbophA_label$wrapOptions$lineHeightEms, 1.1)
  expect_true(dbophA_label$wrapOptions$splitOnHyphen)
  expect_equal(dbophA_label$fontSize, "14px")
  expect_equal(dbophA_label$fontStyle, "italic")
  expect_equal(dbophA_label$fontWeight, "bold")
  expect_equal(dbophA_label$fontFamily, "Arial")
  expect_equal(dbophA_label$cursor, "pointer")

  # Verify grid settings for label width
  expect_equal(updated_chart$x$series$ophA$container$margin$left, "150px")
  expect_equal(updated_chart$x$series$dbophA$container$margin$right, "150px")

})

# GC_clusterFooter
test_that("GC_clusterFooter updates footers with all options correctly", {

  # Load ophA_clusters data
  data("ophA_clusters", package = "geneviewer")

  # Generate a GC chart
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class")

  # Update footers with all options
  updated_chart <- GC_clusterFooter(
    GC_chart = chart,
    title = c("Footer for ophA", "Footer for dbophA"),
    subtitle = c("Subtitle for ophA", "Subtitle for dbophA"),
    spacing = c(15, 20),
    show = TRUE,
    cluster = c("ophA", "dbophA"),
    x = c(6, 8),
    y = c(-20, -25),
    align = c("center", "right"),
    titleFont = list(
      fontSize = "12px",
      fontWeight = "bold",
      fontFamily = "sans-serif",
      fill = "black",
      cursor = "default"
    ),
    subtitleFont = list(
      fill = "grey",
      fontSize = "10px",
      fontStyle = "normal",
      fontFamily = "sans-serif",
      cursor = "default"
    )
  )

  # Assertions
  expect_s3_class(updated_chart, "htmlwidget")

  # Verify footer settings for cluster ophA
  ophA_footer <- updated_chart$x$series$ophA$footer
  expect_equal(ophA_footer$title, "Footer for ophA")
  expect_equal(ophA_footer$subtitle, "Subtitle for ophA")
  expect_true(ophA_footer$show)
  expect_equal(ophA_footer$titleFont$fontSize, "12px")
  expect_equal(ophA_footer$titleFont$fontWeight, "bold")
  expect_equal(ophA_footer$titleFont$fontFamily, "sans-serif")
  expect_equal(ophA_footer$titleFont$fill, "black")
  expect_equal(ophA_footer$titleFont$cursor, "default")
  expect_equal(ophA_footer$subtitleFont$fontSize, "10px")
  expect_equal(ophA_footer$subtitleFont$fontStyle, "normal")
  expect_equal(ophA_footer$subtitleFont$fontFamily, "sans-serif")
  expect_equal(ophA_footer$subtitleFont$fill, "grey")
  expect_equal(ophA_footer$subtitleFont$cursor, "default")
  expect_equal(ophA_footer$x, 6)
  expect_equal(ophA_footer$y, -20)

  # Verify footer settings for cluster dbophA
  dbophA_footer <- updated_chart$x$series$dbophA$footer
  expect_equal(dbophA_footer$title, "Footer for dbophA")
  expect_equal(dbophA_footer$subtitle, "Subtitle for dbophA")
  expect_true(dbophA_footer$show)
  expect_equal(dbophA_footer$titleFont$fontSize, "12px")
  expect_equal(dbophA_footer$titleFont$fontWeight, "bold")
  expect_equal(dbophA_footer$titleFont$fontFamily, "sans-serif")
  expect_equal(dbophA_footer$titleFont$fill, "black")
  expect_equal(dbophA_footer$titleFont$cursor, "default")
  expect_equal(dbophA_footer$subtitleFont$fontSize, "10px")
  expect_equal(dbophA_footer$subtitleFont$fontStyle, "normal")
  expect_equal(dbophA_footer$subtitleFont$fontFamily, "sans-serif")
  expect_equal(dbophA_footer$subtitleFont$fill, "grey")
  expect_equal(dbophA_footer$subtitleFont$cursor, "default")
  expect_equal(dbophA_footer$x, 8)
  expect_equal(dbophA_footer$y, -25)

})

test_that("GC_labels updates labels with all options correctly", {

  # Load ophA_clusters data
  data("ophA_clusters", package = "geneviewer")

  # Generate a GC chart
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class")

  # Update labels with all options
  updated_chart <- GC_labels(
    GC_chart = chart,
    label = "name",
    show = TRUE,
    cluster = c("ophA", "dbophA"),
    itemStyle = list(
      list(index = 0, fill = "red", fontSize = "14px", fontWeight = "bold"),
      list(index = 1, fill = "blue", fontSize = "12px", fontWeight = "normal")
    ),
    x = 10,
    y = -5,
    adjustLabels = TRUE,
    fontSize = "12px",
    fontStyle = "italic",
    fill = "black",
    fontFamily = "sans-serif",
    textAnchor = "middle",
    cursor = "default"
  )

  # Assertions
  expect_s3_class(updated_chart, "htmlwidget")

  # Verify label settings for cluster ophA
  ophA_label <- updated_chart$x$series$ophA$labels
  expect_equal(ophA_label$label, "name")
  expect_true(ophA_label$show)
  expect_equal(ophA_label$itemStyle[[1]]$fill, "red")
  expect_equal(ophA_label$itemStyle[[1]]$fontSize, "14px")
  expect_equal(ophA_label$itemStyle[[1]]$fontWeight, "bold")
  expect_equal(ophA_label$x, 10)
  expect_equal(ophA_label$y, -5)

  # Verify label settings for cluster dbophA
  dbophA_label <- updated_chart$x$series$dbophA$labels
  expect_equal(dbophA_label$label, "name")
  expect_true(dbophA_label$show)
  expect_equal(dbophA_label$itemStyle[[2]]$fill, "blue")
  expect_equal(dbophA_label$itemStyle[[2]]$fontSize, "12px")
  expect_equal(dbophA_label$itemStyle[[2]]$fontWeight, "normal")
  expect_equal(dbophA_label$x, 10)
  expect_equal(dbophA_label$y, -5)

})




