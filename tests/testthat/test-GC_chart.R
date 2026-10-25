
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

  # The label width is sent with the label; the browser sets the margin from it
  expect_equal(ophA_label$width, "150px")
  expect_equal(dbophA_label$width, "150px")

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

# GC_labels
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

# GC_coordinates
test_that("GC_coordinates updates coordinates with all options correctly", {

  # Load ophA_clusters data
  data("ophA_clusters", package = "geneviewer")

  # Generate a GC chart
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class")

  # Update coordinates with all options
  updated_chart <- GC_coordinates(
    GC_chart = chart,
    show = TRUE,
    tickValuesTop = c(2522, 5286, 9536),
    tickValuesBottom = c(4276, 4718, 10904),
    ticksFormat = ",.0f",
    tickStyle = list(
      fontSize = "10px",
      fontWeight = "normal",
      fontFamily = "sans-serif",
      fill = "grey",
      cursor = "default"
    ),
    textStyle = list(
      fontSize = "12px",
      fontStyle = "italic",
      fontFamily = "sans-serif",
      fill = "black",
      textAnchor = "middle",
      cursor = "default"
    ),
    cluster = c("ophA"),
    x = c(10, 15),
    y = c(-5, -10)
  )

  # Assertions
  expect_s3_class(updated_chart, "htmlwidget")

  # Verify coordinate settings for cluster ophA
  ophA_coordinates <- updated_chart$x$series$ophA$coordinates
  expect_true(ophA_coordinates$show)
  expect_equal(ophA_coordinates$tickValuesTop, c(2522, 5286, 9536))
  expect_equal(ophA_coordinates$tickValuesBottom, c(4276, 4718, 10904))
  expect_equal(ophA_coordinates$ticksFormat, ",.0f")
  expect_equal(ophA_coordinates$tickStyle$fontSize, "10px")
  expect_equal(ophA_coordinates$tickStyle$fill, "grey")
  expect_equal(ophA_coordinates$textStyle$fontSize, "12px")
  expect_equal(ophA_coordinates$textStyle$fill, "black")
  expect_equal(ophA_coordinates$x, 10)
  expect_equal(ophA_coordinates$y, -5)

})

# GC_genes test
test_that("GC_genes updates gene characteristics with all options correctly", {

  # Load ophA_clusters data
  data("ophA_clusters", package = "geneviewer")

  # Generate a GC chart
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class")

  # Update genes with all options
  updated_chart <- GC_genes(
    GC_chart = chart,
    group = "class",
    show = TRUE,
    marker = "arrow",
    marker_size = "medium",
    colorScheme = "schemeCategory10",
    customColors = c("red", "blue", "green"),
    cluster = c("ophA", "dbophA"),
    itemStyle = list(
      list(index = 0, fill = "red", stroke = "black"),
      list(index = 1, fill = "blue", stroke = "grey")
    ),
    x = c(10, 15),
    y = c(-5, -10),
    stroke = "black",
    strokeWidth = 1,
    arrowheadWidth = NULL,
    arrowheadHeight = NULL,
    arrowHeight = NULL,
    markerHeight = NULL
  )

  # Assertions
  expect_s3_class(updated_chart, "htmlwidget")

  # Verify gene settings for cluster ophA
  ophA_genes <- updated_chart$x$series$ophA$genes
  expect_equal(ophA_genes$group, "class")
  expect_true(ophA_genes$show)
  expect_equal(ophA_genes$marker, "arrow")
  expect_equal(ophA_genes$markerSize, "medium")
  expect_equal(ophA_genes$colorScheme, "schemeCategory10")
  expect_equal(ophA_genes$customColors, c("red", "blue", "green"))
  expect_equal(ophA_genes$itemStyle[[1]]$fill, "red")
  expect_equal(ophA_genes$itemStyle[[1]]$stroke, "black")
  expect_equal(ophA_genes$itemStyle[[2]]$fill, "blue")
  expect_equal(ophA_genes$itemStyle[[2]]$stroke, "grey")
  expect_equal(ophA_genes$x, c(10, 15))
  expect_equal(ophA_genes$y, c(-5, -10))
  expect_equal(ophA_genes$stroke, "black")
  expect_equal(ophA_genes$strokeWidth, 1)

  # Verify gene settings for cluster dbophA
  dbophA_genes <- updated_chart$x$series$dbophA$genes
  expect_equal(dbophA_genes$group, "class")
  expect_true(dbophA_genes$show)
  expect_equal(dbophA_genes$marker, "arrow")
  expect_equal(dbophA_genes$markerSize, "medium")
  expect_equal(dbophA_genes$colorScheme, "schemeCategory10")
  expect_equal(dbophA_genes$customColors, c("red", "blue", "green"))
  expect_equal(dbophA_genes$itemStyle[[1]]$fill, "red")
  expect_equal(dbophA_genes$itemStyle[[1]]$stroke, "black")
  expect_equal(dbophA_genes$itemStyle[[2]]$fill, "blue")
  expect_equal(dbophA_genes$itemStyle[[2]]$stroke, "grey")
  expect_equal(dbophA_genes$x, c(10, 15))
  expect_equal(dbophA_genes$y, c(-5, -10))
  expect_equal(dbophA_genes$stroke, "black")
  expect_equal(dbophA_genes$strokeWidth, 1)

})

#GC_color
test_that("GC_color updates color scheme with all options correctly", {

  # Load ophA_clusters data
  data("ophA_clusters", package = "geneviewer")

  # Generate a GC chart
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class", height = "100px")

  # Update color scheme with predefined scheme
  updated_chart1 <- GC_color(
    GC_chart = chart,
    colorScheme = "schemeCategory10"
  )

  # Assertions for predefined color scheme
  expect_s3_class(updated_chart1, "htmlwidget")
  expect_equal(updated_chart1$x$legend$legendOptions$colorScheme, "schemeCategory10")
  expect_null(updated_chart1$x$legend$legendOptions$customColors)

  # Validate series colorScheme
  clusters <- names(updated_chart1$x$series)
  for (cluster_name in clusters) {
    expect_equal(updated_chart1$x$series[[cluster_name]]$genes$colorScheme, "schemeCategory10")
    expect_null(updated_chart1$x$series[[cluster_name]]$genes$customColors)
    expect_equal(updated_chart1$x$series[[cluster_name]]$transcript$colorScheme, "schemeCategory10")
    expect_null(updated_chart1$x$series[[cluster_name]]$transcript$customColors)
  }

  # Update color scheme with custom colors
  updated_chart2 <- GC_color(
    GC_chart = chart,
    customColors = c("red", "orange", "green")
  )

  # Assertions for custom colors
  expect_s3_class(updated_chart2, "htmlwidget")
  expect_null(updated_chart2$x$legend$legendOptions$colorScheme)
  expect_equal(updated_chart2$x$legend$legendOptions$customColors, c("red", "orange", "green"))

  # Validate series customColors
  for (cluster_name in clusters) {
    expect_null(updated_chart2$x$series[[cluster_name]]$genes$colorScheme)
    expect_equal(updated_chart2$x$series[[cluster_name]]$genes$customColors, c("red", "orange", "green"))
    expect_null(updated_chart2$x$series[[cluster_name]]$transcript$colorScheme)
    expect_equal(updated_chart2$x$series[[cluster_name]]$transcript$customColors, c("red", "orange", "green"))
  }

  # Update color scheme with named custom colors
  updated_chart3 <- GC_color(
    GC_chart = chart,
    customColors = list(A = "yellow", B = "pink", C = "purple")
  )

  # Assertions for named custom colors
  expect_s3_class(updated_chart3, "htmlwidget")
  expect_null(updated_chart3$x$legend$legendOptions$colorScheme)
  expect_equal(updated_chart3$x$legend$legendOptions$customColors, list(A = "yellow", B = "pink", C = "purple"))

  # Validate series named customColors
  for (cluster_name in clusters) {
    expect_null(updated_chart3$x$series[[cluster_name]]$genes$colorScheme)
    expect_equal(updated_chart3$x$series[[cluster_name]]$genes$customColors, list(A = "yellow", B = "pink", C = "purple"))
    expect_null(updated_chart3$x$series[[cluster_name]]$transcript$colorScheme)
    expect_equal(updated_chart3$x$series[[cluster_name]]$transcript$customColors, list(A = "yellow", B = "pink", C = "purple"))
  }

})

#GC_pattern
test_that("GC_pattern sets patterns on genes, transcripts and legend", {

  data("ophA_clusters", package = "geneviewer")
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class", height = "100px")

  # Defaults: black patterns on white
  updated_chart1 <- GC_pattern(chart)
  expected <- list(color = "#000000", background = "#FFFFFF", size = 6, strokeWidth = 1)
  expect_s3_class(updated_chart1, "htmlwidget")
  expect_equal(updated_chart1$x$legend$legendOptions$pattern, expected)
  for (cluster_name in names(updated_chart1$x$series)) {
    expect_equal(updated_chart1$x$series[[cluster_name]]$genes$pattern, expected)
    expect_equal(updated_chart1$x$series[[cluster_name]]$transcript$pattern, expected)
  }

  # Vectors become arrays, named vectors objects, R colour names hex colours
  updated_chart2 <- GC_pattern(
    chart,
    patterns = c("dots", "grid"),
    background = c(A = "grey75", B = "transparent")
  )
  pattern <- updated_chart2$x$legend$legendOptions$pattern
  expect_equal(pattern$patterns, list("dots", "grid"))
  expect_equal(pattern$background, list(A = "#BFBFBF", B = "#FFFFFF00"))

  # Pattern colours by group, and the new patterns
  updated_chart4 <- GC_pattern(
    chart,
    patterns = c("checkerboard", "rings", "bricks", "plus", "wideStripes", "inverseDots"),
    color = c(A = "white", B = "grey20"),
    background = "black"
  )
  pattern <- updated_chart4$x$legend$legendOptions$pattern
  expect_equal(pattern$color, list(A = "#FFFFFF", B = "#333333"))
  expect_length(pattern$patterns, 6)
  expect_equal(GC_pattern(chart, color = c("red", "blue"))$x$legend$legendOptions$pattern$color,
               list("#FF0000", "#0000FF"))

  # background = NULL keeps the gene colours
  updated_chart3 <- GC_pattern(chart, patterns = list(A = "crosshatch"), background = NULL)
  expect_null(updated_chart3$x$legend$legendOptions$pattern$background)
  expect_equal(updated_chart3$x$legend$legendOptions$pattern$patterns, list(A = "crosshatch"))

  # Unknown patterns
  expect_error(GC_pattern(chart, patterns = "stripes"), "Unknown pattern")
})

test_that("GC_legend updates legend options with all options correctly", {

  # Load ophA_clusters data
  data("ophA_clusters", package = "geneviewer")

  # Generate a GC chart
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class", height = "200px")

  # Update legend with all options
  updated_chart <- GC_legend(
    GC_chart = chart,
    group = "class",
    show = TRUE,
    backgroundColor = "#0000",
    order = rev(unique(ophA_clusters$class)),
    position = "bottom",
    orientation = "horizontal",
    x = 0,
    y = 0,
    adjustHeight = TRUE,
    style = list(
      backgroundColor = "red"
    ),
    legendOptions = list(
      cursor = "pointer",
      colorScheme = NULL,
      customColors = NULL
    ),
    legendTextOptions = list(
      cursor = "pointer",
      textAnchor = "start",
      dy = ".35em",
      fontSize = "14px",
      fontFamily = "sans-serif",
      fill = "blue"
    )
  )

  # Assertions for all options
  expect_s3_class(updated_chart, "htmlwidget")

  # Verify legend settings
  expect_equal(updated_chart$x$legend$group, "class")
  expect_true(updated_chart$x$legend$show)
  # backgroundColor is sent as style$backgroundColor; style takes precedence
  expect_null(updated_chart$x$legend$backgroundColor)
  expect_equal(GC_legend(chart, backgroundColor = "yellow")$x$legend$style$backgroundColor, "yellow")
  expect_equal(updated_chart$x$legend$order, rev(unique(ophA_clusters$class)))
  expect_equal(updated_chart$x$legend$position, "bottom")
  expect_equal(updated_chart$x$legend$orientation, "horizontal")
  expect_equal(updated_chart$x$legend$x, 0)
  expect_equal(updated_chart$x$legend$y, 0)
  expect_true(updated_chart$x$legend$adjustHeight)
  expect_equal(updated_chart$x$legend$style$backgroundColor, "red")
  expect_equal(updated_chart$x$legend$legendOptions$cursor, "pointer")
  expect_null(updated_chart$x$legend$legendOptions$colorScheme)
  expect_null(updated_chart$x$legend$legendOptions$customColors)
  expect_equal(updated_chart$x$legend$legendTextOptions$cursor, "pointer")
  expect_equal(updated_chart$x$legend$legendTextOptions$textAnchor, "start")
  expect_equal(updated_chart$x$legend$legendTextOptions$dy, ".35em")
  expect_equal(updated_chart$x$legend$legendTextOptions$fontSize, "14px")
  expect_equal(updated_chart$x$legend$legendTextOptions$fontFamily, "sans-serif")
  expect_equal(updated_chart$x$legend$legendTextOptions$fill, "blue")

})

# GC_annotation
test_that("GC_annotation adds annotations to GC chart correctly", {

  data("ophA_clusters", package = "geneviewer")

  # Generate a GC chart
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class", height = "220px")

  # Add multiple annotations
  updated_chart <- chart %>%
    GC_annotation(type = "textMarker", cluster = 1, position = 24, text = "Gene 1", arrowSize = 8) %>%
    GC_annotation(type = "text", text = "feature 1", x = 91, y = 71) %>%
    GC_annotation(type = "symbol", symbol = "triangle", x = 95, y = 64, size = 10, rotation = 180) %>%
    GC_annotation(type = "terminator", x = 81) %>%
    GC_annotation(type = "promoter", x = 49)

  # Assertions
  expect_s3_class(updated_chart, "htmlwidget")

  # Verify textMarker annotation
  text_marker <- updated_chart$x$series[[1]]$annotations[[1]]
  expect_equal(text_marker$type, "textMarker")
  expect_equal(text_marker$position, 24)
  expect_equal(text_marker$text, "Gene 1")
  expect_equal(text_marker$arrowSize, 8)

  # Verify text annotation
  text_annotation <- updated_chart$x$series[[1]]$annotations[[2]]
  expect_equal(text_annotation$type, "text")
  expect_equal(text_annotation$text, "feature 1")
  expect_equal(text_annotation$x, 91)
  expect_equal(text_annotation$y, 71)

  # Verify symbol annotation
  symbol_annotation <- updated_chart$x$series[[1]]$annotations[[3]]
  expect_equal(symbol_annotation$type, "symbol")
  expect_equal(symbol_annotation$symbol, "triangle")
  expect_equal(symbol_annotation$x, 95)
  expect_equal(symbol_annotation$y, 64)
  expect_equal(symbol_annotation$size, 10)
  expect_equal(symbol_annotation$rotation, 180)

  # Verify terminator annotation
  terminator_annotation <- updated_chart$x$series[[1]]$annotations[[4]]
  expect_equal(terminator_annotation$type, "terminator")
  expect_equal(terminator_annotation$x, 81)

  # Verify promoter annotation
  promoter_annotation <- updated_chart$x$series[[1]]$annotations[[5]]
  expect_equal(promoter_annotation$type, "promoter")
  expect_equal(promoter_annotation$x, 49)

})

test_that("GC_trackMouse enables or disables mouse tracking correctly", {

  data("ophA_clusters", package = "geneviewer")

  # Generate a GC chart
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class", height = "200px")

  # Enable mouse tracking on all clusters
  updated_chart <- GC_trackMouse(GC_chart = chart, show = TRUE)

  # Assertions
  expect_s3_class(updated_chart, "htmlwidget")
  clusters <- names(updated_chart$x$series)
  for (cluster_name in clusters) {
    expect_true(updated_chart$x$series[[cluster_name]]$trackMouse$show)
  }

  # Disable mouse tracking on all clusters
  updated_chart <- GC_trackMouse(GC_chart = chart, show = FALSE)

  # Assertions
  for (cluster_name in clusters) {
    expect_false(updated_chart$x$series[[cluster_name]]$trackMouse$show)
  }

})

# Test for GC_cluster function
test_that("GC_cluster applies prevent_gene_overlap and other options correctly", {

  # Load or create the human_hox_genes dataset
  data("human_hox_genes", package = "geneviewer")

  # Create a basic GC_chart object
  chart <- GC_chart(human_hox_genes, cluster = "cluster", group = "name", height = "600px")

  # Apply GC_cluster function with specified options
  updated_chart <- GC_cluster(
    GC_chart = chart,
    separate_strands = NULL,
    prevent_gene_overlap = TRUE,
    cluster = NULL,
    strand_spacing = 5,
    overlap_spacing = 10,
    style = list(backgroundColor = "lightgrey"),
    tooltip = TRUE
  )

  # Assertions
  expect_s3_class(updated_chart, "htmlwidget")

  # Check that 'prevent_gene_overlap' has been applied
  clusters <- updated_chart$x$series
  for (i in seq_along(clusters)) {
    cluster_options <- clusters[[i]]$cluster
    expect_true(cluster_options$preventGeneOverlap)
    expect_equal(cluster_options$overlapSpacing, 10)
  }

  # Check that 'strand_spacing' is set correctly if applicable
  for (i in seq_along(clusters)) {
    cluster_options <- clusters[[i]]$cluster
    expect_equal(cluster_options$strandSpacing, 5)
  }

  # Verify style settings
  for (i in seq_along(clusters)) {
    container_style <- clusters[[i]]$container$style
    expect_equal(container_style$backgroundColor, "lightgrey")
  }

  # Verify tooltip option
  for (i in seq_along(clusters)) {
    container_tooltip <- clusters[[i]]$container$tooltip
    expect_true(container_tooltip)  # Ensure tooltips are enabled
  }

})

test_that("GC_transcript modifies transcript characteristics correctly", {

  # Load or create the transcript dataset
  transcript_data <- data.frame(
    transcript = c("transcript1", "transcript1", "transcript1",
                   "transcript2", "transcript2", "transcript2"),
    type = c("5_utr", "exon", "3_utr", "5_utr", "exon", "3_utr"),
    start = c(1, 101, 301, 1, 101, 301),
    end = c(50, 200, 350, 50, 200, 350),
    strand = c("forward", "forward", "forward", "forward", "forward", "forward")
  )

  # Create a basic GC_chart object
  chart <- GC_chart(
    transcript_data,
    start = "start",
    end = "end",
    height = "300px"
  )

  # Apply GC_transcript with various options
  updated_chart <- GC_transcript(
    GC_chart = chart,
    transcript = "transcript",
    type = "type",
    strand = "strand",
    group = "transcript",
    selection = "transcript1",
    show = TRUE,
    colorScheme = "schemeCategory10",
    customColors = list("red", "blue", "green"),
    styleExons = list(marker = "box", fill = "yellow", strokeWidth = 2),
    styleIntrons = list(marker = "intron", fill = "grey", strokeWidth = 1),
    styleUTRs = list(fill = "lightblue", strokeWidth = 0.5),
    itemStyleExons = list(list(index = 1, fill = "orange")),
    labelOptions = list(fontSize = "12px", color = "black", xOffset = 2)
  )

  # Assertions
  expect_s3_class(updated_chart, "htmlwidget") # Verify object type

  # Check transcript settings for 'transcript1'
  transcript1 <- updated_chart$x$series$transcript1$transcript

  expect_equal(transcript1$group, "transcript") # Verify grouping column
  expect_true(transcript1$show) # Ensure show option is TRUE
  expect_equal(transcript1$colorScheme, "schemeCategory10") # Color scheme
  expect_equal(transcript1$customColors, list("red", "blue", "green")) # Custom colors

  # Verify exon styles
  expect_equal(transcript1$styleExons$marker, "box")
  expect_equal(transcript1$styleExons$fill, "yellow")
  expect_equal(transcript1$styleExons$strokeWidth, 2)

  # Verify intron styles
  expect_equal(transcript1$styleIntrons$marker, "intron")
  expect_equal(transcript1$styleIntrons$fill, "grey")
  expect_equal(transcript1$styleIntrons$strokeWidth, 1)

  # Verify UTR styles
  expect_equal(transcript1$styleUTRs$fill, "lightblue")
  expect_equal(transcript1$styleUTRs$strokeWidth, 0.5)

  # Verify item style for specific exon
  expect_equal(transcript1$itemStyleExons[[1]]$index, 1)
  expect_equal(transcript1$itemStyleExons[[1]]$fill, "orange")

  # Verify label options
  expect_equal(transcript1$labelOptions$fontSize, "12px")
  expect_equal(transcript1$labelOptions$color, "black")
  expect_equal(transcript1$labelOptions$xOffset, 2)

  # Verify strand information
  expect_equal(updated_chart$x$params$strand, "strand")
})

test_that("GC_chart accepts a percentage height (B01)", {
  genes_data <- data.frame(
    start = c(10, 90, 170),
    end = c(75, 115, 185),
    cluster = c("A", "A", "B")
  )

  chart <- GC_chart(genes_data, cluster = "cluster", height = "100%")
  expect_equal(chart$x$series[["A"]]$container$height, "50%")
  expect_equal(chart$x$series[["B"]]$container$height, "50%")

  # Pixel heights are still split into numbers
  chart <- GC_chart(genes_data, cluster = "cluster", height = "400px")
  expect_equal(chart$x$series[["A"]]$container$height, 200)
})

test_that("GC_cluster prevent_gene_overlap only changes the selected cluster (B02)", {
  genes_data <- data.frame(
    start = c(10, 40, 10, 40),
    end = c(80, 120, 80, 120),
    cluster = c("A", "A", "B", "B")
  )

  chart <- GC_chart(genes_data, cluster = "cluster") %>%
    GC_cluster(prevent_gene_overlap = TRUE, cluster = 2)

  expect_null(chart$x$series[["A"]]$data$geneTrack)
  expect_setequal(chart$x$series[["B"]]$data$geneTrack, c(1, 2))
  expect_true(chart$x$series[["B"]]$cluster$preventGeneOverlap)
  expect_null(chart$x$series[["B"]]$cluster$subset_data)

  # More than two clusters, selected by index and by name
  genes_data <- data.frame(
    start = rep(c(10, 40), 4),
    end = rep(c(80, 120), 4),
    cluster = rep(c("A", "B", "C", "D"), each = 2)
  )
  has_tracks <- function(chart) {
    vapply(chart$x$series, function(s) !is.null(s$data$geneTrack), logical(1))
  }

  chart <- GC_chart(genes_data, cluster = "cluster") %>%
    GC_cluster(prevent_gene_overlap = TRUE, cluster = c(2, 4))
  expect_equal(unname(has_tracks(chart)), c(FALSE, TRUE, FALSE, TRUE))

  chart <- GC_chart(genes_data, cluster = "cluster") %>%
    GC_cluster(prevent_gene_overlap = TRUE, cluster = "C")
  expect_equal(unname(has_tracks(chart)), c(FALSE, FALSE, TRUE, FALSE))

  # A single cluster
  chart <- GC_chart(genes_data[1:2, ], cluster = "cluster") %>%
    GC_cluster(prevent_gene_overlap = TRUE)
  expect_setequal(chart$x$series[["A"]]$data$geneTrack, c(1, 2))
})

test_that("scale breaks work with one gene or one merged region (B09)", {
  expect_equal(get_scale_breaks(data.frame(start = 100, end = 400)), list())
  expect_equal(get_scale_breaks(data.frame(start = c(100, 200), end = c(400, 500))), list())
  expect_length(get_scale_breaks(data.frame(start = c(100, 5000), end = c(400, 5600))), 1)

  genes_data <- data.frame(start = c(100, 5000, 9000), end = c(400, 5600, 9500), cluster = c("A", "B", "B"))
  chart <- GC_chart(genes_data, cluster = "cluster") %>% GC_scale(scale_breaks = TRUE)
  expect_equal(chart$x$series[["A"]]$scale$breaks, list())
  expect_length(chart$x$series[["B"]]$scale$breaks, 1)
})

test_that("GC_scale range axis shifts only the selected clusters (B10)", {
  genes <- data.frame(
    cluster = rep(c("A", "B", "C"), each = 2),
    start = c(10000, 11500, 50000, 50800, 200, 1200),
    end = c(11000, 12800, 50600, 51300, 1000, 2000)
  )
  base <- GC_chart(genes, cluster = "cluster")

  # hidden = TRUE still shifts the genes
  chart <- base %>% GC_scale(axis_type = "range", hidden = TRUE)
  expect_equal(chart$x$series[["A"]]$data$start, c(1, 1501))
  expect_true(all(vapply(chart$x$series, function(s) s$scale$hidden, logical(1))))

  # One selected cluster only gets its own genes; other clusters are unchanged
  chart <- base %>% GC_scale(axis_type = "range", cluster = 2)
  expect_equal(nrow(chart$x$series[["B"]]$data), 2)
  expect_equal(chart$x$series[["B"]]$data$start, c(1, 801))
  expect_equal(chart$x$series[["B"]]$scale$xMax, 1301)
  expect_equal(chart$x$series[["A"]]$data$start, c(10000, 11500))
  expect_equal(chart$x$data$start, c(10000, 11500, 1, 801, 200, 1200))

  # Gene tracks from prevent_gene_overlap are kept
  chart <- base %>% GC_cluster(prevent_gene_overlap = TRUE) %>% GC_scale(axis_type = "range")
  expect_false(is.null(chart$x$series[["A"]]$data$geneTrack))

  # Additional options are passed on
  chart <- base %>% GC_scale(myOption = "x")
  expect_equal(chart$x$series[["A"]]$scale$myOption, "x")
})

test_that("custom_gaps measures gaps by gene edges and handles repeated names (B11)", {
  run <- function(data, gaps) {
    chart <- GC_chart(data, group = "name") %>% GC_normalize(normalize = FALSE, custom_gaps = gaps)
    chart$x$series[[1]]$data
  }
  # Reverse-strand gene before the gap
  reverse <- data.frame(name = c("G1", "G2", "G3"), start = c(500, 1000, 2000), end = c(100, 1400, 2400))
  result <- run(reverse, list(G1 = 2))
  expect_equal(result$start, c(500, 1500, 2500))
  expect_equal(result$end, c(100, 1900, 2900))

  # Reverse-strand gene after the gap
  next_reverse <- data.frame(name = c("G1", "G2"), start = c(100, 1400), end = c(500, 1000))
  expect_equal(run(next_reverse, list(G1 = 2))$start, c(100, 1900))

  # A name used twice: the gap after each copy is changed
  duplicate <- data.frame(name = c("tnp", "G2", "tnp", "G4"), start = c(100, 1000, 2000, 3000), end = c(500, 1400, 2400, 3400))
  expect_equal(run(duplicate, list(tnp = 2))$start, c(100, 1500, 2500, 4100))

  # The last gene has no gap after it; unknown names leave the data unchanged
  expect_equal(run(reverse, list(G3 = 2))$start, reverse$start)
  expect_warning(result <- run(reverse, list(G9 = 2)), "G9 not found")
  expect_equal(result$start, reverse$start)
  expect_false(any(c("is_reverse", "actual_start", "original_order") %in% names(result)))
})

test_that("GC_clusterLabel labels selected clusters with their own name (B12)", {
  genes <- data.frame(cluster = rep(c("A", "B", "C"), each = 2), start = rep(c(1, 50), 3), end = rep(c(40, 90), 3))
  labels <- function(chart) vapply(chart$x$series, function(s) if (is.null(s$clusterLabel$title)) NA_character_ else s$clusterLabel$title, "")

  expect_equal(unname(labels(GC_chart(genes, cluster = "cluster") %>% GC_clusterLabel())), c("A", "B", "C"))
  expect_equal(unname(labels(GC_chart(genes, cluster = "cluster") %>% GC_clusterLabel(cluster = 2))), c(NA, "B", NA))
  expect_equal(unname(labels(GC_chart(genes, cluster = "cluster") %>% GC_clusterLabel(cluster = c("C", "B")))), c(NA, "B", "C"))
  expect_equal(unname(labels(GC_chart(genes, cluster = "cluster") %>% GC_clusterLabel(title = "x", cluster = 3))), c(NA, NA, "x"))
})

test_that("vector arguments are recycled per cluster or give a clear error (B13)", {
  genes <- data.frame(start = c(1, 50, 1, 50), end = c(40, 90, 40, 90), name = c("a", "b", "c", "d"),
                      cluster = c("A", "A", "B", "B"), type = "exon")
  chart <- GC_chart(genes, cluster = "cluster", group = "name")
  per_cluster <- function(chart, part, field) unname(sapply(chart$x$series, function(s) s[[part]][[field]]))

  expect_equal(per_cluster(chart %>% GC_labels(label = c("name", "cluster")), "labels", "label"), c("name", "cluster"))
  expect_equal(per_cluster(chart %>% GC_labels(show = c(TRUE, FALSE)), "labels", "show"), c(TRUE, FALSE))
  expect_equal(per_cluster(chart %>% GC_genes(group = c("name", "cluster")), "genes", "group"), c("name", "cluster"))
  expect_equal(per_cluster(chart %>% GC_clusterLabel(show = c(TRUE, FALSE)), "clusterLabel", "show"), c(TRUE, FALSE))
  expect_equal(per_cluster(chart %>% GC_coordinates(show = c(TRUE, FALSE)), "coordinates", "show"), c(TRUE, FALSE))
  expect_equal(per_cluster(chart %>% GC_scaleBar(show = c(TRUE, FALSE)), "scaleBar", "show"), c(TRUE, FALSE))
  expect_equal(per_cluster(chart %>% GC_tooltip(show = c(TRUE, FALSE)), "tooltip", "show"), c(TRUE, FALSE))
  expect_equal(per_cluster(chart %>% GC_clusterTitle("x", show = c(TRUE, FALSE)), "clusterTitle", "show"), c(TRUE, FALSE))
  expect_equal(per_cluster(chart %>% GC_clusterFooter("x", show = c(TRUE, FALSE)), "footer", "show"), c(TRUE, FALSE))

  # A single FALSE still leaves the chart unchanged
  expect_identical(chart %>% GC_labels(show = FALSE), chart)

  # Arguments that take one value give a clear error
  expect_error(chart %>% GC_title("x", show = c(TRUE, FALSE)), "single TRUE or FALSE")
  expect_error(chart %>% GC_align("name", c("a", "c")), "single values")
  expect_error(chart %>% GC_transcript(type = c("type", "name")), "single column name")
  expect_error(chart %>% GC_normalize(group = c("name", "cluster")), "single column name")
})

test_that("GC_legend checks the group column (B14)", {
  genes <- data.frame(start = c(1, 50), end = c(40, 90), class = c("x", "y"))
  expect_error(GC_chart(genes) %>% GC_legend(group = "nope"), "group column not found")
  expect_error(GC_chart(genes) %>% GC_legend(), "Please define a group")
  expect_s3_class(GC_chart(genes) %>% GC_legend(FALSE), "htmlwidget")
  expect_equal((GC_chart(genes) %>% GC_legend(group = "class"))$x$legend$group, "class")
})

test_that("GC_grid margins work with pixel and percentage heights (B18)", {
  genes <- data.frame(start = c(1, 50, 1, 50), end = c(40, 90, 40, 90), cluster = c("A", "A", "B", "B"))
  heights <- function(chart) unname(sapply(chart$x$series, function(s) s$container$height))

  # Pixel heights: both margins are taken from the clusters
  chart <- GC_chart(genes, cluster = "cluster", height = "400px") %>% GC_grid(margin = list(top = 40, bottom = 20))
  expect_equal(heights(chart), c(170, 170))
  expect_equal(chart$x$style$paddingTop, "40px")
  expect_equal(chart$x$style$paddingBottom, "20px")

  # Percentage heights stay percentages (reduced in the browser)
  chart <- GC_chart(genes, cluster = "cluster", height = "100%") %>% GC_grid(margin = list(top = 40, bottom = 20))
  expect_equal(heights(chart), c("50%", "50%"))
  expect_equal(chart$x$style$paddingBottom, "20px")

  expect_error(GC_chart(genes, cluster = "cluster", height = "100%") %>% GC_grid(margin = list(top = "10%")),
               "pixel values")
})

test_that("GC_genes and GC_transcript keep GC_color and GC_pattern settings (B43)", {
  genes <- data.frame(
    start = c(10, 50, 90, 130), end = c(40, 80, 120, 160),
    group = c("A", "A", "B", "C"), cluster = c(1, 1, 2, 2)
  )
  cols <- list(A = "gold", B = "hotpink", C = "purple")
  chart <- GC_chart(genes, cluster = "cluster", group = "group")

  # GC_color() then GC_genes(): colours kept
  chart1 <- chart %>% GC_color(customColors = cols) %>% GC_genes(marker = "box")
  for (cl in names(chart1$x$series)) {
    expect_equal(chart1$x$series[[cl]]$genes$customColors, cols)
    expect_equal(chart1$x$series[[cl]]$genes$marker, "box")
  }

  # GC_pattern() then GC_genes(): pattern kept
  chart2 <- chart %>% GC_pattern() %>% GC_genes(marker = "box")
  for (cl in names(chart2$x$series)) {
    expect_equal(chart2$x$series[[cl]]$genes$pattern,
                 chart2$x$legend$legendOptions$pattern)
  }

  # New colours in GC_genes() replace both old colour options
  chart3 <- chart %>% GC_color(customColors = cols) %>% GC_genes(colorScheme = "schemeAccent")
  expect_equal(chart3$x$series[["1"]]$genes$colorScheme, "schemeAccent")
  expect_null(chart3$x$series[["1"]]$genes$customColors)

  # GC_genes() colours reach the legend, but not when only some clusters are set
  expect_equal(GC_genes(chart, customColors = cols)$x$legend$legendOptions$customColors, cols)
  expect_null(GC_genes(chart, customColors = cols, cluster = 1)$x$legend$legendOptions$customColors)

  # Same for GC_transcript()
  tx <- data.frame(
    transcript = c("t1", "t1", "t2"), type = "exon",
    start = c(1, 50, 1), end = c(20, 80, 30), group = c("A", "B", "A")
  )
  tchart <- GC_chart(tx, group = "group") %>%
    GC_transcript(transcript = "transcript", type = "type") %>%
    GC_color(customColors = cols) %>%
    GC_pattern() %>%
    GC_transcript(show = TRUE)
  for (tr in names(tchart$x$series)) {
    expect_equal(tchart$x$series[[tr]]$transcript$customColors, cols)
    expect_false(is.null(tchart$x$series[[tr]]$transcript$pattern))
  }
  expect_equal(
    (GC_chart(tx, group = "group") %>%
       GC_transcript(transcript = "transcript", type = "type", customColors = cols)
    )$x$legend$legendOptions$customColors,
    cols
  )
})

test_that("GC_transcript keeps the settings of the whole chart made before it (B50)", {
  brca <- subset(BRCA1_splice_variants, ensembl_transcript_id %in% unique(ensembl_transcript_id)[1:2])
  chart <- GC_chart(brca, start = "start", end = "end", strand = "strand", group = "type", height = "300px")
  transcripts <- function(chart) GC_transcript(chart, transcript = "ensembl_transcript_id", type = "type")
  settings <- function(chart) {
    chart %>%
      GC_title("Title", subtitle = "Subtitle") %>%
      GC_legend(position = "top", style = list(backgroundColor = "yellow")) %>%
      GC_theme("dark") %>%
      GC_grid(margin = list(top = 40, bottom = 20, left = 150), direction = "row")
  }

  before <- transcripts(settings(chart))
  after <- settings(transcripts(chart))

  for (name in c("title", "legend", "theme", "style", "graphContainer")) {
    expect_equal(before$x[[name]], after$x[[name]], label = name)
  }
  expect_equal(before$height, after$height)
  expect_equal(lapply(before$x$series, `[[`, "container"),
               lapply(after$x$series, `[[`, "container"))
  expect_equal(before$x$title$title, "Title")
  expect_equal(before$x$series[[1]]$container$margin$left, 150)
})

test_that("GC_genes() sends corner_radius to the browser as cornerRadius", {
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class")
  rounded <- GC_genes(chart, marker = "boxarrow", corner_radius = 3)
  expect_true(all(sapply(rounded$x$series, function(s) s$genes$cornerRadius) == 3))
  expect_null(GC_genes(chart)$x$series[[1]]$genes$cornerRadius)
  # In a theme, by its argument name
  themed <- GC_theme(chart, genes = list(corner_radius = 3))
  expect_equal(theme_to_js(themed$x$theme)$geneOptions$cornerRadius, 3)
})

test_that("GC_legend() sends corner_radius to the browser as cornerRadius", {
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class")
  expect_equal(GC_legend(chart, corner_radius = 3)$x$legend$cornerRadius, 3)
  expect_null(GC_legend(chart)$x$legend$cornerRadius)
  themed <- GC_theme(chart, legend = list(corner_radius = 3))
  expect_equal(theme_to_js(themed$x$theme)$legendOptions$cornerRadius, 3)
})

test_that("GC_color takes colorScheme with a named list of customColors (B53)", {
  genes <- data.frame(
    start = c(10, 50, 90), end = c(40, 80, 120), group = c("A", "B", "Other")
  )
  chart <- GC_chart(genes, group = "group")

  # A named list sets some groups; colorScheme colours the others
  both <- GC_color(chart, colorScheme = "schemeSet2", customColors = list(Other = "grey"))
  expect_equal(both$x$legend$legendOptions$colorScheme, "schemeSet2")
  expect_equal(both$x$legend$legendOptions$customColors, list(Other = "grey"))
  for (cl in names(both$x$series)) {
    expect_equal(both$x$series[[cl]]$genes$colorScheme, "schemeSet2")
    expect_equal(both$x$series[[cl]]$genes$customColors, list(Other = "grey"))
  }

  # Unnamed customColors replace the scheme: still one or the other
  expect_warning(
    unchanged <- GC_color(chart, colorScheme = "schemeSet2", customColors = c("red", "blue")),
    "Both colorScheme and customColors"
  )
  expect_identical(unchanged, chart)
})

test_that("a title of the default height is not cut off (B54)", {
  genes <- data.frame(start = c(10, 50), end = c(40, 80), group = c("A", "B"))
  file <- tempfile(fileext = ".html")
  # An 18px title with descenders (g, y, p)
  chart <- GC_chart(genes, group = "group", height = "150px") %>%
    GC_title("Gypsy group") %>%
    GC_theme("classic")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)

  session <- chrome_page(file)
  overflow <- session$Runtime$evaluate("
    (() => {
      const svg = document.querySelector('[id^=geneviewer-title-container] svg');
      const text = svg.querySelector('text');
      return text.getBoundingClientRect().bottom - svg.getBoundingClientRect().bottom;
    })()")$result$value

  expect_lte(overflow, 0)
})

test_that("the legend of a chart with a fixed width is not scaled down (B55)", {
  genes <- data.frame(start = c(10, 50), end = c(40, 80), group = c("A", "B"))
  file <- tempfile(fileext = ".html")
  chart <- GC_chart(genes, group = "group", width = "600px", height = "150px")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)

  # A window much wider than the chart
  session <- chrome_page(file, width = 1600, height = 600)
  scale <- session$Runtime$evaluate("
    (() => {
      const svg = document.querySelector('[id^=geneviewer-legend-container] svg');
      return svg.getBoundingClientRect().width / svg.viewBox.baseVal.width;
    })()")$result$value

  expect_equal(scale, 1)
})

test_that("GC_legend() sends align and checks it", {
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class")
  expect_equal(GC_legend(chart, align = "center")$x$legend$align, "center")
  expect_null(GC_legend(chart)$x$legend$align)
  expect_error(GC_legend(chart, align = "middle"), "align must be")
  expect_error(GC_legend(chart, align = c("left", "right")), "align must be")
})

test_that("GC_legend(align = ) aligns the legend rows left, centre or right", {
  genes <- data.frame(start = c(10, 50, 90), end = c(40, 80, 120), group = c("A", "B", "C"))
  # Space between the legend items and the left and right edges of the legend
  margins <- sapply(c("left", "center", "right"), function(align) {
    file <- tempfile(fileext = ".html")
    chart <- GC_chart(genes, group = "group", width = "600px", height = "150px") %>%
      GC_legend(align = align)
    htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
    session <- chrome_page(file)
    unlist(session$Runtime$evaluate("
      (() => {
        const svg = document.querySelector('[id^=geneviewer-legend-container] svg');
        const items = svg.querySelectorAll('.legend');
        const box = svg.getBoundingClientRect();
        const left = Math.min(...[...items].map(d => d.getBoundingClientRect().left));
        const right = Math.max(...[...items].map(d => d.getBoundingClientRect().right));
        return [left - box.left, box.right - right];
      })()", returnByValue = TRUE)$result$value)
  })

  expect_lt(margins[1, "left"], margins[2, "left"])
  expect_lt(margins[2, "right"], margins[1, "right"])
  expect_equal(margins[1, "center"], margins[2, "center"], tolerance = 0.1)
})

test_that("GC_title(), GC_clusterTitle() and GC_clusterFooter() send align and check it", {
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class")
  expect_equal(GC_title(chart, "Title", align = "left")$x$title$align, "left")
  expect_null(GC_title(chart, "Title")$x$title$align)
  expect_error(GC_title(chart, "Title", align = c("left", "right")), "align must be")

  # One per cluster, recycled
  titled <- GC_clusterTitle(chart, title = "Cluster", align = c("left", "right"))
  expect_equal(unname(sapply(titled$x$series, function(s) s$clusterTitle$align)), c("left", "right"))
  expect_null(GC_clusterTitle(chart, title = "Cluster")$x$series[[1]]$clusterTitle$align)
  expect_error(GC_clusterTitle(chart, title = "Cluster", align = "middle"), "align must be")

  footed <- GC_clusterFooter(chart, title = "Footer", align = "right")
  expect_true(all(sapply(footed$x$series, function(s) s$footer$align) == "right"))
  expect_null(GC_clusterFooter(chart, title = "Footer")$x$series[[1]]$footer$align)
  expect_error(GC_clusterFooter(chart, title = "Footer", align = 1), "align must be")
})

test_that("a cluster footer is aligned like the cluster title, beside a cluster label (B56)", {
  genes <- data.frame(start = c(10, 50), end = c(40, 80), group = c("A", "B"))
  # Left and right edges of the cluster title and the footer
  edges <- lapply(c(left = "left", right = "right"), function(align) {
    file <- tempfile(fileext = ".html")
    chart <- GC_chart(genes, group = "group", width = "600px", height = "200px") %>%
      GC_clusterLabel("Cluster") %>%
      GC_clusterTitle(title = "Title", align = align) %>%
      GC_clusterFooter(title = "Footer", align = align)
    htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
    session <- chrome_page(file)
    session$Runtime$evaluate("
      (() => {
        const box = text => {
          const node = [...document.querySelectorAll('text')].find(d => d.textContent === text);
          const rect = node.getBoundingClientRect();
          return { left: rect.left, right: rect.right };
        };
        return { title: box('Title'), footer: box('Footer') };
      })()", returnByValue = TRUE)$result$value
  })

  expect_equal(edges$left$footer$left, edges$left$title$left, tolerance = 0.01)
  expect_equal(edges$right$footer$right, edges$right$title$right, tolerance = 0.01)
})

test_that("GC_clusterLabel() sends align and checks it", {
  chart <- GC_chart(ophA_clusters, cluster = "cluster", group = "class")
  labelled <- GC_clusterLabel(chart, align = c("left", "right"))
  expect_equal(unname(sapply(labelled$x$series, function(s) s$clusterLabel$align)), c("left", "right"))
  expect_null(GC_clusterLabel(chart)$x$series[[1]]$clusterLabel$align)
  expect_error(GC_clusterLabel(chart, align = "middle"), "align must be")
})

test_that("GC_clusterLabel(align = ) lines up the lines of a label in its margin", {
  genes <- data.frame(start = c(10, 50), end = c(40, 80), group = c("A", "B"))
  # Left and right edges of each line of a wrapped label, and of the margin
  edges <- lapply(c(left = "left", center = "center", right = "right"), function(align) {
    file <- tempfile(fileext = ".html")
    chart <- GC_chart(genes, group = "group", width = "600px", height = "200px") %>%
      GC_clusterLabel("A long cluster name wrapped", width = "120px", align = align)
    htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
    session <- chrome_page(file)
    session$Runtime$evaluate("
      (() => {
        const lines = [...document.querySelectorAll('tspan.wrap-line')]
          .filter(d => d.closest('text').textContent.startsWith('A long'))
          .map(d => d.getBoundingClientRect());
        const svg = lines.length && document.querySelector('tspan.wrap-line').ownerSVGElement.getBoundingClientRect();
        return { left: lines.map(d => d.left - svg.left), right: lines.map(d => d.right - svg.left) };
      })()", returnByValue = TRUE)$result$value
  })

  expect_gt(length(edges$left$left), 1)
  # Left: all lines start at the left edge; right: all end at the margin (120px);
  # centre: the lines are centred in the margin. The boxes of the letters stick
  # out a pixel or so past where the text starts, depending on the font
  expect_lt(max(abs(unlist(edges$left$left) - 0)), 2)
  expect_lt(max(abs(unlist(edges$right$right) - 120)), 2)
  mid <- (unlist(edges$center$left) + unlist(edges$center$right)) / 2
  expect_lt(max(abs(mid - 60)), 2)
})

test_that("a wrapped cluster label keeps <i> styles over its lines", {
  genes <- data.frame(start = c(10, 50), end = c(40, 80), group = c("A", "B"))
  file <- tempfile(fileext = ".html")
  chart <- GC_chart(genes, group = "group", width = "600px", height = "200px") %>%
    GC_clusterLabel("pQEB1_inv3 <i>Enterobacter cloacae ECL-21</i>", width = "120px",
                    wrapOptions = list(splitOnHyphen = FALSE))
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
  session <- chrome_page(file)
  label <- session$Runtime$evaluate("
    (() => {
      const text = [...document.querySelectorAll('text')]
        .find(d => d.textContent.startsWith('pQEB1'));
      const lines = [...text.querySelectorAll('tspan.wrap-line')];
      return {
        lines: lines.map(d => d.textContent),
        italic: [...text.querySelectorAll('tspan.wrap-line tspan')]
          .map(d => [d.textContent, getComputedStyle(d).fontStyle])
      };
    })()", returnByValue = TRUE)$result$value

  lines <- unlist(label$lines)
  expect_gt(length(lines), 1)
  expect_false(any(grepl("<|>", lines)))
  expect_equal(paste(lines, collapse = " "), "pQEB1_inv3 Enterobacter cloacae ECL-21")
  styles <- setNames(sapply(label$italic, `[[`, 2), trimws(sapply(label$italic, `[[`, 1)))
  expect_equal(unname(styles["pQEB1_inv3"]), "normal")
  expect_true(all(styles[c("Enterobacter", "cloacae", "ECL-21")] == "italic"))
})

test_that("GC_clusterLabel() sends a subtitle, its font and spacing per cluster", {
  genes <- data.frame(start = c(10, 50, 90), end = c(40, 80, 120), cluster = c(1, 1, 2))
  chart <- GC_chart(genes, cluster = "cluster")

  labelled <- GC_clusterLabel(chart, subtitle = c("E. cloacae", "K. oxytoca"),
                              subtitleFont = list(fontStyle = "italic"), spacing = 16)
  labels <- lapply(labelled$x$series, `[[`, "clusterLabel")
  expect_equal(unname(sapply(labels, `[[`, "subtitle")), c("E. cloacae", "K. oxytoca"))
  expect_equal(labels[[2]]$subtitleFont, list(fontStyle = "italic"))
  expect_equal(labels[[1]]$spacing, 16)

  # Without them nothing is sent, so the theme and the defaults apply
  label <- GC_clusterLabel(chart)$x$series[[1]]$clusterLabel
  expect_null(label$subtitle)
  expect_null(label$subtitleFont)
  expect_null(label$spacing)
  expect_equal(unname(sapply(GC_clusterLabel(chart, spacing = c(10, 12))$x$series,
                             function(s) s$clusterLabel$spacing)), c(10, 12))
  expect_error(GC_clusterLabel(chart, spacing = "10"), "spacing must be")
})

test_that("GC_title(), GC_clusterTitle() and GC_clusterFooter() take spacing", {
  genes <- data.frame(start = c(10, 50), end = c(40, 80), cluster = c(1, 2))
  chart <- GC_chart(genes, cluster = "cluster")
  expect_equal(GC_title(chart, "Title", "Subtitle", spacing = 24)$x$title$spacing, 24)
  expect_equal(GC_clusterTitle(chart, "Title", "Subtitle", spacing = 24)$x$series[[2]]$clusterTitle$spacing, 24)
  expect_equal(GC_clusterFooter(chart, "Title", "Subtitle", spacing = 24)$x$series[[1]]$footer$spacing, 24)
  expect_null(GC_clusterFooter(chart, "Title")$x$series[[1]]$footer$spacing)
  expect_error(GC_title(chart, "Title", spacing = "a"), "spacing must be")
  expect_error(GC_title(chart, "Title", spacing = c(10, 12)), "spacing must be")
  expect_error(GC_clusterFooter(chart, "Title", spacing = NA), "spacing must be")
})

test_that("a cluster label subtitle sits below the title, centred together on the cluster", {
  genes <- data.frame(start = c(10, 50), end = c(40, 80), group = c("A", "B"))
  # Middle of each line (from its y and dy) and the bounding boxes of the title
  # and subtitle
  label_lines <- function(chart) {
    file <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
    session <- chrome_page(file)
    session$Runtime$evaluate("
      (() => {
        const texts = [...document.querySelectorAll('text[dominant-baseline=central]')];
        const middles = t => [...t.querySelectorAll('tspan.wrap-line')].map(l =>
          parseFloat(l.getAttribute('y')) +
          parseFloat(l.getAttribute('dy')) * parseFloat(getComputedStyle(t).fontSize));
        return texts.map(t => ({
          text: t.textContent, middles: middles(t),
          top: t.getBoundingClientRect().top, bottom: t.getBoundingClientRect().bottom,
          fontStyle: getComputedStyle(t).fontStyle, fontWeight: getComputedStyle(t).fontWeight
        }));
      })()", returnByValue = TRUE)$result$value
  }
  chart <- GC_chart(genes, group = "group", width = "600px", height = "200px")

  # Without a subtitle the label is on the middle of the cluster
  middle <- label_lines(chart %>% GC_clusterLabel("pQEB1"))[[1]]$middles[[1]]

  # Default spacing: 1.2 times the mean font size of 12px and 11px
  label <- label_lines(chart %>%
    GC_clusterLabel("pQEB1", subtitle = "E. cloacae", subtitleFont = list(fontStyle = "italic")))
  expect_length(label, 2)
  title <- label[[1]]
  subtitle <- label[[2]]
  expect_equal(subtitle$text, "E. cloacae")
  expect_equal(subtitle$fontStyle, "italic")
  expect_equal(subtitle$fontWeight, "400")
  expect_equal(subtitle$middles[[1]] - title$middles[[1]], 1.2 * (12 + 11) / 2, tolerance = 1e-6)
  expect_equal((title$middles[[1]] + subtitle$middles[[1]]) / 2, middle, tolerance = 1e-6)
  expect_gt(subtitle$top, title$top)

  # A wrapped title: the subtitle is spacing below its last line
  label <- label_lines(chart %>%
    GC_clusterLabel("A long cluster name wrapped", subtitle = "E. cloacae",
                    width = "100px", spacing = 20))
  title_middles <- unlist(label[[1]]$middles)
  subtitle_middle <- label[[2]]$middles[[1]]
  expect_gt(length(title_middles), 1)
  expect_equal(subtitle_middle - max(title_middles), 20, tolerance = 1e-6)
  expect_equal((min(title_middles) + subtitle_middle) / 2, middle, tolerance = 1e-6)
  expect_gte(label[[2]]$top, label[[1]]$bottom - 1)
})

test_that("GC_links() sends vmin and vmax and checks them", {
  genes <- data.frame(cluster = c("A", "B"), start = c(1, 1), end = c(100, 100))
  links <- data.frame(cluster1 = "A", start1 = 1, end1 = 100,
                      cluster2 = "B", start2 = 1, end2 = 100, identity = 90)
  chart <- GC_chart(genes, cluster = "cluster") %>%
    GC_links(data = links, vmin = 80, vmax = 99)
  expect_equal(chart$x$links[[1]]$options$vmin, 80)
  expect_equal(chart$x$links[[1]]$options$vmax, 99)
  expect_null(GC_links(GC_chart(genes, cluster = "cluster"), data = links)$x$links[[1]]$options$vmin)

  chart <- GC_chart(genes, cluster = "cluster")
  expect_error(GC_links(chart, data = links, vmin = 90, vmax = 80), "smaller")
  expect_error(GC_links(chart, data = links, vmin = 100), "smaller")
  expect_error(GC_links(chart, data = links, vmin = "a"), "single numbers")
})

test_that("links fade from nearly white at vmin to the link colour at vmax", {
  genes <- data.frame(cluster = c("A", "B"), start = c(1, 1), end = c(400, 400))
  links <- data.frame(cluster1 = "A", start1 = c(1, 101, 201, 301), end1 = c(90, 190, 290, 390),
                      cluster2 = "B", start2 = c(1, 101, 201, 301), end2 = c(90, 190, 290, 390),
                      identity = c(40, 60, 80, 100))
  file <- tempfile(fileext = ".html")
  chart <- GC_chart(genes, cluster = "cluster", height = "300px") %>%
    GC_links(data = links, vmin = 60, vmax = 100, normal_color = "#000000")
  htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)

  session <- chrome_page(file)
  res <- session$Runtime$evaluate("
    JSON.stringify({
      fills: [...document.querySelectorAll('path.GeneLink')].map(p => p.style.fill),
      labels: [...document.querySelectorAll('.color-bar text')].map(t => t.textContent)
    })")$result$value
  res <- jsonlite::fromJSON(res)

  # 10% black on white is rgb(230, 230, 230); 40 is clamped to vmin
  expect_equal(res$fills, c("rgb(230, 230, 230)", "rgb(230, 230, 230)",
                            "rgb(115, 115, 115)", "rgb(0, 0, 0)"))
  expect_true(all(c("60%", "100%") %in% res$labels))
})

test_that("GC_scale with a range axis moves the annotations with the genes", {
  genes <- data.frame(
    start = c(1000, 1500, 200, 600),
    end = c(1400, 1900, 500, 900),
    name = c("a", "b", "c", "d"),
    cluster = c(1, 1, 2, 2)
  )
  annotate <- function(chart) {
    chart %>%
      GC_annotation(cluster = 1, type = "text", text = "x", x = 1450) %>%
      GC_annotation(cluster = 1, type = "textMarker", position = 1450, x1 = 1450) %>%
      GC_annotation(cluster = 1, type = "rectangle",
                    position = list(list(c(1100, 60), c(1300, 40))))
  }
  check <- function(chart) {
    a <- chart$x$series[["1"]]$annotations
    expect_equal(a[[1]]$x, 451)
    expect_equal(a[[2]]$position, 451)
    expect_equal(a[[2]]$x1, 451)
    expect_equal(a[[3]]$position[[1]][[1]], c(101, 60))
    expect_equal(a[[3]]$position[[1]][[2]], c(301, 40))
    expect_equal(chart$x$series[["1"]]$data$start, c(1, 501))
  }

  base <- GC_chart(genes, cluster = "cluster", group = "name")
  # annotations added before or after the range axis end up in the same place
  check(base %>% annotate() %>% GC_scale(axis_type = "range"))
  check(base %>% GC_scale(axis_type = "range") %>% annotate())
  # a second range axis does not move them again
  check(base %>% annotate() %>% GC_scale(axis_type = "range") %>% GC_scale(axis_type = "range"))
  # without a range axis nothing moves
  expect_equal(annotate(base)$x$series[["1"]]$annotations[[1]]$x, 1450)
})

test_that("GC_scale with a range axis moves the line ends with the genes", {
  genes <- data.frame(start = c(1000, 1500), end = c(1400, 1900), cluster = c(1, 1))
  base <- GC_chart(genes, cluster = "cluster")
  before <- base %>% GC_sequence(end = 1800) %>% GC_scale(axis_type = "range")
  after <- base %>% GC_scale(axis_type = "range") %>% GC_sequence(end = 1800)
  expect_equal(before$x$series[["1"]]$sequence$end, 801)
  expect_equal(after$x$series[["1"]]$sequence$end, 801)
  # without a range axis the line is left alone
  expect_equal((base %>% GC_sequence(end = 1800))$x$series[["1"]]$sequence$end, 1800)
})

test_that("GC_genes stores and checks groupStyle", {
  genes <- data.frame(start = c(1, 50), end = c(40, 90), class = c("x", "y"))
  chart <- GC_chart(genes, group = "class") %>%
    GC_genes(groupStyle = list(y = list(marker = "box", fill = "red")))
  expect_equal(chart$x$series[[1]]$genes$groupStyle,
               list(y = list(marker = "box", fill = "red")))
  expect_error(GC_chart(genes, group = "class") %>% GC_genes(groupStyle = list("box")),
               "named list")
  expect_error(GC_chart(genes, group = "class") %>% GC_genes(groupStyle = list(y = "box")),
               "named list")
})

test_that("GC_labels stores groupStyle with the group column", {
  genes <- data.frame(start = c(1, 50), end = c(40, 90), class = c("x", "y"))
  chart <- GC_chart(genes, group = "class") %>%
    GC_labels(groupStyle = list(y = list(fill = "white")))
  expect_equal(chart$x$series[[1]]$labels$groupStyle, list(y = list(fill = "white")))
  # the column the groups are in, set by GC_chart()
  expect_equal(chart$x$series[[1]]$labels$group, "class")
  expect_error(GC_chart(genes, group = "class") %>% GC_labels(groupStyle = list(y = "white")),
               "named list")
})

test_that("scale break padding is a share of the region shown", {
  # the same 2 kb of genes around a gap of 10 kb and of 100 kb
  short <- get_scale_breaks(data.frame(start = c(1, 11001), end = c(1000, 12000)),
                            threshold_percentage = 20, padding = 1)
  long <- get_scale_breaks(data.frame(start = c(1, 101001), end = c(1000, 102000)),
                           threshold_percentage = 20, padding = 1)
  # 1% of the 2 kb shown, on both sides of each break
  expect_equal(short[[1]][["start"]], 1001 + 20)
  expect_equal(long[[1]][["start"]], 1001 + 20)
  expect_equal(short[[1]][["end"]], 11000 - 20)
  expect_equal(long[[1]][["end"]], 101000 - 20)
})

test_that("scale break padding can be given in base pairs", {
  short <- get_scale_breaks(data.frame(start = c(1, 11001), end = c(1000, 12000)),
                            threshold_percentage = 20, padding = "500bp")
  long <- get_scale_breaks(data.frame(start = c(1, 101001), end = c(1000, 102000)),
                           threshold_percentage = 20, padding = "500 bp")
  expect_equal(short[[1]][["start"]], 1001 + 500)
  expect_equal(long[[1]][["end"]], 101000 - 500)
  # a gap too short for the padding gets no break
  expect_length(get_scale_breaks(data.frame(start = c(1, 11001), end = c(1000, 12000)),
                                 threshold_percentage = 20, padding = "6000bp"), 0)
  genes <- data.frame(start = c(1, 11001), end = c(1000, 12000))
  expect_error(GC_chart(genes) %>% GC_scale(scale_breaks = TRUE, scale_break_padding = "500"),
               "base pairs")
})

test_that("scale breaks are an unnamed list, sent to the chart as an array", {
  genes <- data.frame(start = c(1, 11001), end = c(1000, 12000))
  for (padding in list(1, "500bp")) {
    breaks <- get_scale_breaks(genes, threshold_percentage = 20, padding = padding)
    expect_null(names(breaks))
    expect_match(as.character(jsonlite::toJSON(breaks, auto_unbox = TRUE)), "^\\[")
  }
})

test_that("GC_labels and GC_genes match groupStyle against style_by", {
  genes <- data.frame(start = c(1, 50), end = c(40, 90), class = c("x", "y"),
                      name = c("a", "b"))
  chart <- GC_chart(genes, group = "class") %>%
    GC_genes(style_by = "name", groupStyle = list(b = list(marker = "box"))) %>%
    GC_labels(label = "name", style_by = "name", groupStyle = list(b = list(fill = "white")))
  expect_equal(chart$x$series[[1]]$genes$styleBy, "name")
  expect_equal(chart$x$series[[1]]$labels$styleBy, "name")
  # the colours still follow the group
  expect_equal(chart$x$series[[1]]$genes$group, "class")
  expect_null((GC_chart(genes, group = "class") %>% GC_labels("name"))$x$series[[1]]$labels$styleBy)
  expect_error(GC_chart(genes, group = "class") %>% GC_labels("name", style_by = "nope"),
               "single column name")
  expect_error(GC_chart(genes, group = "class") %>% GC_genes(style_by = c("a", "b")),
               "single column name")
})

test_that("each GC_links() call labels its links with its own options (B47)", {
  genes <- data.frame(cluster = rep(c("A", "B"), each = 2), start = c(1, 201, 1, 201),
                      end = c(150, 350, 150, 350), name = rep(c("a", "b"), 2),
                      identity = c(NA, NA, 90, 70))
  labels <- function(chart) {
    file <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(chart, file, selfcontained = FALSE)
    session <- chrome_page(file)
    res <- session$Runtime$evaluate("
      JSON.stringify([...document.querySelectorAll('.link-text')].map(t =>
        ({ text: t.textContent, fill: t.style.fill, id: t.id })))")$result$value
    jsonlite::fromJSON(res)
  }
  base <- GC_chart(genes, cluster = "cluster", group = "name", height = "300px")

  res <- labels(base %>%
    GC_links("name", value1 = "a", value2 = "a", labelStyle = list(fill = "red")) %>%
    GC_links("name", value1 = "b", value2 = "b", labelStyle = list(fill = "blue")))
  expect_equal(res$text, c("90%", "70%"))
  expect_equal(res$fill, c("red", "blue"))
  expect_false(anyDuplicated(res$id) > 0)

  # a gene in two link sets gets one label
  res <- labels(base %>% GC_links("name") %>% GC_links("name"))
  expect_equal(res$text, c("90%", "70%"))
})
