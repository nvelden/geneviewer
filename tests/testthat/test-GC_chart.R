
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
  expect_equal(updated_chart$x$legend$backgroundColor, "#0000")
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
    expect_true(cluster_options$subset_data)
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
