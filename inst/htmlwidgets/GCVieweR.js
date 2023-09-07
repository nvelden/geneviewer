HTMLWidgets.widget({
  name: 'GCVieweR',
  type: 'output',

  factory: function(el, width, height) {
    var data,
        series,
        legendOptions;

    var draw = function(width, height, backgroundColor) {
      // Clear out the container if it has anything
      d3.select(el).selectAll('*').remove();

      var legendHeight = (legendOptions?.show === false) ? 0 : computeSize(legendOptions?.height, height);

      if (legendOptions?.group !== null && legendOptions?.show) {

      var legendContainer = d3.select(el)
        .append("div")
        .attr("id", "GCvieweR-legend-container")
        .classed("GCVieweR-container", true);

      var legendContainer = createLegendContainer("#GCvieweR-legend-container",
      {
        width:  width,
        height: legendHeight,
        backgroundColor: legendOptions.backgroundColor,
        margin: legendOptions.margin
      })
       .legendData(data)
       .legend(legendOptions?.group ?? false, legendOptions?.show ?? false, legendOptions);

       var legendElement = d3.select("#GCvieweR-legend-container").node();
       var legendDimensions = legendElement.getBoundingClientRect();
       legendHeight = legendDimensions.height;

      }

      var graph = d3.select(el)
      .append("div")
      .attr("id", "GCvieweR-graph-container")
      .classed("GCVieweR-container", true);

var clusters = Object.keys(series);

clusters.forEach(function(clusterKey) {

      // Compute margins
      var margin = { top: 0, right: 0, bottom: 0, left: 0 }
      var clusterMargins = series[clusterKey]["grid"].margin
      var clusterHeight = computeSize(el.clientHeight - legendHeight,series[clusterKey]["grid"].height) / clusters.length
      var clusterWidth = computeSize(series[clusterKey]["grid"].width, width)

      if (Object.keys(clusterMargins).length > 0) {
          margin = {
              top: computeSize(clusterMargins.top || 0, height),
              right: computeSize(clusterMargins.right || 0, width),
              bottom: computeSize(clusterMargins.bottom || 0, height),
              left: computeSize(clusterMargins.left || 0, width)
          };
         }

    var cluster = series[clusterKey],
        clusterData = HTMLWidgets.dataframeToD3(series[clusterKey].data),
        scaleOptions = cluster.scale,
        titleOptions = cluster.title,
        footerOptions = cluster.footer,
        clusterLabelOptions = cluster.clusterLabel,
        labelOptions = cluster.labels,
        sequenceOptions = cluster.sequence,
        geneOptions = cluster.genes,
        markerOptions = cluster.markers
        coordinateOptions = cluster.coordinates;
        scaleBarOptions = cluster.scaleBar;
        tooltipOptions = cluster.tooltip;

    //var clusterHeight = Math.floor(el.clientHeight - legendHeight);
    var clusterOptions = {
        width: clusterWidth,
        height: clusterHeight
    };

    if (Object.keys(clusterMargins).length > 0) {
        clusterOptions.margin = margin;
    }


    var cluster = createClusterContainer("#GCvieweR-graph-container", clusterOptions)
        .theme("preset")
        .title(titleOptions?.title, titleOptions?.subtitle, titleOptions?.show ?? false, titleOptions)
        .footer(footerOptions?.title, footerOptions?.subtitle, footerOptions?.show ?? false, footerOptions)
        .clusterLabel(clusterLabelOptions?.title, clusterLabelOptions?.show ?? false, clusterLabelOptions)
        .geneData(clusterData)  // Access data using the cluster key
        .scale(scaleOptions)
        .sequence(sequenceOptions?.show ?? false, sequenceOptions)
        .genes(geneOptions?.group, geneOptions?.show ?? false, geneOptions)
        .markers(markerOptions?.group, markerOptions?.show ?? false, markerOptions)
        .coordinates(coordinateOptions?.show ?? false, coordinateOptions)
        .labels(labelOptions?.label, labelOptions?.show ?? false, labelOptions)
        .scaleBar(scaleBarOptions?.show ?? false, scaleBarOptions)
        .tooltip(tooltipOptions?.show ?? false, tooltipOptions);
});

      if (legendOptions?.position == "bottom" && legendOptions?.show && legendOptions?.group !== null) {

      d3.select("#GCvieweR-legend-container").remove();

      var legendContainer = d3.select(el)
        .append("div")
        .attr("id", "GCvieweR-legend-container")
        .classed("GCVieweR-container", true);

      var legendContainer = createLegendContainer("#GCvieweR-legend-container",
      {
        width:  width,
        height: legendHeight,
        backgroundColor: legendOptions.backgroundColor,
        margin: legendOptions.margin
      })
       .legendData(data)
       .legend(legendOptions?.group ?? false, legendOptions?.show ?? false, legendOptions?.options);

      }

    };

    return {
      renderValue: function(input) {
        data = HTMLWidgets.dataframeToD3(input.data);
        series = input.series;
        legendOptions = input.legend;
        draw(width, height);
      },
      resize: function(width, height) {
        draw(width, height);
      }
    };
  }
});
