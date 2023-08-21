HTMLWidgets.widget({
  name: 'GCVieweR',
  type: 'output',

  factory: function(el, width, height) {
    var data,
        series,
        gridOptions,
        legendOptions;
    var draw = function(width, height, backgroundColor) {
      // Clear out the container if it has anything
      d3.select(el).selectAll('*').remove();

      // Compute margins
      var margins = { top: 0, right: 0, bottom: 0, left: 0 }
      if (Object.keys(gridOptions).length > 0) {
          margins = {
              top: computeSize(gridOptions.top || 0, height),
              right: computeSize(gridOptions.right || 0, width),
              bottom: computeSize(gridOptions.bottom || 0, height),
              left: computeSize(gridOptions.left || 0, width)
          };
         }

      var legendHeight = (legendOptions?.show === false) ? 0 : computeSize(legendOptions?.height, height);
      //var groupedData = d3.flatGroup(data, (d) => d.cluster);

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

    var cluster = series[clusterKey],
        clusterData = HTMLWidgets.dataframeToD3(series[clusterKey].data),
        titleOptions = cluster.title,
        footerOptions = cluster.footer,
        clusterLabelOptions = cluster.clusterLabel,
        labelOptions = cluster.labels,
        sequenceOptions = cluster.sequence,
        geneOptions = cluster.genes,
        coordinateOptions = clusterData.coordinates;
        scaleBarOptions = cluster.scaleBar;

    var clusterHeight = Math.floor(el.clientHeight - legendHeight);
    var clusterOptions = {
        width: width,
        height: clusterHeight / clusters.length
    };

    if (Object.keys(gridOptions).length > 0) {
        clusterOptions.margin = margins;
    }

    var cluster = createClusterContainer("#GCvieweR-graph-container", clusterOptions)
        .theme("preset")
        .title(titleOptions?.title, titleOptions?.subtitle, titleOptions?.show ?? false, titleOptions)
        .footer(footerOptions?.title, footerOptions?.subtitle, footerOptions?.show ?? false, footerOptions)
        .clusterLabel(clusterLabelOptions?.title, clusterLabelOptions?.show ?? false, clusterLabelOptions)
        .geneData(clusterData)  // Access data using the cluster key
        .sequence(sequenceOptions?.show ?? false, sequenceOptions)
        .genes(geneOptions?.group, geneOptions?.show ?? false, geneOptions)
        .coordinates(coordinateOptions?.show ?? false, coordinateOptions)
        .labels(labelOptions?.label, labelOptions)
        .scaleBar(scaleBarOptions?.show ?? false, scaleBarOptions);
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
        gridOptions = input.grid;
        legendOptions = input.legend;
        draw(width, height);
      },
      resize: function(width, height) {
        draw(width, height);
      }
    };
  }
});
