HTMLWidgets.widget({
  name: 'GCVieweR',
  type: 'output',

  factory: function (el, width, height) {
    var
      graphContainer,
      style,
      data,
      series,
      titleOptions,
      legendOptions;

    var widgetId = el.id.split('-')[1];

    var draw = function (width, height) {
      // Clear out the container if it has anything
      d3.select(el).selectAll('*').remove();

      // Apply styles
      if (style && typeof style === 'object' && Object.keys(style).length > 0) {
          // Apply styles from the style object
          for (var key in style) {
              if (style.hasOwnProperty(key)) {
                  el.style[key] = style[key];
              }
          }
      }

      // Add Title

      if (titleOptions !== null && titleOptions?.height !== null && titleOptions?.show) {

        var titleContainer = d3.select(el)
          .append("div")
          .attr("id", `GCvieweR-title-container-${widgetId}`)
          .classed("GCVieweR-container", true);


        titleOptions.width = el.clientWidth
        titleOptions.height = computeSize(titleOptions.height, el.clientHeight)

        var title = createContainer(
          `#GCvieweR-title-container-${widgetId}`,
          "svg-container",
          "titleOptions",
          titleOptions)
          .title(titleOptions?.title, titleOptions?.subtitle, titleOptions?.show ?? false, titleOptions)
      }

      // Add legend

      var legendHeight = (legendOptions?.show === false) ? 0 : computeSize(legendOptions?.height, el.clientHeight);

      if (legendOptions?.group !== null && legendOptions?.show) {

        var legendContainer = d3.select(el)
          .append("div")
          .attr("id", `GCvieweR-legend-container-${widgetId}`)
          .classed("GCVieweR-container", true);

        legendOptions.width = width
        legendOptions.height = computeSize(legendOptions.height, el.clientHeight)

        var legendContainer = createContainer(`#GCvieweR-legend-container-${widgetId}`,
          "svg-container",
          "legendOptions",
          legendOptions)
          .legendData(data)
          .legend(legendOptions?.group ?? false, legendOptions?.show ?? false, el.id, legendOptions);

        var legendElement = d3.select(`#GCvieweR-legend-container-${widgetId}`).node();
        var legendDimensions = legendElement.getBoundingClientRect();
        legendHeight = legendDimensions.height;

      }

      var graph = d3.select(el)
        .append("div")
        .attr("id", `GCvieweR-graph-container-${widgetId}`)
        .style("flex-direction", graphContainer["direction"])
        .classed("GCVieweR-container", true);

      // Add Clusters
      var clusters = Object.keys(series);

      clusters.forEach(function (clusterKey) {

        var cluster = series[clusterKey],
            clusterOptions = cluster.options,
            clusterData = HTMLWidgets.dataframeToD3(series[clusterKey].data),
            scaleOptions = cluster.scale,
            clusterTitleOptions = cluster.clusterTitle,
            footerOptions = cluster.footer,
            clusterLabelOptions = cluster.clusterLabel,
            labelOptions = cluster.labels,
            sequenceOptions = cluster.sequence,
            geneOptions = cluster.genes,
            coordinateOptions = cluster.coordinates;
            scaleBarOptions = cluster.scaleBar;
            tooltipOptions = cluster.tooltip;



        var clonedClusterOptions = JSON.parse(JSON.stringify(clusterOptions));
        clonedClusterOptions.height = computeSize(clonedClusterOptions.height, el.clientHeight);
        clonedClusterOptions.height -= titleOptions.height ? (titleOptions.height / clusters.length) : 0;
        clonedClusterOptions.height -= legendHeight ? (legendHeight / clusters.length) : 0;
        clonedClusterOptions.width = computeSize(clonedClusterOptions.width, el.clientWidth);

        var cluster = createContainer(`#GCvieweR-graph-container-${widgetId}`, "svg-container", 'clusterOptions',  clonedClusterOptions)
          .theme("preset")
          .title(clusterTitleOptions?.title, clusterTitleOptions?.subtitle, clusterTitleOptions?.show ?? false, clusterTitleOptions)
          .footer(footerOptions?.title, footerOptions?.subtitle, footerOptions?.show ?? false, footerOptions)
          .clusterLabel(clusterLabelOptions?.title, clusterLabelOptions?.show ?? false, clusterLabelOptions)
          .geneData(data, clusterData)  // Access data using the cluster key
          .scale(scaleOptions)
          .sequence(sequenceOptions?.show ?? false, sequenceOptions)
          .genes(geneOptions?.group, geneOptions?.show ?? false, geneOptions)
          .coordinates(coordinateOptions?.show ?? false, coordinateOptions)
          .labels(labelOptions?.label, labelOptions?.show ?? false, labelOptions)
          .scaleBar(scaleBarOptions?.show ?? false, scaleBarOptions)
          .tooltip(tooltipOptions?.show ?? false, tooltipOptions);
      });

      // Bottom Legend
      if (legendOptions?.position == "bottom" && legendOptions?.show && legendOptions?.group !== null) {

        d3.select(`#GCvieweR-legend-container-${widgetId}`).remove();

        var legendContainer = d3.select(el)
          .append("div")
          .attr("id", `GCvieweR-legend-container-${widgetId}`)
          .classed("GCVieweR-container", true);

        var legendContainer = createContainer(`#GCvieweR-legend-container-${widgetId}`,
          "svg-container",
          "legendOptions",
          legendOptions)
          .legendData(data)
          .legend(legendOptions?.group ?? false, legendOptions?.show ?? false, el.id, legendOptions);

      }

    };

    return {
      renderValue: function (input) {
        graphContainer = input.graphContainer;
        style = input.style;
        data = HTMLWidgets.dataframeToD3(input.data);
        series = input.series;
        titleOptions = input.title;
        legendOptions = input.legend;
        draw(width, height);
      },
      resize: function (width, height) {
        draw(width, height);
      }
    };
  }
});
