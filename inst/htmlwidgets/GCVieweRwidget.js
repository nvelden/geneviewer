HTMLWidgets.widget({
  name: 'GCVieweR',
  type: 'output',

  factory: function (el, width, height) {
    var data,
      series,
      titleOptions,
      legendOptions;

    var widgetId = el.id.split('-')[1];

    var draw = function (width, height) {
      // Clear out the container if it has anything
      d3.select(el).selectAll('*').remove();

      // Add Title

      if (titleOptions !== null && titleOptions?.height !== null && titleOptions?.show) {

        var titleContainer = d3.select(el)
          .append("div")
          .attr("id", `GCvieweR-title-container-${widgetId}`)
          .classed("GCVieweR-container", true);

        var titleHeight = computeSize(titleOptions?.height, height)

        var title = createContainer(
          `#GCvieweR-title-container-${widgetId}`,
          "svg-container",
          "titleOptions",
          {
            width: width,
            height: titleHeight,
            backgroundColor: titleOptions?.backgroundColor ?? "#0000",
            margin: titleOptions?.margin
          })
          .title(titleOptions?.title, titleOptions?.subtitle, titleOptions?.show ?? false, titleOptions)

      }

      // Add legend

      var legendHeight = (legendOptions?.show === false) ? 0 : computeSize(legendOptions?.height, height);

      if (legendOptions?.group !== null && legendOptions?.show) {

        var legendContainer = d3.select(el)
          .append("div")
          .attr("id", `GCvieweR-legend-container-${widgetId}`)
          .classed("GCVieweR-container", true);

        var legendContainer = createContainer(`#GCvieweR-legend-container-${widgetId}`,
          "svg-container",
          "titleOptions",
          {
            width: width,
            height: legendHeight,
            backgroundColor: legendOptions?.backgroundColor ?? "#0000",
            margin: legendOptions.margin
          })
          .legendData(data)
          .legend(legendOptions?.group ?? false, legendOptions?.show ?? false, legendOptions);

        var legendElement = d3.select(`#GCvieweR-legend-container-${widgetId}`).node();
        var legendDimensions = legendElement.getBoundingClientRect();
        legendHeight = legendDimensions.height;

      }

      var graph = d3.select(el)
        .append("div")
        .attr("id", `GCvieweR-graph-container-${widgetId}`)
        .classed("GCVieweR-container", true);

      // Add Clusters

      var clusters = Object.keys(series);

      clusters.forEach(function (clusterKey) {

        // Compute margins
        var margin = { top: 0, right: 0, bottom: 0, left: 0 }
        var clusterMargins = series[clusterKey]["grid"].margin
        var clusterHeight = computeSize(series[clusterKey]["grid"].height, el.clientHeight);
        clusterHeight -= titleHeight ? (titleHeight / clusters.length) : 0;
        clusterHeight -= legendHeight ? (legendHeight / clusters.length) : 0;

        var clusterWidth = computeSize(series[clusterKey]["grid"].width, width)

        margin = {
            top: computeSize(clusterMargins?.top ?? 0, height),
            right: computeSize(clusterMargins?.right ?? 0, width),
            bottom: computeSize(clusterMargins?.bottom ?? 0, height),
            left: computeSize(clusterMargins?.left ?? 0, width)
          };

        var cluster = series[clusterKey],
            clusterStyle = cluster.style,
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

        //var clusterHeight = Math.floor(el.clientHeight - legendHeight);
        var clusterOptions = {
          width: clusterWidth,
          height: clusterHeight,
          style: clusterStyle
        };

        if (clusterMargins && Object.keys(clusterMargins).length > 0) {
          clusterOptions.margin = margin;
        }

        var cluster = createContainer(`#GCvieweR-graph-container-${widgetId}`, "svg-container", 'clusterOptions',  clusterOptions)
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
          "titleOptions",
          {
            width: width,
            height: legendHeight,
            backgroundColor: legendOptions?.backgroundColor ?? "#0000",
            margin: legendOptions.margin
          })
          .legendData(data)
          .legend(legendOptions?.group ?? false, legendOptions?.show ?? false, legendOptions);


      }

      // Bottom Title
      if (titleOptions?.position == "bottom" && titleOptions !== null && titleOptions?.height !== null && titleOptions?.show) {

        d3.select(`#GCvieweR-title-container-${widgetId}`).remove();

        var titleContainer = d3.select(el)
          .append("div")
          .attr("id", `GCvieweR-title-container-${widgetId}`)
          .classed("GCVieweR-container", true);

        var title = createContainer(
          `#GCvieweR-title-container-${widgetId}`,
          "svg-container",
          "titleOptions",
          {
            width: width,
            height: titleHeight,
            backgroundColor: titleOptions?.backgroundColor ?? "#0000",
            margin: titleOptions?.margin
          })
          .title(titleOptions?.title, titleOptions?.subtitle, titleOptions?.show ?? false, titleOptions)

      }

    };

    return {
      renderValue: function (input) {
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
