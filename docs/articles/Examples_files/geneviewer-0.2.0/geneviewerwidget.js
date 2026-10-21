HTMLWidgets.widget({
  name: 'geneviewer',
  type: 'output',

  factory: function (el, width, height) {
    var
      graphContainer,
      style,
      data,
      links,
      series,
      titleOptions,
      legendOptions,
      saveButton;

    // Unique per chart on the page; used in container and save button ids
    var widgetId = el.id.replace(/[^A-Za-z0-9_-]/g, '_');

    var draw = function (width, height) {

      // Clear out the container if it has anything
      d3.select(el).selectAll('*').remove();

      el.style["height"] = "100%"
      el.style["position"] = "relative"
      // Keep padding (GC_grid top/bottom margins) inside the chart height
      el.style["boxSizing"] = "border-box"

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
      // Sizes are computed into copies of the options on every draw, so a "%"
      // height from R is used again on the next draw (resize)

      var titleHeight = 0;

      if (titleOptions !== null && titleOptions?.height !== null && titleOptions?.show) {

        var titleContainer = d3.select(el)
          .append("div")
          .attr("id", `geneviewer-title-container-${widgetId}`)
          .classed("geneviewer-container", true);


        titleHeight = computeSize(titleOptions.height, el.clientHeight);
        var titleOpts = { ...titleOptions, width: el.clientWidth, height: titleHeight };

        var title = createContainer(
          `#geneviewer-title-container-${widgetId}`,
          "svg-container",
          "titleOptions",
          titleOpts)
          .title(titleOpts.title, titleOpts.subtitle, titleOpts.show ?? false, titleOpts)
      }

      // Add legend

      var legendHeight = (legendOptions?.show === false) ? 0 : computeSize(legendOptions?.height, el.clientHeight);

      if (legendOptions?.group !== null && legendOptions?.show) {

        d3.select(el)
          .append("div")
          .attr("id", `geneviewer-legend-container-${widgetId}`)
          .classed("geneviewer-container", true);

        var legendOpts = { ...legendOptions, width: width,
                           height: computeSize(legendOptions.height, el.clientHeight) };

        createContainer(`#geneviewer-legend-container-${widgetId}`,
          "svg-container",
          "legendOptions",
          legendOpts)
          .legendData(data)
          .legend(legendOpts.group ?? false, legendOpts.show ?? false, el.id, legendOpts);

        var legendElement = d3.select(`#geneviewer-legend-container-${widgetId}`).node();
        var legendDimensions = legendElement.getBoundingClientRect();
        legendHeight = legendDimensions.height;

      }

      d3.select(el)
        .append("div")
        .attr("id", `geneviewer-graph-container-${widgetId}`)
        .style("flex-direction", graphContainer["direction"])
        .style("position", "relative")
        .classed("geneviewer-container", true);

      // Add Clusters
      var clusters = Object.keys(series);

      // Add Links
      if(links && links.length > 0){
      var graphLinks = links.reduce((acc, entry) => {
         const convertedData = HTMLWidgets.dataframeToD3(entry.data);
        acc = acc.concat(convertedData);
        return acc;
      }, []);
      }

      clusters.forEach(function (clusterKey) {

        var cluster = series[clusterKey],
            containerOptions = cluster.container,
            clusterOptions = cluster.cluster,
            clusterData = HTMLWidgets.dataframeToD3(series[clusterKey].data),
            scaleOptions = cluster.scale,
            clusterTitleOptions = cluster.clusterTitle,
            footerOptions = cluster.footer,
            clusterLabelOptions = cluster.clusterLabel,
            labelOptions = cluster.labels,
            sequenceOptions = cluster.sequence,
            geneOptions = cluster.genes,
            transcriptOptions = cluster.transcript,
            coordinateOptions = cluster.coordinates,
            scaleBarOptions = cluster.scaleBar,
            annotationOptions = cluster.annotations,
            trackMouse = cluster.trackMouse,
            tooltipOptions = cluster.tooltip;

        var clonedContainerOptions = JSON.parse(JSON.stringify(containerOptions));
        clonedContainerOptions.height = computeSize(clonedContainerOptions.height, el.clientHeight);
        clonedContainerOptions.height -= titleHeight / clusters.length;
        clonedContainerOptions.height -= legendHeight ? (legendHeight / clusters.length) : 0;
        // Percentage heights: take the top and bottom margins (padding) from the clusters
        if (typeof containerOptions.height === "string" && containerOptions.height.endsWith("%")) {
          var padding = (parseFloat(style?.paddingTop) || 0) + (parseFloat(style?.paddingBottom) || 0);
          clonedContainerOptions.height -= padding / clusters.length;
        }
        clonedContainerOptions.width = computeSize(clonedContainerOptions.width, el.clientWidth);

        var clusterLinks = getClusterLinks(graphLinks, clusterKey);
        var linksOptions = {};

        if (Array.isArray(links) && links.length > 0 && links[0].options) {
        linksOptions = links[0].options;
        }

        createContainer(`#geneviewer-graph-container-${widgetId}`, "svg-container", 'containerOptions',  clonedContainerOptions)
          .cluster(clusterOptions)
          .theme("preset")
          .title(clusterTitleOptions?.title, clusterTitleOptions?.subtitle, clusterTitleOptions?.show ?? false, clusterTitleOptions)
          .footer(footerOptions?.title, footerOptions?.subtitle, footerOptions?.show ?? false, footerOptions)
          .clusterLabel(clusterLabelOptions?.title, clusterLabelOptions?.show ?? false, clusterLabelOptions)
          .geneData(data, clusterData)
          .scale(scaleOptions)
          .sequence(sequenceOptions?.show ?? false, sequenceOptions)
          .genes(geneOptions?.group, geneOptions?.show ?? false, geneOptions)
          .transcript(transcriptOptions?.group, transcriptOptions?.show ?? false, transcriptOptions)
          .links(clusterLinks, clusterKey,  linksOptions)
          .coordinates(coordinateOptions?.show ?? false, coordinateOptions)
          .labels(labelOptions?.label, labelOptions?.show ?? false, labelOptions)
          .scaleBar(scaleBarOptions?.show ?? false, scaleBarOptions)
          .addAnnotations(annotationOptions)
          .trackMouse(trackMouse?.show ?? false)
          .tooltip(tooltipOptions?.show ?? false, tooltipOptions);

      });

      // Bottom legend: drawn above to measure its height, then moved below the clusters
      if (legendOptions?.position == "bottom" && legendOptions?.show && legendOptions?.group !== null) {
        el.appendChild(legendElement);
      }

      // Save icon
      createSaveIcon(widgetId, el, saveButton);

    };

    var addLinks = function () {
      if (!links || links.length === 0) {
        return;
      }
      const graphContainer = d3.select(`#geneviewer-graph-container-${widgetId}`);
      makeLinks(graphContainer, links);
      makeColorBar(graphContainer, links);
    };

    return {
      renderValue: function (input) {
        graphContainer = input.graphContainer;
        style = input.style;
        data = HTMLWidgets.dataframeToD3(input.data);
        links = input.links;
        series = input.series;
        saveButton = input.saveButton;
        titleOptions = input.title;
        legendOptions = input.legend;
        draw(width, height);
        addLinks();
      },
      resize: function (width, height) {
        draw(width, height);
        addLinks();
      }
    };
  }
});
