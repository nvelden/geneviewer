HTMLWidgets.widget({
  name: 'GCVieweR',
  type: 'output',

  factory: function(el, width, height) {
    var data,
        GC_legend,
        GC_title,
        GC_genes,
        GC_labels,
        GC_coordinates,
        GC_scaleBar,
        GC_footer,
        GC_clusterLabel;
    var draw = function(width, height, backgroundColor) {
      // Clear out the container if it has anything
      d3.select(el).selectAll('*').remove();

      var legendHeight = calculateLegendHeight(GC_legend?.options?.height, height)
      var groupedData = d3.flatGroup(data, (d) => d.cluster);

      if (GC_legend?.options?.position == "top") {

      var legendContainer = d3.select(el)
        .append("div")
        .attr("id", "GCvieweR-legend-container")
        .classed("GCVieweR-container", true);

      var legend = createLegendContainer("#GCvieweR-legend-container",
      {
        width:  width,
        height: legendHeight,
        backgroundColor: GC_legend.options.backgroundColor,
        margin: GC_legend.options.margin
      })
       .legendData(data)
       .legend(GC_legend?.options?.color, GC_legend?.options);

      }

      var graph = d3.select(el)
      .append("div")
      .attr("id", "GCvieweR-graph-container")
      .classed("GCVieweR-container", true);
      console.log(GC_clusterLabel)
      groupedData.forEach(function(item) {

        var cluserHeight = Math.floor(el.clientHeight - legendHeight)

        var cluster = createClusterContainer("#GCvieweR-graph-container", {width: width, height: cluserHeight / groupedData.length})
            .theme("vintage")
            .title(GC_title?.options?.title, GC_title?.options?.subtitle, GC_title?.options)
            .footer(GC_footer?.options?.title, GC_footer?.options?.subtitle, GC_footer?.options)
            .clusterLabel(GC_clusterLabel?.options?.title, GC_clusterLabel?.options)
            .geneData(item[1])
            .sequence()
            .genes(color = GC_genes?.options?.color, GC_genes.options)
            .coordinates(GC_coordinates?.options?.coordinates ?? false, GC_coordinates?.options)
            .labels(GC_labels?.options?.label, GC_labels.options)
            .scaleBar(GC_scaleBar?.options?.scaleBar ?? false, GC_scaleBar?.options);
      });

      if (GC_legend?.options?.position == "bottom") {

      var legendContainer = d3.select(el)
        .append("div")
        .attr("id", "GCvieweR-legend-container")
        .classed("GCVieweR-container", true);
      var legend = createLegendContainer("#GCvieweR-legend-container",
      {
        width:  width,
        height: legendHeight,
        backgroundColor: GC_legend.options.backgroundColor,
        margin: GC_legend.options.margin
      })
       .legendData(data)
       .legend(GC_legend?.options?.color, GC_legend?.options);

      }

    };

    return {
      renderValue: function(input) {
        data = HTMLWidgets.dataframeToD3(input.data);
        GC_legend = input.GC_legend
        GC_title = input.GC_title
        GC_genes = input.GC_genes
        GC_labels = input.GC_labels
        GC_coordinates = input.GC_coordinates
        GC_scaleBar = input.GC_scaleBar
        GC_footer = input.GC_footer
        GC_clusterLabel = input.GC_clusterLabel
        draw(width, height);
      },
      resize: function(newWidth, newHeight) {
        draw(newWidth, newHeight);
      }
    };
  }
});
