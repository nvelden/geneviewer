HTMLWidgets.widget({
  name: 'GCVieweR',
  type: 'output',

  factory: function(el, width, height) {
    var data, GC_legend, GC_title, GC_genes;
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
       .legendData(GC_legend.data)
       .legend(GC_legend.options);

      }

      var graph = d3.select(el)
      .append("div")
      .attr("id", "GCvieweR-graph-container")
      .classed("GCVieweR-container", true);
      console.log(GC_genes.options)
      groupedData.forEach(function(item) {

        var cluserHeight = Math.floor(el.clientHeight - legendHeight)

        var cluster = createClusterContainer("#GCvieweR-graph-container", {width: width, height: cluserHeight / groupedData.length})
            .theme("vintage")
            .title(GC_title?.options?.title, GC_title?.options?.subtitle, GC_title?.options)
            .footer("Description", "OphA")
            .clusterLabel("ophA gene cluster")
            .geneData(item[1])
            .sequence()
            .genes(colour = GC_genes?.options?.color, GC_genes.options)
            .geneCoordinates()
            .geneLabels("class")
            .scaleBar()
            .adjustGeneLabels("text.label");
      });

      if (GC_legend?.options?.position == "bottom") {

      var graph = d3.select(el)
        .append("div")
        .attr("id", "GCvieweR-legend-container")

      var legend = createLegendContainer("#GCvieweR-legend-container",
      {
        width:  width,
        height: legendHeight,
        backgroundColor: GC_legend.options.backgroundColor,
        margin: GC_legend.options.margin
      })
       .legendData(GC_legend.data)
       .legend(GC_legend.options);

      }

    };

    return {
      renderValue: function(input) {
        data = HTMLWidgets.dataframeToD3(input.data);
        GC_legend = input.GC_legend
        GC_title = input.GC_title
        GC_genes = input.GC_genes
        draw(width, height);
      },

      resize: function(newWidth, newHeight) {
        draw(newWidth, newHeight);
      }
    };
  }
});
