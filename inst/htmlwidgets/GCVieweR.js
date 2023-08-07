HTMLWidgets.widget({
  name: 'GCVieweR',
  type: 'output',

  factory: function(el, width, height) {
    var data, addLegend;
    var draw = function(width, height) {
      // Clear out the container if it has anything
      d3.select(el).selectAll('*').remove();

      var groupedData = d3.flatGroup(data, (d) => d.cluster);

      var legendHeight = 100;

      if (addLegend.position == "top") {

      var legendContainer = d3.select(el)
        .append("div")
        .attr("id", "GCvieweR-legend-container")
        .classed("GCVieweR-container", true);

      var legend = createLegendContainer("#GCvieweR-legend-container", {width: width, height: legendHeight})
       .legendData(data, "class")
       .legend();

      }

      var graph = d3.select(el)
      .append("div")
      .attr("id", "GCvieweR-graph-container")
      .classed("GCVieweR-container", true);

      groupedData.forEach(function(item) {

        var cluserHeight = Math.floor(el.clientHeight - legendHeight)

        var cluster = createClusterContainer("#GCvieweR-graph-container", {width: width, height: cluserHeight / groupedData.length})
            //.theme("vintage")
            .title("Transcription cluster", "OphA")
            .footer("Description", "OphA")
            .clusterLabel("ophA gene cluster")
            .geneData(item[1])
            .sequence()
            .genes("class")
            .geneCoordinates()
            .geneLabels("class")
            .scaleBar()
            .adjustGeneLabels("text.label");
      });

      if (addLegend.position == "bottom") {

      var graph = d3.select(el)
        .append("div")
        .attr("id", "GCvieweR-legend-container")

      var legend = createLegendContainer("#GCvieweR-legend-container", {width: width, height: legendHeight})
         .legendData(data, "class")
         .legend();

      }

    };

    return {
      renderValue: function(input) {
        data = HTMLWidgets.dataframeToD3(input.data);
        addLegend = input.addLegend
        draw(width, height);
      },

      resize: function(newWidth, newHeight) {
        draw(newWidth, newHeight);
      }
    };
  }
});
