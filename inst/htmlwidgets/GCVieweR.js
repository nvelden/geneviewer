HTMLWidgets.widget({
  name: 'GCVieweR',
  type: 'output',

  factory: function(el, width, height) {
    var data, addLegend;
    var draw = function(width, height) {
      // Clear out the container if it has anything
      d3.select(el).selectAll('*').remove();

      var groupedData = d3.flatGroup(data, (d) => d.cluster);

      if (addLegend.position == "top") {

        var graph = d3.select(el)
        .append("div")
        .attr("id", "GCvieweR-legend-container")

        var legend = createLegendContainer("#GCvieweR-legend-container")
         .legendData(data, "class")
         .legend();
      }

      var graph = d3.select(el)
      .append("div")
      .attr("id", "GCvieweR-graph-container")

      groupedData.forEach(function(item) {

        var cluster = createClusterContainer("#GCvieweR-graph-container", {width: width, height: height / groupedData.length})
            //.theme("vintage")
            .title("Transcription cluster", "OphA")
            .footer("Description", "OphA")
            .clusterLabel("ophA gene cluster")
            .geneData(item[1])
            .sequence()
            .genes("class")
            .geneLabels("class")
            .adjustLabels("text.label");
      //    .drawCluster(item[1], {}, "class")
      //    .adjustLabels("text.label")
      //    .addTitleLeft("Hello")
      //    .adjustViewBox();

      //  var Title = createTitleContainer("#" + divId)
      //    .addTitle();

      });

    if (addLegend.position == "bottom") {

        var graph = d3.select(el)
        .append("div")
        .attr("id", "GCvieweR-legend-container")

        var legend = createLegendContainer("#GCvieweR-legend-container")
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
