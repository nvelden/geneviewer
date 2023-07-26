HTMLWidgets.widget({
  name: 'GCVieweR',
  type: 'output',

  factory: function(el, width, height) {
    var data, addLegend;

    var draw = function() {
      // Clear out the container if it has anything
      d3.select(el).selectAll('*').remove();

      var groupedData = d3.flatGroup(data, (d) => d.cluster);

   //   if (addLegend.position == "top") {
  //      var legend = createSvgContainer(el)
  //        .drawLegend(data, opts = {}, group = "class")
  //        .adjustViewBox();
  //    }

      groupedData.forEach(function(item) {

        var divId = getUniqueId("div-container");

     //   var clusterContainer = d3.select(el)
    //      .append("div")
    //      .attr("id", divId)
    //      .classed("div-content", true);

        var cluster = createClusterContainer(el)
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

  //    if (addLegend.position != "top") {
  //      var legend = createLegendContainer(el)
  //        .drawLegend(data, opts = {}, group = "class")
  //        .adjustViewBox();
  //    }
    };

    return {
      renderValue: function(input) {
        data = HTMLWidgets.dataframeToD3(input.data);
        addLegend = input.addLegend;
        draw();
      },

      resize: function(width, height) {
        draw();
      }
    };
  }
});
