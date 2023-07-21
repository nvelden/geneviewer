HTMLWidgets.widget({
  name: 'GCVieweR',
  type: 'output',

  factory: function(el, width, height) {
    var data, addLegend;

    var draw = function() {
      // Clear out the container if it has anything
      d3.select(el).selectAll('*').remove();

      var groupedData = d3.flatGroup(data, (d) => d.cluster);

      if (addLegend.position == "top") {
        var legend = createSvgContainer(el)
          .drawLegend(data, opts = {}, group = "class")
          .adjustViewBox();
      }

      groupedData.forEach(function(item) {
        var divId = getUniqueId("div-container");

        var clusterContainer = d3.select(el)
          .append("div")
          .attr("id", divId)
          .classed("div-content", true);

        var cluster = createSvgContainer("#" + divId)
          .drawGeneLabels(item[1])
          .drawCluster(item[1], {}, "class")
          .adjustLabels("text.label")
          .adjustViewBox();

      });

      if (addLegend.position != "top") {
        var legend = createSvgContainer(el)
          .drawLegend(data, opts = {}, group = "class")
          .adjustViewBox();
      }
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
