HTMLWidgets.widget({

  name: 'GCVieweR',

  type: 'output',

  factory: function(el, width, height) {

    return {

      renderValue: function(input) {
        console.log("Test")
        // clear out the container if it has anything
        d3.select(el).selectAll('*').remove();

        var data = HTMLWidgets.dataframeToD3(input.data);

        //Legend Container
        var svgLegend = d3.select(el)
          .append("div")
          .attr("id", "legend")
          .append("svg")
          .attr("preserveAspectRatio", "xMinYMin meet")
          .style("background-color","red")
          .attr("viewBox", "0 0 800 300")
          .classed("svg-content", true);

        //Graph Container
        var svgCluster = d3.select(el)
          .append("div")
          .attr("id", "graph")
          .append("svg")
          .attr("preserveAspectRatio", "xMinYMin meet")
          .attr("viewBox", "0 0 800 400")
          .classed("svg-content", true);

        drawCluster(svgCluster, data);
        drawLegend(svgLegend, data);
      },

      resize: function(width, height) {
        // TODO: code to re-render the widget with a new size
      }

    };
  }
});
