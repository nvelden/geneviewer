HTMLWidgets.widget({

  name: 'GCVieweR',
  type: 'output',

  factory: function(el, width, height) {

    var data, addLegend;
    var draw = function() {
      // clear out the container if it has anything
      d3.select(el).selectAll('*').remove();

      if(addLegend.position == "top") {

        drawLegend(el, data, addLegend);
        var { svgContainer, width, height } = createSvgContainer(el)
        drawCluster(svgContainer, width, height, data);

      } else {
        var { svgContainer, width, height } = createSvgContainer(el)
        drawCluster(svgContainer, width, height, data);

        drawLegend(el, data, addLegend);
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
