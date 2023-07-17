HTMLWidgets.widget({

  name: 'GCVieweR',
  type: 'output',

  factory: function(el, width, height) {

    var data, addLegend;


    var draw = function() {
      // clear out the container if it has anything
      d3.select(el).selectAll('*').remove();

      drawCluster(el, data);
      drawLegend(el, data, addLegend);
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
