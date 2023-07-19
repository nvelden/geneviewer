HTMLWidgets.widget({

  name: 'GCVieweR',
  type: 'output',

  factory: function(el, width, height) {

    var data, addLegend;
    var draw = function() {
      // clear out the container if it has anything
      d3.select(el).selectAll('*').remove();

      if(addLegend.position == "top") {

      var legend = createSvgContainer(el)
           .drawLegend(data, options = {})
           .adjustViewBox();

        //Create gene cluster
        var cluster = createSvgContainer(el)
            .drawLabels(data)
            .drawCluster(data)
            .adjustViewBox();

      } else {

        //Create gene cluster
        var cluster = createSvgContainer(el)
            .drawLabels(data)
            .drawCluster(data)
            .adjustViewBox();

        var legend = createSvgContainer(el)
             .drawLegend(data, options = {})
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
