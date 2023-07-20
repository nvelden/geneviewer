HTMLWidgets.widget({

  name: 'GCVieweR',
  type: 'output',

  factory: function(el, width, height) {

    var data, addLegend;

    var draw = function() {

      // clear out the container if it has anything
      d3.select(el).selectAll('*').remove();

      var groupedData = d3.flatGroup(data, (d) => d.cluster);

      if(addLegend.position == "top") {

      var legend = createSvgContainer(el)
           .drawLegend(data, opts = {}, group = "class")
           .adjustViewBox();

      groupedData.forEach(function(item) {
      var cluster = createSvgContainer(el)
          .drawLabels(item[1])
          .drawCluster(item[1], opts = {}, group = "class")
          .adjustViewBox();
      });


      } else {

        //Create gene cluster
      groupedData.forEach(function(item) {
      var cluster = createSvgContainer(el)
          .drawLabels(item[1])
          .drawCluster(item[1], opts = {}, group = "class")
          .adjustViewBox();
      });

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
