function drawCluster(el, data, padding = {}) {

  padding = {
    left: padding.left !== undefined ? padding.left : 20,
    right: padding.right !== undefined ? padding.right : 20,
    top: padding.top !== undefined ? padding.top : 20,
    bottom: padding.bottom !== undefined ? padding.bottom : 20
  };

  //Graph Container
  var svgCluster = d3.select(el)
      .append("div")
      .attr("id", "graph")
      .append("svg")
      .attr("preserveAspectRatio", "xMinYMin meet")
      .attr("viewBox", "0 0 800 400")
      .classed("svg-content", true);

  var parentWidth = svgCluster.node().getBoundingClientRect().width;
  var parentHeight = svgCluster.node().getBoundingClientRect().height;

  var maxStart = d3.max(data, function (d) {
    return d.start;
  });
  var maxStop = d3.max(data, function (d) {
    return d.stop;
  });

  var xScale = d3
    .scaleLinear()
    .domain([0, maxStop])
    .range([padding.left, parentWidth - padding.left]);

  var yScale = d3
    .scaleLinear()
    .domain([0, maxStart])
    .range([parentHeight - padding.bottom, padding.top]);



  var marker = svgCluster
    .append("defs")
    .selectAll("marker")
    .data(data)
    .enter()
    .append("marker")
    .attr("id", function (d) {
      return d.name;
    })
    .attr("viewBox", "0 -5 10 10")
    .attr("refX", 5)
    .attr("refY", 0)
    .attr("markerWidth", 6)
    .attr("markerHeight", 6)
    .attr("orient", "auto")
    .append("path")
    .attr("d", "M0,-5L10,0L0,5")
    .attr("class", "arrowHead")
    .attr("fill", function (d) {
      return d.color;
    });

  var line = svgCluster
    .append("line")
    .attr("class", "baseline")
    .attr("x1", yScale(maxStop))
    .attr("y1", yScale(0))
    .attr("x2", xScale(maxStart))
    .attr("y2", yScale(0))
    .attr("stroke", "grey")
    .attr("stroke-width", 2);

  var Genelines = svgCluster
    .selectAll("geneLine")
    .data(data)
    .enter()
    .append("line")
    .attr("class", "geneline")
    .attr("x1", function (d) {
      return xScale(d.start);
    })
    .attr("y1", function (d) {
      return yScale(d.y);
    })
    .attr("x2", function (d) {
      return xScale(d.stop);
    })
    .attr("y2", function (d) {
      return yScale(d.y);
    })
    .attr("stroke-width", 2)
    .attr("stroke", function (d) {
      return d.color;
    })
    .attr("marker-end", function (d) {
      return "url(#" + d.name + ")";
    });

  var label = svgCluster
    .selectAll("text.label")
    .data(data)
    .enter()
    .append("text")
    .attr("class", "label")
    .attr("x", function (d) {
      return xScale((d.start + d.stop) / 2);
    })
    .attr("y", function (d) {
      return yScale(d.y);
    })
    .attr("dy", "-1em")
    .attr("text-anchor", "middle")
    .text(function (d) {
      return d.name;
    })
    .attr("font-size", "1em")
    .attr("font-style", "italic")
    .attr("fill", "black");

  var bbox = svgCluster.node().getBBox();
  svgCluster.attr("viewBox", [bbox.x - padding.left, bbox.y - padding.top, bbox.width   + padding.left + padding.right, bbox.height + padding.top + padding.bottom]);

}

function drawLegend(
    el,
    data,
    padding = {},
    legend = {},
    text = {},
    align = "right",
    orientation = "horizontal",
    backgroundColor = "#FFF"
  ) {

  // Set default padding and legend sizes if not provided
  padding = {
    left: padding.left !== undefined ? padding.left : 20,
    right: padding.right !== undefined ? padding.right : 20,
    top: padding.top !== undefined ? padding.top : 20,
    bottom: padding.bottom !== undefined ? padding.bottom : 20
  };

  legend = {
    size: legend.size !== undefined ? legend.size : 10,
    padding: legend.padding !== undefined ? legend.padding : 5
  };

  text = {
    size: text.size !== undefined ? text.size : 10
  };


  //Legend Container
  var svgLegend = d3.select(el)
      .append("div")
      .attr("id", "legend")
      .append("svg")
      .attr("preserveAspectRatio", "xMinYMin meet")
      .style("background-color", backgroundColor)
      .attr("viewBox", "0 0 800 300")
      .classed("svg-content", true);

  var parentWidth = svgLegend.node().getBoundingClientRect().width;

  // Extract unique classes
  var classes = Array.from(new Set(data.map(function (d) {
    return d.class;
  })));

  // Create the legend
  var legendElements = svgLegend
    .selectAll(".legend")
    .data(classes)
    .enter()
    .append("g")
    .attr("class", "legend");

  var currentX = padding.left, currentY = padding.top;
  legendElements.each(function(d, i) {
    var textElement = this;

    var textSample = d3.select(this).append("text") // Create a sample text
      .attr("x", currentX + legend.size + legend.padding)
      .attr("y", currentY + legend.size / 2)
      .attr("dy", ".35em")
      .style("text-anchor", "start")
      .text(d);

    // Compute the length of the text
    var textLength = textSample.node().getComputedTextLength();
    textSample.remove(); // Remove sample text

    // Check if box + text will exceed the parentWidth
    if (currentX + textLength + legend.size + 2 * legend.padding > parentWidth) {
      // Wrap to next line
      currentX = padding.left;
      currentY += legend.size + legend.padding;
    }

    var rect = d3.select(this).append("rect")
      .attr("x", currentX)
      .attr("y", currentY)
      .attr("width", legend.size)
      .attr("height", legend.size)
      .style("stroke", "black")
      .style("stroke-width", 1)
      .style("fill", function (d) {
        var match = data.find(function (item) {
          return item.class === d;
        });
        return match.color;
      });

    var text = d3.select(this).append("text") // Draw the text
      .attr("x", currentX + legend.size + legend.padding)
      .attr("y", currentY + legend.size / 2)
      .attr("dy", ".35em")
      .style("text-anchor", "start")
      .text(d);

    // Move to the next item
    if (orientation === "horizontal") {
      currentX += textLength + legend.size + 2 * legend.padding;
    } else {
      currentY += legend.size + legend.padding;
    }
  });

  var bbox = svgLegend.node().getBBox();
  svgLegend.attr("viewBox", [bbox.x - padding.left, bbox.y - padding.top, bbox.width + padding.left + padding.right, bbox.height + padding.top + padding.bottom]);
}
