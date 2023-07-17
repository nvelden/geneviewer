function drawCluster(svg, data, paddingLeft = 20, paddingRight = 20, paddingTop = 20, paddingBottom = 20) {
  var parentWidth = svg.node().getBoundingClientRect().width;
  var parentHeight = svg.node().getBoundingClientRect().height;

  var maxStart = d3.max(data, function (d) {
    return d.start;
  });
  var maxStop = d3.max(data, function (d) {
    return d.stop;
  });

  var xScale = d3
    .scaleLinear()
    .domain([0, maxStop])
    .range([paddingLeft, parentWidth - paddingRight]);

  var yScale = d3
    .scaleLinear()
    .domain([0, maxStart])
    .range([parentHeight - paddingBottom, paddingTop]);

  var mainSvg = svg;

  var marker = mainSvg
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

  var line = mainSvg
    .append("line")
    .attr("class", "baseline")
    .attr("x1", yScale(maxStop))
    .attr("y1", yScale(0))
    .attr("x2", xScale(maxStart))
    .attr("y2", yScale(0))
    .attr("stroke", "grey")
    .attr("stroke-width", 2);

  var Genelines = mainSvg
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

  var label = mainSvg
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

  var bbox = svg.node().getBBox();
  svg.attr("viewBox", [bbox.x - paddingLeft, bbox.y - paddingTop, bbox.width   + paddingLeft + paddingRight, bbox.height + paddingTop + paddingBottom]);

}

function drawLegend(svg, data, paddingLeft = 20, paddingRight = 20, paddingTop = 20, paddingBottom = 20, legendSize = 10, legendPadding = 5, align = "right", orientation = "horizontal") {

  var parentWidth = svg.node().getBoundingClientRect().width;

  // Extract unique classes
  var classes = Array.from(new Set(data.map(function (d) {
    return d.class;
  })));

  // Create the legend
  var legend = svg
    .selectAll(".legend")
    .data(classes)
    .enter()
    .append("g")
    .attr("class", "legend");

  var currentX = paddingLeft, currentY = paddingTop;
  legend.each(function(d, i) {
    var textElement = this;

    var textSample = d3.select(this).append("text") // Create a sample text
      .attr("x", currentX + legendSize + legendPadding)
      .attr("y", currentY + legendSize / 2)
      .attr("dy", ".35em")
      .style("text-anchor", "start")
      .text(d);

    // Compute the length of the text
    var textLength = textSample.node().getComputedTextLength();
    textSample.remove(); // Remove sample text

    // Check if box + text will exceed the parentWidth
    if (currentX + textLength + legendSize + 2 * legendPadding > parentWidth) {
      // Wrap to next line
      currentX = paddingLeft;
      currentY += legendSize + legendPadding;
    }

    var rect = d3.select(this).append("rect")
      .attr("x", currentX)
      .attr("y", currentY)
      .attr("width", legendSize)
      .attr("height", legendSize)
      .style("stroke", "black")
      .style("stroke-width", 1)
      .style("fill", function (d) {
        var match = data.find(function (item) {
          return item.class === d;
        });
        return match.color;
      });

    var text = d3.select(this).append("text") // Draw the text
      .attr("x", currentX + legendSize + legendPadding)
      .attr("y", currentY + legendSize / 2)
      .attr("dy", ".35em")
      .style("text-anchor", "start")
      .text(d);

    // Move to the next item
    if (orientation === "horizontal") {
      currentX += textLength + legendSize + 2 * legendPadding;
    } else {
      currentY += legendSize + legendPadding;
    }
  });

    var bbox = svg.node().getBBox();
  svg.attr("viewBox", [bbox.x - paddingLeft, bbox.y - paddingTop, bbox.width   + paddingLeft + paddingRight, bbox.height + paddingTop + paddingBottom]);
}
