function createSvgContainer(targetElement, customId = null) {
  // Create SVG Container
  var svgContainer = d3.select(targetElement)
    .append("div")
    .attr("id", customId != null ? customId : targetElement + "-svg-container")
    .append("svg")
    .attr("preserveAspectRatio", "xMinYMin meet")
    .attr("viewBox", "0 0 800 400")
    .classed("svg-content", true);

  // Get Container Dimensions
  var width = svgContainer.node().getBoundingClientRect().width;
  var height = svgContainer.node().getBoundingClientRect().height;
  return {svgContainer, width, height};

}

function drawCluster(svgContainer, width, height, data, padding = {}) {

  // Default padding
  padding = {
    left: padding.left !== undefined ? padding.left : 20,
    right: padding.right !== undefined ? padding.right : 20,
    top: padding.top !== undefined ? padding.top : 20,
    bottom: padding.bottom !== undefined ? padding.bottom : 20
  };

  // Data processing
  var maxStart = d3.max(data, d => d.start);
  var maxStop = d3.max(data, d => d.stop);
  var minStart = d3.min(data, d => d.start);
  var minStop = d3.min(data, d => d.stop);

  // Scale Setup
  var xScale = d3.scaleLinear()
    .domain([minStop, maxStop])
    .range([padding.left, width - padding.left]);

  var yScale = d3.scaleLinear()
    .domain([minStart, maxStart])
    .range([height - padding.bottom, padding.top]);

  // Marker Setup
  var marker = svgContainer.append("defs")
    .selectAll("marker")
    .data(data)
    .enter()
    .append("marker")
    .attr("id", d => d.name)
    .attr("viewBox", "0 -5 10 10")
    .attr("refX", 5)
    .attr("refY", 0)
    .attr("markerWidth", 6)
    .attr("markerHeight", 6)
    .attr("orient", "auto")
    .append("path")
    .attr("d", "M0,-5L10,0L0,5")
    .attr("class", "arrowHead")
    .attr("fill", d => d.color);

  // Draw baseline
  var line = svgContainer.append("line")
    .attr("class", "baseline")
    .attr("x1", yScale(maxStop))
    .attr("y1", yScale(0))
    .attr("x2", xScale(maxStart))
    .attr("y2", yScale(0))
    .attr("stroke", "grey")
    .attr("stroke-width", 2);

  // Draw Gene lines
  var Genelines = svgContainer.selectAll("geneLine")
    .data(data)
    .enter()
    .append("line")
    .attr("class", "geneline")
    .attr("x1", d => xScale(d.start))
    .attr("y1", d => yScale(0))
    .attr("x2", d => xScale(d.stop))
    .attr("y2", d => yScale(0))
    .attr("stroke-width", 2)
    .attr("stroke", d => d.color)
    .attr("marker-end", d => "url(#" + d.name + ")");

  // Draw Labels
  var label = svgContainer.selectAll("text.label")
    .data(data)
    .enter()
    .append("text")
    .attr("class", "label")
    .attr("x", d => xScale((d.start + d.stop) / 2))
    .attr("y", d => yScale(0))
    .attr("dy", "-1em")
    .attr("text-anchor", "middle")
    .text(d => d.name)
    .attr("font-size", "1em")
    .attr("font-style", "italic")
    .attr("fill", "black");

  // Adjust viewBox
  var bbox = svgContainer.node().getBBox();
  svgContainer.attr("viewBox", [
    bbox.x - padding.left,
    bbox.y - padding.top,
    bbox.width + padding.left + padding.right,
    bbox.height + padding.top + padding.bottom
  ]);
}

function drawLegend(
    el,
    data,
    {
      padding = {},
      legend = {},
      text = {},
      align = "right",
      orientation = "horizontal",
      backgroundColor = "#FFF"
    } = {}
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
      currentX = padding.left; // Reset X to the left padding
      currentY += legend.size + legend.padding; // Increment Y to move items down
    }
  });
  // Adjust height to fit legend
  var bbox = svgLegend.node().getBBox();
  svgLegend.attr("viewBox", [bbox.x - padding.left, bbox.y - padding.top, parentWidth, bbox.height + padding.top + padding.bottom]);
}
