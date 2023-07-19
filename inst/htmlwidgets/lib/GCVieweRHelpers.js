function SvgContainer(svg) {
  this.svg = svg;
}

function createSvgContainer(targetElement, customId = null) {
  // Create SVG Container
  var svgContainer = d3
    .select(targetElement)
    .append("svg")
    .attr("id", customId != null ? customId : targetElement + "-svg-container")
    .attr("preserveAspectRatio", "xMinYMin meet")
    .attr("viewBox", "0 0 800 400")
    .classed("svg-content", true);

  return new SvgContainer(svgContainer);
}

SvgContainer.prototype.drawLabels = function (data, padding = {}) {
  // Default padding
  padding = {
    left: padding.left !== undefined ? padding.left : 20,
    right: padding.right !== undefined ? padding.right : 20,
    top: padding.top !== undefined ? padding.top : 20,
    bottom: padding.bottom !== undefined ? padding.bottom : 20,
  };

  // Data processing
  var maxStart = d3.max(data, (d) => d.start);
  var maxStop = d3.max(data, (d) => d.stop);
  var minStart = d3.min(data, (d) => d.start);
  var minStop = d3.min(data, (d) => d.stop);

  // Get Container Dimensions
  var width = this.svg.node().getBoundingClientRect().width;
  var height = this.svg.node().getBoundingClientRect().height;

  // Scale Setup
  var xScale = d3
    .scaleLinear()
    .domain([minStop, maxStop])
    .range([padding.left, width - padding.left]);

  var yScale = d3
    .scaleLinear()
    .domain([minStart, maxStart])
    .range([height - padding.bottom, padding.top]);

  // Draw Labels
  var label = this.svg
    .selectAll("text.label")
    .data(data)
    .enter()
    .append("text")
    .attr("class", "label")
    .attr("x", (d) => xScale((d.start + d.stop) / 2))
    .attr("y", (d) => yScale(0))
    .attr("dy", "-1em")
    .attr("text-anchor", "middle")
    .text((d) => d.name)
    .attr("font-size", "1em")
    .attr("font-style", "italic")
    .attr("fill", "black");

  return this;
};

SvgContainer.prototype.drawCluster = function (data, padding = {}) {
  // Default padding
  padding = {
    left: padding.left !== undefined ? padding.left : 20,
    right: padding.right !== undefined ? padding.right : 20,
    top: padding.top !== undefined ? padding.top : 20,
    bottom: padding.bottom !== undefined ? padding.bottom : 20,
  };

  // Data processing
  var maxStart = d3.max(data, (d) => d.start);
  var maxStop = d3.max(data, (d) => d.stop);
  var minStart = d3.min(data, (d) => d.start);
  var minStop = d3.min(data, (d) => d.stop);

  // Get Container Dimensions
  var width = this.svg.node().getBoundingClientRect().width;
  var height = this.svg.node().getBoundingClientRect().height;

  // Scale Setup
  var xScale = d3
    .scaleLinear()
    .domain([minStop, maxStop])
    .range([padding.left, width - padding.left]);

  var yScale = d3
    .scaleLinear()
    .domain([minStart, maxStart])
    .range([height - padding.bottom, padding.top]);

  // Marker Setup
  var marker = this.svg
    .append("defs")
    .selectAll("marker")
    .data(data)
    .enter()
    .append("marker")
    .attr("id", (d) => d.name)
    .attr("viewBox", "0 -5 10 10")
    .attr("refX", 5)
    .attr("refY", 0)
    .attr("markerWidth", 6)
    .attr("markerHeight", 6)
    .attr("orient", "auto")
    .append("path")
    .attr("d", "M0,-5L10,0L0,5")
    .attr("class", "arrowHead")
    .attr("fill", (d) => d.color);

  // Draw baseline
  var line = this.svg
    .append("line")
    .attr("class", "baseline")
    .attr("x1", yScale(maxStop))
    .attr("y1", yScale(0))
    .attr("x2", xScale(maxStart))
    .attr("y2", yScale(0))
    .attr("stroke", "grey")
    .attr("stroke-width", 2);

  // Draw Gene lines
  var Genelines = this.svg
    .selectAll("geneLine")
    .data(data)
    .enter()
    .append("line")
    .attr("class", "geneline")
    .attr("x1", (d) => xScale(d.start))
    .attr("y1", (d) => yScale(0))
    .attr("x2", (d) => xScale(d.stop))
    .attr("y2", (d) => yScale(0))
    .attr("stroke-width", 2)
    .attr("stroke", (d) => d.color)
    .attr("marker-end", (d) => "url(#" + d.name + ")");

  // Adjust viewBox
  var bbox = this.svg.node().getBBox();
  this.svg.attr("viewBox", [
    bbox.x - padding.left,
    bbox.y - padding.top,
    bbox.width + padding.left + padding.right,
    bbox.height + padding.top + padding.bottom,
  ]);

  return this;
};

SvgContainer.prototype.adjustViewBox = function (padding = {}) {
  // Default padding
  padding = {
    left: padding.left !== undefined ? padding.left : 20,
    right: padding.right !== undefined ? padding.right : 20,
    top: padding.top !== undefined ? padding.top : 20,
    bottom: padding.bottom !== undefined ? padding.bottom : 20,
  };

  // Get Container Dimensions
  var width = this.svg.node().getBoundingClientRect().width;
  var height = this.svg.node().getBoundingClientRect().height;

  // Adjust viewBox
  var bbox = this.svg.node().getBBox();
  this.svg.attr("viewBox", [
    bbox.x - padding.left,
    bbox.y - padding.top,
    bbox.width + padding.left + padding.right,
    bbox.height + padding.top + padding.bottom,
  ]);

  return this;
};

SvgContainer.prototype.drawLegend = function(data, options = {}) {

  const defaultOptions = {
    padding: {
      left: 20,
      right: 20,
      top: 20,
      bottom: 20
    },
    legend: {
      size: 10,
      padding: 5
    },
    text: {
      size: 10
    },
    align: "right",
    orientation: "horizontal",
    backgroundColor: "#FFF"
  };

  const {
    padding,
    legend,
    text,
    align,
    orientation,
    backgroundColor
  } = { ...defaultOptions, ...options };

  const svgLegend = this.svg;
  const parentWidth = svgLegend.node().getBoundingClientRect().width;

  const classes = Array.from(new Set(data.map((d) => d.class)));

  const legendElements = svgLegend.selectAll(".legend")
    .data(classes)
    .enter()
    .append("g")
    .attr("class", "legend");

  let currentX = padding.left,
      currentY = padding.top;

  legendElements.each((d, i, nodes) => {
    const textElement = nodes[i];
    const textSample = d3.select(textElement)
      .append("text")
      .attr("x", currentX + legend.size + legend.padding)
      .attr("y", currentY + legend.size / 2)
      .attr("dy", ".35em")
      .style("text-anchor", "start")
      .text(d);

    const textLength = textSample.node().getComputedTextLength();
    textSample.remove();

    if (currentX + textLength + legend.size + 2 * legend.padding > parentWidth) {
      currentX = padding.left;
      currentY += legend.size + legend.padding;
    }

    const rect = d3.select(textElement)
      .append("rect")
      .attr("x", currentX)
      .attr("y", currentY)
      .attr("width", legend.size)
      .attr("height", legend.size)
      .style("stroke", "black")
      .style("stroke-width", 1)
      .style("fill", (d) => {
        const match = data.find((item) => item.class === d);
        return match.color;
      });

    const text = d3.select(textElement)
      .append("text")
      .attr("x", currentX + legend.size + legend.padding)
      .attr("y", currentY + legend.size / 2)
      .attr("dy", ".35em")
      .style("text-anchor", "start")
      .text(d);

    if (orientation === "horizontal") {
      currentX += textLength + legend.size + 2 * legend.padding;
    } else {
      currentX = padding.left;
      currentY += legend.size + legend.padding;
    }
  });

  return this;
};









