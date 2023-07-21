
function getUniqueId(baseId) {
  var i = 1;
  while (document.getElementById(baseId + "-" + i)) {
    i++;
  }
  return baseId + "-" + i;
}

function SvgContainer(svg) {
  this.svg = svg;
}

function DivContainer(div) {
  this.div = div;
}

function createDivContainer(targetElement) {
  var baseIdDiv = "div-container";

  var div = d3.select(targetElement)
    .attr("id", getUniqueId(baseIdDiv))
    .classed("div-content", true);

  return new DivContainer(div);
}

function createSvgContainer(targetElement) {
  var baseIdSvg = "svg-container";

  var svg = d3.select(targetElement)
    .append("svg")
    .attr("id", getUniqueId(baseIdSvg))
    .attr("preserveAspectRatio", "xMinYMin meet")
    .attr("viewBox", "0 0 800 400")
    .classed("svg-content", true);

  return new SvgContainer(svg);
}

SvgContainer.prototype.drawGeneLabels = function (data, options = {}) {
  const defaultOptions = {
    padding: {
      left: 0,
      right: 0,
      top: 0,
      bottom: 0
    },
    font: {
      size: "1em",
      style: "italic",
      weight: "normal",
      decoration: "none",
      family: "sans-serif",
      color: "black"
    },
    anchor: "middle",
    dy: "-1em",
    rotate: 0  // Default rotation angle
  };

  const {
    padding,
    font,
    anchor,
    dy,
    rotate
  } = { ...defaultOptions, ...options };

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
    .range([padding.left, width - padding.right]);

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
    .attr("dy", dy)
    .attr("text-anchor", anchor)
    .text((d) => d.name)
    .attr("font-size", font.size)
    .attr("font-style", font.style)
    .attr("font-weight", font.weight)
    .attr("text-decoration", font.decoration)
    .attr("font-family", font.family)
    .attr("fill", font.color)
    .attr("transform", (d) => `rotate(${rotate},${xScale((d.start + d.stop) / 2)},${yScale(0)})`);

  return this;
};

SvgContainer.prototype.drawCluster = function (data, options = {}, group = null) {

  // Verify that the group exists in the data if it is defined
  if (group && !data.some(d => group in d)) {
    console.warn(`Group "${group}" does not exist in the data`);
  }

  const defaultOptions = {
    padding: {
      left: 0,
      right: 0,
      top: 0,
      bottom: 0
    },
    cluster: {
      colorScheme: null,
      customColors: null
    }
  };

  const { padding, cluster } = { ...defaultOptions, ...options };

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
    .range([padding.left, width - padding.right]);

  var yScale = d3
    .scaleLinear()
    .domain([minStart, maxStart])
    .range([height - padding.bottom, padding.top]);

  // Color Scale Setup
  let colorScale;
  if (cluster.colorScheme) {
    colorScale = d3.scaleOrdinal(d3[cluster.colorScheme]);
  } else if (cluster.customColors && cluster.customColors.length > 0) {
    colorScale = d3.scaleOrdinal().range(cluster.customColors);
  } else {
    colorScale = d3.scaleOrdinal(d3.schemeCategory10);
  }

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
    .attr("fill", (d) => colorScale(d[group])); // Use grouping variable here

  // Draw baseline
  var line = this.svg
    .append("line")
    .attr("class", "baseline")
    .attr("x1", xScale(minStop))
    .attr("y1", yScale(0))
    .attr("x2", xScale(maxStart))
    .attr("y2", yScale(0))
    .attr("stroke", "grey")
    .attr("stroke-width", 2);

  // Draw Gene lines
  var genelines = this.svg
    .selectAll(".geneline")
    .data(data)
    .enter()
    .append("line")
    .attr("class", "geneline")
    .attr("x1", (d) => xScale(d.start))
    .attr("y1", yScale(0))
    .attr("x2", (d) => xScale(d.stop))
    .attr("y2", yScale(0))
    .attr("stroke-width", 2)
    .attr("marker-end", (d) => "url(#" + d.name + ")")
    .each(function (d) {
      const groupColor = colorScale(d[group]); // Use grouping variable here
      d3.select(this).attr("stroke", groupColor);
    });

  return this;
};

SvgContainer.prototype.adjustViewBox = function (options = {}) {
  const defaultOptions = {
    padding: {
      left: 10,
      right: 10,
      top: 10,
      bottom: 10
    }
  };

  const { padding } = { ...defaultOptions, ...options };

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

SvgContainer.prototype.drawLegend = function(data, options = {}, group = null) {

    if (!group) {
    throw new Error("Group is not defined");
  }

  // Verify that the group exists in the data
  if (!data.some(d => group in d)) {
    throw new Error(`Group "${group}" does not exist in the data`);
  }

  const defaultOptions = {
    padding: {
      left: 0,
      right: 0,
      top: 0,
      bottom: 0
    },
    legend: {
      size: 8,
      padding: 8,
      color: "black",
      stroke: "black",
      strokeWidth: 1,
      colorScheme: null,
      customColors: null
    },
    text: {
      size: 8,
      anchor: "start",
      dy: ".35em",
      fill: "black"
    },
    font: {
      size: "1em",
      style: "normal",
      weight: "normal",
      decoration: "none",
      family: "sans-serif",
      color: "black"
    },
    align: "right",
    orientation: "horizontal",
    backgroundColor: "#FFF"
  };

  const {
    padding,
    legend,
    text,
    font,
    align,
    orientation,
    backgroundColor
  } = { ...defaultOptions, ...options };

  const svgLegend = this.svg;
  const parentWidth = svgLegend.node().getBoundingClientRect().width;

  const classes = Array.from(new Set(data.map((d) => d[group])));

  let colorScale;
  if (legend.colorScheme) {
    colorScale = d3.scaleOrdinal(d3[legend.colorScheme])
      .domain(classes);
  } else if (legend.customColors && legend.customColors.length > 0) {
    colorScale = d3.scaleOrdinal()
      .domain(classes)
      .range(legend.customColors);
  } else {
    colorScale = d3.scaleOrdinal(d3.schemeCategory10)
      .domain(classes);
  }

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
      .attr("dy", text.dy)
      .style("text-anchor", text.anchor)
      .style("font-size", font.size)
      .style("font-style", font.style)
      .style("font-weight", font.weight)
      .style("text-decoration", font.decoration)
      .style("font-family", font.family)
      .style("fill", font.color)
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
      .style("stroke", legend.stroke)
      .style("stroke-width", legend.strokeWidth)
      .style("fill", colorScale(d));

    const textLabel = d3.select(textElement)
      .append("text")
      .attr("x", currentX + legend.size + legend.padding)
      .attr("y", currentY + legend.size / 2)
      .attr("dy", text.dy)
      .style("text-anchor", text.anchor)
      .style("fill", text.fill)
      .style("font-size", font.size)
      .style("font-style", font.style)
      .style("font-weight", font.weight)
      .style("text-decoration", font.decoration)
      .style("font-family", font.family)
      .style("fill", font.color)
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

SvgContainer.prototype.adjustLabels = function(labelSelector) {
    // Select all the labels based on the provided selector
    var labels = this.svg.selectAll(labelSelector).nodes();

    // Iterate over each label
    for (var i = 0; i < labels.length - 1; i++) {
        var label1 = labels[i].getBoundingClientRect();

        // Compare it with all the labels that come after it
        for (var j = i + 1; j < labels.length; j++) {
            var label2 = labels[j].getBoundingClientRect();

            // If the labels overlap
            if (!(label1.right < label2.left ||
                  label1.left > label2.right ||
                  label1.bottom < label2.top ||
                  label1.top > label2.bottom)) {

                // Get the current x and y attributes of the labels
                var x1 = parseFloat(d3.select(labels[i]).attr('x'));
                var y1 = parseFloat(d3.select(labels[i]).attr('y'));
                var x2 = parseFloat(d3.select(labels[j]).attr('x'));
                var y2 = parseFloat(d3.select(labels[j]).attr('y'));

                // Calculate the width of the labels and their center points
                var width1 = labels[i].getBBox().width;
                var centerX1 = x1 + width1 / 2;
                var width2 = labels[j].getBBox().width;
                var centerX2 = x2 + width2 / 2;

                // Rotate both labels
                d3.select(labels[i])
                    .style("text-anchor", "start")
                    .attr("dx", "1.8em")
                    .attr("transform", `rotate(-65, ${centerX1}, ${y1})`);

                d3.select(labels[j])
                    .style("text-anchor", "start")
                    .attr("dx", "1.8em")
                    .attr("transform", `rotate(-65, ${centerX2}, ${y2})`);
            }
        }
    }
    return this;
};


/*
function collide(alpha, labels, padding) {
  var quadtree = d3.quadtree(labels, d => d.x, d => d.y);
  return function(d) {
    var rb = 2*d.radius + padding,
        nx1 = d.x - rb,
        nx2 = d.x + rb,
        ny1 = d.y - rb,
        ny2 = d.y + rb;
    quadtree.visit(function(quad, x1, y1, x2, y2) {
      if (quad.data && (quad.data !== d)) {
        var x = d.x - quad.data.x,
            y = d.y - quad.data.y,
            l = Math.sqrt(x * x + y * y),
            r = d.radius + quad.data.radius;
        if (l < r) {
          l = (l - r) / l * alpha;
          d.x -= x *= l;
          d.y -= y *= l;
          quad.data.x += x;
          quad.data.y += y;
        }
      }
      return x1 > nx2 || x2 < nx1 || y1 > ny2 || y2 < ny1;
    });
  };
}

SvgContainer.prototype.adjustLabels = function(labelSelector, padding = 10) {
  var labels = this.svg.selectAll(labelSelector).nodes().map(function(d) {
    var bbox = d.getBoundingClientRect();
    return {
      x: parseFloat(d.getAttribute('x')),
      y: parseFloat(d.getAttribute('y')),
      radius: bbox.width / 2,
      element: d
    };
  });

  var simulation = d3.forceSimulation(labels)
    .velocityDecay(0.2)
    .force("x", d3.forceX().strength(.0005))
    .force("y", d3.forceY().strength(.0005))
    .force("collide", collide(1, labels, padding)) // Pass the labels and padding to the collide function
    .on("tick", function() {
      labels.forEach(function(d) {
        d.element.setAttribute('x', d.x);
        d.element.setAttribute('y', d.y);
      });
    });

  return this; // Return 'this' for chaining
}
*/

