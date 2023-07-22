


function SvgContainer(svg) {
  this.svg = svg;
}

function DivContainer(div) {
  this.div = div;
}

//utils
function getUniqueId(baseId) {
  var i = 1;
  while (document.getElementById(baseId + "-" + i)) {
    i++;
  }
  return baseId + "-" + i;
}

// title

function titleContainer(svg) {
  this.svg = svg;
}

function createTitleContainer(targetElement, options = {}) {
  var baseIdSvg = "title-container";

  // Default options
  const defaultOptions = {
    attributes: {
      fill: "black", // Fill color
      backgroundColor: "transparent", // Background color
      border: "0px solid #000" // SVG border
    }
  };

  // Combine default and passed options
  const { attributes } = { ...defaultOptions, ...options };

  var svg = d3.select(targetElement)
    .append("svg")
    .attr("id", getUniqueId(baseIdSvg))
    .attr("width", "100%")
    .attr("fill", attributes.fill) // set fill color
    .style("border", attributes.border) // set border
    .style("background-color", attributes.backgroundColor) // set background color
    .classed("svg-content legend", true);

  return new titleContainer(svg);
}

titleContainer.prototype.addTitle = function(options = {}) {
  const defaultOptions = {
    text: 'Main Title',
    textAlign: 'center', // left, center, right
    padding: {
      left: 5,
      right: 5,
      top: 5,
      bottom: 5
    },
    textStyle: {
      color: '#333',
      fontStyle: 'normal',
      fontWeight: 'normal',
      fontFamily: 'sans-serif',
      fontSize: 18
    },
    backgroundColor: 'transparent',
    borderColor: '#ccc',
    borderWidth: 0,
  };

  const { padding, textAlign, text, textStyle, backgroundColor, borderColor, borderWidth } = { ...defaultOptions, ...options };

  // Calculate x and y coordinates based on alignment options
  let x, textAnchor;
  switch (textAlign) {
    case 'left':
      x = 0;
      textAnchor = 'start';
      break;
    case 'center':
      x = this.svg.node().getBoundingClientRect().width / 2;
      textAnchor = 'middle';
      break;
    case 'right':
      x = this.svg.node().getBoundingClientRect().width;
      textAnchor = 'end';
      break;
    default:
      x = 0;
      textAnchor = 'start';
  }

  const g = this.svg.append('g')
    .attr('transform', `translate(${padding.left}, ${padding.top})`);

  const textElement = g.append('text')
    .attr('x', x)
    .style('text-anchor', textAnchor)
    .style('dominant-baseline', 'hanging')
    .style('font-size', `${textStyle.fontSize}px`)
    .style('font-family', textStyle.fontFamily)
    .style('font-weight', textStyle.fontWeight)
    .style('fill', textStyle.color)
    .text(text);

  const bbox = textElement.node().getBBox();

  // Draw the rectangle background after calculating text bbox
  g.insert('rect', 'text') // Insert a rectangle before 'text' element
    .attr('x', bbox.x - borderWidth / 2)
    .attr('y', bbox.y - borderWidth / 2)
    .attr('width', bbox.width + borderWidth)
    .attr('height', bbox.height + borderWidth)
    .style('fill', backgroundColor)
    .style('stroke', borderColor)
    .style('stroke-width', borderWidth);

  this.svg.attr("height", bbox.height + padding.top + padding.bottom);

  return this;
};

//title

//cluster




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
      size: "12px",
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
    .attr("id", (d) => getUniqueId(d.name))
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

SvgContainer.prototype.addTitle = function(options = {}) {
  const defaultOptions = {
    padding: {
      left: 10,
      right: 10,
      top: 10,
      bottom: 10
    },
    title: {
      text: 'Testtttt',
      alignment: 'center', // options: 'left', 'center', 'right'
      fontSize: '16px'
    }
  };

  const { padding, title } = { ...defaultOptions, ...options };

  // Set the dimensions of the SVG
  this.svg.attr("height", "50px").attr("width", "100%")

  // Now get the SVG dimensions
  const width = this.svg.node().getBoundingClientRect().width;
  const height = this.svg.node().getBoundingClientRect().height;
  console.log(width)
  // Add title
  this.svg.append('text')
    .attr('x', width / 4) // Center the text horizontally
    .attr('y', padding.top)
    .style('text-anchor', 'middle') // Align the text to the center
    .style('dominant-baseline', 'hanging') // Ensure the text is inside the SVG
    .style('font-size', title.fontSize)
    .text(title.text);

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
      color: "black",
      stroke: "black",
      strokeWidth: 1,
      colorScheme: null,
      customColors: null
    },
    text: {
      anchor: "start",
      dy: ".35em",
      fill: "black"
    },
    font: {
      size: "12px",
      style: "normal",
      weight: "normal",
      decoration: "none",
      family: "sans-serif",
      color: "black"
    },
    align: "right",
    orientation: "horizontal",
    backgroundColor: "#FFF",
    width: "50%"
  };

  const {
    padding,
    legend,
    text,
    font,
    align,
    orientation,
    backgroundColor,
    width
  } = { ...defaultOptions, ...options };

  const svgLegend = this.svg;

  if(width !== null) {
    svgLegend.attr('width', width); // Set the SVG width if the width option is defined
  }
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

  const legendSize = parseFloat(font.size);
  const legendPadding = legendSize / 2;
  let currentX = padding.left,
    currentY = padding.top;

  legendElements.each((d, i, nodes) => {
    const textElement = nodes[i];
    const textSample = d3.select(textElement)
      .append("text")
      .attr("x", currentX + legendSize + legendPadding)
      .attr("y", currentY + legendSize / 2)
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

    if (currentX + textLength + legendSize + 2 * legendPadding > parentWidth) {
      currentX = padding.left;
      currentY += legendSize + legendPadding;
    }

    const rect = d3.select(textElement)
      .append("rect")
      .attr("x", currentX)
      .attr("y", currentY)
      .attr("width", legendSize)
      .attr("height", legendSize)
      .style("stroke", legend.stroke)
      .style("stroke-width", legend.strokeWidth)
      .style("fill", colorScale(d));

    const textLabel = d3.select(textElement)
      .append("text")
      .attr("x", currentX + legendSize + legendPadding)
      .attr("y", currentY + legendSize / 2)
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
      currentX += textLength + legendSize + 2 * legendPadding;
    } else {
      currentX = padding.left;
      currentY += legendSize + legendPadding;
    }
  });

  return this;
};

SvgContainer.prototype.adjustLabels = function(labelSelector, options = {}) {
    // Default options
    const defaultOptions = {
        rotation: 65, // Rotation angle (in degrees)
        dx: "-0.8em", // Horizontal adjustment
        dy: "0.15em", // Vertical adjustment
    };

    // Merge default options with the provided options
    const { rotation, dx, dy } = { ...defaultOptions, ...options };

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

                // Rotate both labels
                d3.select(labels[i])
                    .style("text-anchor", "end")
                    .attr("dx", dx)
                    .attr("dy", dy)
                    .attr("transform", `rotate(${rotation}, ${x1}, ${y1})`);

                d3.select(labels[j])
                    .style("text-anchor", "end")
                    .attr("dx", dx)
                    .attr("dy", dy)
                    .attr("transform", `rotate(${rotation}, ${x2}, ${y2})`);
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

