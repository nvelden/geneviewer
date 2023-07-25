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
//utils
function getUniqueId(baseId) {
  var i = 1;
  while (document.getElementById(baseId + "-" + i)) {
    i++;
  }
  return baseId + "-" + i;
}

function wrap(text, width, options = {}) {
  // Default options
  const defaultOptions = {
    dyAdjust: 0,
    lineHeightEms: 1.05,
    lineHeightSquishFactor: 1,
    splitOnHyphen: true,
    centreVertically: true,
  };

  // Merge default options and user-specified options
  const {
    dyAdjust,
    lineHeightEms,
    lineHeightSquishFactor,
    splitOnHyphen,
    centreVertically
  } = { ...defaultOptions, ...options };

  text.each(function () {
    var text = d3.select(this),
      x = text.attr("x"),
      y = text.attr("y");

    var words = [];
    text
      .text()
      .split(/\s+/)
      .forEach(function (w) {
        if (splitOnHyphen) {
          var subWords = w.split("-");
          for (var i = 0; i < subWords.length - 1; i++)
            words.push(subWords[i] + "-");
          words.push(subWords[subWords.length - 1] + " ");
        } else {
          words.push(w + " ");
        }
      });

    text.text(null); // Empty the text element

    var tspan = text.append("tspan");
    var line = "";
    var prevLine = "";
    var nWordsInLine = 0;

    for (var i = 0; i < words.length; i++) {
      var word = words[i];
      prevLine = line;
      line = line + word;
      ++nWordsInLine;
      tspan.text(line.trim());

      if (tspan.node().getComputedTextLength() > width && nWordsInLine > 1) {
        tspan.text(prevLine.trim());
        prevLine = "";
        line = word;
        nWordsInLine = 1;
        tspan = text.append("tspan").text(word.trim());
      }
    }

    var tspans = text.selectAll("tspan");
    var h = lineHeightEms;

    if (tspans.size() > 2)
      for (var i = 0; i < tspans.size(); i++) h *= lineHeightSquishFactor;

    tspans.each(function (d, i) {
      var dy = i * h + dyAdjust;
      if (centreVertically) dy -= ((tspans.size() - 1) * h) / 2;
      d3.select(this)
        .attr("y", y)
        .attr("x", x)
        .attr("dy", dy + "em");
    });
  });
}



// Container
function clusterContainer(svg, margin, width, height) {
  this.svg = svg;
  this.margin = margin;
  this.width = width;
  this.height = height;
}

function createClusterContainer(targetElement, options = {}) {

  const defaultOptions = {
    id: "svg-container",
    margin: { top: 50, right: 50, bottom: 50, left: 100 },
  };

  // Merge default options and user-specified options
  const { id, margin } = { ...defaultOptions, ...options };

  const width = targetElement.clientWidth;
  const height = targetElement.clientHeight;

  var svg = d3.select(targetElement)
    .append("svg")
    .attr("id", getUniqueId(id))  // use user-specified id, or default
    .attr("preserveAspectRatio", "xMinYMin meet")
    .attr("viewBox", `0 0 ${width} ${height}`)
    .classed("svg-content", true);

  return new clusterContainer(svg, margin, width, height);

}

clusterContainer.prototype.addGeneData = function (data) {

  this.data = data.map(item => {
    var newItem = {...item};

    newItem.direction = "forward";
    if(newItem.start > newItem.stop) {
      newItem.direction = "reverse";
      [newItem.start, newItem.stop] = [newItem.stop, newItem.start];
    }

    return newItem;
  });

  return this;
};

clusterContainer.prototype.addTitle = function (title, options = {}) {

  const defaultOptions = {
    x: 0,        // Default x coordinate
    y: 0,        // Default y coordinate
    font: {
      size: "12px",
      style: "italic",
      weight: "normal",
      decoration: "none",
      family: "sans-serif",
      color: "black"
    },
  };

  const { x, y, font } = { ...defaultOptions, ...options };

  // Add title
  this.svg.append("text")
    .attr("x", x + (this.width / 2))
    .attr("y", y + (this.margin.top / 2))
    .attr("text-anchor", "middle")
    .style("font-size", "16px")
    .style("text-decoration", "underline")
    .text(title);

  return this;
};

clusterContainer.prototype.addClusterTitle = function(title, options = {}) {
  // Default options
  const defaultOptions = {
    x: 0,
    y: 0,
    font: {
      size: "12px",
      style: "italic",
      weight: "bold",
      decoration: "none",
      family: "sans-serif",
      color: "black"
    },
  };

  // Merge default options and user-specified options
  const {
    x,
    y,
    font
  } = { ...defaultOptions, ...options };

  // calculate middle y position
  const middleY = this.height / 2 + y;  // Apply the y option
  const titleWidth = this.margin.left - x;

  let clusterTitle = this.svg.append("text")
    .attr("x", this.margin.left / 2 + x)  // Apply the x option
    .attr("y", middleY)
    .attr("text-anchor", "middle")
    .attr("dominant-baseline", "central")  // Vertically center text
    .style("font-size", font.size)
    .style("font-style", font.style)
    .style("font-weight", font.weight)
    .style("text-decoration", font.decoration)
    .style("font-family", font.family)
    .style("fill", font.color)
    .text(title);


  // wrap the text
  wrap(clusterTitle, titleWidth, 0, 1.05, 1, true, true);

  return this;
};

clusterContainer.prototype.drawGeneLine = function (options = {}) {
  const defaultOptions = {
    y: 50  // default y value
  };

  const { y } = { ...defaultOptions, ...options };

  if (!this.data) {
    console.error('No data has been added to this cluster container. Please use the addGeneData() function before attempting to draw a gene line.');
    return this;
  }

  // Data processing
  var maxStart = d3.max(this.data, (d) => d.start);
  var maxStop = d3.max(this.data, (d) => d.stop);
  var minStart = d3.min(this.data, (d) => d.start);
  var minStop = d3.min(this.data, (d) => d.stop);

  var xScale = d3.scaleLinear()
    .domain([minStart, maxStop])
    .range([0, this.width - this.margin.left - this.margin.right]);

  var yScale = d3.scaleLinear()
    .domain([0, 100])
    .range([this.height - this.margin.bottom - this.margin.top , 0]);

  // Create the group
  var g = this.svg.append("g")
    .attr("transform", "translate(" + this.margin.left + "," + this.margin.top + ")");

  // Draw baseline
  var line = g
    .append("line")
    .attr("class", "baseline")
    .attr("x1", xScale(minStart))
    .attr("y1", yScale(y))  // updated to use the y value
    .attr("x2", xScale(maxStop))
    .attr("y2", yScale(y))  // updated to use the y value
    .attr("stroke", "grey")
    .attr("stroke-width", 1);

  return this;
};

clusterContainer.prototype.drawGenes = function (group = null, options = {}) {

  // Verify that the data exists
  if (!this.data) {
    console.error('No data has been added to this cluster container. Please use the addGeneData() function before attempting to draw genes.');
    return this;
  }

  // Verify that the group exists in the data if it is defined
  if (group && !this.data.some(d => group in d)) {
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
  var maxStart = d3.max(this.data, (d) => d.start);
  var maxStop = d3.max(this.data, (d) => d.stop);
  var minStart = d3.min(this.data, (d) => d.start);
  var minStop = d3.min(this.data, (d) => d.stop);

  var xScale = d3.scaleLinear()
    .domain([minStart, maxStop])
    .range([0, this.width - this.margin.left - this.margin.right]);

  var yScale = d3.scaleLinear()
    .domain([0, 100])
    .range([this.height - this.margin.bottom - this.margin.top , 0]);

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
    .data(this.data)
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

   // Create the group
  var g = this.svg.append("g")
    .attr("transform", "translate(" + this.margin.left + "," + this.margin.top + ")");

  // Draw Genes
  var gene = g
    .selectAll(".geneline")
    .data(this.data)
    .enter()
    .append("line")
    .attr("class", "geneline")
    .attr("x1", (d) => d.direction === 'forward' ? xScale(d.start) : xScale(d.stop))
    .attr("y1", yScale(50))
    .attr("x2", (d) => d.direction === 'forward' ? xScale(d.stop) : xScale(d.start))
    .attr("y2", yScale(50))
    .attr("stroke-width", 2)
    .attr("marker-end", (d) => "url(#" + d.name + ")")
    .each(function (d) {
        const groupColor = colorScale(d[group]); // Use grouping variable here
        d3.select(this).attr("stroke", groupColor);
    });

  return this;
};

clusterContainer.prototype.drawGeneLabels = function (options = {}) {

  // Verify that the data exists
  if (!this.data) {
    console.error('No data has been added to this cluster container. Please use the addGeneData() function before attempting to draw genes.');
    return this;
  }

  const defaultOptions = {
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
    dx: "0em",
    y: 50, // Default y value
    rotate: 0  // Default rotation angle
  };

  const {
    font,
    anchor,
    dy,
    dx,
    y,
    rotate
  } = { ...defaultOptions, ...options };

  // Data processing
  var maxStart = d3.max(this.data, (d) => d.start);
  var maxStop = d3.max(this.data, (d) => d.stop);
  var minStart = d3.min(this.data, (d) => d.start);
  var minStop = d3.min(this.data, (d) => d.stop);

  var xScale = d3.scaleLinear()
    .domain([minStart, maxStop])
    .range([0, this.width - this.margin.left - this.margin.right]);

  var yScale = d3.scaleLinear()
    .domain([0, 100])
    .range([this.height - this.margin.top - this.margin.bottom, 0]);

  // Create the group
  var g = this.svg.append("g")
    .attr("transform", "translate(" + this.margin.left + "," + this.margin.top + ")");

  // Adding the Label
  var gene = g.selectAll("text.label")
    .data(this.data)
    .enter()
    .append("text")
    .attr("id", (d) => getUniqueId(d.name))
    .attr("class", "label")
    .attr("x", (d) => xScale((d.start + d.stop) / 2))
    .attr("y", () => yScale(y))
    .attr("dx", dx)
    .attr("dy", dy)
    .attr("text-anchor", anchor)
    .attr("transform", `rotate(${rotate})`)
    .style("font-size", font.size)
    .style("font-style", font.style)
    .style("font-weight", font.weight)
    .style("text-decoration", font.decoration)
    .style("font-family", font.family)
    .style("fill", font.color)
    .text((d) => d.name);

  return this;
};

clusterContainer.prototype.adjustLabels = function(labelSelector, options = {}) {
    // Default options
    const defaultOptions = {
        rotation: 65, // Rotation angle (in degrees)
        dx: "-0.8em", // Horizontal adjustment
        dy: "0.15em", // Vertical adjustment
    };

    // Merge default options with the provided options
    const { rotation, dx, dy } = { ...defaultOptions, ...options };

    // Select all the labels based on the provided selector
    var labels = this.svg.selectAll(".label").nodes();

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
clusterContainer.prototype.drawGeneLabels = function (data, options) {

  const defaultOptions = {
    padding: {
      left: 10,
      right: 10,
      top: 10,
      bottom: 10
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

  var xScale = d3.scaleLinear()
    .domain([minStart, maxStop])
    .range([0 + padding.left, this.width - this.margin.left - this.margin.right - padding.right]);

  var yScale = d3.scaleLinear()
    .domain([0, 100])
    .range([this.height - this.margin.top - this.margin.bottom - padding.bottom, 0 + padding.top]);

  // Draw Labels
  this.svg
  .append("g")
  .attr("transform", "translate(" + this.margin.left + "," + this.margin.top + ")")
    .selectAll("text.label")
    .data(data)
    .enter()
    .append("text")
    .attr("id", (d) => getUniqueId(d.name))
    .attr("class", "label")
    .attr("x", (d) => xScale((d.start + d.stop) / 2))
    .attr("y", () => yScale(50))
    .attr("dy", dy)
    .attr("text-anchor", anchor)
    .attr("transform", `rotate(${rotate})`)
    .style("font-size", font.size)
    .style("font-style", font.style)
    .style("font-weight", font.weight)
    .style("text-decoration", font.decoration)
    .style("font-family", font.family)
    .style("fill", font.color)
    .text((d) => d.name);

  return this;
};

clusterContainer.prototype.drawGeneLabels = function (data, options) {

  const defaultOptions = {
    padding: {
      left: 10,
      right: 10,
      top: 10,
      bottom: 10
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

  var xScale = d3.scaleLinear()
    .domain([minStart, maxStop])
    .range([0 + padding.left, this.width - this.margin.left - this.margin.right - padding.right]);

  var yScale = d3.scaleLinear()
    .domain([0, 100])
    .range([this.height - this.margin.top - this.margin.bottom - padding.bottom, 0 + padding.top]);

  // Draw Labels
  this.svg
  .append("g")
  .attr("transform", "translate(" + this.margin.left + "," + this.margin.top + ")")
    .selectAll("text.label")
    .data(data)
    .enter()
    .append("text")
    .attr("id", (d) => getUniqueId(d.name))
    .attr("class", "label")
    .attr("x", (d) => xScale((d.start + d.stop) / 2))
    .attr("y", () => yScale(50))
    .attr("dy", dy)
    .attr("text-anchor", anchor)
    .attr("transform", `rotate(${rotate})`)
    .style("font-size", font.size)
    .style("font-style", font.style)
    .style("font-weight", font.weight)
    .style("text-decoration", font.decoration)
    .style("font-family", font.family)
    .style("fill", font.color)
    .text((d) => d.name);

  return this;
};

clusterContainer.prototype.setTopTitle = function (text, options = {}) {
  const defaultOptions = {
    x: 0,        // Default x coordinate
    y: 0,        // Default y coordinate
    font: {
      size: "12px",
      style: "italic",
      weight: "normal",
      decoration: "none",
      family: "sans-serif",
      color: "black"
    },
  };

  const { x, y, font } = { ...defaultOptions, ...options };

  var xScale = d3.scaleLinear()
    .domain([0, 100])
    .range([0, this.viewBoxWidth - this.margin.left - this.margin.right]);

  var yScale = d3.scaleLinear()
    .domain([0, 100])
    .range([this.margin.top, 0]);

  this.titleTop
    .append("text")
    .attr("x", xScale(x))
    .attr("y", yScale(y))
    .style("font-size", font.size)
    .style("font-style", font.style)
    .style("font-weight", font.weight)
    .style("text-decoration", font.decoration)
    .style("font-family", font.family)
    .style("fill", font.color)
    .text(text);

  return this;
};

clusterContainer.prototype.setClusterTitle = function (text, options = {}, position = "bottom", align = "left") {
  const defaultOptions = {
    font: {
      size: "12px",
      style: "italic",
      weight: "normal",
      decoration: "none",
      family: "sans-serif",
      color: "black"
    },
    dy: "-1em",
    rotate: 0,  // Default rotation angle
    x: 10,       // Default x coordinate
    y: 0        // Default y coordinate
  };

  const {
    font,
    dy,
    rotate,
    x,  // Get the x option
    y   // Get the y option
  } = { ...defaultOptions, ...options };

  let xTranslate, anchor;
  switch (align) {
    case "left":
      xTranslate = this.margin.left / 2;
      anchor = "start";
      break;
    case "right":
      xTranslate = this.viewBoxWidth - this.margin.right / 2;
      anchor = "end";
      break;
    default:  // Center
      xTranslate = this.viewBoxWidth / 2;
      anchor = "middle";
      break;
  }

  targetGroup
    .append("text")
    .attr("x", x)  // Use the x option
    .attr("y", -y)  // Use the y option
    .attr("dy", dy)
    .attr("text-anchor", anchor) // Set anchor based on alignment
    .style("font-size", font.size)
    .style("font-style", font.style)
    .style("font-weight", font.weight)
    .style("text-decoration", font.decoration)
    .style("font-family", font.family)
    .style("fill", font.color)
    .text(text);

  return this;
};

clusterContainer.prototype.adjustLabels = function(labelSelector, options = {}) {
    // Default options
    const defaultOptions = {
        rotation: 65, // Rotation angle (in degrees)
        dx: "-0.8em", // Horizontal adjustment
        dy: "0.15em", // Vertical adjustment
    };

    // Merge default options with the provided options
    const { rotation, dx, dy } = { ...defaultOptions, ...options };

    // Select all the labels based on the provided selector
    var labels = this.plotArea.selectAll(labelSelector).nodes();

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

clusterContainer.prototype.drawGeneLabels = function (data, options = {}) {
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
  var width = this.viewBoxWidth - this.margin.left - this.margin.right;
  var height = this.viewBoxHeight - this.margin.top - this.margin.bottom;

  var xScale = d3
  .scaleLinear()
  .domain([minStop, maxStop])
  .range([this.margin.left, this.viewBoxWidth - this.margin.right]);

  var yScale = d3
  .scaleLinear()
  .domain([minStart, maxStart])
  .range([this.viewBoxHeight - this.margin.bottom, this.margin.top]);

  // Draw Labels
  this.plotArea
    .selectAll("text.label")
    .data(data)
    .enter()
    .append("text")
    .attr("id", (d) => getUniqueId(d.name))
    .attr("class", "label")
    .attr("x", (d) => xScale(d.start + d.stop / 2))
    .attr("y", (d) =>yScale(0))
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

clusterContainer.prototype.drawCluster = function (data, options = {}, group = null) {

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

clusterContainer.prototype.adjustViewBox = function (options = {}) {
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

clusterContainer.prototype.drawLegend = function(data, options = {}, group = null) {

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

clusterContainer.prototype.adjustLabels = function(labelSelector, options = {}) {
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

*/
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

