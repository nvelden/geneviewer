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

//Utils

function getUniqueId(baseId) {
  var i = 1;
  while (document.getElementById(baseId + "-" + i)) {
    i++;
  }
  return baseId + "-" + i;
}

function sanitizeId(str) {
  // Replace any character that is not a letter, number, underscore, or hyphen with an underscore
  return str.replace(/[^a-zA-Z0-9_-]/g, '_');
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

function adjustViewBox(svg, options = {}) {
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
  var width = svg.node().getBoundingClientRect().width;
  var height = svg.node().getBoundingClientRect().height;

  // Adjust viewBox
  var bbox = svg.node().getBBox();
  svg.attr("viewBox", [
    bbox.x - padding.left,
    bbox.y - padding.top,
    bbox.width + padding.left + padding.right,
    bbox.height + padding.top + padding.bottom,
  ]);

  return svg;
};

function computeSize(inputSize, containerSize) {

    // If inputSize is undefined or null, return 0
    if (typeof inputSize === "undefined" || inputSize === null) {
        return 0;
    }

    // If inputSize is a number, return it directly
    if (typeof inputSize === "number") {
        return inputSize;
    }

    // Initialize resultSize
    var resultSize;

    // Check if the size is given as a percentage
    if (inputSize.includes("%")) {
        var percentageValue = parseFloat(inputSize);
        var fraction = percentageValue / 100;
        resultSize = Math.round(fraction * containerSize);
    }
    // Check if the size is given in pixels
    else if (inputSize.includes("px")) {
        resultSize = parseFloat(inputSize);
    }
    // Assume it's a plain number otherwise
    else {
        resultSize = parseFloat(inputSize);
    }

    return resultSize;
}

function adjustGeneLabels(clusterContainer, labelSelector, options = {}) {
    // Default options
    const defaultOptions = {
        rotation: 65, // Rotation angle (in degrees)
        dx: "-0.8em", // Horizontal adjustment
        dy: "0.15em", // Vertical adjustment
    };

    // Merge default options with the provided options
    const { rotation, dx, dy } = { ...defaultOptions, ...options };

    // Select all the labels based on the provided selector
    var labels = clusterContainer.svg.selectAll(".label").nodes();

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
    return clusterContainer;
}

function adjustSpecificLabel(clusterContainer, labelSelector, elementId, options = {}) {
    // Default options
    const defaultOptions = {
        rotation: 65, // Rotation angle (in degrees)
        dx: "-0.8em", // Horizontal adjustment
        dy: "0.15em", // Vertical adjustment
    };

    // Merge default options with the provided options
    const { rotation, dx, dy } = { ...defaultOptions, ...options };

    // Select all the labels based on the provided selector
    var labels = clusterContainer.svg.selectAll(labelSelector).nodes();

    // Select the specific label using the provided elementId
    var specificLabel = clusterContainer.svg.select(`#${elementId}`).node();
    var specificLabelRect = specificLabel.getBoundingClientRect();

    // Check for overlap with other labels
    for (var i = 0; i < labels.length; i++) {
        if (labels[i] !== specificLabel) { // Ensure we're not comparing the label with itself
            var labelRect = labels[i].getBoundingClientRect();

            // If the specific label overlaps with another label
            if (!(specificLabelRect.right < labelRect.left ||
                  specificLabelRect.left > labelRect.right ||
                  specificLabelRect.bottom < labelRect.top ||
                  specificLabelRect.top > labelRect.bottom)) {

                // Get the current x and y attributes of the specific label
                var x = parseFloat(d3.select(specificLabel).attr('x'));
                var y = parseFloat(d3.select(specificLabel).attr('y'));

                // Rotate the specific label
                d3.select(specificLabel)
                    .style("text-anchor", "end")
                    .attr("dx", dx)
                    .attr("dy", dy)
                    .attr("transform", `rotate(${rotation}, ${x}, ${y})`);

                // Break out of the loop once we've adjusted the specific label
                break;
            }
        }
    }
    return clusterContainer;
}

function camelToKebab(string) {
    return string.replace(/([a-z0-9]|(?=[A-Z]))([A-Z])/g, '$1-$2').toLowerCase();
}

function extractAdditionalOptions(combinedOptions, defaultOptions) {
  // Extract additional options that are not in defaultOptions
  const additionalOptions = Object.keys(combinedOptions).reduce((acc, key) => {
    if (!(key in defaultOptions)) {
      acc[key] = combinedOptions[key];
    }
    return acc;
  }, {});

  return additionalOptions;
}

function setAttributesFromOptions(currentElement, additionalOptions) {
  for (const [key, value] of Object.entries(additionalOptions)) {
    currentElement.attr(camelToKebab(key), value);
  }
}

function applyStyleToElement(currentElement, itemStyle, i) {
  const style = itemStyle.find(s => s.index === i);
  if (style) {
    for (const [key, value] of Object.entries(style)) {
      if (key !== 'index' && key !== 'labelAdjustmentOptions') {
        currentElement.style(camelToKebab(key), value);
      }
    }
  }
}

// CLuster

function clusterContainer(svg, margin, width, height) {
  this.svg = svg;
  this.margin = margin;
  this.width = width;
  this.height = height;
}

function createClusterContainer(targetElementId, options = {}) {

  const defaultOptions = {
    id: "svg-container",
    margin: { top: 0, right: "10%", bottom: 0, left: "10%" },
    backgroundColor: "white",
    width: null,
    height: null
  };

  // Merge default options and user-specified options
  const { id, margin: originalMargin, backgroundColor, width, height } = { ...defaultOptions, ...options };

  // Compute margins without modifying the original margin object
  const computedMargin = {
    top: computeSize(originalMargin.top, height),
    right: computeSize(originalMargin.right, width),
    bottom: computeSize(originalMargin.bottom, height),
    left: computeSize(originalMargin.left, width)
  };

  var svg = d3.select(targetElementId)
    .append("svg")
    .attr("id", getUniqueId(id))
    .attr("width", "100%")
    .attr("height", "100%")
    .attr("preserveAspectRatio", "xMinYMin meet")
    .attr("viewBox", `0 0 ${width} ${height}`)
    .classed("GCVieweR-svg-content", true)
    .style("box-sizing", "border-box")
    .style("background-color", backgroundColor);

  return new clusterContainer(svg, computedMargin, width, height);
}

clusterContainer.prototype.theme = function (themeName) {
  // Make sure the theme exists
  if (!themes.hasOwnProperty(themeName)) {
    throw new Error(`Theme '${themeName}' does not exist.`);
  }

  // Retrieve the theme
  const themeOptions = themes[themeName];

  // Save the theme options to the instance for later use
  this.themeOptions = themeOptions;

  return this;
};

clusterContainer.prototype.geneData = function (data) {

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

clusterContainer.prototype.title = function(title, subtitle, show = true, options = {}) {

  // Return early if neither title nor subtitle is provided
  if (!title && !subtitle) {
    return this;
  }

  if (!show) {
    return this;
  }

  // Default options for title and subtitle
  const defaultOptions = {
    x: 0,
    y: 0,
    position: "center",
    spacing: 20, // Default spacing between title and subtitle
    titleFont: {
      size: "16px",
      style: "normal",
      weight: "bold",
      decoration: "normal",
      family: "sans-serif",
      color: "black"
    },
    subtitleFont: {
      size: "14px",
      style: "normal",
      weight: "normal",
      decoration: "none",
      family: "sans-serif",
      color: "black"
    },
  };

  // Merge theme options if they exist
  if (this.themeOptions && this.themeOptions.titleOptions) {
    options = { ...this.themeOptions.titleOptions, ...options };
  }

  // Merge default options with provided options, ensuring font properties are also merged
  const mergedOptions = {
    ...defaultOptions,
    ...options,
    titleFont: { ...defaultOptions.titleFont, ...options.titleFont },
    subtitleFont: { ...defaultOptions.subtitleFont, ...options.subtitleFont }
  };

  // Destructure the merged options
  const { x, y, titleFont, subtitleFont, position, spacing } = mergedOptions;

  let xPos;
  let textAnchor;

  // Determine text position and anchor based on the provided position
  switch (position) {
    case "left":
      xPos = x;
      textAnchor = "start";
      break;
    case "right":
      xPos = this.width - x;
      textAnchor = "end";
      break;
    default:
      xPos = x + (this.width / 2);
      textAnchor = "middle";
  }

  // Add title to the SVG
  this.svg.append("text")
    .attr("x", xPos)
    .attr("y", y + (this.margin.top / 2))
    .attr("text-anchor", textAnchor)
    .style("font-size", titleFont.size)
    .style("font-style", titleFont.style)
    .style("font-weight", titleFont.weight)
    .style("text-decoration", titleFont.decoration)
    .style("font-family", titleFont.family)
    .style("fill", titleFont.color)
    .text(title);

  // Add subtitle to the SVG if provided
  if (subtitle) {
    this.svg.append("text")
      .attr("x", xPos)
      .attr("y", y + (this.margin.top / 2) + spacing) // Use the spacing option for subtitle spacing
      .attr("text-anchor", textAnchor)
      .style("font-size", subtitleFont.size)
      .style("font-style", subtitleFont.style)
      .style("font-weight", subtitleFont.weight)
      .style("text-decoration", subtitleFont.decoration)
      .style("font-family", subtitleFont.family)
      .style("fill", subtitleFont.color)
      .text(subtitle);
  }

  return this;
};

clusterContainer.prototype.footer = function(title, subtitle, show = true, options = {}) {

  // Return early if neither title nor subtitle is provided
  if (!title && !subtitle) {
    return this;
  }

  if (!show) {
    return this;
  }

  // Default options for title and subtitle
  const defaultOptions = {
    x: 6,
    y: 0,
    position: "left",
    spacing: 12, // Default spacing between title and subtitle
    titleFont: {
      fontSize: "12px",
      fontWeight: "bold",
      fontFamily: "sans-serif"
    },
    subtitleFont: {
      fontSize: "10px",
      fontStyle: "normal",
      fontFamily: "sans-serif"
    },
  };

  // Merge theme options if they exist
  if (this.themeOptions && this.themeOptions.footerOptions) {
    options = { ...this.themeOptions.footerOptions, ...options };
  }

    const combinedOptions = {
    ...defaultOptions,
    ...options,
    titleFont: { ...defaultOptions.titleFont, ...options.titleFont },
    subtitleFont: { ...defaultOptions.subtitleFont, ...options.subtitleFont }
  };
  const { x, y, titleFont, subtitleFont, position, spacing } = combinedOptions;

  // Extract additional options that are not in defaultOptions
  const additionalOptionsTitleFont = extractAdditionalOptions(titleFont, defaultOptions.titleFont);
  const additionalOptionsSubtitleFont = extractAdditionalOptions(subtitleFont, defaultOptions.subtitleFont);

  let xPos;
  let textAnchor;

  // Determine text position and anchor based on the provided position
  switch (position) {
    case "left":
      xPos = x;
      textAnchor = "start";
      break;
    case "right":
      xPos = this.width - x;
      textAnchor = "end";
      break;
    default:
      xPos = x + (this.width / 2);
      textAnchor = "middle";
  }

  // Calculate y position for title and subtitle based on the SVG height and bottom margin
  const titleYPos = this.height - this.margin.bottom + y;
  const subtitleYPos = titleYPos + spacing;

  // Add title to the SVG
  this.svg.append("text")
    .attr("x", xPos)
    .attr("y", titleYPos)
    .attr("text-anchor", textAnchor)
    .style("font-size", titleFont.fontSize)
    .style("font-weight", titleFont.fontWeight)
    .style("font-family", titleFont.fontFamily)
    .text(title)
    .each(function() {
      const currentElement = d3.select(this);
      setAttributesFromOptions(currentElement, additionalOptionsTitleFont);
    });

  // Add subtitle to the SVG if provided
  if (subtitle) {
    this.svg.append("text")
      .attr("x", xPos)
      .attr("y", subtitleYPos)
      .attr("text-anchor", textAnchor)
      .style("font-size", subtitleFont.fontSize)
      .style("font-style", subtitleFont.fontStyle)
      .style("font-family", subtitleFont.fontFamily)
      .text(subtitle)
      .each(function() {
        const currentElement = d3.select(this);
        setAttributesFromOptions(currentElement, additionalOptionsSubtitleFont);
      });
  }

  return this;
};

clusterContainer.prototype.clusterLabel = function(title, show = true, options = {}) {

  if (!show) {
    return this;
  }

  // Default options
const defaultOptions = {
    x: 0,
    y: 0,
    position: 'left',
    wrapLabel: true,
    wrapOptions: {},  // Added wrapOptions to store options for the wrap function
    font: {
      size: "12px",
      style: "normal",
      weight: "bold",
      decoration: "none",
      family: "sans-serif",
      color: "black"
    },
  };

  const mergedOptions = {
    ...defaultOptions,
    ...options,
    font: { ...defaultOptions.font, ...options.font },
    wrapOptions: { ...defaultOptions.wrapOptions, ...options.wrapOptions }
  };

  const {
    x,
    y,
    position,
    font,
    wrapLabel,
    wrapOptions// Destructured wrapLabel option
  } = mergedOptions;

  // calculate middle y position
  const adjustedHeight = this.height - this.margin.top - this.margin.bottom;
  const middleY = this.margin.top + adjustedHeight / 2 + y;
  const titleWidth = position === 'left' ? this.margin.left - x : this.margin.right - x;

  let xPosition;
  if (position === 'left') {
    xPosition = this.margin.left / 2 + x;  // title is in the left margin
  } else {  // 'right'
    xPosition = this.width - this.margin.right / 2 - x;  // title is in the right margin
  }

  let clusterTitle = this.svg.append("text")
    .attr("x", xPosition)
    .attr("y", middleY)
    .attr("text-anchor", "middle")  // text is always centered
    .attr("dominant-baseline", "central")  // Vertically center text
    .style("font-size", font.size)
    .style("font-style", font.style)
    .style("font-weight", font.weight)
    .style("text-decoration", font.decoration)
    .style("font-family", font.family)
    .style("fill", font.color)
    .text(title);

  // If wrapLabel is true, wrap the text
  if (wrapLabel) {
    wrap(clusterTitle, titleWidth, wrapOptions);
  }

  return this;
};

clusterContainer.prototype.sequence = function (show = true, options = {}) {

  if (!show) {
    return this;
  }

  if (!this.data) {
    console.error('No data has been added to this cluster container. Please use the addGeneData() function before attempting to draw a gene line.');
    return this;
  }

  const defaultOptions = {
    y: 50,  // default y value
    start: null,
    stop: null,
    stroke: {
      color: "grey",
      width: 1,
      dashArray: null  // e.g., "5,5" for dashed line
    }
  };

  // If theme options exist, use them as the default options
  if (this.themeOptions && this.themeOptions.sequenceOptions) {
    options = { ...this.themeOptions.sequenceOptions, ...options };
  }

  // Merge the default options with the provided options at a deeper level
  const mergedOptions = {
    ...defaultOptions,
    ...options,
    stroke: { ...defaultOptions.stroke, ...options.stroke }
  };

  const { y, start, stop, stroke } = mergedOptions;

  // Data processing
  var minStart = d3.min(this.data, (d) => d.start);
  var maxStop = d3.max(this.data, (d) => d.stop);

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
    .attr("x1", xScale(start || minStart))
    .attr("y1", yScale(y))
    .attr("x2", xScale(stop || maxStop))
    .attr("y2", yScale(y))
    .attr("stroke", stroke.color)
    .attr("stroke-width", stroke.width)
    .attr("stroke-dasharray", stroke.dashArray);

  return this;
};

clusterContainer.prototype.markers = function(group, show = true, options = {}) {

  if (!show) {
    return this;
  }

  // Verify that the data exists
  if (!this.data) {
    console.error('No data has been added to this cluster container. Please use the geneData() function before attempting to draw markers.');
    return this;
  }

  const defaultOptions = {
    x: 0,
    y: 50,
    start: null,
    stop: null,
    size: 10,
    colorScheme: null,
    customColors: null,
    marker: "arrow",
    itemStyle: [] // {index: 1, strokeWidth : 10, stroke : "black"}
  };

  // If theme options exist, use them as the default options
  if (this.themeOptions && this.themeOptions.genesOptions) {
    options = { ...this.themeOptions.markersOptions, ...options };
  }

  const combinedOptions = { ...defaultOptions, ...options };
  const { x, y, start, stop, size, colorScheme, customColors, marker, itemStyle } = combinedOptions;

  // Extract additional options that are not in defaultOptions
  const additionalOptions = extractAdditionalOptions(combinedOptions, defaultOptions);

  // Data processing
  var maxStop = stop || d3.max(this.data, (d) => d.stop);
  var minStart = start || d3.min(this.data, (d) => d.start);

  var xScale = d3.scaleLinear()
    .domain([minStart, maxStop])
    .range([0, this.width - this.margin.left - this.margin.right]);

  var yScale = d3.scaleLinear()
    .domain([0, 100])
    .range([this.height - this.margin.bottom - this.margin.top , 0]);

  // Color Scale Setup
  let colorScale;
  if (colorScheme) {
    colorScale = d3.scaleOrdinal(d3[colorScheme]);
  } else if (customColors && customColors.length > 0) {
    colorScale = d3.scaleOrdinal().range(customColors);
  } else {
    colorScale = d3.scaleOrdinal(d3.schemeCategory10);
  }

  // Create the group
  var g = this.svg.append("g")
    .attr("transform", "translate(" + (this.margin.left) + "," + this.margin.top + ")");  // Apply x offset here

  const getAttributesForIndex = (d, i) => {
    const style = itemStyle.find(s => s.index === i) || {};
    const currentX = style.x || x;
    const currentY = style.y || y;
    const currentSize = style.size || size;
    const currentMarker = style.marker || marker;

    const offset = currentSize / 2;
    const yPos = yScale(currentY);
    const xPos = d.direction === 'forward' ? xScale(d.stop) - offset + currentX : xScale(d.start) + offset - currentX;

    return { xPos, yPos, currentSize, currentMarker };
   };

  // Create triangles
  g.selectAll(".marker")
   .data(this.data)
   .enter()
   .append("path")
   .attr("d", (d, i) => {
      const { xPos, yPos, currentSize, currentMarker } = getAttributesForIndex(d, i);
      return getMarker(currentMarker, xPos, yPos, currentSize);
    })
    .attr("transform", (d, i) => {
      const { xPos, yPos } = getAttributesForIndex(d, i);
      const rotation = d.direction === 'forward' ? 90 : -90;
      return `rotate(${rotation}, ${xPos}, ${yPos})`;
    })
   .attr("fill", (d) => colorScale(d[group]))
   .each(function (d, i) {
      const currentElement = d3.select(this);

      // Set additional options as attributes
      setAttributesFromOptions(currentElement, additionalOptions);

      // Override with itemStyle based on the index
      applyStyleToElement(currentElement, itemStyle, i);
  });

  return this;
};

clusterContainer.prototype.genes = function(group, show = true, options = {}) {

  if (!show) {
    return this;
  }

  // Verify that the data exists
  if (!this.data) {
    console.error('No data has been added to this cluster container. Please use the geneData() function before attempting to draw genes.');
    return this;
  }

  const defaultOptions = {
    x: -5,
    y: 50,  // default y value
    start: null,
    stop: null,
    colorScheme: null,
    customColors: null,
    itemStyle: [] //[{index: 1,opacity: 0.5}]
  };

  // If theme options exist, use them as the default options
  if (this.themeOptions && this.themeOptions.genesOptions) {
    options = { ...this.themeOptions.genesOptions, ...options };
  }

  const combinedOptions = { ...defaultOptions, ...options };
  const { x, y, start, stop, colorScheme, customColors, itemStyle } = { ...defaultOptions, ...options };

  // Extract additional options that are not in defaultOptions
  const additionalOptions = extractAdditionalOptions(combinedOptions, defaultOptions);

  // Data processing
  var maxStop = stop || d3.max(this.data, (d) => d.stop);
  var minStart = start || d3.min(this.data, (d) => d.start);

  var xScale = d3.scaleLinear()
    .domain([minStart, maxStop])
    .range([0, this.width - this.margin.left - this.margin.right]);

  var yScale = d3.scaleLinear()
    .domain([0, 100])
    .range([this.height - this.margin.bottom - this.margin.top , 0]);

  // Color Scale Setup
  let colorScale;
  if (colorScheme) {
    colorScale = d3.scaleOrdinal(d3[colorScheme]);
  } else if (customColors && customColors.length > 0) {
    colorScale = d3.scaleOrdinal().range(customColors);
  } else {
    colorScale = d3.scaleOrdinal(d3.schemeCategory10);
  }

  // Create the group
  const g = this.svg.append("g")
    .attr("transform", "translate(" + this.margin.left + "," + this.margin.top + ")");


  const getAttributesForIndex = (d, i) => {
    const style = itemStyle.find(s => s.index === i) || {};
    const currentX = style.x || x;
    const currentY = style.y || y;

    const xPosStart = d.direction === 'forward' ? xScale(d.start) : xScale(d.stop);
    const xPosEnd = d.direction === 'forward' ? Math.max(xScale(d.start), (xScale(d.stop) + currentX)) : Math.min(xScale(d.stop), (xScale(d.start) - currentX));
    const yPos = yScale(currentY);

    return { xPosStart, xPosEnd, yPos };
  };

  // Draw Genes
  var gene = g
    .selectAll(".geneline")
    .data(this.data)
    .enter()
    .append("line")
    .attr("class", "geneline")
    .attr("id", (d, i) => `${sanitizeId(d.cluster)}-geneline-${i}`)
    .attr("x1", (d, i) => getAttributesForIndex(d, i).xPosStart)
    .attr("y1", (d, i) => getAttributesForIndex(d, i).yPos)
    .attr("x2", (d, i) => getAttributesForIndex(d, i).xPosEnd)
    .attr("y2", (d, i) => getAttributesForIndex(d, i).yPos)
    .attr("stroke", (d) => colorScale(d[group]))
   .each(function (d, i) {
      const currentElement = d3.select(this);
      // Set additional options as attributes
      setAttributesFromOptions(currentElement, additionalOptions);
      // Override with itemStyle based on the index
      applyStyleToElement(currentElement, itemStyle, i);
  });

  return this;
};

clusterContainer.prototype.coordinates = function (show = true, options = {}) {

  if (!show) {
    return this;
  }

  const defaultOptions = {
      start: null,
      stop: null,
      rotate: -45,
      yPositionTop: 55,  // position for top axis
      yPositionBottom: 45,  // position for bottom axis
      tickValues: null,  // add an option to provide your own tick values
      tickValueThreshold: 300
  };

  // If theme options exist, use them as the default options
  if (this.themeOptions && this.themeOptions.coordinatesOptions) {
    options = { ...this.themeOptions.coordinatesOptions, ...options };
  }

  const combinedOptions = { ...defaultOptions, ...options };
  const { start, stop, rotate, yPositionTop, yPositionBottom, tickValues, tickValueThreshold } = combinedOptions;

  // Extract additional options that are not in defaultOptions
  const additionalOptions = extractAdditionalOptions(combinedOptions, defaultOptions);

  // Data processing
  var maxStop = stop || d3.max(this.data, (d) => d.stop);
  var minStart = start || d3.min(this.data, (d) => d.start);

  var xScale = d3.scaleLinear()
    .domain([minStart, maxStop])
    .range([0, this.width - this.margin.left - this.margin.right]);

  var yScale = d3.scaleLinear()
    .domain([0, 100])
    .range([this.height - this.margin.bottom - this.margin.top , 0]);

  // Create the group
  var g = this.svg.append("g")
    .attr("transform", "translate(" + this.margin.left + "," + this.margin.top + ")");

  // Get all start and stop values
  var allTickValues = tickValues || this.data.reduce((acc, d) => {
    acc.push(d.start);
    acc.push(d.stop);
    return acc;
  }, []);

  // Sort the array in ascending order
  allTickValues.sort((a, b) => a - b);

  var tickValuesTop = [];
  var tickValuesBottom = allTickValues.filter((value, index, array) => {
    if (index === 0) return true; // Always include the first element
    var diff = value - array[index - 1];
    if (diff < tickValueThreshold) {
        tickValuesTop.push(value);
        return false; // Exclude this value from the new array
    }
    return true; // Include this value in the new array
  });

  // Add X-axis scale at the top
  var xAxisTop = g.append("g")
    .attr("transform", "translate(0," + yScale(yPositionTop) + ")")
    .call(d3.axisTop(xScale).tickValues(tickValuesTop));

  // Add X-axis scale at the bottom
  var xAxisBottom = g.append("g")
    .attr("transform", "translate(0," + yScale(yPositionBottom) + ")")
    .call(d3.axisBottom(xScale).tickValues(tickValuesBottom));

  // Hide axis line
  xAxisTop.select(".domain").attr("stroke", "none");
  xAxisBottom.select(".domain").attr("stroke", "none");

  xAxisTop.selectAll("text")
    .data(this.data)
    .attr("class", "coordinate")
    .attr("id", (d, i) => `${sanitizeId(this.data[0].cluster)}-coordinate-top-${i}`)
    .style("text-anchor", "end")
    .attr("dx", "-.8em")
    .attr("dy", ".4em")
    .attr("transform", "rotate(" + (-rotate) + ")")
    .each(function() {
      const currentElement = d3.select(this);
      setAttributesFromOptions(currentElement, additionalOptions);
    });

  xAxisBottom.selectAll("text")
    .attr("class", "coordinate")
    .attr("id", (d, i) => `${sanitizeId(this.data[0].cluster)}-coordinate-bottom-${i}`)
    .style("text-anchor", "start")
    .attr("dx", ".8em")
    .attr("dy", "-.15em")
    .attr("transform", "rotate(" + (-rotate) + ")")
    .each(function() {
      const currentElement = d3.select(this);
      setAttributesFromOptions(currentElement, additionalOptions);
    });

  return this;
};

clusterContainer.prototype.scaleBar = function (show = true, options = {}) {

  if (!show) {
    return this;
  }

  const defaultOptions = {
    title: "1 kb",
    scaleBarUnit: 1000,
    x: 0, // default x offset
    y: 10,
    labelPosition: "left", // default label position
    font: {
      size: "10px",
      style: "normal",
      weight: "normal",
      decoration: "none",
      family: "sans-serif",
      color: "black"
    }
  };

  const mergedOptions = {
    ...defaultOptions,
    ...options,
    font: { ...defaultOptions.font, ...options.font }
  };

  const { x, y, font, title, scaleBarUnit, labelPosition } = mergedOptions;

  // Data processing
  const [minStart, maxStop] = [
    d3.min(this.data, d => d.start),
    d3.max(this.data, d => d.stop)
  ];

  const xScale = d3.scaleLinear()
    .domain([minStart, maxStop])
    .range([0, this.width - this.margin.left - this.margin.right]);

  // Calculate the length of the scale bar in pixels
  const scaleBarLength = xScale(minStart + scaleBarUnit);

  // Create the group with the x offset applied
  const g = this.svg.append("g")
    .attr("transform", `translate(${this.width - this.margin.right - scaleBarLength - parseInt(font.size) - 5 + x}, ${this.height - this.margin.bottom - this.margin.top})`);

  // Create the scale bar line
  g.append("line")
    .attr("x1", parseInt(font.size) + 5)
    .attr("x2", parseInt(font.size) + 5 + scaleBarLength)
    .attr("y1", -y)
    .attr("y2", -y)
    .attr("stroke", "grey")
    .attr("stroke-width", 1);

  // Add the ticks
  [parseInt(font.size) + 5, parseInt(font.size) + 5 + scaleBarLength].forEach(d => {
    g.append("line")
      .attr("x1", d)
      .attr("x2", d)
      .attr("y1", -y - 5)
      .attr("y2", -y + 5)
      .attr("stroke", "grey")
      .attr("stroke-width", 1);
  });

  // Determine the x position of the title based on the labelPosition
  const titleX = labelPosition === "left" ? parseInt(font.size) : parseInt(font.size) + 5 + scaleBarLength;
  const textAnchor = labelPosition === "left" ? "end" : "start";

  // Add the title
  g.append("text")
    .attr("x", titleX)
    .attr("y", -y)
    .style("text-anchor", textAnchor)
    .style("dominant-baseline", "middle")
    .style("font-size", font.size)
    .style("font-style", font.style)
    .style("font-weight", font.weight)
    .style("text-decoration", font.decoration)
    .style("font-family", font.family)
    .style("fill", font.color)
    .text(title);

  return this;
};

clusterContainer.prototype.labels = function (label, show = true, options = {}) {

  if (!show) {
    return this;
  }

  // Verify that the data exists
  if (!this.data) {
    console.error('No data has been added to this cluster container. Please use the addGeneData() function before attempting to draw genes.');
    return this;
  }

  const defaultOptions = {
    x: 0,
    y: 50,
    dy: "-1em",
    dx: "0em",
    rotate: 0,
    start: null,
    stop: null,
    adjustLabels: true,
    fontSize: "12px",
    fontStyle: "italic",
    fontFamily: "sans-serif",
    textAnchor: "middle",
    labelAdjustmentOptions: {
      rotation: 65,
      dx: "-0.8em",
      dy: "0.15em"
    },
    itemStyle: []
  };

  // If theme options exist, use them as the default options
  if (this.themeOptions && this.themeOptions.labelsOptions) {
    options = { ...this.themeOptions.labelsOptions, ...options };
  }

  const combinedOptions = { ...defaultOptions, ...options };
  const { x, y, start, stop, adjustLabels, labelAdjustmentOptions, itemStyle, dx, dy, anchor, rotate, fontSize, fontStyle, fontFamily, textAnchor} = combinedOptions;

  // Extract additional options that are not in defaultOptions
  const additionalOptions = extractAdditionalOptions(combinedOptions, defaultOptions);

  // Data processing
  const maxStop = stop || d3.max(this.data, d => d.stop);
  const minStart = start || d3.min(this.data, d => d.start);

  const xScale = d3.scaleLinear()
    .domain([minStart, maxStop])
    .range([0, this.width - this.margin.left - this.margin.right]);

  const yScale = d3.scaleLinear()
    .domain([0, 100])
    .range([this.height - this.margin.top - this.margin.bottom, 0]);

  // Placeholder function for getUniqueId
  const getUniqueId = (label) => label; // Replace with your actual implementation

  // Create the group
  const g = this.svg.append("g")
    .attr("transform", `translate(${this.margin.left},${this.margin.top})`);

  const getAttributesForIndex = (d, i) => {
    const style = itemStyle.find(s => s.index === i) || {};
    const currentX = style.x || x;
    const currentY = style.y || y;
    const currentDx = style.dx || dx;
    const currentDy = style.dy || dy;
    const currentRotate = style.rotate || rotate;
    const currentLabelAdjustmentOptions = style.labelAdjustmentOptions || undefined;
    const currentAdjustLabels = style.adjustLabels !== undefined ? style.adjustLabels : adjustLabels;

    const xPos = xScale((d.start + d.stop) / 2) + currentX;
    const yPos = yScale(currentY);

    return {
        xPos,
        yPos,
        dx: currentDx,
        dy: currentDy,
        rotate: currentRotate,
        labelAdjustmentOptions: currentLabelAdjustmentOptions,
        adjustLabels: currentAdjustLabels
    };
  };

  const self = this;
  console.log(this.data)
  // Adding the Label
  g.selectAll("text.label")
    .data(this.data)
    .enter()
    .append("text")
    .attr("id", (d, i) => `${sanitizeId(d.cluster)}-label-${i}`)
    .attr("class", "label")
    .attr("x", (d, i) => getAttributesForIndex(d, i).xPos)
    .attr("y", (d, i) => getAttributesForIndex(d, i).yPos)
    .attr("dx", (d, i) => getAttributesForIndex(d, i).dx)
    .attr("dy", (d, i) => getAttributesForIndex(d, i).dy)
    .attr("text-anchor", textAnchor)
    .attr("transform", (d, i) => {
    const xPos = getAttributesForIndex(d, i).xPos;
    const yPos = getAttributesForIndex(d, i).yPos;
    const rotateValue = getAttributesForIndex(d, i).rotate;
    return `rotate(${rotateValue}, ${xPos}, ${yPos})`;
  })
    .style("font-size", fontSize)
    .style("font-style", fontStyle)
    .style("font-family", fontFamily)
    .text(d => d[label])
    .each(function(d, i) {

      const currentElement = d3.select(this);
      const attributes = getAttributesForIndex(d, i);

     if (attributes.adjustLabels) {
        adjustSpecificLabel(self, "text.label", currentElement.attr("id"), attributes.labelAdjustmentOptions);
     }
      // Set additional options as attributes
      setAttributesFromOptions(currentElement, additionalOptions);
      // Override with itemStyle based on the index
      applyStyleToElement(currentElement, itemStyle, i);

      // Adjust labels if needed
     if (attributes.labelAdjustmentOptions) {
        console.log(attributes.labelAdjustmentOptions)
        const { rotation, dx, dy } = attributes.labelAdjustmentOptions;
        const x = parseFloat(currentElement.attr('x'));
        const y = parseFloat(currentElement.attr('y'));

        currentElement
          .attr("dx", dx)
          .attr("dy", dy)
          .attr("transform", `rotate(${rotation}, ${x}, ${y})`);
      }

    });

  return this;
};

function legendContainer(svg, margin, width, height) {
  this.svg = svg;
  this.margin = margin;
  this.width = width;
  this.height = height;
}

function createLegendContainer(targetElementId, options = {}) {

  const defaultOptions = {
    id: "svg-legend-container",
    margin: { top: 0, right: 0, bottom: 0, left: 0 },
    backgroundColor: "white",
    width: null,
    height: null
  };

  const mergedOptions = {
    ...defaultOptions,
    ...options,
    margin: { ...defaultOptions.margin, ...options.margin }
  };

  const { id, backgroundColor, width, height, margin } = mergedOptions;

  var svg = d3.select(targetElementId)
    .append("svg")
    .attr("id", getUniqueId(id))
    .attr("width", "100%")
    .attr("height", "100%")
    .attr("preserveAspectRatio", "xMinYMin meet")
    .attr("viewBox", `0 0 ${width} ${height}`)
    .classed("GCVieweR-svg-content", true)
    .style("background-color", backgroundColor);

  return new legendContainer(svg, margin, width, height);
}

legendContainer.prototype.legendData = function (data) {

  this.data = [...new Set(data)];

  return this;

};

legendContainer.prototype.legend = function(group, show = true, options = {}) {

  if (!show) {
    return this;
  }

  const defaultOptions = {
    x: 10,
    y: 10,
    orientation: "horizontal",
    adjustHeight: true,
    labels: null, // Add labels option here
    legend: {
      colorScheme: null,
      customColors: null
    },
    legendText: {
      textAnchor: "start",
      dy: ".35em",
      fontSize: "12px",
      fontFamily: "sans-serif"
    }
  };

  // If theme options exist, use them as the default options
  if (this.themeOptions && this.themeOptions.legendOptions) {
    options = { ...this.themeOptions.legendOptions, ...options };
  }

  const combinedOptions = {
    ...defaultOptions,
    ...options,
    legend: { ...defaultOptions.legend, ...options.legend },
    legendText: { ...defaultOptions.legendText, ...options.legendText }
  };

  const { x, y, orientation, adjustHeight, legend, legendText, labels } = combinedOptions;

  // Extract additional options that are not in defaultOptions
  const additionalOptionsLegend = extractAdditionalOptions(legend, defaultOptions.legend);
  const additionalOptionsLegendText = extractAdditionalOptions(legendText, defaultOptions.legendText);

  const svgLegend = this.svg;
  const parentWidth = svgLegend.node().getBoundingClientRect().width;

  // Create the group taking into account the margins
  var g = svgLegend.append("g")
    .attr("transform", `translate(${this.margin.left},${this.margin.top})`);

  // Use labels if provided, otherwise extract unique values from the group key
  const uniqueGroups = labels || [...new Set(this.data.map(d => d[group]))];

  if (!uniqueGroups.length) {
    console.error(`Error: No labels provided and the group "${group}" does not exist in the data.`);
    return;
  }

  // Adjust colorScale to use uniqueGroups
  let colorScale;
    if (legend.colorScheme) {
      if (!d3[legend.colorScheme]) {
          console.warn(`Warning: The color scheme "${legend.colorScheme}" does not exist. Defaulting to black.`);
          colorScale = d3.scaleOrdinal()
              .domain(uniqueGroups)
              .range(uniqueGroups.map(() => "black")); // Set all colors to black
      } else {
          colorScale = d3.scaleOrdinal(d3[legend.colorScheme])
              .domain(uniqueGroups);
      }
  } else if (legend.customColors && legend.customColors.length > 0) {
      colorScale = d3.scaleOrdinal()
          .domain(uniqueGroups)
          .range(legend.customColors);
  } else {
      colorScale = d3.scaleOrdinal(d3.schemeCategory10)
          .domain(uniqueGroups);
  }

  const legendSize = parseFloat(legendText.fontSize);
  const legendPadding = legendSize / 2;
  let currentX = x;
  let currentY = y;

  g.selectAll(".legend")
    .data(uniqueGroups)
    .enter()
    .append("g")
    .attr("class", "legend")
    .each((d, i, nodes) => {
      const textElement = nodes[i];

      const textLabel = d3.select(textElement)
        .append("text")
        .attr("dy", legendText.dy)
        .style("text-anchor", legendText.textAnchor)
        .style("font-size", legendText.fontSize)
        .style("font-family", legendText.fontFamily)
        .text(d)
        .each(function() {
          const currentElement = d3.select(this);
          setAttributesFromOptions(currentElement, additionalOptionsLegendText);
        });

      const textLength = textLabel.node().getComputedTextLength();

      if (currentX + textLength + legendSize + 2 * legendPadding > parentWidth - this.margin.left - this.margin.right) {
        currentX = x;
        currentY += legendSize + legendPadding;
      }

      textLabel
        .attr("x", currentX + legendSize + legendPadding)
        .attr("y", currentY + legendSize / 2);

      d3.select(textElement)
        .append("rect")
        .attr("x", currentX)
        .attr("y", currentY)
        .attr("width", legendSize)
        .attr("height", legendSize)
        .style("fill", colorScale(d))
        .each(function() {
          const currentElement = d3.select(this);
          setAttributesFromOptions(currentElement, additionalOptionsLegend);
        });

      if (orientation === "horizontal") {
        currentX += textLength + legendSize + 2 * legendPadding;
      } else {
        currentX = x;
        currentY += legendSize + legendPadding;
      }
    });

  // Adjust height
  if (adjustHeight && this.height === 0) {
    var contentHeight = currentY + legendSize + legendPadding;
    svgLegend.attr("height", contentHeight);
    var viewBoxWidth = svgLegend.node().getBoundingClientRect().width;
    svgLegend.attr("viewBox", `0 0 ${viewBoxWidth} ${contentHeight}`);
  }

  return this;
};
