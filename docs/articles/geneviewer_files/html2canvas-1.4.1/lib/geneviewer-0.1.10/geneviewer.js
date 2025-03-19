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

// Utils

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

  return Math.floor(resultSize);
}

function adjustGeneLabels(container, labelSelector, options = {}) {
  // Default options
  const defaultOptions = {
    rotation: 65, // Rotation angle (in degrees)
    dx: "-0.8em", // Horizontal adjustment
    dy: "0.15em", // Vertical adjustment
  };

  // Merge default options with the provided options
  const { rotation, dx, dy } = { ...defaultOptions, ...options };

  // Select all the labels based on the provided selector
  var labels = container.svg.selectAll(".label").nodes();

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
  return container;
}

function adjustSpecificLabel(container, labelSelector, elementId, options = {}) {
  // Default options
  const defaultOptions = {
    rotation: -65, // Rotation angle (in degrees)
    offsetX: 0,
    offsetY: 0,
    dx: "0em", // Horizontal adjustment
    dy: "0em", // Vertical adjustment
  };

  // Merge default options with the provided options
  const { rotation, offsetX, offsetY, dx, dy } = { ...defaultOptions, ...options };

  const overlapPercentage = (rect1, rect2) => {
    const x_overlap = Math.max(0, Math.min(rect1.right, rect2.right) - Math.max(rect1.left, rect2.left));
    const y_overlap = Math.max(0, Math.min(rect1.bottom, rect2.bottom) - Math.max(rect1.top, rect2.top));
    const overlapArea = x_overlap * y_overlap;
    const rect1Area = (rect1.right - rect1.left) * (rect1.bottom - rect1.top);
    return (overlapArea / rect1Area) * 100;
  };

  // Select all the labels based on the provided selector
  var labels = container.svg.selectAll(labelSelector).nodes();

  // Select the specific label using the provided elementId
  var specificLabel = container.svg.select(`#${elementId}`).node();

  // Calculate the label's original center position
  const bbox = specificLabel.getBBox();
  var centerX = bbox.x + bbox.width / 2;
  var centerY = bbox.y;

  centerX += offsetX
  centerY += (rotation < 0) ? bbox.height : bbox.height / 2;
  centerY += offsetY;

  // Check for overlap with other labels
  for (var i = 0; i < labels.length; i++) {
    if (labels[i] !== specificLabel) { // Ensure we're not comparing the label with itself
      var labelRect = labels[i].getBoundingClientRect();
      var specificLabelRect = specificLabel.getBoundingClientRect();

      // If the specific label overlaps with another label
      if (overlapPercentage(specificLabelRect, labelRect) > 0) {

        // Adjust the label rotation and position
        d3.select(specificLabel)
          .style("text-anchor", "start")
          .attr("dx", dx)
          .attr("dy", dy)
          .attr("x", centerX + offsetX)
          .attr("y", centerY)
          .attr("transform", `rotate(${rotation}, ${centerX}, ${centerY})`);

        // Break out of the loop once we've adjusted the specific label
        break;
      }
    }
  }
  return container;
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

function setStyleFromOptions(currentElement, additionalOptions) {
  for (const [key, value] of Object.entries(additionalOptions)) {
    currentElement.style(camelToKebab(key), value);
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

function removeNullKeys(obj) {
  const cleanObj = { ...obj };
  for (let key in cleanObj) {
    if (cleanObj[key] === null) {
      delete cleanObj[key];
    }
  }
  return cleanObj;
}

function mergeOptions(defaultOptions, themeOptionsKey, userOptions) {
  // Start with default options
  let combinedOptions = { ...defaultOptions };
  let themeOpts = removeNullKeys(this.themeOptions?.[themeOptionsKey] ?? {});

  userOptions = removeNullKeys(userOptions);

  // Iterate over the keys in defaultOptions
  for (let key in defaultOptions) {
    if (typeof defaultOptions[key] === 'object' && !Array.isArray(defaultOptions[key]) && defaultOptions[key] !== null) {
      combinedOptions[key] = {
        ...defaultOptions[key],
        ...(themeOpts[key] || {}), // Safeguard against undefined values
        ...(userOptions[key] || {}) // Safeguard against undefined values
      };
    } else {
      // Direct merge for non-object or null properties
      combinedOptions[key] = userOptions[key] !== undefined ? userOptions[key] : (themeOpts[key] !== undefined ? themeOpts[key] : defaultOptions[key]);
    }
  }

  return combinedOptions;
}

function getColorScale(colorScheme, customColors, uniqueGroups) {
  let colorScale;

  const schemeCategory30 = [
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
    "#393b79", "#637939", "#8c6d31", "#843c39", "#7b4173",
    "#bd9e39", "#ad494a", "#d6616b", "#31a354", "#cedb9c",
    "#9c9ede", "#637939", "#8ca252", "#b5cf6b", "#cedb9c",
    "#8c6d31", "#e7cb94", "#e7969c", "#7b4173", "#a55194"
  ];

  if (customColors && typeof customColors === "string") {
    // Handle customColors as a single color
    colorScale = d3.scaleOrdinal()
      .domain(uniqueGroups)
      .range(uniqueGroups.map(() => customColors));
  } else if (customColors && Array.isArray(customColors)) {
    // Handle customColors as an array
    colorScale = d3.scaleOrdinal()
      .domain(uniqueGroups)
      .range(uniqueGroups.map((group, index) => customColors[index % customColors.length] || "#FFF"));
  } else if (customColors && typeof customColors === 'object') {
    // Handle customColors as an object
    colorScale = d3.scaleOrdinal()
      .domain(uniqueGroups)
      .range(uniqueGroups.map(group => customColors[group] || "#FFF"));
  } else if (colorScheme) {
    if (!d3[colorScheme]) {
      console.warn(`Warning: The color scheme "${colorScheme}" does not exist. Defaulting to schemeCategory30.`);
      colorScale = d3.scaleOrdinal(schemeCategory30).domain(uniqueGroups);
    } else {
      colorScale = d3.scaleOrdinal(d3[colorScheme])
        .domain(uniqueGroups);
      // Check if uniqueGroups are more than the colors in the colorScale
      if (uniqueGroups.length > colorScale.range().length) {
        console.warn(`Warning: More unique groups than colors. Some colors will repeat.`);
      }
    }
  } else {
    // Default to schemeCategory30
    colorScale = d3.scaleOrdinal(schemeCategory30)
      .domain(uniqueGroups)
      .range(uniqueGroups.map(group =>
        group === "No Hit" || group === null ? "#FFF" : schemeCategory30[uniqueGroups.indexOf(group) % schemeCategory30.length]
      ));
  }

  return colorScale;
}


function isInAnyDiscontinuity(value, breaks) {
  for (let gap of breaks) {
    if (value >= gap.start && value <= gap.end) {
      return true;
    }
  }
  return false;
}

function createDiscontinuousScale(minStart, maxEnd, width, margin, breaks, reverse = false) {
  let totalGap = 0;

  // Calculate the total gap based on all discontinuities
  for (let gap of breaks) {
    if (gap.start >= minStart && gap.end <= maxEnd) {
      totalGap += (gap.end - gap.start);
    }
  }

  // Define the linear scale. Adjust the scale based on the reverse option.
  let domainStart = reverse ? maxEnd - totalGap : minStart;
  let domainEnd = reverse ? minStart : maxEnd - totalGap;

  const linearScale = d3.scaleLinear()
    .domain([domainStart, domainEnd])
    .range([0, width - margin.left - margin.right]);

  // Proxy object for discontinuous scale
  const scaleProxy = function (value) {
    if (isInAnyDiscontinuity(value, breaks)) {
      return null;
    }

    let cumulativeAdjustment = 0;

    // Adjust the value by all previous discontinuities
    for (let gap of breaks) {
      if (value > gap.end) {
        cumulativeAdjustment += (gap.end - gap.start);
      } else {
        break;
      }
    }

    // Apply reverse logic to the value adjustment
    value = value - cumulativeAdjustment;

    return linearScale(value);
  };

  // Dynamically copy all methods and properties from linearScale to scaleProxy
  for (let prop in linearScale) {
    if (typeof linearScale[prop] === 'function') {
      scaleProxy[prop] = (...args) => {
        const result = linearScale[prop](...args);
        return result === linearScale ? scaleProxy : result;
      };
    } else {
      scaleProxy[prop] = linearScale[prop];
    }
  }

  return scaleProxy;  // Return the discontinuous scale
}

function parseAndStyleText(text, parentElement, fontOptions) {
  const tagRegex = /<([biu])>(.*?)<\/\1>/g;

  let lastIndex = 0;

  // Helper function to append text with specific styles
  const appendText = (content, isBold = false, isItalic = false, isUnderline = false) => {
    const tspan = parentElement.append("tspan")
      .style("font-weight", isBold ? "bold" : fontOptions.weight)
      .style("font-style", isItalic ? "italic" : fontOptions.style)
      .text(content);

    if (isUnderline) {
      tspan.style("text-decoration", "underline");
    }
  };

  // Iterate through the string and apply styles
  text.replace(tagRegex, function (match, tag, content, offset) {
    // Append text before the tag
    if (offset > lastIndex) {
      appendText(text.substring(lastIndex, offset), false, false, false);
    }

    // Apply style based on the tag
    appendText(content, tag === 'b', tag === 'i', tag === 'u');

    lastIndex = offset + match.length;
    return match; // This return is not used, but is necessary for the replace function
  });

  // Append any remaining text after the last tag
  if (lastIndex < text.length) {
    appendText(text.substring(lastIndex), false, false, false);
  }
}

function container(svg, margin, width, height) {
  this.svg = svg;
  this.margin = margin;
  this.width = width;
  this.height = height;
}

function createContainer(targetElementId, id, themeOptionsKey, options = {}) {

  const defaultOptions = {
    id: id || "svg-container",
    margin: { top: 5, right: 50, bottom: 5, left: 50 },
    style: {
      backgroundColor: "#0000"
    },
    width: null,
    height: null
  };

  // Merge default options and user-specified options
  const combinedOptions = mergeOptions.call(this, defaultOptions, themeOptionsKey, options);
  const { id: containerId, margin: originalMargin, style, width, height } = combinedOptions;

  // Extract additional options that are not in defaultOptions
  const additionalOptionsStyle = extractAdditionalOptions(style, defaultOptions.style);

  // Compute margins without modifying the original margin object
  const computedMargin = {
    top: computeSize(originalMargin?.top ?? 0, height),
    right: computeSize(originalMargin?.right ?? 0, width),
    bottom: computeSize(originalMargin?.bottom ?? 0, height),
    left: computeSize(originalMargin?.left ?? 0, width)
  };

  var svg = d3.select(targetElementId)
    .append("svg")
    .attr("id", getUniqueId(containerId))
    .attr("width", width || "100%")
    .attr("height", height || "100%")
    .attr("preserveAspectRatio", "xMinYMin meet")
    .attr("viewBox", `0 0 ${width} ${height}`)
    .classed("geneviewer-svg-content", true)
    .style("box-sizing", "border-box")
    .style("background-color", style.backgroundColor)
    .each(function () {
      const currentElement = d3.select(this);
      setStyleFromOptions(currentElement, additionalOptionsStyle);
    });

  // Apply styles from the combined options
  Object.entries(style).forEach(([key, value]) => {
    svg.style(key, value);
  });

  return new container(svg, computedMargin, width, height);
}

function addScalePadding(startValue, endValue, padding, to) {
  let paddingValue;

  // Check if padding is a percentage string
  if (typeof padding === 'string' && padding.endsWith('%')) {
    paddingValue = parseFloat(padding) / 100;
  }
  // Check if padding is a number between 0 and 100
  else if (typeof padding === 'number' && padding >= 0 && padding <= 100) {
    paddingValue = padding / 100;
  }
  // If padding is not in a valid format or range, log a warning and return the original value
  else {
    console.warn('Invalid padding format. Padding should be a percentage (as a string or a number between 0 and 100). No padding applied.');
    return to === 'start' ? startValue : endValue;
  }

  // Calculate the total range
  const totalRange = Math.abs(endValue - startValue);

  // Calculate the actual padding value based on the total range
  paddingValue = totalRange * paddingValue;

  // Adjust the value based on whether we're padding the start or end
  if (to === 'start') {
    return startValue - paddingValue; // Subtract padding from the start
  } else if (to === 'end') {
    return endValue + paddingValue; // Add padding to the end
  } else {
    console.error('Invalid "to" parameter in addScalePadding. Must be "start" or "end".');
    return to === 'start' ? startValue : endValue; // Return the original value if 'to' is neither
  }
}

function createSaveIcon(widgetId, el, show = true) {

  if (show) {
      var icon = d3.select(el).append("i")
        .attr("class", "fa-solid fa-download")
        .attr("id", `saveIcon-${widgetId}`)
        .style("position", "absolute")
        .style("z-index", "1000")
        .style("top", "10px")
        .style("right", "10px")
        .style("display", "none")
        .style("color", "#D3D3D3")
        .style("padding", "5px")
        .style("cursor", "pointer");

    // Create tooltip
    var tooltip = d3.select(el).append("div")
        .attr("id", `saveIconTooltip-${widgetId}`)
        .attr("class", "save-tooltip")
        .style("position", "absolute")
        .style("top", "40px")
        .style("right", "10px")
        .style("display", "none")
        .style("background-color", "#333")
        .style("color", "#fff")
        .style("padding", "5px")
        .style("border-radius", "3px")
        .style("font-size", "12px")
        .style("font-family", "Arial, sans-serif")
        .style("z-index", "1001")
        .style("pointer-events", "none")
        .text("Save as PNG");

    // Ensure icon is displayed on hover and change color
    d3.select(el).on("mouseenter", function() {
        d3.select(`#saveIcon-${widgetId}`).style("display", "block");
    }).on("mouseleave", function() {
        d3.select(`#saveIcon-${widgetId}`).style("display", "none");
    });

    // Change icon color on hover and display tooltip
    d3.select(`#saveIcon-${widgetId}`).on("mouseenter", function() {
        d3.select(this).style("color", "#555555");
        tooltip.style("display", "block");
    }).on("mouseleave", function() {
        d3.select(this).style("color", "#D3D3D3");
        tooltip.style("display", "none");
    });

    // Add save icon functionality
    document.getElementById(`saveIcon-${widgetId}`).addEventListener("click", function () {
        // Hide the icon and tooltip temporarily
        var iconElement = document.getElementById(`saveIcon-${widgetId}`);
        var tooltipElement = document.getElementById(`saveIconTooltip-${widgetId}`);
        iconElement.style.display = 'none';
        tooltipElement.style.display = 'none';

        html2canvas(el, { backgroundColor: null }).then(canvas => {
            canvas.toBlob(function (blob) {
                saveAs(blob, "graph.png");
            });
        });
    });
  }
}

// Make links function

function getLinkCoordinates(graphContainer, data) {

    const links = data.map(item => {
        // Construct the selectors from the data
        const selector1 = `.link-marker[cluster='${item.cluster1}'][linkID='${item.linkID}'][position='${item.start1}']`;
        const selector2 = `.link-marker[cluster='${item.cluster2}'][linkID='${item.linkID}'][position='${item.start2}']`;
        const selector3 = `.link-marker[cluster='${item.cluster1}'][linkID='${item.linkID}'][position='${item.end1}']`;
        const selector4 = `.link-marker[cluster='${item.cluster2}'][linkID='${item.linkID}'][position='${item.end2}']`;

        // Get the elements within graphContainer using D3
        const element1 = graphContainer.select(selector1).node();
        const element2 = graphContainer.select(selector2).node();
        const element3 = graphContainer.select(selector3).node();
        const element4 = graphContainer.select(selector4).node();

        // Check if elements exist
        if (!element1 || !element2 || !element3 || !element4) {
            return null;
        }

        // Get bounding rectangles
        const rect1 = element1.getBoundingClientRect();
        const rect2 = element2.getBoundingClientRect();
        const rect3 = element3.getBoundingClientRect();
        const rect4 = element4.getBoundingClientRect();

        // Adjust the coordinates relative to the graphContainer's position
        const containerRect = graphContainer.node().getBoundingClientRect();
        const adjustRect = rect => ({
            x: rect.left - containerRect.left,
            y: rect.top - containerRect.top
        });

        // Determine the strand based on the x-coordinate positions
        const strand1 = item.start1 <= item.end1 ? "forward" : "reverse";
        const strand2 = item.start2 <= item.end2 ? "forward" : "reverse";
        const groupColor1 = element1.getAttribute("groupColor")
        const groupColor2 = element2.getAttribute("groupColor")

        return [
            { startPoint: adjustRect(rect1), endPoint: adjustRect(rect2), strand: strand1, groupColor: groupColor1 },
            { startPoint: adjustRect(rect3), endPoint: adjustRect(rect4), strand: strand2, groupColor: groupColor2 }
        ];
    });

    return links.filter(link => link !== null);
};

function makeLinks(graphContainer, links, clusters) {

    if (!links || links.length === 0) {
        return;
    }
    // Default options for title and subtitle
    const defaultOptions = {
      curve: true,
      invertedColor: "red",
      normalColor: "blue",
      useGroupColors: false,
      color: "lightgrey",
      measure: "identity",
      label: true,
      linkStyle: {
        stroke: "none",
        fillOpacity: 0.8
      },
      labelStyle: {
        fontSize: "10px",
        fontStyle: "normal",
        fontFamily: "sans-serif",
        cursor: "default",
        textAnchor: "middle",
        alignmentBaseline: "middle"
      }
    };

  const graphRect = graphContainer.node().getBoundingClientRect();
  // Create a container div for the SVG
  const svgContainer = graphContainer.insert("div", ":first-child")
        .style("position", "relative")
        .style("width", "100%")
        .style("height", "100%");


  // Create an SVG element inside graphContainer
  var lineSvg = svgContainer.insert("svg", ":first-child")
      .attr("width", graphRect.width)
      .attr("height", graphRect.height)
      .classed("GeneLink", true)
      .style("position", "absolute")
      .style("z-index", -1)
      .style("left", `${graphContainer.left}px`)
      .style("top", `${graphContainer.top}px`);

    links.forEach(function(link) {

        const combinedOptions = mergeOptions(defaultOptions, "linkOptions", link.options)
        const { curve, invertedColor, normalColor, useGroupColors, color, linkStyle, labelStyle, label, measure } = combinedOptions;

        const additionalOptionsLinkStyle = extractAdditionalOptions(linkStyle, defaultOptions.linkStyle);
        const additionalOptionsLabelStyle = extractAdditionalOptions(labelStyle, defaultOptions.labelStyle);

        const linkData = HTMLWidgets.dataframeToD3(link.data)
        const coordinates = getLinkCoordinates(graphContainer, linkData);

      // Add svg paths
      coordinates.forEach(function(coordinate, index) {

                const baseColor = coordinate[0].strand == coordinate[1].strand ? d3.rgb(normalColor) : d3.rgb(invertedColor)
                var colorScale = d3.scaleSequential(t => d3.interpolate("#FFFFFF", baseColor)(t))
                    .domain([0, 100]);
                const identity = combinedOptions.measure === "none" ? 100 : (link.data?.[measure]?.[index] ?? 100);
                const linkColor = useGroupColors ? coordinate[0].groupColor : colorScale(identity)

            lineSvg.append("path")
                .attr("d", createLinkerPath(coordinate[0], coordinate[1], curve))
                .style("fill", linkColor)
                .style("stroke", linkStyle.stroke)
                .style("fill-opacity", linkStyle.fillOpacity)
                .classed("GeneLink", true)
                .each(function () {
                    const currentElement = d3.select(this);
                    setStyleFromOptions(currentElement, additionalOptionsLinkStyle);
                });

      // Add identity label
      linkData.forEach(function(data, index){

        const linkLabelselector = `.link-text[cluster='${data.cluster2}'][linkID='${data.linkID}']`;
        const labelContainer = graphContainer.select(linkLabelselector);

        if (!labelContainer.empty() && identityLabel) {
            let currentX = labelContainer.attr("x");
                let currentY = labelContainer.attr("y");

                if (additionalOptionsLabelStyle.x !== undefined) {
                    currentX = parseFloat(currentX) + parseFloat(additionalOptionsLabelStyle.x);
                }
                if (additionalOptionsLabelStyle.y !== undefined) {
                    currentY = parseFloat(currentY) + parseFloat(additionalOptionsLabelStyle.y);
                }

        labelContainer
            .attr("x", currentX)
            .attr("y", currentY)
            .style("font-size", labelStyle.fontSize)
            .style("font-style", labelStyle.fontStyle)
            .style("font-family", labelStyle.fontFamily)
            .style("cursor", labelStyle.cursor)
            .attr("text-anchor", labelStyle.textAnchor)
            .attr("alignment-baseline", labelStyle.alignmentBaseline)
            .each(function () {
                const currentElement = d3.select(this);
                setStyleFromOptions(currentElement, additionalOptionsLabelStyle);
            });
        } else { // If identityLabel is FALSE, remove the labelContainer
          labelContainer.remove();
        }

      });

    });
  });

}

function createLinkerPath(link1, link2, curve = true) {
  var path = d3.path();

  if(curve){
  var midY1 = (link1.startPoint.y + link1.endPoint.y) / 2;
      path.moveTo(link1.startPoint.x, link1.startPoint.y);
      path.bezierCurveTo(link1.startPoint.x, midY1, link1.endPoint.x, midY1, link1.endPoint.x, link1.endPoint.y);

      // Line to second curve's end point
      path.lineTo(link2.endPoint.x, link2.endPoint.y);

      // Second Bezier curve in reverse
      var midY2 = (link2.startPoint.y + link2.endPoint.y) / 2;
      path.bezierCurveTo(link2.endPoint.x, midY2, link2.startPoint.x, midY2, link2.startPoint.x, link2.startPoint.y);


  } else {
        path.moveTo(link1.startPoint.x, link1.startPoint.y);
        path.lineTo(link1.endPoint.x, link1.endPoint.y);
        path.lineTo(link2.endPoint.x, link2.endPoint.y);
        path.lineTo(link2.startPoint.x, link2.startPoint.y);
  }
  // Close the path
  path.closePath();

      return path.toString();
};

function getClusterLinks(data, cluster) {

    if (!data || data.length === 0) {
        return [];
    }

    const linksCluster1 = data.filter(item => item.cluster1 === cluster);
    const linksCluster2 = data.filter(item => item.cluster2 === cluster);
    const clusterLinks = linksCluster1.concat(linksCluster2);

    return clusterLinks;
}

function allStrandsEqual(data) {
    data.strand1 = data.strand1 || [];
    data.strand2 = data.strand2 || [];
    let allStrandsEqual = true;
    let previousStrand1, previousStrand2;

    for (let i = 0; i < data.start1.length; i++) {
        const strand1 = data.start1[i] > data.end1[i] ? "forward" : "reverse";
        const strand2 = data.start2[i] > data.end2[i] ? "forward" : "reverse";

        data.strand1[i] = strand1;
        data.strand2[i] = strand2;

        if (i > 0 && (strand1 !== previousStrand1 || strand2 !== previousStrand2)) {
            allStrandsEqual = false;
        }

        previousStrand1 = strand1;
        previousStrand2 = strand2;
    }

    return allStrandsEqual;
}

function makeColorBar(graphContainer, links) {

    if (links[0].options.colorBar === false){
      return null;
    }
    if (links[0].options.measure === 'none'){
      return null;
    }
    if (!links[0].data.hasOwnProperty('identity') && !links[0].data.hasOwnProperty('similarity')) {
      return null;
    }

    // Default options for the color bars (both normal and inverted)
    const defaultOptions = {
        invertedColor: "#d62728",
        normalColor: "#969696",
        measure: "identity",
        margin: { top: 5, right: 25, bottom: 0, left: 50 },
        colorBarOptions: {
            x: 0,
            y: 24,
            title: true,
            width: 10,
            height: 60,
            labelOptions: {
              fontSize: 8,
              xOffset: 2,
              yOffset: 0
            },
            titleOptions: {
              fontSize: 10,
              xOffset: 2,
              yOffset: 0
            },
            barOptions: {
              stroke: "#000",
              strokeWidth: 0.5,
              opacity: 1
            },
        }
    };

    // Extract user-supplied options and merge them with the defaults
    const linkOptions = { ...defaultOptions, ...links[0].options };
    const colorBarOptions = { ...defaultOptions.colorBarOptions, ...links[0].options.colorBarOptions };

    // Extract sub-options for labels, titles, and bars
    const labelOptions = { ...defaultOptions.colorBarOptions.labelOptions, ...colorBarOptions.labelOptions };
    const titleOptions = { ...defaultOptions.colorBarOptions.titleOptions, ...colorBarOptions.titleOptions };
    const barOptions = { ...defaultOptions.colorBarOptions.barOptions, ...colorBarOptions.barOptions };

    // Assuming `extractAdditionalOptions` is a function that extracts non-default options
    const additionalOptionsLabel = extractAdditionalOptions(labelOptions, defaultOptions.colorBarOptions.labelOptions);
    const additionalOptionsTitle = extractAdditionalOptions(titleOptions, defaultOptions.colorBarOptions.titleOptions);
    const additionalOptionsBar = extractAdditionalOptions(barOptions, defaultOptions.colorBarOptions.barOptions);


    const graphRect = graphContainer.node().getBoundingClientRect();

    // Create a container div for the SVG
    const svgContainer = graphContainer.insert("div", ":first-child")
        .style("position", "relative")
        .style("width", "100%")
        .style("height", "100%");

    // Create an SVG element inside the newly created container
    const svg = svgContainer.append("svg")
        .attr("width", graphRect.width)
        .attr("height", graphRect.height)
        .classed("color-bar", true)
        .style("position", "absolute");

    // Append a group element to the SVG and transform based on margins
    const g = svg.append("g")
        .attr("transform", `translate(${linkOptions.margin.left}, ${linkOptions.margin.top})`);

    // Create a linear gradient for the normal bar
    const defs = g.append("defs");
    const forwardGradientId = getUniqueId("linear-gradient")
    const linearGradient = defs.append("linearGradient")
        .attr("id", forwardGradientId)
        .attr("gradientTransform", "rotate(90)");

    const identityArray = links.flatMap(link => link.data[linkOptions.measure]);
    const minValue = Math.round(Math.min(...identityArray));
    const maxValue = Math.round(Math.max(...identityArray));

    // Define a color scale for the normal gradient
    const colorScale = d3.scaleSequential(t => d3.interpolate("#FFF", linkOptions.normalColor)(t))
        .domain([0, 100]);

    // Set the gradient stops for the normal gradient
    linearGradient.append("stop")
        .attr("offset", "0%")
        .attr("stop-color", colorScale(maxValue));

    linearGradient.append("stop")
        .attr("offset", "100%")
        .attr("stop-color", colorScale(minValue));

    // Calculate dimensions for drawing the color bar
    const contentWidth = graphRect.width - linkOptions.margin.left - linkOptions.margin.right;
    const contentHeight = graphRect.height - linkOptions.margin.top - linkOptions.margin.bottom;

    const width = parseFloat(colorBarOptions.width);
    const height = parseFloat(colorBarOptions.height);

    const xPosition = contentWidth - width - colorBarOptions.x;
    const yPosition = contentHeight - height - colorBarOptions.y;

     // Add the title text with rotation
    g.append("text")
        .attr("x", xPosition + width + titleOptions.xOffset)
        .attr("y", yPosition + height / 2 + titleOptions.yOffset + 10)
        .attr("transform", `rotate(-90, ${xPosition + width + titleOptions.xOffset}, ${yPosition + height / 2 + titleOptions.yOffset})`)
        .attr("alignment-baseline", "bottom")
        .attr("text-anchor", "middle")
        .attr("font-size", titleOptions.fontSize)
        .text(links[0].options.measure)
        .each(function () {
            const currentElement = d3.select(this);
            setStyleFromOptions(currentElement, additionalOptionsTitle);
        });

    // Draw the normal color gradient bar with stroke and opacity
    g.append("rect")
        .attr("x", xPosition)
        .attr("y", yPosition)
        .attr("width", width)
        .attr("height", height)
        .style("fill", `url(#${forwardGradientId})`)
        .style("stroke", barOptions.stroke)
        .style("stroke-width", barOptions.strokeWidth)
        .style("opacity", barOptions.opacity)
        .each(function () {
            const currentElement = d3.select(this);
            setStyleFromOptions(currentElement, additionalOptionsBar);
        });

    // Draw labels for the normal gradient
    g.append("text")
        .attr("x", xPosition + width + labelOptions.xOffset)
        .attr("y", yPosition - labelOptions.yOffset)
        .attr("alignment-baseline", "middle")
        .attr("font-size", labelOptions.fontSize)
        .text(maxValue + '%')
        .each(function () {
            const currentElement = d3.select(this);
            setStyleFromOptions(currentElement, additionalOptionsLabel);
        });

    g.append("text")
        .attr("x", xPosition + width + labelOptions.xOffset)
        .attr("y", yPosition + height + labelOptions.yOffset)
        .attr("alignment-baseline", "middle")
        .attr("font-size", labelOptions.fontSize)
        .text(minValue + '%')
                .each(function () {
            const currentElement = d3.select(this);
            setStyleFromOptions(currentElement, additionalOptionsLabel);
        });

    // Draw the inverted bar only if the strands are not the same
    if (!allStrandsEqual(links[0].data)) {
        const reverseGradientId = getUniqueId("reverse-gradient")
        const reverseGradient = defs.append("linearGradient")
            .attr("id", reverseGradientId)
            .attr("gradientTransform", "rotate(90)");

        // Define a color scale for the inverted gradient
        const reverseColorScale = d3.scaleSequential(t => d3.interpolate("#FFF", linkOptions.invertedColor)(t))
            .domain([0, 100]);

        // Set the gradient stops for the inverted gradient
        reverseGradient.append("stop")
            .attr("offset", "0%")
            .attr("stop-color", reverseColorScale(maxValue));

        reverseGradient.append("stop")
            .attr("offset", "100%")
            .attr("stop-color", reverseColorScale(minValue));

        g.append("rect")
            .attr("x", xPosition - width)
            .attr("y", yPosition)
            .attr("width", width)
            .attr("height", height)
            .style("fill", `url(#${reverseGradientId})`)
            .style("stroke", barOptions.stroke)
            .style("stroke-width", barOptions.strokeWidth)
            .style("opacity", barOptions.opacity)
            .each(function () {
                const currentElement = d3.select(this);
                setStyleFromOptions(currentElement, additionalOptionsBar);
            });
    }
}

// Cluster functions

container.prototype.cluster = function (options = {}) {

  // Default options for title and subtitle
  const defaultOptions = {
    separateStrands: false,
    strandSpacing: 0,
    preventGeneOverlap: false,
    overlapSpacing: 5
  };

  const combinedOptions = mergeOptions.call(this, defaultOptions, 'clusterOptions', options);
  const { separateStrands, strandSpacing,  preventGeneOverlap, overlapSpacing} = combinedOptions;

  this.separateStrands = separateStrands;
  this.strandSpacing = strandSpacing;
  this.preventGeneOverlap = preventGeneOverlap;
  this.overlapSpacing = overlapSpacing;

  return this;
};

container.prototype.theme = function (themeName) {
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

container.prototype.geneData = function (data, clusterData) {

  // Needed to set color
  this.dataAll = data

  this.data = clusterData.map(item => {
    var newItem = { ...item };

    // Convert cluster to string
    newItem.cluster = String(newItem.cluster);

    newItem.direction = "forward";
    if (newItem.start > newItem.end) {
      newItem.direction = "reverse";
    }

    return newItem;
  });

  return this;
};

container.prototype.scale = function (options = {}) {
  // Verify that the data exists
  if (!this.data) {
    console.error('No data has been added to this cluster container.');
    return this;
  }

  // Default options specific for scales and axis
  const defaultScaleOptions = {
    start: null,
    end: null,
    xMin: null,
    xMax: null,
    padding: 2,
    hidden: true,
    reverse: false,
    axisPosition: "bottom",
    breaks: [],
    tickValues: null,
    ticksCount: 10,
    ticksFormat: ",.0f",
    y: null,
    tickStyle: {
      stroke: "grey",
      strokeWidth: 1,
      lineLength: 6
    },
    textStyle: {
      fill: "black",
      fontSize: "10px",
      fontFamily: "Arial",
      cursor: "default"
    },
    lineStyle: {
      stroke: "grey",
      strokeWidth: 1
    }
  };

  // Merge provided options with the default ones
  const combinedOptions = mergeOptions.call(this, defaultScaleOptions, 'scaleOptions', options);

  // De-structure the combined options
  const { start, end, xMin, xMax, padding, hidden, breaks,
          tickValues, reverse, axisPosition, ticksCount,
          ticksFormat, y: initialY, tickStyle, textStyle,
          lineStyle } = combinedOptions;

  // Determine y based on axisPosition and initialY
  const y = initialY !== null ? initialY : (axisPosition === 'bottom' ? 20 : 80);

  // Extract additional options that are not in defaultScaleOptions
  const additionalOptionsTickStyle = extractAdditionalOptions(tickStyle, defaultScaleOptions.tickStyle);
  const additionalOptionsTextStyle = extractAdditionalOptions(textStyle, defaultScaleOptions.textStyle);
  const additionalOptionslineStyle = extractAdditionalOptions(lineStyle, defaultScaleOptions.lineStyle);

  // Filter data based on the provided start and end values
  if (start !== null ) {
    this.data = this.data.filter(d => d.start >= start);
  }
  if (end !== null) {
    this.data = this.data.filter(d => d.end <= end);
  }

  // Filter out data where start or end falls within any of the breaks
  this.data = this.data.filter(d => {
    return !breaks.some(gap =>
      (d.start >= gap.start && d.start <= gap.end) ||
      (d.end >= gap.start && d.end <= gap.end)
    );
  });

  this.reverse = reverse;

  this.padding = padding;

  // Use provided start and end values if they exist, otherwise compute them from data
  this.minStart = start !== null ? start :
                xMin !== null ? xMin :
                d3.min(this.data, d => Math.min(d.start, d.end));

  this.maxEnd = end !== null ? end :
              xMax !== null ? xMax :
              d3.max(this.data, d => Math.max(d.start, d.end));

  if(start == null){
  this.minStart = addScalePadding(this.minStart, this.maxEnd, padding, to = "start")
  }

  if(end == null){
  this.maxEnd = addScalePadding(this.minStart, this.maxEnd, padding, to = "end")
  }

  // Create scales
  this.xScale = createDiscontinuousScale(this.minStart, this.maxEnd, this.width, this.margin, breaks, reverse);
  this.yScale = d3.scaleLinear()
    .domain([0, 100])
    .range([this.height - this.margin.bottom - this.margin.top, 0]);

  // Filter breaks within the scale range
  this.breaks = breaks.filter(gap => gap.start >= this.minStart && gap.end <= this.maxEnd);

  const that = this;
  if (!hidden) {

    // Create and configure the X-axis
    const adjustedYOffset = this.yScale ? this.yScale(y) : y;
    const axisGroup = this.svg.append("g")
      .attr("transform", `translate(${this.margin.left},${this.margin.top + adjustedYOffset})`);

    linearScale = d3.scaleLinear()
      .domain([this.minStart, this.maxEnd])
      .range([0, this.width - this.margin.left - this.margin.right]);

    const xAxis = d3.axisBottom(linearScale)
      .tickFormat(d3.format(ticksFormat));

    if (Array.isArray(tickValues) && tickValues.length > 0) {
      xAxis.tickValues(tickValues);
    } else if (typeof tickValues === 'number') {
      xAxis.tickValues([tickValues]);
    } else {
      xAxis.ticks(ticksCount);
    }

    const axis = axisGroup.append("g").call(xAxis);

    // Style axis lines and text
    axis.selectAll(".tick line")
      .style("stroke", tickStyle.stroke)
      .style("stroke-width", tickStyle.strokeWidth)
      .attr("y2", tickStyle.lineLength)
      .each(function () {
        const currentElement = d3.select(this)
        setStyleFromOptions(currentElement, additionalOptionsTickStyle);
      });

    axis.selectAll(".tick text")
      .style("fill", textStyle.fill)
      .style("font-size", textStyle.fontSize)
      .style("font-family", textStyle.fontFamily)
      .style("cursor", textStyle.cursor)
      .each(function () {
        const currentElement = d3.select(this)
        setStyleFromOptions(currentElement, additionalOptionsTextStyle);
      });

    axis.selectAll(".tick").each(function (d) {
      let tickValue = d3.select(this).data()[0];
      let newX = that.xScale(tickValue);

      if (newX === null) {
        // If the new X position is null, remove the tick
        d3.select(this).remove();
      } else {
        // Otherwise, update the transform attribute
        d3.select(this).attr("transform", `translate(${newX},0)`);
      }
    });

    axis.select(".domain")
      .style("stroke", lineStyle.stroke)
      .style("stroke-width", lineStyle.strokeWidth)
      .each(function () {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionslineStyle);
      });

  }
  return this;
};

container.prototype.title = function (title, subtitle, show = true, options = {}) {

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
    y: 20,
    align: "center",
    spacing: 20, // Default spacing between title and subtitle
    titleFont: {
      fontSize: "16px",
      fontStyle: "normal",
      fontWeight: "bold",
      textDecoration: "normal",
      fontFamily: "sans-serif",
      cursor: "default"
    },
    subtitleFont: {
      fontSize: "14px",
      fontStyle: "normal",
      fontWeight: "normal",
      textDecoration: "none",
      fontFamily: "sans-serif",
      cursor: "default"
    },
  };

  const combinedOptions = mergeOptions.call(this, defaultOptions, 'titleOptions', options);
  const { x, y, titleFont, subtitleFont, align, spacing } = combinedOptions;

  // Extract additional options that are not in defaultOptions
  const additionalOptionsTitleFont = extractAdditionalOptions(titleFont, defaultOptions.titleFont);
  const additionalOptionsSubtitleFont = extractAdditionalOptions(subtitleFont, defaultOptions.subtitleFont);

  let xPos;
  let textAnchor;

  // Determine text align and anchor based on the provided align
  switch (align) {
    case "left":
      xPos = x;
      textAnchor = "start";
      break;
    case "right":
      xPos = this.width - this.margin.left - this.margin.right + x;
      textAnchor = "end";
      break;
    default:
    const effectiveWidth = this.width - this.margin.left - this.margin.right;
    xPos = (effectiveWidth / 2) + x;
    textAnchor = "middle";
  }

  var g = this.svg.append("g")
    .attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);

  if (title) {
    // Add title to the SVG
    g.append("text")
      .attr("x", xPos)
      .attr("y", y + (this.margin.top / 2))
      .attr("text-anchor", textAnchor)
      .style("font-size", titleFont.fontSize)
      .style("font-style", titleFont.fontStyle)
      .style("font-weight", titleFont.fontWeight)
      .style("text-decoration", titleFont.textDecoration)
      .style("font-family", titleFont.fontFamily)
      .style("cursor", titleFont.cursor)
      .each(function () {
        const currentElement = d3.select(this);
        parseAndStyleText(title, currentElement, titleFont);
        setStyleFromOptions(currentElement, additionalOptionsTitleFont);
      });
  }

  if (subtitle) {
    // Add subtitle to the SVG
    g.append("text")
      .attr("x", xPos)
      .attr("y", y + (this.margin.top / 2) + spacing)
      .attr("text-anchor", textAnchor)
      .style("font-size", subtitleFont.fontSize)
      .style("font-style", subtitleFont.fontStyle)
      .style("font-weight", subtitleFont.fontWeight)
      .style("text-decoration", subtitleFont.textDecoration)
      .style("font-family", subtitleFont.fontFamily)
      .style("cursor", subtitleFont.cursor)
      .each(function () {
        const currentElement = d3.select(this);
        parseAndStyleText(subtitle, currentElement, subtitleFont);
        setStyleFromOptions(currentElement, additionalOptionsSubtitleFont);
      });
  }

  return this;
};

container.prototype.footer = function (title, subtitle, show = true, options = {}) {

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
    align: "left",
    spacing: 12, // Default spacing between title and subtitle
    titleFont: {
      fontSize: "12px",
      fontWeight: "bold",
      fontStyle: "normal",
      fontFamily: "sans-serif",
      cursor: "default"
    },
    subtitleFont: {
      fontSize: "10px",
      fontWeight: "normal",
      fontStyle: "normal",
      fontFamily: "sans-serif",
      cursor: "default"
    },
  };

  const combinedOptions = mergeOptions.call(this, defaultOptions, 'footerOptions', options);
  const { x, y, titleFont, subtitleFont, align, spacing } = combinedOptions;

  // Extract additional options that are not in defaultOptions
  const additionalOptionsTitleFont = extractAdditionalOptions(titleFont, defaultOptions.titleFont);
  const additionalOptionsSubtitleFont = extractAdditionalOptions(subtitleFont, defaultOptions.subtitleFont);

  let xPos;
  let textAnchor;

  // Determine text align and anchor based on the provided align
  switch (align) {
    case "left":
      xPos = x;
      textAnchor = "start";
      break;
    case "right":
      xPos = this.width - this.margin.left - this.margin.right + x;
      textAnchor = "end";
      break;
    default:
    const effectiveWidth = this.width - this.margin.left - this.margin.right;
    xPos = this.margin.left + (effectiveWidth / 2) + x;
    textAnchor = "middle";
  }


  // Calculate y align for title and subtitle based on the SVG height and bottom margin
  const titleYPos = this.height - this.margin.bottom + y - 20;
  const subtitleYPos = titleYPos + spacing;

  if (title) {
    this.svg.append("text")
      .attr("x", xPos)
      .attr("y", titleYPos)
      .attr("text-anchor", textAnchor)
      .style("font-size", titleFont.fontSize)
      .style("font-weight", titleFont.fontWeight)
      .style("font-style", titleFont.fontStyle)
      .style("font-family", titleFont.fontFamily)
      .style("cursor", titleFont.cursor)
      .each(function () {
        const currentElement = d3.select(this);
        parseAndStyleText(title, currentElement, titleFont);
        setStyleFromOptions(currentElement, additionalOptionsTitleFont);
      });

  }

  // Add subtitle to the SVG if provided
  if (subtitle) {
    this.svg.append("text")
      .attr("x", xPos)
      .attr("y", subtitleYPos)
      .attr("text-anchor", textAnchor)
      .style("font-size", subtitleFont.fontSize)
      .style("font-weight", subtitleFont.fontWeight)
      .style("font-style", subtitleFont.fontStyle)
      .style("font-family", subtitleFont.fontFamily)
      .style("cursor", subtitleFont.cursor)
      .each(function () {
        const currentElement = d3.select(this);
        parseAndStyleText(subtitle, currentElement, subtitleFont);
        setStyleFromOptions(currentElement, subtitleFont);
      });
  }

  return this;
};

container.prototype.clusterLabel = function (title, show = true, options = {}) {
  if (!show) {
    return this;
  }

  // Default options
  const defaultOptions = {
    x: 0,
    y: 0,
    position: 'left',
    wrapLabel: true,
    wrapOptions: {},
    fontSize: "12px",
    fontStyle: "normal",
    fontWeight: "bold",
    fontFamily: "sans-serif",
    cursor: "default"
  };

  // Merge the options using the generic function
  const combinedOptions = mergeOptions.call(this, defaultOptions, 'clusterLabelOptions', options);
  const {
    x,
    y,
    position,
    wrapLabel,
    wrapOptions,
    fontSize,
    fontStyle,
    fontWeight,
    fontFamily,
    cursor,
  } = combinedOptions;

  const additionalwrapOptions = extractAdditionalOptions(wrapOptions, defaultOptions.wrapOptions);
  // Extract additional options that are not in defaultOptions
  const additionalOptions = extractAdditionalOptions(combinedOptions, defaultOptions);

  const titleFont = {
    size: fontSize,
    style: fontStyle,
    weight: fontWeight,
    family: fontFamily,
    cursor: cursor
  };

  // calculate middle y position
  const adjustedHeight = this.height - this.margin.top - this.margin.bottom;
  const middleY = this.margin.top + adjustedHeight / 2 + y;
  const titleWidth = position === 'left' ? this.margin.left - x : this.margin.right - x;

  let xposition;
  if (position === 'left') {
    xposition = this.margin.left / 2 + x;  // title is in the left margin
  } else {  // 'right'
    xposition = this.width - this.margin.right / 2 - x;  // title is in the right margin
  }

  let clusterTitle = this.svg.append("text")
    .attr("x", xposition)
    .attr("y", middleY)
    .attr("text-anchor", "middle")  // text is always centered
    .attr("dominant-baseline", "central")  // Vertically center text
    .style("font-size", fontSize)
    .style("font-style", fontStyle)
    .style("font-weight", fontWeight)
    .style("font-family", fontFamily)
    .style("cursor", cursor)
    .each(function () {
      const currentElement = d3.select(this);

      if (!wrapLabel) {
        // Set the text and apply styles only if wrapLabel is false
        parseAndStyleText(title, currentElement, titleFont);
      } else {
        currentElement.text(title);
        // If wrapLabel is true, wrap the text
        wrap(currentElement, titleWidth, wrapOptions);
        currentElement.selectAll("tspan").each(function () {
          const currentTspan = d3.select(this);
          const tspanText = currentTspan.text();
          currentTspan.text('');
          parseAndStyleText(tspanText, currentTspan, titleFont);
        });
      }
      setStyleFromOptions(currentElement, additionalOptions);
    });

  return this;
};

container.prototype.sequence = function (show = true, options = {}) {

  if (!show) {
    return this;
  }

  if (!this.data) {
    console.error('No data has been added to this cluster container. Please use the addGeneData() function before attempting to draw a gene line.');
    return this;
  }

  const defaultOptions = {
    y: 50,
    start: null,
    end: null,
    sequenceStyle: { // Adding sequenceStyle
      stroke: "grey",
      strokeWidth: 1
    },
    markerStyle: {
      markerHeight: 10,
      stroke: "grey",
      strokeWidth: 1,
      tiltAmount: -5,
      gap: 0
    }
  };

  // Merge the default options with any predefined sequenceOptions and the provided options
  const combinedOptions = mergeOptions.call(this, defaultOptions, 'sequenceOptions', options);
  const { y, start, end, markerStyle, sequenceStyle } = combinedOptions;

  // Extract additional options that are not in defaultOptions for sequenceStyle
  const additionalOptionsSequence = extractAdditionalOptions(sequenceStyle, defaultOptions.sequenceStyle);

  var g = this.svg.append("g")
    .attr("transform", `translate(${this.margin.left},${this.margin.top})`);

  var minStart = start || d3.min(this.data, d => Math.min(d.start, d.end));
  var maxEnd = end || d3.max(this.data, d => Math.max(d.start, d.end));

  if(start == null){
  minStart = addScalePadding(minStart, maxEnd, this.padding, to = "start")
  }

  if(end == null){
  maxEnd = addScalePadding(minStart, maxEnd, this.padding, to = "end")
  }

  // Draw baseline with sequenceStyle
  g.append("line")
    .attr("class", "baseline")
    .attr("x1", this.xScale(minStart))
    .attr("y1", this.yScale(y))
    .attr("x2", this.xScale(maxEnd))
    .attr("y2", this.yScale(y))
    .style("stroke", sequenceStyle.stroke)
    .style("stroke-width", sequenceStyle.strokeWidth)
    .each(function () {
      const currentElement = d3.select(this);
      setStyleFromOptions(currentElement, additionalOptionsSequence);
    });

  // Draw break markers with tilted lines
  for (let gap of this.breaks) {
    const xStart = this.xScale(gap.start - 0.001) * (1 - markerStyle.gap / 100);
    const xEnd = this.xScale(gap.end + 0.001) * (1 + markerStyle.gap / 100);
    const yBase = this.yScale(y);
    const yTop = yBase - markerStyle.markerHeight / 2;
    const yBottom = yBase + markerStyle.markerHeight / 2;



    if (xStart !== null && xEnd !== null) {
      g.append("line")
        .attr("class", "gap-line")
        .attr("x1", xStart + (markerStyle.tiltAmount / 2))
        .attr("y1", yBase)
        .attr("x2", xEnd - (markerStyle.tiltAmount / 2))
        .attr("y2", yBase)
        .attr("stroke", "#0000")
        .style("stroke-width", sequenceStyle.strokeWidth * 1.1);
    }

    if (xStart !== null) {
      // Draw the tilted line before the gap
      g.append("line")
        .attr("x1", xStart)
        .attr("y1", yTop)
        .attr("x2", xStart + markerStyle.tiltAmount)
        .attr("y2", yBottom)
        .attr("stroke", markerStyle.stroke)
        .attr("stroke-width", markerStyle.strokeWidth);
    }

    if (xEnd !== null) {
      // Draw the tilted line after the gap
      g.append("line")
        .attr("x1", xEnd - markerStyle.tiltAmount)
        .attr("y1", yTop)
        .attr("x2", xEnd)
        .attr("y2", yBottom)
        .attr("stroke", markerStyle.stroke)
        .attr("stroke-width", markerStyle.strokeWidth);
    }
  }

  return this;
};

container.prototype.coordinates = function (show = true, options = {}) {
  if (!show) {
    return this;
  }

  const defaultOptions = {
    rotate: -45,
    yPositionTop: 53,
    yPositionBottom: 48,
    tickValuesTop: null,
    tickValuesBottom: null,
    overlapThreshold: 20,
    ticksFormat: ",.0f",
    tickStyle: {
      stroke: "black",
      strokeWidth: 1,
      lineLength: 6
    },
    textStyle: {
      fill: "black",
      fontSize: "10px",
      fontFamily: "Arial",
      cursor: "default",
      x: 0,
      y: 0
    }
  };

  const combinedOptions = mergeOptions.call(this, defaultOptions, 'coordinatesOptions', options);
  const { rotate, yPositionTop, yPositionBottom, tickValuesBottom, tickValuesTop, ticksFormat, tickStyle, textStyle } = combinedOptions;

  // Extract additional options that are not in defaultOptions
  const additionalOptionsTickStyle = extractAdditionalOptions(tickStyle, defaultOptions.tickStyle);
  const additionalOptionsTextStyle = extractAdditionalOptions(textStyle, defaultOptions.textStyle);


  const g = this.svg.append("g")
    .attr("transform", "translate(" + this.margin.left + "," + this.margin.top + ")");

  // Convert provided tickValues to the required format
  let tickValuesTopFinal = Array.isArray(tickValuesTop) ? tickValuesTop.map(value => ({ value, rowID: null }))
    : (tickValuesTop != null ? [{ value: tickValuesTop, rowID: null }] : []);
  let tickValuesBottomFinal = Array.isArray(tickValuesBottom) ? tickValuesBottom.map(value => ({ value, rowID: null }))
    : (tickValuesBottom != null ? [{ value: tickValuesBottom, rowID: null }] : []);

  // If neither tickValuesTop nor tickValuesBottom are provided, calculate them
  if (!tickValuesTop && !tickValuesBottom) {

    let allTickValues = this.data.reduce((acc, d) => {
      // Define tickValueStart and tickValueStop
      let tickValueStart = { value: d.start, rowID: d.rowID };
      let tickValueStop = { value: d.end, rowID: d.rowID };
      // Add strand property if it exists
      tickValueStart.strand = d.strand;
      tickValueStop.strand = d.strand;
      // Add geneTrack property if it exists
      if ('geneTrack' in d) {
        tickValueStart.geneTrack = d.geneTrack;
        tickValueStop.geneTrack = d.geneTrack;
      }


      acc.push(tickValueStart);
      acc.push(tickValueStop);

      return acc;
    }, []);

    // Remove duplicates based on the 'value' property
    allTickValues = allTickValues.filter((obj, index, self) =>
      index === self.findIndex((t) => t.value === obj.value)
    );

    allTickValues.sort((a, b) => a.value - b.value);

    if (this.separateStrands) {
      allTickValues.forEach(tickValue => {
        if (tickValue.strand === "forward") {
          tickValuesTopFinal.push(tickValue);
        } else {
          tickValuesBottomFinal.push(tickValue);
        }
      });
    } else {
         // Calculate overlap and distribute tick values between top and bottom
    const totalXValueRange = this.xScale(allTickValues[allTickValues.length - 1].value) - this.xScale(allTickValues[0].value);
    const tickValueThreshold = combinedOptions.overlapThreshold;

    for (let i = 0; i < allTickValues.length; i++) {
      if (i === 0) {
      // First tick always goes to the bottom
      tickValuesBottomFinal.push(allTickValues[i]);
      continue;
    }

  const diff = this.xScale(allTickValues[i].value) - this.xScale(allTickValues[i - 1].value);

  if (diff < tickValueThreshold) {
    // If the difference exceeds the threshold, place this tick at the top
    tickValuesTopFinal.push(allTickValues[i]);

    // Place the next tick at the bottom, if it exists
    if (i + 1 < allTickValues.length) {
      tickValuesBottomFinal.push(allTickValues[i + 1]);
      i++; // Skip the next index, as it's already processed
    }
    } else {
    // Otherwise, place this tick at the bottom
    tickValuesBottomFinal.push(allTickValues[i]);
      }
    }
  }
    }

  const self = this;

  // Create and configure the top axis
  const xAxisTop = g.append("g")
    .attr("transform", "translate(0," + this.yScale(yPositionTop) + ")")
    .call(d3.axisTop(this.xScale).tickValues(tickValuesTopFinal.map(t => t.value))
    .tickFormat(d3.format(ticksFormat))
    );

  xAxisTop.selectAll(".tick")
    .data(tickValuesTopFinal)
    .attr("rowID", d => d.rowID)
    .attr("transform", function (d) {
      const xOffset = self.xScale(d.value);
      var currentOverlapSpacing = d.geneTrack ? (d.geneTrack - 1) * self.geneOverlapSpacing : 0;
      return "translate(" + xOffset + "," + -currentOverlapSpacing + ")";
    });

  xAxisTop.select(".domain").attr("stroke", "none");

  xAxisTop.selectAll("text")
            .data(tickValuesTopFinal)
            .attr("class", "coordinate")
            .style("text-anchor", "end")
            .attr("dx", `${-0.8 + textStyle.x}em`)
            .attr("dy", `${0.4 + textStyle.y}em`)
            .attr("transform", "rotate(" + (-rotate) + ")")
            .style("fill", textStyle.fill)
            .style("font-size", textStyle.fontSize)
            .style("font-family", textStyle.fontFamily)
            .style("cursor", textStyle.cursor)
            .each(function() {
                const currentElement = d3.select(this);
                setStyleFromOptions(currentElement, additionalOptionsTextStyle);
            });

  xAxisTop.selectAll(".tick line")
    .style("stroke", tickStyle.stroke)
    .style("stroke-width", tickStyle.strokeWidth)
    .attr("y2", -tickStyle.lineLength)
    .each(function () {
      const currentElement = d3.select(this);
      setStyleFromOptions(currentElement, additionalOptionsTickStyle);
    });

  // Create and configure the bottom axis
  const xAxisBottom = g.append("g")
    .attr("transform", "translate(0," + this.yScale(yPositionBottom) + ")")
    .call(d3.axisBottom(this.xScale).tickValues(tickValuesBottomFinal.map(t => t.value))
    .tickFormat(d3.format(ticksFormat))
    );

  xAxisBottom.selectAll(".tick")
    .data(tickValuesBottomFinal)
    .attr("rowID", d => d.rowID)
    .attr("transform", function (d) {
       const xOffset = self.xScale(d.value);
      var currentOverlapSpacing = d.geneTrack ? -(d.geneTrack - 1) * self.geneOverlapSpacing : 0;
      return "translate(" + xOffset + "," + -currentOverlapSpacing + ")";
    });

  xAxisBottom.select(".domain").attr("stroke", "none");

  xAxisBottom.selectAll("text")
    .data(tickValuesBottomFinal)
    .attr("class", "coordinate")
    .style("text-anchor", "start")
    .attr("dx", `${0.8 + textStyle.x}em`)
    .attr("dy", `${-0.15 + textStyle.y}em`)
    .attr("transform", "rotate(" + (-rotate) + ")")
    .style("fill", textStyle.fill)
    .style("font-size", textStyle.fontSize)
    .style("font-family", textStyle.fontFamily)
    .style("cursor", textStyle.cursor)
    .each(function() {
      const currentElement = d3.select(this);
            setStyleFromOptions(currentElement, additionalOptionsTextStyle);
    });

  xAxisBottom.selectAll(".tick line")
    .style("stroke", tickStyle.stroke)
    .style("stroke-width", tickStyle.strokeWidth)
    .attr("y2", tickStyle.lineLength)
    .each(function () {
      const currentElement = d3.select(this);
      setStyleFromOptions(currentElement, additionalOptionsTickStyle);
    });

  return this;
};

container.prototype.scaleBar = function (show = true, options = {}) {
  if (!show) {
    return this;
  }

  const defaultOptions = {
    title: "1 kb",
    scaleBarUnit: 1000,
    x: 0, // default x offset
    y: 10,
    labelStyle: { // default styling for the label
      fontSize: "10px",
      fontFamily: "sans-serif",
      cursor: "default",
      fill: "black", // default text color
      labelPosition: "left" // moved labelPosition into labelStyle
    },
    textPadding: 0, // padding between text and line in x-direction
    scaleBarLineStyle: { // default styling for the scale bar line
      stroke: "grey",
      strokeWidth: 1
    },
    scaleBarTickStyle: { // default styling for the scale bar ticks
      stroke: "grey",
      strokeWidth: 1
    }
  };

  // Merge the default options with any predefined scaleBarOptions and the provided options
  const combinedOptions = mergeOptions.call(this, defaultOptions, 'scaleBarOptions', options);
  const { title, scaleBarUnit, x, y, textPadding, labelStyle, scaleBarLineStyle, scaleBarTickStyle } = combinedOptions;

  // Extract additional options that are not in defaultOptions
  const additionalOptionsLine = extractAdditionalOptions(scaleBarLineStyle, defaultOptions.scaleBarLineStyle);
  const additionalOptionsTick = extractAdditionalOptions(scaleBarTickStyle, defaultOptions.scaleBarTickStyle);
  const additionalOptionsLabel = extractAdditionalOptions(labelStyle, defaultOptions.labelStyle);

  // Calculate the length of the scale bar in pixels
  const scaleBarLength = Math.abs(this.xScale(scaleBarUnit) - this.xScale(0));

  // Create the group with the x offset applied
  const g = this.svg.append("g")
    .attr("transform", `translate(${this.width - this.margin.right - scaleBarLength - parseInt(labelStyle.fontSize) - 5 + x}, ${this.height - this.margin.bottom})`);

  // Create the scale bar line
  g.append("line")
    .attr("x1", parseInt(labelStyle.fontSize) + 5 + scaleBarLength)
    .attr("x2", parseInt(labelStyle.fontSize) + 5)
    .attr("y1", -y)
    .attr("y2", -y)
    .style("stroke", scaleBarLineStyle.stroke)
    .style("stroke-width", scaleBarLineStyle.strokeWidth)
    .each(function () {
      const currentElement = d3.select(this);
      setStyleFromOptions(currentElement, additionalOptionsLine);
    });

  // Add the ticks
  [parseInt(labelStyle.fontSize) + 5, parseInt(labelStyle.fontSize) + 5 + scaleBarLength].forEach(d => {
    g.append("line")
      .attr("x1", d)
      .attr("x2", d)
      .attr("y1", -y - 5)
      .attr("y2", -y + 5)
      .style("stroke", scaleBarTickStyle.stroke)
      .style("stroke-width", scaleBarTickStyle.strokeWidth)
      .each(function () {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsTick);
      });
  });

  // Determine the x position of the title based on the labelPosition within labelStyle and adjust with textPadding
  const titleX = labelStyle.labelPosition === "left" ? (parseInt(labelStyle.fontSize) - textPadding) : (parseInt(labelStyle.fontSize) + 5 + scaleBarLength + textPadding);
  const textAnchor = labelStyle.labelPosition === "left" ? "end" : "start";

  // Add the title
  g.append("text")
    .attr("x", titleX)
    .attr("y", -y)
    .style("text-anchor", textAnchor)
    .style("dominant-baseline", "middle")
    .style("font-size", labelStyle.fontSize)
    .style("font-family", labelStyle.fontFamily)
    .style("cursor", labelStyle.cursor)
    .style("fill", labelStyle.fill) // Apply text color
    .each(function () {
      const currentElement = d3.select(this);
      setStyleFromOptions(currentElement, additionalOptionsLabel);
    })
    .text(title);

  return this;
};

container.prototype.labels = function (label, show = true, options = {}) {

  if (!show) {
    return this;
  }

    if (!label) {
    return this;
  }

  // Verify that the data exists
  if (!this.data) {
    console.error('No data has been added to this cluster container. Please use the addGeneData() function before attempting to draw genes.');
    return this;
  }

  const defaultOptions = {
    x: 0,
    y: 55,
    dy: "0em",
    dx: "0em",
    rotate: 0,
    start: null,
    end: null,
    adjustLabels: true,
    fontSize: "12px",
    fontStyle: "italic",
    fontFamily: "sans-serif",
    textAnchor: "middle",
    cursor: "default",
    labelAdjustmentOptions: {
      rotation: 65,
      offsetX: 0,
      offsetY: 0,
      dx: "0em",
      dy: "0em"
    },
      itemStyle: [] // [{"index": 3,"y": 20}]
  };

  // If theme options exist, use them as the default options
  if (this.themeOptions && this.themeOptions.labelsOptions) {
    options = { ...this.themeOptions.labelsOptions, ...options };
  }

  const combinedOptions = { ...defaultOptions, ...options };
  const { x, y, start, end, adjustLabels, labelAdjustmentOptions, itemStyle, dx, dy, anchor, rotate, fontSize, fontStyle, fontFamily, textAnchor, cursor } = combinedOptions;

  // Extract additional options that are not in defaultOptions
  const additionalOptions = extractAdditionalOptions(combinedOptions, defaultOptions);

  // Placeholder function for getUniqueId
  const getUniqueId = (label) => label; // Replace with your actual implementation

  // Create the group
  const g = this.svg.append("g")
    .attr("transform", `translate(${this.margin.left},${this.margin.top})`);

  // Sort the data first by the minimum value of start and end.
  this.data.sort((a, b) => Math.min(a.start, a.end) - Math.min(b.start, b.end));

  // Check for existing labels
  const existingLabels = g.selectAll("text.label");

  const getAttributesForIndex = (d, i) => {
    const style = itemStyle.find(s => s.index === i) || {};
    const currentX = style.x || x;
    const currentY = style.y || y;

    const currentDx = style.dx || dx;
    const currentDy = style.dy || dy;
    const currentRotate = style.rotate || rotate;
    var currentLabelAdjustmentOptions = style.labelAdjustmentOptions || labelAdjustmentOptions;
      currentLabelAdjustmentOptions.rotation = -Math.abs(currentLabelAdjustmentOptions.rotation);
    const currentAdjustLabels = style.adjustLabels !== undefined ? style.adjustLabels : adjustLabels;

      // Calculate Y position based on geneTrack
    const currentGeneStrandSpacing = (d.strand == "forward" && this.geneStrandSpacing !== 0)
                                 ? this.geneStrandSpacing
                                 : -this.geneStrandSpacing;
    var currentOverlapSpacing = d.geneTrack ? (d.geneTrack - 1) * this.geneOverlapSpacing : 0;

    const yPos = this.yScale(currentY) - (this.markerHeight / 2) - currentGeneStrandSpacing + currentOverlapSpacing;
    const xPos = this.xScale((d.start + d.end) / 2) + currentX;

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

  // Adding the Label
  g.selectAll("text.label")
    .data(this.data)
    .enter()
    .append("text")
    .attr("id", (d, i) => `cluster-${sanitizeId(d.cluster)}-label-${i}`)
    .attr("rowID", (d, i) => `${d["rowID"]}`)
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
    .style("cursor", cursor)
    .text(d => d[label])
    .each(function (d, i) {

      const currentElement = d3.select(this);
      const attributes = getAttributesForIndex(d, i);

      if (attributes.adjustLabels) {
        adjustSpecificLabel(self, "text.label", currentElement.attr("id"), attributes.labelAdjustmentOptions);
      }
      // Set additional options as attributes
      setStyleFromOptions(currentElement, additionalOptions);
      // Override with itemStyle based on the index
      applyStyleToElement(currentElement, itemStyle, i);

    });

  //Make markers available to tooltip
  this.labels = g.selectAll(".label");

  return this;
};

container.prototype.tooltip = function (show = true, options = {}) {
  if (!show) {
    return this;
  }

  const defaultOptions = {
    triggers: ["markers", "genes", "labels", "exons", "introns", "utrs"],
    formatter: "<b>Start:</b> {start}<br><b>End: {end}</b>",
    opacity: 0,
    position: "absolute",
    backgroundColor: "rgba(255, 255, 255, 0.9)",
    padding: "8px",
    borderRadius: "4px",
    border: "1px solid rgba(0,0,0,0.1)",
    boxShadow: "0 4px 6px rgba(0, 0, 0, 0.1)",
    pointerEvents: "none",
    fontFamily: "Arial, sans-serif",
    fontSize: "12px",
    zIndex: 1000,
    color: "#333",
    lineHeight: "1.5"
  };

  // If theme options exist, use them as the default options
  if (this.themeOptions && this.themeOptions.tooltipOptions) {
    options = { ...this.themeOptions.tooltipOptions, ...options };
  }

  const combinedOptions = { ...defaultOptions, ...options };

  // Extract additional options that are not in defaultOptions
  const additionalOptions = extractAdditionalOptions(combinedOptions, defaultOptions);

  // Generate CSS for the tooltip and its pseudo-element
  const generateTooltipCSS = (opts, additionalOpts) => {
    let additionalStyles = Object.entries(additionalOpts).map(([key, value]) => `${camelToKebab(key)}: ${value};`).join(' ');
    return `
            .cluster-tooltip {
                ${additionalStyles}
                opacity: ${opts.opacity};
                position: ${opts.position};
                background-color: ${opts.backgroundColor};
                padding: ${opts.padding};
                border-radius: ${opts.borderRadius};
                border: ${opts.border};
                box-shadow: ${opts.boxShadow};
                pointer-events: ${opts.pointerEvents};
                font-family: ${opts.fontFamily};
                font-size: ${opts.fontSize};
                z-index: ${opts.zIndex};
                color: ${opts.color};
                line-height: ${opts.lineHeight};
            }
            .cluster-tooltip::before {
                content: "";
                position: absolute;
                top: 100%;
                left: 50%;
                transform: translateX(-50%);
                border-width: 5px;
                border-style: solid;
                border-color: ${opts.backgroundColor} transparent transparent transparent;
            }
        `;
  };

  // Inject the generated CSS into the document
  const styleTag = document.createElement("style");
  styleTag.innerHTML = generateTooltipCSS(combinedOptions, additionalOptions);
  document.head.appendChild(styleTag);

  // Ensure triggers is an array
  if (typeof combinedOptions.triggers === 'string') {
    combinedOptions.triggers = [combinedOptions.triggers];
  }

  // Create the tooltip div if it doesn't exist
  let tooltip = d3.select("body").select(".cluster-tooltip");
  if (tooltip.empty()) {
    tooltip = d3.select("body")
      .append("div")
      .attr("class", "cluster-tooltip")
      .style("opacity", combinedOptions.opacity)
      .style("position", combinedOptions.position)
      .style("background-color", combinedOptions.backgroundColor)
      .style("padding", combinedOptions.padding)
      .style("border-radius", combinedOptions.borderRadius)
      .style("border", combinedOptions.border)
      .style("box-shadow", combinedOptions.boxShadow)
      .style("pointer-events", combinedOptions.pointerEvents)
      .style("font-family", combinedOptions.fontFamily)
      .style("font-size", combinedOptions.fontSize)
      .style("z-index", combinedOptions.zIndex)
      .style("color", combinedOptions.color)
      .style("line-height", combinedOptions.lineHeight);
  }

  // Function to generate tooltip content
  const d3Format = d3.format(",");

  const textAccessor = (d) => {
    return combinedOptions.formatter.replace(/\{(\w+)\}/g, (match, p1) => {
      if (typeof d[p1] === 'number') {
        return d3Format(d[p1]);
      }
      return d[p1] || '';
    });
  };

  combinedOptions.triggers.forEach(trigger => {

    if (!this.hasOwnProperty(trigger)) {
      return;
    }

    const selection = this[trigger];

    // Check if the selection exists and is not empty
    if (!selection || selection.empty()) {
      return; // Skip this iteration of the loop
    }

    // Mouseover event to show the tooltip
    selection.on("mouseover", (event, d) => {
      const dataPoint = this.data.find(item => item === d);

      const x = event.pageX;
      const y = event.pageY;

      const element = d3.select(event.currentTarget);
      element.classed("hovered", true);

      tooltip.transition()
        .duration(200)
        .style("opacity", 1);
      tooltip.html(textAccessor(dataPoint))
        .style("left", (x - tooltip.node().offsetWidth / 2) + "px")
        .style("top", (y - tooltip.node().offsetHeight - 15) + "px");
    });

    // Mousemove event to reposition the tooltip as the mouse moves
    selection.on("mousemove", (event, d) => {
      const x = event.pageX;
      const y = event.pageY;

      tooltip.style("left", (x - tooltip.node().offsetWidth / 2) + "px")
        .style("top", (y - tooltip.node().offsetHeight - 15) + "px");
    });

    // Mouseout event to hide the tooltip
    selection.on("mouseout", () => {

      const element = d3.select(event.currentTarget);
      element.classed("hovered", false);

      tooltip.transition()
        .duration(500)
        .style("opacity", 0);
    });
  });

  return this; // Return the instance for method chaining
};

container.prototype.genes = function (group, show = true, options = {}) {

  if (!show) {
    return this;
  }

  if (!this.data) {
    console.error('No data has been added to this cluster container. Please use the geneData() function before attempting to draw arrows.');
    return this;
  }

  const defaultOptions = {
    x: 1,
    y: 50,
    stroke: "black",
    strokeWidth: 1,
    colorScheme: null,
    customColors: null,
    cursor: "default",
    marker: "arrow",
    markerSize: "medium",
    itemStyle: [],
    arrowheadWidth: null,
    arrowheadHeight: null,
    markerHeight: null,
    cornerRadius: null
  };

  const combinedOptions = mergeOptions.call(this, defaultOptions, 'geneOptions', options);
  const { x, y, stroke, strokeWidth, colorScheme, customColors, cursor, itemStyle } = combinedOptions;

  // Extract additional options that aren't in defaultOptions
  const additionalOptions = extractAdditionalOptions(combinedOptions, defaultOptions);

  const uniqueGroups = [...new Set(this.dataAll.map(d => d[group]))];

  const colorScale = getColorScale(colorScheme, customColors, uniqueGroups);
  this.genesColorScale = colorScale;

  var g = this.svg.append("g")
    .attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);

  // Sort the data first by the minimum value of start and end.
  this.data.sort((a, b) => Math.min(a.start, a.end) - Math.min(b.start, b.end));

  const getAttributesForIndex = (d, i) => {
    const style = itemStyle.find(s => s.index === i) || {};
    const geneLength = Math.abs(this.xScale(d.end) - this.xScale(d.start));
    const currentX = style.x || x;
    const currentY = style.y || y;
    const marker = style.marker || combinedOptions.marker;
    const markerSize = style.markerSize || combinedOptions.markerSize;


    // GenePath
    const  { path, arrowheadWidth, arrowheadHeight, height }  = getGenePath(
        marker,
        geneLength,
        markerSize,
        options = {
          arrowheadWidth: style.arrowheadWidth || combinedOptions.arrowheadWidth,
          arrowheadHeight: style.arrowheadHeight || combinedOptions.arrowheadHeight,
          markerHeight: style.markerHeight || combinedOptions.markerHeight,
          cornerRadius: style.cornerRadius || combinedOptions.cornerRadius
        }
      )

    this.markerHeight = height;
    this.geneStrandSpacing = this.separateStrands ? (height / 2 + this.strandSpacing) : 0;
    this.geneOverlapSpacing = (height + this.overlapSpacing);

    // Calculate Y position based on geneTrack
    const currentGeneStrandSpacing = (d.strand == "forward" && this.geneStrandSpacing !== 0)
                                 ? this.geneStrandSpacing
                                 : -this.geneStrandSpacing;
    var currentOverlapSpacing = d.geneTrack ? (d.geneTrack - 1) * this.geneOverlapSpacing : 0;

    const yPos = this.yScale(currentY) - currentGeneStrandSpacing + currentOverlapSpacing;
    const xPos = this.xScale(d.start);

    return { xPos, yPos, path, arrowheadWidth, arrowheadHeight, height };
  };

  g.selectAll(".gene")
    .data(this.data)
    .enter()
    .append("path")
    .attr("d", (d, i) => {
      return getAttributesForIndex(d, i).path;
    })
    .attr("transform", (d, i) => {
      const { xPos, yPos, arrowheadHeight, height } = getAttributesForIndex(d, i);
      const rotation = this.reverse
    ? (d.direction === 'forward' ? 180 : 0)
    : (d.direction === 'forward' ? 0 : 180);
      return `rotate(${rotation}, ${xPos}, ${yPos}) translate(${xPos}, ${yPos - (height / 2 )})`;
    })
    .attr("fill", (d) => colorScale(d[group]))
    .attr("class", "gene")
    .attr("id", (d, i) => `${sanitizeId(d.cluster)}-gene-${i}`)
    .attr("rowID", (d, i) => `${d["rowID"]}`)
    .attr("start", (d, i) => `${d["start"]}`)
    .attr("end", (d, i) => `${d["end"]}`)
    .attr("strand", (d, i) => `${d["strand"]}`)
    .attr("cluster", (d, i) => `${d["cluster"]}`)
    .style("stroke-width", strokeWidth)
    .style("stroke", stroke)
    .style("cursor", cursor)
    .each(function (d, i) {
      const currentElement = d3.select(this);
      // Set additional options as attributes
      setStyleFromOptions(currentElement, additionalOptions);
      // Override with itemStyle based on the index
      applyStyleToElement(currentElement, itemStyle, i);
    });

  // Update the reference
  this.genes = g.selectAll(".gene");

  return this;
};

container.prototype.transcript = function (group, show = true, options = {}) {

  if (!show) {
    return this;
  }

  if (!this.data) {
    console.error('No data has been added to this cluster container. Please use the geneData() function before attempting to draw arrows.');
    return this;
  }

  const defaultOptions = {
    x: 1,
    y: 50,
    colorScheme: null,
    customColors: null,
    styleExons: {
      show: true,
      strokeWidth: 0,
      cursor: "default",
      marker: "box",
      markerSize: "medium",
      arrowheadWidth: null,
      arrowheadHeight: null,
      markerHeight: null,
      cornerRadius: null
    },
    styleIntrons: {
      show: true,
      strokeWidth: 0,
      fill: "none",
      strokeWidth: 1,
      cursor: "default",
      marker: "intron",
      markerSize: "medium",
      arrowheadWidth: null,
      arrowheadHeight: null,
      markerHeight: null,
      cornerRadius: null
    },
    styleUTRs: {
      show: true,
      fontSize: "10px",
      fontStyle: "normal",
      fontFamily: "sans-serif",
      cursor: "default",
      color: "black",
      fill: "#FFF",
      strokeWidth: 1,
      cursor: "default",
      marker: "box",
      markerSize: "medium",
      arrowheadWidth: null,
      arrowheadHeight: null,
      markerHeight: null,
      cornerRadius: null
    },
     labelOptions: {
      show: true,
      xOffset: 2,
      yOffset: 0,
      fontSize: "12px",
      fontStyle: "normal",
      fontWeight: "normal",
      fontFamily: "sans-serif",
      cursor: "default",
      color: "black"
    },
    itemStyleExons: [],
    itemStyleIntrons: [],
    itemStyleUTRs: []
  };

  const combinedOptions = mergeOptions.call(this, defaultOptions, 'geneOptions', options);
  const { x, y, colorScheme, customColors, styleExons, styleIntrons, styleUTRs, itemStyleExons, itemStyleIntrons, itemStyleUTRs, labelOptions } = combinedOptions;

  // Extract additional options that aren't in defaultOptions
  const additionalOptionsExons = extractAdditionalOptions(combinedOptions.styleExons, defaultOptions.styleExons);
  const additionalOptionsIntrons = extractAdditionalOptions(combinedOptions.styleIntrons, defaultOptions.styleIntrons);
  const additionalOptionsUTRs = extractAdditionalOptions(combinedOptions.styleUTRs, defaultOptions.styleUTRs);

  const uniqueGroups = [...new Set(this.dataAll.map(d => d[group]))];

  const colorScale = getColorScale(colorScheme, customColors, uniqueGroups);
  this.genesColorScale = colorScale;

  const g = this.svg.append("g")
    .attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);

  // Sort the data first by the minimum value of start and end.
  this.data.sort((a, b) => Math.min(a.start, a.end) - Math.min(b.start, b.end));

  const getAttributesForIndex = (d, i, styles, itemStyles) => {
    const style = itemStyles.find(s => s.index === i) || {};
    const geneLength = Math.abs(this.xScale(d.end) - this.xScale(d.start));
    const currentX = style.x || x;
    const currentY = style.y || y;
    const marker = style.marker || styles.marker;
    const markerSize = style.markerSize || styles.markerSize;

    // GenePath
    const { path, arrowheadWidth, arrowheadHeight, height } = getGenePath(
      marker,
      geneLength,
      markerSize,
      options = {
        arrowheadWidth: style.arrowheadWidth || styles.arrowheadWidth,
        arrowheadHeight: style.arrowheadHeight || styles.arrowheadHeight,
        markerHeight: style.markerHeight || styles.markerHeight,
        cornerRadius: style.cornerRadius || styles.cornerRadius
      }
    );

    this.markerHeight = height;
    this.geneStrandSpacing = this.separateStrands ? (height / 2 + this.strandSpacing) : 0;
    this.geneOverlapSpacing = (height + this.overlapSpacing);

    // Calculate Y position based on geneTrack
    const currentGeneStrandSpacing = (d.strand === "forward" && this.geneStrandSpacing !== 0)
      ? this.geneStrandSpacing
      : -this.geneStrandSpacing;
    const currentOverlapSpacing = d.geneTrack ? (d.geneTrack - 1) * this.geneOverlapSpacing : 0;

    const yPos = this.yScale(currentY) - currentGeneStrandSpacing + currentOverlapSpacing;
    const xPos = this.xScale(d.start);

    return { xPos, yPos, path, arrowheadWidth, arrowheadHeight, height };
  };

  if (styleExons.show) {
    // Add exons
    const exons = this.data.filter(transcript => /exon/i.test(transcript.type));

    g.selectAll(".exon")
      .data(exons)
      .enter()
      .append("path")
      .attr("d", (d, i) => {
        return getAttributesForIndex(d, i, styleExons, itemStyleExons).path;
      })
      .attr("transform", (d, i) => {
        const { xPos, yPos, height } = getAttributesForIndex(d, i, styleExons, itemStyleExons);
        const rotation = this.reverse
          ? (d.direction === 'forward' ? 180 : 0)
          : (d.direction === 'forward' ? 0 : 180);
        return `rotate(${rotation}, ${xPos}, ${yPos}) translate(${xPos}, ${yPos - (height / 2)})`;
      })
      .attr("fill", (d) => colorScale(d[group]))
      .attr("class", "exon")
      .attr("id", (d, i) => `${sanitizeId(d.cluster)}-exon-${i}`)
      .attr("rowID", (d, i) => `${d["rowID"]}`)
      .attr("start", (d, i) => `${d["start"]}`)
      .attr("end", (d, i) => `${d["end"]}`)
      .attr("strand", (d, i) => `${d["strand"]}`)
      .attr("cluster", (d, i) => `${d["cluster"]}`)
      .style("stroke-width", styleExons.strokeWidth)
      .style("stroke", styleExons.stroke)
      .style("cursor", styleExons.cursor)
      .each(function (d, i) {
        const currentElement = d3.select(this);
        // Set additional options as attributes
        setStyleFromOptions(currentElement, additionalOptionsExons);
        // Override with itemStyle based on the index
        applyStyleToElement(currentElement, itemStyleExons, i);
      });

    // Update the reference
    this.exons = g.selectAll(".exon");
  }

  if (styleIntrons.show) {
    // Add introns
    const introns = this.data.filter(transcript => /intron/i.test(transcript.type));

    g.selectAll(".intron")
      .data(introns)
      .enter()
      .append("path")
      .attr("d", (d, i) => {
        return getAttributesForIndex(d, i, styleIntrons, itemStyleIntrons).path;
      })
      .attr("transform", (d, i) => {
        const { xPos, yPos, height } = getAttributesForIndex(d, i, styleIntrons, itemStyleIntrons);
        const yOffset = d.direction === 'forward' ? -height : +height;
        const rotation = this.reverse
          ? (d.direction === 'reverse' ? 180 : 0)
          : (d.direction === 'forward' ? 0 : 180);
        return `rotate(${rotation}, ${xPos}, ${yPos + yOffset}) translate(${xPos}, ${yPos + yOffset})`;
      })
      .attr("fill", styleIntrons.fill)
      .attr("class", "intron")
      .attr("id", (d, i) => `${sanitizeId(d.cluster)}-intron-${i}`)
      .attr("rowID", (d, i) => `${d["rowID"]}`)
      .attr("start", (d, i) => `${d["start"]}`)
      .attr("end", (d, i) => `${d["end"]}`)
      .attr("strand", (d, i) => `${d["strand"]}`)
      .attr("cluster", (d, i) => `${d["cluster"]}`)
      .style("stroke-width", styleIntrons.strokeWidth)
      .style("stroke", (d) => colorScale(d[group]))
      .style("cursor", styleIntrons.cursor)
      .each(function (d, i) {
        const currentElement = d3.select(this);
        // Set additional options as attributes
        setStyleFromOptions(currentElement, additionalOptionsIntrons);
        // Override with itemStyle based on the index
        applyStyleToElement(currentElement, itemStyleIntrons, i);
      });

    // Update the reference
    this.introns = g.selectAll(".intron");
  }

  if (styleUTRs.show) {
    // Add UTRs
    const utrs = this.data.filter(transcript => /utr/i.test(transcript.type));

    g.selectAll(".utr")
      .data(utrs)
      .enter()
      .append("path")
      .attr("d", (d, i) => {
        return getAttributesForIndex(d, i, styleUTRs, itemStyleUTRs).path;
      })
      .attr("transform", (d, i) => {
        const { xPos, yPos, height } = getAttributesForIndex(d, i, styleUTRs, itemStyleUTRs);
        const rotation = this.reverse
          ? (d.direction === 'forward' ? 180 : 0)
          : (d.direction === 'forward' ? 0 : 180);
        return `rotate(${rotation}, ${xPos}, ${yPos}) translate(${xPos}, ${yPos - (height / 2)})`;
      })
      .attr("fill", styleUTRs.fill)
      .attr("class", "utr")
      .attr("id", (d, i) => `${sanitizeId(d.cluster)}-utr-${i}`)
      .attr("rowID", (d, i) => `${d["rowID"]}`)
      .attr("start", (d, i) => `${d["start"]}`)
      .attr("end", (d, i) => `${d["end"]}`)
      .attr("strand", (d, i) => `${d["strand"]}`)
      .attr("cluster", (d, i) => `${d["cluster"]}`)
      .style("stroke-width", styleUTRs.strokeWidth)
      .style("stroke", (d) => colorScale(d[group]))
      .style("cursor", styleUTRs.cursor)
      .each(function (d, i) {
        const currentElement = d3.select(this);
        // Set additional options as attributes
        setStyleFromOptions(currentElement, additionalOptionsUTRs);
        // Override with itemStyle based on the index
        applyStyleToElement(currentElement, itemStyleUTRs, i);
      });

    // Update the reference
    this.utrs = g.selectAll(".utr");

    if (labelOptions.show) {

    const xPosition3Prime = this.data[0].direction === 'forward'
      ? this.xScale(this.maxEnd) + labelOptions.xOffset
      : this.xScale(this.minStart) - labelOptions.xOffset;

    const xPosition5Prime = this.data[0].direction === 'forward'
      ? this.xScale(this.minStart) - labelOptions.xOffset
      : this.xScale(this.maxEnd) + labelOptions.xOffset;

    // add 3' label
    g.append("text")
        .attr("x", xPosition3Prime)
        .attr("y", (d) => this.yScale(y + labelOptions.yOffset))
        .attr("class", `5-label`)
        .attr("text-anchor", "middle")
        .attr("alignment-baseline", "middle")
        .style("font-size", labelOptions.fontSize)
        .style("font-style", labelOptions.fontStyle)
        .style("font-weight", labelOptions.fontWeight)
        .style("font-family", labelOptions.fontFamily)
        .style("cursor", labelOptions.cursor)
        .style("fill", labelOptions.color)
        .text("3'");

    // add 5' label
    g.append("text")
        .attr("x", xPosition5Prime)
        .attr("y", (d) => this.yScale(y + labelOptions.yOffset))
        .attr("class", `5-label`)
        .attr("text-anchor", "middle")
        .attr("alignment-baseline", "middle")
        .style("font-size", labelOptions.fontSize)
        .style("font-style", labelOptions.fontStyle)
        .style("font-weight", labelOptions.fontWeight)
        .style("font-family", labelOptions.fontFamily)
        .style("cursor", labelOptions.cursor)
        .style("fill", labelOptions.color)
        .text("5'");

    }


  }

  return this;
};

container.prototype.legendData = function (data) {

  this.data = [...new Set(data)];

  return this;

};

container.prototype.legend = function (group, show = true, parentId = null, options = {}) {
  if (!show) {
    return this;
  }

  const defaultOptions = {
    x: 0,
    y: 0,
    width: null, // Default width set to null
    orientation: "horizontal",
    adjustHeight: true,
    order: [],
    legendOptions: {
      cursor: "pointer",
      colorScheme: null,
      customColors: null
    },
    legendTextOptions: {
      cursor: "pointer",
      textAnchor: "start",
      dy: ".35em",
      fontSize: "12px",
      fontFamily: "sans-serif"
    }
  };

  const combinedOptions = mergeOptions.call(this, defaultOptions, 'legendOptions', options);
  const { x, y, width, orientation, adjustHeight, order, legendOptions, legendTextOptions } = combinedOptions;

  const additionalLegendOptions = extractAdditionalOptions(legendOptions, defaultOptions.legendOptions);
  const additionalLegendTextOptions = extractAdditionalOptions(legendTextOptions, defaultOptions.legendTextOptions);

  const svgLegend = this.svg;
  const parentWidth = computeSize(width, svgLegend.node().getBoundingClientRect().width) ||
    svgLegend.node().getBoundingClientRect().width;

  let uniqueGroups = [...new Set(this.data.map(d => d[group]))];
  uniqueGroups = uniqueGroups.filter(color => color !== null);

  const colorScale = getColorScale(legendOptions.colorScheme, legendOptions.customColors, uniqueGroups);

  // Separate special groups ("Other", "No Hit", "Unknown") from the rest
  const specialGroups = ["Other", "No Hit", "Unknown"];
  const regularGroups = uniqueGroups.filter(group => !specialGroups.includes(group));
  const specialGroupsInData = specialGroups.filter(group => uniqueGroups.includes(group));
  // Reassemble uniqueGroups with special groups placed last
  uniqueGroups = [...regularGroups, ...specialGroupsInData];

  if (order && order.length > 0) {
    uniqueGroups = order
      .filter(item => uniqueGroups.includes(item))
      .concat(uniqueGroups.filter(item => !order.includes(item)));
  }

  if (!uniqueGroups.length) {
    console.error(`Error: No labels provided and the group "${group}" does not exist in the data.`);
    return;
  }

  const legendSize = parseFloat(legendTextOptions.fontSize);
  const legendPadding = legendSize / 2;

  let currentX = x;
  let currentY = y;

  var g = this.svg.append("g")
    .attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);

  g.selectAll(".legend")
    .data(uniqueGroups)
    .enter()
    .append("g")
    .attr("class", "legend")
    .each((d, i, nodes) => {
      const legendGroup = d3.select(nodes[i]);

      const textLabel = legendGroup
        .append("text")
        .attr("class", "legend-label")
        .attr("id", (d, i) => `legend-label-${i}`)
        .attr("dy", legendTextOptions.dy)
        .style("text-anchor", legendTextOptions.textAnchor)
        .style("font-size", legendTextOptions.fontSize)
        .style("font-family", legendTextOptions.fontFamily)
        .style("cursor", legendTextOptions.cursor)
        .text(d)
        .each(function () {
          const currentElement = d3.select(this);
          setStyleFromOptions(currentElement, additionalLegendTextOptions);
        });

      const textLength = textLabel.node().getComputedTextLength();
      const availableWidth = this.width - this.margin.left - this.margin.right;
      const totalItemWidth = textLength + legendSize + 2 * legendPadding;

      if (currentX + totalItemWidth > availableWidth) {
        currentX = x;
        currentY += legendSize + legendPadding;
      }

      textLabel
        .attr("x", currentX + legendSize + legendPadding)
        .attr("y", currentY + legendSize / 2);

      const rect = legendGroup
        .append("rect")
        .attr("class", "legend-marker")
        .attr("id", (d, i) => `legend-marker-${i}`)
        .style("cursor", legendOptions.cursor)
        .attr("x", currentX)
        .attr("y", currentY)
        .attr("width", legendSize)
        .attr("height", legendSize)
        .style("fill", colorScale(d))
        .each(function () {
          const currentElement = d3.select(this);
          setStyleFromOptions(currentElement, additionalLegendOptions);
        })

      if (orientation === "horizontal") {
        currentX += textLength + legendSize + 2 * legendPadding;
      } else {
        currentY += legendSize + legendPadding;
      }
    })
    .on("mouseover", (event, d) => {
      const element = d3.select(event.currentTarget);
      element.classed("hovered", true);
    })
    .on("mouseout", (event, d) => {
      const element = d3.select(event.currentTarget);
      element.classed("hovered", false);
    })
    .on("click", (event, d) => {
      const element = d3.select(event.currentTarget);
      // If it's currently highlighted, unhighlight it, else highlight it
      if (element.classed("unselected")) {
        element.classed("unselected", false);
      } else {
        element.classed("unselected", true);
      }

      const unselectedLegend = d3.selectAll(".unselected").data();
      const unselectedRowIds = this.data
        .filter(item => unselectedLegend.includes(item[group]))
        .map(item => item.rowID);

      if (parentId && typeof parentId === 'string') {
           // If parentId is not null and is a string, select within the parent
          d3.select('#' + parentId).selectAll('[rowID]').each(function () {
          const currentRowID = +d3.select(this).attr("rowID"); // Convert string to number
      if (unselectedRowIds.includes(currentRowID)) {
          d3.select(this).style("display", "none"); // Hide it
      } else {
          d3.select(this).style("display", ""); // Show it
        }
      });
      } else {
      // If parentId is null or not a string, select globally
      d3.selectAll('[rowID]').each(function () {
        const currentRowID = +d3.select(this).attr("rowID"); // Convert string to number
        if (unselectedRowIds.includes(currentRowID)) {
          d3.select(this).style("display", "none"); // Hide it
      } else {
        d3.select(this).style("display", ""); // Show it
      }
      });
}
    });

  if (adjustHeight && this.height === 0) {
    var padding = 20;
    var contentHeight = currentY + legendSize + legendPadding + padding;

    svgLegend.attr("height", contentHeight);
    var viewBoxWidth = parentWidth;
    svgLegend.attr("viewBox", `0 0 ${viewBoxWidth} ${contentHeight}`);
    g.attr("transform", `translate(${this.margin.left}, ${this.margin.top + padding / 2})`);

  }

  return this;
};

container.prototype.links = function (links, clusterKey, options = {}) {

  const defaultOptions = {
    y: 50,
    showLinks: true,
    label: true,
    measure: "identity",
    linkWidth: 1,
    labelStyle: {
        cursor: "pointer",
        fontSize: "12px",
        fontStyle: "italic",
        fontFamily: "sans-serif",
        textAnchor: "middle",
    },
    labelAdjustmentOptions: {
      rotation: -65,
      offsetX: 0,
      offsetY: 0,
      dx: "0em",
      dy: "0em"
    },
  };

  if (!links || links.length === 0) {
      return this;
  }

  const combinedOptions = mergeOptions.call(this, defaultOptions, 'linkOptions', options);
  const { x, y, cursor, fontSize, fontStyle, fontFamily, textAnchor, showLinks, label, measure, labelStyle, labelAdjustmentOptions, linkWidth } = combinedOptions;

  const additionalOptions = extractAdditionalOptions(options, defaultOptions);
  const additionalOptionsLabelStyle = extractAdditionalOptions(labelStyle, defaultOptions.labelStyle);

  var group = this.svg.append("g")
      .attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);

  const hasReverseStrand = this.data.some(item => item.strand === "reverse");
  const clusterStrandSpacing = hasReverseStrand ? this.geneStrandSpacing * 1 : 0;

  if(showLinks){
    links.forEach(link => {
        const widthFactor = (1 - linkWidth) / 2;
        const adjustedStart1 = link.start1 + (link.end1 - link.start1) * widthFactor;
        const adjustedEnd1 = link.end1 - (link.end1 - link.start1) * widthFactor;
        const adjustedStart2 = link.start2 + (link.end2 - link.start2) * widthFactor;
        const adjustedEnd2 = link.end2 - (link.end2 - link.start2) * widthFactor;

        // Check if the link is relevant to cluster1
        if (link.cluster1 === clusterKey) {
            const groupColor1 = link.groupColor ? this.genesColorScale(link.groupColor) : this.genesColorScale(link.group1);

            group.append("circle")
                .attr("cx", this.xScale(adjustedStart1))
                .attr("cy", this.yScale(y) + clusterStrandSpacing)
                .attr("position", link.start1)
                .attr("r", 1)
                .attr("cluster", clusterKey)
                .attr("visibility", "hidden")
                .attr("groupColor", groupColor1)
                .attr("linkID", link.linkID)
                .attr("class", "link-marker");

            group.append("circle")
                .attr("cx", this.xScale(adjustedEnd1))
                .attr("cy", this.yScale(y) + clusterStrandSpacing)
                .attr("position", link.end1)
                .attr("class", "link-marker")
                .attr("groupColor", groupColor1)
                .attr("visibility", "hidden")
                .attr("linkID", link.linkID)
                .attr("cluster", clusterKey)
                .attr("r", 1);
        }

        // Check if the link is relevant to cluster2
        if (link.cluster2 === clusterKey) {
            const groupColor2 = link.groupColor ? this.genesColorScale(link.groupColor) : this.genesColorScale(link.group2);

            group.append("circle")
                .attr("cx", this.xScale(adjustedStart2))
                .attr("cy", this.yScale(y) - clusterStrandSpacing)
                .attr("position", link.start2)
                .attr("class", "link-marker")
                .attr("cluster", clusterKey)
                .attr("visibility", "hidden")
                .attr("groupColor", groupColor2)
                .attr("linkID", link.linkID)
                .attr("r", 1);

            group.append("circle")
                .attr("cx", this.xScale(adjustedEnd2))
                .attr("cy", this.yScale(y) - clusterStrandSpacing)
                .attr("position", link.end2)
                .attr("class", "link-marker")
                .attr("linkID", link.linkID)
                .attr("cluster", clusterKey)
                .attr("visibility", "hidden")
                .attr("groupColor", groupColor2)
                .attr("r", 1);
        }
    });
  }

  if(label){
    const self = this;

    const labelData = this.data.filter(d =>
      d[measure] != null &&
      d.BlastP != "No Hit" &&
      (d.BlastP === undefined || (d.BlastP != d.protein_id && d.BlastP)) &&
      ((options.value1 === undefined && options.value2 === undefined) ||
      (options.value1 && options.value1.includes(d[options.group])) ||
      (options.value2 && options.value2.includes(d[options.group])))
    );

    // Adding the Label
    group.selectAll(".link-text")
    .data(labelData)
    .enter()
    .append("text")
    .attr("id", (d, i) => `cluster-${sanitizeId(d.cluster)}-label-${i}`)
    .attr("rowID", (d, i) => `${d["rowID"]}`)
    .attr("class", "link-text")
    .attr("x", (d, i) => this.xScale((d.start + d.end) / 2))
    .attr("y", (d, i) => this.yScale(y) - (this.markerHeight / 1.5) - clusterStrandSpacing)
    .text(d => `${parseFloat(d[measure].toFixed(1))}%`)
    .style("font-size", labelStyle.fontSize)
    .style("font-style", labelStyle.fontStyle)
    .style("font-family", labelStyle.fontFamily)
    .style("cursor", labelStyle.cursor)
    .attr("text-anchor", labelStyle.textAnchor)
    .attr("cluster", clusterKey)
    .each(function (d, i) {
      const currentElement = d3.select(this);
      adjustSpecificLabel(self, ".link-text", currentElement.attr("id"), labelAdjustmentOptions);
      setStyleFromOptions(currentElement, additionalOptionsLabelStyle);
    });
  }

  return this;
};
// Annotations

container.prototype.trackMouse = function(track = true) {
  if (!track) {
    return this;
  }

  // Change cursor to crosshair
  this.svg.style("cursor", "crosshair");
    this.svg.selectAll("*").each(function() {
    this.style.cssText += "cursor: crosshair !important;";
  });

  // Tooltip for displaying coordinates
  const tooltip = d3.select("body").append("div")
    .attr("class", "coordinate-tooltip")
    .style("background-color", "rgba(255, 255, 255, 0.9)")
    .style("padding", "8px")
    .style("border-radius", "4px")
    .style("border", "1px solid rgba(0,0,0,0.1)")
    .style("box-shadow", "0 4px 6px rgba(0, 0, 0, 0.1)")
    .style("pointer-events", "none")
    .style("font-family", "Arial, sans-serif")
    .style("font-size", "12px")
    .style("color", "#333")
    .style("line-height", "1.5")
    .style("position", "absolute")
    .style("visibility", "hidden")
    .style("z-index", "1000");

  const xScale = d3.scaleLinear().domain([0 + this.margin.left, this.width - this.margin.right]).range([0, 100]);
  const yScale = d3.scaleLinear().domain([this.height - this.margin.bottom, 0 + this.margin.top]).range([0, 100]);
  const linearScale = d3.scaleLinear().domain([0 - this.margin.left, this.width]).range([0, 100]);

  this.svg.on("mousemove", (event) => {
    const [x, y] = d3.pointer(event);
    const adjustedX = x - this.margin.left
    loci = Math.round(this.xScale.invert(adjustedX))
    const format = d3.format(",");

    tooltip.html(`x: ${format(loci)} <br>y: ${yScale(y).toFixed(1)}`)
      .style("visibility", "visible")
      .style("left", (event.pageX + 10) + "px")
      .style("top", (event.pageY - 10) + "px");
  });

  this.svg.on("mouseout", () => {
    tooltip.style("visibility", "hidden");
  });

  return this;
};

container.prototype.addAnnotations = function (annotations) {
  if (!annotations || annotations.length === 0) {
    return this;
  }

  var g = this.svg.append("g")
    .attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);
  annotations.forEach(annotation => {
    this.createAnnotation(g, annotation);
  });

  return this;
};

container.prototype.createAnnotation = function (group, options) {

  switch (options.type) {
    case 'text':
      this.createTextAnnotation(group, options);
      break;
    case 'line':
      this.createLineAnnotation(group, options);
      break;
    case 'textMarker':
      this.createTextMarkerAnnotation(group, options);
      break;
    case 'arrow':
      this.createArrowAnnotation(group, options);
      break;
    case 'promoter':
      this.createPromoterAnnotation(group, options);
      break;
    case 'terminator':
      this.createTerminatorAnnotation(group, options);
      break;
    case 'rectangle':
      this.createRectangleAnnotation(group, options);
      break;
    case 'symbol':
      this.createSymbolAnnotation(group, options);
    case 'arc':
      this.createCurveAnnotation(group, options);
      break;
    default:
      console.warn('Unsupported annotation type:', options.type);
  }
};

container.prototype.createTextAnnotation = function (group, options) {
  // Define default styles for text annotations
  const defaultOptions = {
    x: 0,
    y: 60,
    text: '',
    style: {
      fontSize: "10px",
      fontStyle: "normal",
      fontWeight: "normal",
      textDecoration: "none",
      fontFamily: "sans-serif",
      cursor: "default"
    }
  };

  // Merge default options and user-specified options
  const combinedOptions = mergeOptions.call(this, defaultOptions, "textAnnotationOptions", options);
  let { x, y, text, style } = combinedOptions;

  // Convert x, y, and text to arrays if they are not already
  if (!Array.isArray(x)) x = [x];
  if (!Array.isArray(y)) y = [y];
  if (!Array.isArray(text)) text = [text];

  // Extract additional options that are not in defaultOptions
  const additionalOptionsStyle = extractAdditionalOptions(style, defaultOptions.style);

  var group = this.svg.append("g")
    .attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);

  // Iterate over each element in the arrays
  for (let i = 0; i < Math.max(x.length, y.length, text.length); i++) {
    const currentX = x[Math.min(i, x.length - 1)];
    const currentY = y[Math.min(i, y.length - 1)];
    const currentText = text[Math.min(i, text.length - 1)];

    // Create the text element with merged styles for each set of values
    group.append("text")
      .attr("x", this.xScale(currentX))
      .attr("y", this.yScale(currentY))
      .style("font-size", style.fontSize)
      .style("font-style", style.fontStyle)
      .style("font-weight", style.fontWeight)
      .style("text-decoration", style.textDecoration)
      .style("font-family", style.fontFamily)
      .style("cursor", style.cursor)
      .each(function () {
        const currentElement = d3.select(this);
        parseAndStyleText(currentText, currentElement, style);
        setStyleFromOptions(currentElement, additionalOptionsStyle);
      });
  }
};

container.prototype.createLineAnnotation = function (group, options) {
  const defaultOptions = {
    x1: 0,
    y1: 70,
    x2: 0,
    y2: 50,
    style: {
      stroke: "black",
      strokeWidth: 1
    }
  };

  // Merge default options and user-specified options
  const combinedOptions = mergeOptions.call(this, defaultOptions, "lineAnnotationOptions", options);
  let { x1, y1, x2, y2, style } = combinedOptions;

  // Convert x1, y1, x2, y2 to arrays if they are not already
  if (!Array.isArray(x1)) x1 = [x1];
  if (!Array.isArray(y1)) y1 = [y1];
  if (!Array.isArray(x2)) x2 = [x2];
  if (!Array.isArray(y2)) y2 = [y2];

  // Extract additional options that are not in defaultOptions
  const additionalOptionsStyle = extractAdditionalOptions(style, defaultOptions.style);

    var group = this.svg.append("g")
    .attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);

  // Iterate over each set of line coordinates
  for (let i = 0; i < Math.max(x1.length, y1.length, x2.length, y2.length); i++) {
    const currentX1 = x1[Math.min(i, x1.length - 1)];
    const currentY1 = y1[Math.min(i, y1.length - 1)];
    const currentX2 = x2[Math.min(i, x2.length - 1)];
    const currentY2 = y2[Math.min(i, y2.length - 1)];

    // Create the line element with merged styles for each set of values
    group.append("line")
      .attr("x1", this.xScale(currentX1))
      .attr("y1", this.yScale(currentY1))
      .attr("x2", this.xScale(currentX2))
      .attr("y2", this.yScale(currentY2))
      .style("stroke", style.stroke)
      .style("stroke-width", style.strokeWidth)
      .each(function () {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsStyle);
      });
  }
};

container.prototype.createArrowAnnotation = function (group, options) {
  const defaultOptions = {
    x1: 1,
    y1: 70,
    x2: 1,
    y2: 50,
    arrowSize: 8,
    arrowStyle: {
      fill: "black",
    },
    lineStyle: {
      stroke: "black",
      strokeWidth: 1
    }
  };

  // Merge default options and user-specified options
  const combinedOptions = mergeOptions.call(this, defaultOptions, "arrowAnnotationOptions", options);
  let { x1, y1, x2, y2, arrowSize, arrowStyle, lineStyle } = combinedOptions;

  // Convert x1, y1, x2, y2, and arrowSize to arrays if they are not already
  if (!Array.isArray(x1)) x1 = [x1];
  if (!Array.isArray(y1)) y1 = [y1];
  if (!Array.isArray(x2)) x2 = [x2];
  if (!Array.isArray(y2)) y2 = [y2];
  if (!Array.isArray(arrowSize)) arrowSize = [arrowSize];

  // Extract additional options that are not in defaultOptions
  const additionalOptionsLine = extractAdditionalOptions(lineStyle, defaultOptions.lineStyle);
  const additionalOptionsArrow = extractAdditionalOptions(arrowStyle, defaultOptions.arrowStyle);

  var group = this.svg.append("g")
    .attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);

  // Iterate over each element in the arrays
  for (let i = 0; i < Math.max(x1.length, y1.length, x2.length, y2.length, arrowSize.length); i++) {
    const currentX1 = x1[Math.min(i, x1.length - 1)];
    const currentY1 = y1[Math.min(i, y1.length - 1)];
    const currentX2 = x2[Math.min(i, x2.length - 1)];
    const currentY2 = y2[Math.min(i, y2.length - 1)];
    const currentArrowSize = arrowSize[Math.min(i, arrowSize.length - 1)];

    // Create a marker for each line
    this.svg.append("defs").append("marker")
      .attr("id", `arrowhead-${i}-${currentX1}-${currentX2}`)
      .attr("viewBox", "-0 -5 10 10")
      .attr("refX", 5)
      .attr("refY", 0)
      .attr("orient", "auto")
      .attr("markerWidth", currentArrowSize)
      .attr("markerHeight", currentArrowSize)
      .attr("xoverflow", "visible")
      .append("path")
      .attr("d", "M 0,-5 L 10 ,0 L 0,5")
      .attr("fill", arrowStyle.fill)
      .each(function () {
        d3.select(this).style(additionalOptionsArrow);
      });

    // Draw the line with the arrow marker for each set of values
    group.append("line")
      .attr("x1", this.xScale(currentX1))
      .attr("y1", this.yScale(currentY1))
      .attr("x2", this.xScale(currentX2))
      .attr("y2", this.yScale(currentY2 + currentArrowSize / 2))
      .attr("marker-end", `url(#arrowhead-${i}-${currentX1}-${currentX2})`)
      .style("stroke", lineStyle.stroke)
      .style("stroke-width", lineStyle.strokeWidth)
      .each(function () {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsLine);
      });
  }
};

container.prototype.createTextMarkerAnnotation = function (group, options) {
  const defaultOptions = {
    x1: null,
    y1: 66,
    x2: null,
    y2: 50,
    position: 0,
    text: "",
    labelX: 0,
    labelY: 0,
    showArrow: false,
    arrowSize: 8,
    textStyle: {
      fontSize: "10px",
      fontFamily: "sans-serif",
      fill: "black",
      textAnchor: "middle"
    },
    arrowStyle: {
      fill: "black",
    },
    lineStyle: {
      stroke: "black",
      strokeWidth: 1
    }
  };

  // Merge default options and user-specified options
  const combinedOptions = mergeOptions.call(this, defaultOptions, "textMarkerAnnotationOptions", options);
  let { textStyle, arrowStyle, lineStyle } = combinedOptions;

  // Convert all options to arrays if not already
  const keys = ['x1', 'y1', 'x2', 'y2', 'position', 'text', 'labelX', 'labelY', 'showArrow', 'arrowSize'];
  keys.forEach(key => {
    if (!Array.isArray(combinedOptions[key])) {
      combinedOptions[key] = [combinedOptions[key]];
    }
  });

  const additionalOptionsLine = extractAdditionalOptions(lineStyle, defaultOptions.lineStyle);
  const additionalOptionsArrow = extractAdditionalOptions(arrowStyle, defaultOptions.arrowStyle);

  // Calculate the maximum length across all arrays
  const maxLength = Math.max(...keys.map(key => combinedOptions[key].length));

  for (let i = 0; i < maxLength; i++) {
    let currentValues = {};
    keys.forEach(key => {
      currentValues[key] = combinedOptions[key][i % combinedOptions[key].length];
    });

    let offsetY2 = currentValues.showArrow ? currentValues.arrowSize / 2 : 0;
    var group = this.svg.insert("g", ":first-child")
      .attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);

  const currentX1 = Math.round(this.xScale(currentValues.x1 !== null ? currentValues.x1 : currentValues.position));
  const currentX2 = Math.round(this.xScale(currentValues.x2 !== null ? currentValues.x2 : currentValues.position));

  // Create line element
  const line = group.append("line")
    .attr("x1", currentX1)
    .attr("y1", this.yScale(currentValues.y1))
    .attr("x2", currentX2)
    .attr("y2", this.yScale(currentValues.y2 + offsetY2))
    .style("stroke", lineStyle.stroke)
    .style("stroke-width", lineStyle.strokeWidth)
    .each(function () {
      const currentElement = d3.select(this);
      setStyleFromOptions(currentElement, additionalOptionsLine);
    });

  // Create a marker for each line with currentX1 and currentX2 in the ID
  this.svg.append("defs").append("marker")
    .attr("id", `arrowhead-marker-${i}-${currentX1}-${currentX2}`)
    .attr("viewBox", "-0 -5 10 10")
    .attr("refX", 5)
    .attr("refY", 0)
    .attr("orient", "auto")
    .attr("markerWidth", currentValues.arrowSize)
    .attr("markerHeight", currentValues.arrowSize)
    .attr("xoverflow", "visible")
    .append("path")
    .attr("d", "M 0,-5 L 10 ,0 L 0,5")
    .attr("fill", arrowStyle.fill)
    .each(function () {
      const currentElement = d3.select(this);
      setStyleFromOptions(currentElement, additionalOptionsArrow);
    });
    // Add arrow marker to line if showArrow is true
    if (currentValues.showArrow) {
      line.attr("marker-end", `url(#arrowhead-marker-${i}-${currentX1}-${currentX2})`);
    }

    // Create text element if text is provided
    if (currentValues.text) {
      group.append("text")
        .attr("x", this.xScale(currentValues.x1 !== null ? currentValues.x1 : currentValues.position) + currentValues.labelX)
        .attr("y", this.yScale(currentValues.y1) - 5 - currentValues.labelY)
        .style("text-anchor", textStyle.textAnchor)
        .style("font-size", textStyle.fontSize)
        .style("font-family", textStyle.fontFamily)
        .style("fill", textStyle.fill)
        .each(function () {
          const currentElement = d3.select(this);
          parseAndStyleText(currentValues.text, currentElement, textStyle);
          setStyleFromOptions(currentElement, extractAdditionalOptions(textStyle, defaultOptions.textStyle));
        });
    }
  }

  return group;
};

container.prototype.createSymbolAnnotation = function(group, options) {
  const defaultOptions = {
    x: 0,
    y: 50,
    size: 64,
    symbol: "circle",
    style: {
      fill: "black",
      stroke: "black",
      strokeWidth: 2
    },
    rotation: 0
  };

  // Merge default options and user-specified options
  const combinedOptions = mergeOptions.call(this, defaultOptions, "symbolAnnotationOptions", options);
  let { x, y, size, symbol, style, rotation } = combinedOptions;

  // Convert x, y, symbol, and rotation to arrays if they are not already
  if (!Array.isArray(x)) x = [x];
  if (!Array.isArray(y)) y = [y];
  if (!Array.isArray(symbol)) symbol = [symbol];
  if (!Array.isArray(rotation)) rotation = [rotation];
  if (!Array.isArray(size)) size = [size];

  var group = this.svg.append("g")
    .attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);

  // Extract additional options that are not in defaultOptions
  const additionalOptionsStyle = extractAdditionalOptions(style, defaultOptions.style);

  // Symbol type mapping
  const symbolTypes = {
    'circle': d3.symbolCircle,
    'cross': d3.symbolCross,
    'diamond': d3.symbolDiamond,
    'square': d3.symbolSquare,
    'star': d3.symbolStar,
    'triangle': d3.symbolTriangle,
    'wye': d3.symbolWye
  };

  // Iterate over each element in the arrays
  for (let i = 0; i < Math.max(x.length, y.length, symbol.length, rotation.length); i++) {
    const currentX = x[Math.min(i, x.length - 1)];
    const currentY = y[Math.min(i, y.length - 1)];
    const currentSymbol = symbol[Math.min(i, symbol.length - 1)];
    const currentRotation = rotation[Math.min(i, rotation.length - 1)];
    const currentSize = size[Math.min(i, size.length - 1)];

    // Check if the symbol type is valid
    if (!symbolTypes[currentSymbol]) {
      console.error(`Unsupported symbol type: ${currentSymbol}`);
      continue;
    }

    // Create the symbol
    const d3Symbol = d3.symbol().type(symbolTypes[currentSymbol]).size(currentSize);

    // Create the symbol element with merged styles for each set of values
    group.append("path")
      .attr("d", d3Symbol)
      .attr("transform", `translate(${this.xScale(currentX)}, ${this.yScale(currentY)}) rotate(${currentRotation})`)
      .style("fill", style.fill)
      .style("stroke", style.stroke)
      .style("stroke-width", style.strokeWidth)
      .each(function () {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsStyle);
      });
  }

  return group;
};

container.prototype.createRectangleAnnotation = function(group, options) {
  const defaultOptions = {
    position: [[[9300, 20], [9400, 50]]], // Default as a nested array for multiple rectangles
    rotation: [0], // Array to support multiple rotations
    style: {
      fill: "#0000",
      stroke: "black",
      strokeWidth: 2
    }
  };

  // Merge default options and user-specified options
  const combinedOptions = mergeOptions.call(this, defaultOptions, "rectangleAnnotationOptions", options);
  let { position, style, rotation } = combinedOptions;

  // Normalize position to be an array of arrays
  if (!Array.isArray(position[0][0])) {
    position = [position];
  }
  if (!Array.isArray(rotation)) rotation = [rotation];

  // Extract additional options that are not in defaultOptions
  const additionalOptionsStyle = extractAdditionalOptions(style, defaultOptions.style);

  var group = this.svg.append("g")
    .attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);

  // Iterate over each element in the position and rotation arrays
  const numRects = Math.max(position.length, rotation.length);
  for (let i = 0; i < numRects; i++) {
    const currentPos = position[Math.min(i, position.length - 1)];
    const currentRot = rotation[Math.min(i, rotation.length - 1)];

    // Calculate x, y, width, and height from the position array
    const x1 = Math.min(currentPos[0][0], currentPos[1][0]);
    const x2 = Math.max(currentPos[0][0], currentPos[1][0]);
    const y1 = Math.max(currentPos[0][1], currentPos[1][1]);
    const y2 = Math.min(currentPos[0][1], currentPos[1][1]);
    const width = Math.abs(x1 - x2);
    const height = Math.abs(y2 - y1);

    // Create the rectangle element with merged styles
    group.append("rect")
      .attr("x", this.xScale(x1))
      .attr("y", this.yScale(y1))
      .attr("width", this.xScale(x2) - this.xScale(x1))
      .attr("height", this.yScale(y2) - this.yScale(y1))
      .attr("transform", `rotate(${currentRot}, ${this.xScale(x1 + width / 2)}, ${this.yScale(y1 + height / 2)})`)
      .style("fill", style.fill)
      .style("stroke", style.stroke)
      .style("stroke-width", style.strokeWidth)
      .each(function () {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsStyle);
      });
  }
};

container.prototype.createPromoterAnnotation = function(group, options) {

  const defaultOptions = {
    x: 0,
    y: 50,
    direction: 'forward',
    rotation: 0,
    scale: 1,
    style: {
      fill: "none",
      stroke: "black",
      strokeWidth: 1
    },
  };

  // Merge default options and user-specified options
  const combinedOptions = mergeOptions.call(this, defaultOptions, "PromoterAnnotationOptions", options);
  let { x, y, direction, rotation, scale, style } = combinedOptions;

    // Extract additional options that are not in defaultOptions
  const additionalOptionsStyle = extractAdditionalOptions(style, defaultOptions.style);

  if (!Array.isArray(x)) x = [x];
  if (!Array.isArray(y)) y = [y];
  if (!Array.isArray(direction)) direction = [direction];
  if (!Array.isArray(rotation)) rotation = [rotation];
  if (!Array.isArray(scale)) scale = [scale];

    // Define the custom path and mirrored path
  const mirroredPath = "M -8 -17.5 L -13 -14 l 5 3.5 M -13 -14 L 0 -14 v 14";
  const customPath = "M 8 -17.5 L 13 -14 l -5 3.5 M 13 -14 H 0 v 14";

  var group = this.svg.append("g")
    .attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);

  // Iterate over each element in the arrays
  for (let i = 0; i < Math.max(x.length, y.length, direction.length, rotation.length, scale.length); i++) {

    const currentX = x[Math.min(i, x.length - 1)];
    const currentY = y[Math.min(i, y.length - 1)];
    const currentDirection = direction[Math.min(i, direction.length - 1)];
    const currentRotation = rotation[Math.min(i, rotation.length - 1)];
    const currentScale = scale[Math.min(i, scale.length - 1)];

    // Choose the appropriate path based on direction
    const pathToUse = currentDirection === "forward" ? customPath : mirroredPath;

    // Calculate the x position
    const xPosition = this.xScale(currentX);

    // Create the symbol element for each set of values
    group.append("path")
      .attr("d", pathToUse)
      .attr("transform", `translate(${xPosition}, ${this.yScale(currentY)}) scale(${currentScale}) rotate(${currentRotation})`)
      .style("fill", style.fill)
      .style("stroke", style.stroke)
      .style("stroke-width", style.strokeWidth)
      .each(function () {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsStyle);
      });
  }
};

container.prototype.createTerminatorAnnotation = function(group, options) {

  const defaultOptions = {
    x: 0,
    y: 50,
    direction: 'forward',
    rotation: 0,
    scale: 1,
    style: {
      fill: "none",
      stroke: "black",
      strokeWidth: 1
    },
  };

  // Merge default options and user-specified options
  const combinedOptions = mergeOptions.call(this, defaultOptions, "terminatorAnnotationOptions", options);
  let { x, y, direction, rotation, scale, style } = combinedOptions;

    // Extract additional options that are not in defaultOptions
  const additionalOptionsStyle = extractAdditionalOptions(style, defaultOptions.style);

  if (!Array.isArray(x)) x = [x];
  if (!Array.isArray(y)) y = [y];
  if (!Array.isArray(direction)) direction = [direction];
  if (!Array.isArray(rotation)) rotation = [rotation];
  if (!Array.isArray(scale)) scale = [scale];

  // Define the custom paths
  const customPath = "M -8 17.5 L -13 14 l 5 -3.5 M -13 14 H 0 v -14";
  const mirroredPath = "M 8 17.5 L 13 14 l -5 -3.5 M 13 14 L 0 14 v -14";

  var group = this.svg.append("g")
    .attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);

  // Iterate over each element in the arrays
  for (let i = 0; i < Math.max(x.length, y.length, direction.length, rotation.length, scale.length); i++) {

    const currentX = x[Math.min(i, x.length - 1)];
    const currentY = y[Math.min(i, y.length - 1)];
    const currentDirection = direction[Math.min(i, direction.length - 1)];
    const currentRotation = rotation[Math.min(i, rotation.length - 1)];
    const currentScale = scale[Math.min(i, scale.length - 1)];

    // Choose the appropriate path based on direction
    const pathToUse = currentDirection === "forward" ? customPath : mirroredPath;

    // Calculate the x position
    const xPosition = this.xScale(currentX);

    // Create the symbol element for each set of values
    group.append("path")
      .attr("d", pathToUse)
      .attr("transform", `translate(${xPosition}, ${this.yScale(currentY)}) scale(${currentScale}) rotate(${currentRotation})`)
      .style("fill", style.fill)
      .style("stroke", style.stroke)
      .style("stroke-width", style.strokeWidth)
      .each(function () {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsStyle);
      });
  }
};

container.prototype.createCurveAnnotation = function (group, options) {
    const defaultOptions = {
        x1: [0],
        y1: [50],
        x2: [100],
        y2: [50],
        midY: [80],
        stroke: ["black"],
        strokeWidth: [1],
        lineStyle: {},
        text: [""],
        labelX: [0],
        labelY: [0],
        textStyle: {
            fontSize: "10px",
            fontFamily: "sans-serif",
            fill: "black",
            textAnchor: "middle"
        }
    };

    // Merge default options and user-specified options
    const combinedOptions = mergeOptions.call(this, defaultOptions, "curveAnnotationOptions", options);

    // Convert all coordinates and styles to arrays if not already
    const keys = ['x1', 'y1', 'x2', 'y2', 'midY', 'stroke', 'strokeWidth', 'text', 'labelX', 'labelY'];
    keys.forEach(key => {
        if (!Array.isArray(combinedOptions[key])) {
            combinedOptions[key] = [combinedOptions[key]];
        }
    });

    let { x1, y1, x2, y2, midY, stroke, strokeWidth, lineStyle, text, labelX, labelY, textStyle } = combinedOptions;

    var group = this.svg.append("g")
        .attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);

    // Iterate over each set of points
    for (let i = 0; i < Math.max(x1.length, y1.length, x2.length, y2.length, text.length, labelX.length, labelY.length); i++) {
        const currentX1 = x1[i % x1.length];
        const currentY1 = y1[i % y1.length];
        const currentX2 = x2[i % x2.length];
        const currentY2 = y2[i % y2.length];
        const currentMidY = midY[i % midY.length];
        const currentStroke = stroke[i % stroke.length];
        const currentStrokeWidth = strokeWidth[i % strokeWidth.length];
        const currentText = text[i % text.length];
        const currentLabelX = labelX[i % labelX.length];
        const currentLabelY = labelY[i % labelY.length];
        const midX = (currentX1 + currentX2) / 2;

        // Define the line generator with a curve
        const lineGenerator = d3.line()
            .curve(d3.curveBasis)
            .x(d => this.xScale(d.x))
            .y(d => this.yScale(d.y));

        // Create the path using the start, middle (control point), and end coordinates
        const points = [
            { x: currentX1, y: currentY1 },
            { x: midX, y: currentMidY },
            { x: currentX2, y: currentY2 }
        ];

        // Create the path element with merged styles
        const path = group.append("path")
            .attr("d", lineGenerator(points))
            .style("stroke", currentStroke)
            .style("stroke-width", currentStrokeWidth)
            .style("fill", "none");

        // Apply additional line styles if any
        setStyleFromOptions(path, extractAdditionalOptions(lineStyle, defaultOptions.lineStyle));

        // Add text at the highest point of the curve with offsets
        const textElement = group.append("text")
            .attr("x", this.xScale(midX) + currentLabelX)
            .attr("y", this.yScale(currentMidY) - currentLabelY)
            .text(currentText);

        // Apply text styles
        setStyleFromOptions(textElement, textStyle);
    }
};
