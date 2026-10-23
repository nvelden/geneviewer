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

// Wraps a text element into lines of at most width pixels. <b>, <i> and <u>
// tags are read before the text is split into words, so each word keeps its
// style when a tagged part runs over two lines.
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

    // Words as lists of styled pieces: "pQEB1<i>-like</i>" is one word. A word
    // split at a hyphen is not followed by a space
    var words = [];
    var word = [];
    var endWord = function (space) {
      if (word.length) words.push({ pieces: word, space: space });
      word = [];
    };
    styledSegments(text.text()).forEach(function (segment) {
      segment.content.split(/(\s+)/).forEach(function (part) {
        if (/^\s+$/.test(part)) {
          endWord(true);
          return;
        }
        var pieces = splitOnHyphen ? part.split(/(?<=-)/) : [part];
        pieces.forEach(function (piece, i) {
          if (piece) word.push({ ...segment, content: piece });
          if (i < pieces.length - 1) endWord(false);
        });
      });
    });
    endWord(true);

    // Writes words into a line
    var setLine = function (line, lineWords) {
      line.selectAll("*").remove();
      lineWords.forEach(function (lineWord, i) {
        lineWord.pieces.forEach(function (piece, j) {
          var last = j === lineWord.pieces.length - 1 && i < lineWords.length - 1;
          var space = last && lineWord.space ? " " : "";
          var tspan = line.append("tspan").text(piece.content + space);
          if (piece.bold) tspan.style("font-weight", "bold");
          if (piece.italic) tspan.style("font-style", "italic");
          if (piece.underline) tspan.style("text-decoration", "underline");
        });
      });
    };

    text.text(null); // Empty the text element

    var line = text.append("tspan").attr("class", "wrap-line");
    var lineWords = [];

    for (var i = 0; i < words.length; i++) {
      lineWords.push(words[i]);
      setLine(line, lineWords);

      if (line.node().getComputedTextLength() > width && lineWords.length > 1) {
        lineWords.pop();
        setLine(line, lineWords);
        lineWords = [words[i]];
        line = text.append("tspan").attr("class", "wrap-line");
        setLine(line, lineWords);
      }
    }

    var lines = text.selectAll("tspan.wrap-line");
    var h = lineHeightEms;

    if (lines.size() > 2)
      for (var i = 0; i < lines.size(); i++) h *= lineHeightSquishFactor;

    lines.each(function (d, i) {
      var dy = i * h + dyAdjust;
      if (centreVertically) dy -= ((lines.size() - 1) * h) / 2;
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

// Rotates labels that overlap another label of labelSelector in the
// container's SVG. Call adjust(label, options) for each label in turn (it
// returns true when it rotated the label) and refresh(label) after restyling a label. Label sizes are measured once and
// measured again only when a label changes, instead of on every comparison.
function createLabelAdjuster(container, labelSelector) {
  // Default options
  const defaultOptions = {
    rotation: -65, // Rotation angle (in degrees)
    offsetX: 0,
    offsetY: 0,
    dx: "0em", // Horizontal adjustment
    dy: "0em", // Vertical adjustment
  };

  const overlapPercentage = (rect1, rect2) => {
    const x_overlap = Math.max(0, Math.min(rect1.right, rect2.right) - Math.max(rect1.left, rect2.left));
    const y_overlap = Math.max(0, Math.min(rect1.bottom, rect2.bottom) - Math.max(rect1.top, rect2.top));
    const overlapArea = x_overlap * y_overlap;
    const rect1Area = (rect1.right - rect1.left) * (rect1.bottom - rect1.top);
    return (overlapArea / rect1Area) * 100;
  };

  // Measured on the first adjust(), when all labels are drawn
  let labels = null;
  const rects = new Map();

  const refresh = (label) => {
    if (labels) rects.set(label, label.getBoundingClientRect());
  };

  const adjust = (specificLabel, options = {}) => {
    if (!labels) {
      labels = container.svg.selectAll(labelSelector).nodes();
      labels.forEach(label => rects.set(label, label.getBoundingClientRect()));
    }

    const { rotation, offsetX, offsetY, dx, dy } = { ...defaultOptions, ...options };

    // Calculate the label's original center position
    const bbox = specificLabel.getBBox();
    let centerX = bbox.x + bbox.width / 2;
    let centerY = bbox.y;

    centerX += offsetX;
    centerY += (rotation < 0) ? bbox.height : bbox.height / 2;
    centerY += offsetY;

    // Check for overlap with other labels
    const specificLabelRect = rects.get(specificLabel);
    for (const label of labels) {
      // If the specific label overlaps with another label
      if (label !== specificLabel && overlapPercentage(specificLabelRect, rects.get(label)) > 0) {

        // Adjust the label rotation and position
        d3.select(specificLabel)
          .style("text-anchor", "start")
          .attr("dx", dx)
          .attr("dy", dy)
          .attr("x", centerX + offsetX)
          .attr("y", centerY)
          .attr("transform", `rotate(${rotation}, ${centerX}, ${centerY})`);

        refresh(specificLabel);
        return true;
      }
    }
    return false;
  };

  return { adjust, refresh };
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

// True for an opaque white colour ("#FFF", "white", "rgb(255, 255, 255)"),
// false for other colours and pattern fills ("url(#...)")
function isWhite(color) {
  const rgb = typeof color === "string" ? d3.color(color)?.rgb() : null;
  return !!rgb && rgb.r === 255 && rgb.g === 255 && rgb.b === 255 && rgb.opacity === 1;
}

function isPlainObject(value) {
  return value !== null && typeof value === 'object' && !Array.isArray(value);
}

// Deep merge of options: nested objects are merged at every level, other values
// (arrays included) replace the base value. null means not set, and so does an
// empty array in place of an object (an empty list() from R). Returns new objects,
// so the base and override (e.g. a theme shared by all clusters) are not changed.
function deepMergeOptions(base, override) {
  const result = { ...base };
  if (!isPlainObject(override)) {
    return result;
  }
  for (const key in override) {
    const value = override[key];
    if (value === null || value === undefined) {
      continue;
    }
    if (isPlainObject(value)) {
      result[key] = deepMergeOptions(isPlainObject(result[key]) ? result[key] : {}, value);
    } else if (Array.isArray(value) && value.length === 0 && isPlainObject(result[key])) {
      continue;
    } else {
      result[key] = value;
    }
  }
  return result;
}

// Multiplies a size ("12px", "1.2em", 12) by factor, keeping its unit
function scaleSize(value, factor) {
  if (typeof value === "number") {
    return value * factor;
  }
  const match = typeof value === "string" && value.trim().match(/^(-?[\d.]+)([a-z%]*)$/i);
  return match ? `${parseFloat(match[1]) * factor}${match[2]}` : value;
}

// Scales every fontSize in options (at any level) by factor. Returns new objects.
function scaleFontSizes(options, factor) {
  if (!isPlainObject(options)) {
    return options;
  }
  const result = {};
  for (const key in options) {
    result[key] = key === "fontSize" ? scaleSize(options[key], factor) : scaleFontSizes(options[key], factor);
  }
  return result;
}

// Order of precedence: defaultOptions < theme (themeOptions[themeOptionsKey]) < userOptions.
// themeOptions: the chart theme, by default the theme of the container (this).
// The textScale of the theme scales the font sizes of the defaults and the theme,
// not those set by the user.
function mergeOptions(defaultOptions, themeOptionsKey, userOptions = {}, themeOptions = this?.themeOptions) {
  const themeOpts = themeOptions?.[themeOptionsKey];
  let options = deepMergeOptions(defaultOptions, themeOpts);
  const textScale = themeTextScale(themeOptions);
  if (textScale !== 1) {
    options = scaleFontSizes(options, textScale);
  }
  return deepMergeOptions(options, userOptions);
}

// Text size of the theme, as a factor of the default font sizes
function themeTextScale(themeOptions) {
  const textScale = themeOptions?.textScale;
  return typeof textScale === "number" && textScale > 0 ? textScale : 1;
}

function getColorScale(colorScheme, customColors, uniqueGroups) {
  const schemeCategory30 = [
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
    "#393b79", "#637939", "#8c6d31", "#843c39", "#7b4173",
    "#bd9e39", "#ad494a", "#d6616b", "#31a354", "#cedb9c",
    "#9c9ede", "#5254a3", "#8ca252", "#b5cf6b", "#e7ba52",
    "#ce6dbd", "#e7cb94", "#e7969c", "#6b6ecf", "#a55194"
  ];

  // Colours are assigned to the non-null groups only, so genes() (which keeps
  // null groups) and legend() (which drops them) give each group the same colour.
  // Null groups are white.
  const groups = uniqueGroups.filter(group => group !== null);

  // Colours of the colour scheme: a d3 scheme name or an array of colours (e.g.
  // the palette of a theme), by default schemeCategory30
  const schemeRange = () => {
    if (colorScheme && (Array.isArray(colorScheme) || d3[colorScheme])) {
      const scheme = Array.isArray(colorScheme) ? colorScheme : d3[colorScheme];
      // Check if there are more groups than colors in the scheme
      if (groups.length > scheme.length) {
        console.warn(`Warning: More unique groups than colors. Some colors will repeat.`);
      }
      return groups.map((group, index) => scheme[index % scheme.length]);
    }
    if (colorScheme) {
      console.warn(`Warning: The color scheme "${colorScheme}" does not exist. Defaulting to schemeCategory30.`);
    }
    return groups.map((group, index) =>
      group === "No Hit" ? "#FFF" : schemeCategory30[index % schemeCategory30.length]
    );
  };

  let range;
  if (customColors && typeof customColors === "string") {
    // Handle customColors as a single color
    range = groups.map(() => customColors);
  } else if (customColors && Array.isArray(customColors)) {
    // Handle customColors as an array
    range = groups.map((group, index) => customColors[index % customColors.length] || "#FFF");
  } else if (customColors && typeof customColors === 'object') {
    // Handle customColors as an object: groups it leaves out keep the colour
    // of the colour scheme
    const scheme = schemeRange();
    range = groups.map((group, index) => customColors[group] || scheme[index]);
  } else {
    range = schemeRange();
  }

  return d3.scaleOrdinal()
    .domain([...groups, null])
    .range([...range, "#FFF"]);
}

// Pattern tiles: each draws the foreground of a square pattern tile of size s
// on the background colour bg.
const genePatterns = {
  none: () => {},
  solid: (p, s, color) => p.append("rect").attr("width", s).attr("height", s).attr("fill", color),
  horizontal: (p, s, color, w) => p.append("path").attr("d", `M0,${s / 2}H${s}`).attr("stroke", color).attr("stroke-width", w),
  vertical: (p, s, color, w) => p.append("path").attr("d", `M${s / 2},0V${s}`).attr("stroke", color).attr("stroke-width", w),
  // The corner segments keep the lines unbroken where tiles meet
  diagonal: (p, s, color, w) => p.append("path")
    .attr("d", `M0,${s}L${s},0M${-s / 4},${s / 4}L${s / 4},${-s / 4}M${s * 3 / 4},${s * 5 / 4}L${s * 5 / 4},${s * 3 / 4}`)
    .attr("stroke", color).attr("stroke-width", w),
  antidiagonal: (p, s, color, w) => p.append("path")
    .attr("d", `M0,0L${s},${s}M${-s / 4},${s * 3 / 4}L${s / 4},${s * 5 / 4}M${s * 3 / 4},${-s / 4}L${s * 5 / 4},${s / 4}`)
    .attr("stroke", color).attr("stroke-width", w),
  grid: (p, s, color, w) => p.append("path").attr("d", `M0,${s / 2}H${s}M${s / 2},0V${s}`).attr("stroke", color).attr("stroke-width", w),
  crosshatch: (p, s, color, w) => {
    genePatterns.diagonal(p, s, color, w);
    genePatterns.antidiagonal(p, s, color, w);
  },
  dots: (p, s, color) => p.append("circle").attr("cx", s / 2).attr("cy", s / 2).attr("r", s / 5).attr("fill", color),
  zigzag: (p, s, color, w) => p.append("path")
    .attr("d", `M0,${s * 0.7}L${s / 2},${s * 0.3}L${s},${s * 0.7}`)
    .attr("fill", "none").attr("stroke", color).attr("stroke-width", w),
  checkerboard: (p, s, color) => p.append("path")
    .attr("d", `M0,0H${s / 2}V${s / 2}H0ZM${s / 2},${s / 2}H${s}V${s}H${s / 2}Z`)
    .attr("fill", color),
  rings: (p, s, color, w) => p.append("circle").attr("cx", s / 2).attr("cy", s / 2).attr("r", s / 3.2)
    .attr("fill", "none").attr("stroke", color).attr("stroke-width", w * 0.8),
  // Rows of bricks, each row offset by half a brick
  bricks: (p, s, color, w) => p.append("path")
    .attr("d", `M0,0H${s}M0,${s / 2}H${s}M${s / 4},0V${s / 2}M${s * 3 / 4},${s / 2}V${s}`)
    .attr("stroke", color).attr("stroke-width", w),
  plus: (p, s, color, w) => p.append("path")
    .attr("d", `M${s / 2},${s * 0.2}V${s * 0.8}M${s * 0.2},${s / 2}H${s * 0.8}`)
    .attr("stroke", color).attr("stroke-width", w),
  // Filled band, so it reads as thick stripes at any strokeWidth
  wideStripes: (p, s, color) => p.append("path")
    .attr("d", `M0,${s}L${s},0H${s / 2}L0,${s / 2}ZM${s},${s / 2}V${s}H${s / 2}Z`)
    .attr("fill", color),
  // Background-coloured dots on a solid tile
  inverseDots: (p, s, color, w, bg) => {
    genePatterns.solid(p, s, color);
    p.append("circle").attr("cx", s / 2).attr("cy", s / 2).attr("r", s / 4).attr("fill", bg);
  }
};

// Tile size of each pattern relative to size; shapes that need more room get a larger tile
const patternTileScale = { checkerboard: 4 / 3, rings: 7 / 6, bricks: 5 / 3, plus: 4 / 3, wideStripes: 4 / 3 };

// Patterns that look alike (diagonal and wideStripes, dots and rings, grid and plus,
// diagonal and antidiagonal) are far apart, so neighbouring groups do not look alike
const defaultPatternOrder = [
  "diagonal", "dots", "horizontal", "crosshatch", "vertical", "zigzag", "checkerboard", "solid",
  "rings", "bricks", "inverseDots", "grid", "wideStripes", "plus", "antidiagonal"
];

// Returns the value for each group from a single value, an array (recycled) or an object (by group name).
function valuesByGroup(values, groups, fallback) {
  if (values === null || values === undefined) {
    return groups.map(() => fallback);
  }
  if (typeof values === "string") {
    return groups.map(() => values);
  }
  if (Array.isArray(values)) {
    return groups.map((group, index) => values[index % values.length]);
  }
  return groups.map(group => values[group] ?? fallback);
}

// Adds a <pattern> for each group to the <defs> of svg and returns a fill and
// a stroke function per group. Patterns are defined in every SVG that uses
// them, because the save button renders each SVG on its own.
function getPatternScale(svg, pattern, uniqueGroups, colorScale, idPrefix) {
  if (!pattern) {
    return { fill: colorScale, stroke: colorScale, outline: null, align: () => {} };
  }

  const { patterns = null, color = "black", background = null, size = 6, strokeWidth = 1 } = pattern;

  // Same groups and order as getColorScale, so genes and legend match
  const groups = uniqueGroups.filter(group => group !== null);
  const types = valuesByGroup(patterns, groups, "none");
  if (patterns === null || patterns === undefined) {
    groups.forEach((group, index) => {
      types[index] = defaultPatternOrder[index % defaultPatternOrder.length];
    });
    if (groups.length > defaultPatternOrder.length) {
      console.warn(`Warning: More unique groups than patterns. Some patterns will repeat.`);
    }
  }
  const backgrounds = valuesByGroup(background, groups, null);
  const colors = valuesByGroup(color, groups, "black");

  const svgId = svg.attr("id") || "geneviewer";
  const defs = svg.append("defs");
  const fills = new Map();
  const tiles = new Map();

  groups.forEach((group, index) => {
    let type = types[index];
    if (!genePatterns[type]) {
      console.warn(`Warning: The pattern "${type}" does not exist. Using "none".`);
      type = "none";
    }
    // "No Hit" stays a plain white gene, like in the default colours
    if (group === "No Hit" && (patterns === null || patterns === undefined)) {
      type = "none";
    }
    const id = `${svgId}-${idPrefix}-pattern-${index}`;
    const tileSize = size * (patternTileScale[type] ?? 1);
    const bg = backgrounds[index] ?? colorScale(group);
    const tile = defs.append("pattern")
      .attr("id", id)
      .attr("patternUnits", "userSpaceOnUse")
      .attr("width", tileSize)
      .attr("height", tileSize);
    tile.append("rect")
      .attr("width", tileSize)
      .attr("height", tileSize)
      .attr("fill", bg);
    genePatterns[type](tile, tileSize, colors[index], strokeWidth, bg);
    fills.set(group, `url(#${id})`);
    tiles.set(group, tile);
  });

  const colorByGroup = new Map(groups.map((group, index) => [group, colors[index]]));

  return {
    fill: group => fills.get(group) ?? colorScale(group),
    // Lines (introns, UTR outlines) take the pattern colour when the
    // background replaces the gene colours, and keep the gene colour otherwise
    stroke: group => (background === null || background === undefined)
      ? colorScale(group)
      : (colorByGroup.get(group) ?? colors[0]),
    // Outline for legend markers, so white and patterned markers stay visible;
    // black when the groups have different pattern colours
    outline: typeof color === "string" ? color : "black",
    // Starts the tiles of a group at (x, y), e.g. at the corner of its legend marker
    align: (group, x, y) => tiles.get(group)?.attr("x", x).attr("y", y)
  };
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
  // The adjustment loop below stops at the first break right of a value,
  // so the breaks must be sorted by start
  breaks = [...breaks].sort((a, b) => a.start - b.start);

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

  // invert() of the linear scale doesn't know about the breaks: add back the
  // width of every break left of the position (breaks are sorted by start)
  scaleProxy.invert = function (px) {
    let value = linearScale.invert(px);
    for (let gap of breaks) {
      if (value > gap.start) {
        value += (gap.end - gap.start);
      } else {
        break;
      }
    }
    return value;
  };

  return scaleProxy;  // Return the discontinuous scale
}

// Splits a text into parts with their <b>, <i> or <u> style
function styledSegments(text) {
  const tagRegex = /<([biu])>(.*?)<\/\1>/g;
  const segments = [];
  const plain = content => ({ content, bold: false, italic: false, underline: false });
  let lastIndex = 0;
  let match;

  while ((match = tagRegex.exec(text)) !== null) {
    // Text before the tag
    if (match.index > lastIndex) {
      segments.push(plain(text.substring(lastIndex, match.index)));
    }
    const [, tag, content] = match;
    segments.push({ content, bold: tag === "b", italic: tag === "i", underline: tag === "u" });
    lastIndex = match.index + match[0].length;
  }

  // Any remaining text after the last tag
  if (lastIndex < text.length) {
    segments.push(plain(text.substring(lastIndex)));
  }
  return segments;
}

function parseAndStyleText(text, parentElement, fontOptions) {
  styledSegments(text).forEach(({ content, bold, italic, underline }) => {
    const tspan = parentElement.append("tspan")
      .style("font-weight", bold ? "bold" : fontOptions.weight)
      .style("font-style", italic ? "italic" : fontOptions.style)
      .text(content);

    if (underline) {
      tspan.style("text-decoration", "underline");
    }
  });
}

function container(svg, margin, width, height) {
  this.svg = svg;
  this.margin = margin;
  this.width = width;
  this.height = height;
}

// themeOptions: the chart theme ({ geneOptions: {...}, labelsOptions: {...}, ... }),
// kept on the container so all of its components use it
function createContainer(targetElementId, id, themeOptionsKey, options = {}, themeOptions = {}) {

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
  const combinedOptions = mergeOptions(defaultOptions, themeOptionsKey, options, themeOptions);
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

  const newContainer = new container(svg, computedMargin, width, height);
  newContainer.themeOptions = themeOptions;
  return newContainer;
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

// Styles copied from the page onto each element of an exported SVG, so it
// looks the same without the page's stylesheets (e.g. styles.css, fonts)
const svgExportInheritedStyles = [
  "fill", "fill-opacity", "fill-rule", "stroke", "stroke-width", "stroke-opacity",
  "stroke-dasharray", "stroke-linecap", "stroke-linejoin", "font-family",
  "font-size", "font-weight", "font-style", "text-anchor", "dominant-baseline",
  "visibility"
];
const svgExportOwnStyles = { opacity: "1" };

// Combines all SVGs of a chart (title, clusters, links, legend) into one
// standalone SVG at their position in el and returns it as a string
function chartToSVG(el, exclude = null) {
  const ns = "http://www.w3.org/2000/svg";
  const elRect = el.getBoundingClientRect();
  const width = Math.ceil(elRect.width);
  const height = Math.ceil(elRect.height);

  const root = document.createElementNS(ns, "svg");
  root.setAttribute("xmlns", ns);
  root.setAttribute("width", width);
  root.setAttribute("height", height);
  root.setAttribute("viewBox", `0 0 ${width} ${height}`);

  const addBackground = (parent, x, y, w, h, color) => {
    if (!color || color === "transparent" || color === "rgba(0, 0, 0, 0)") return;
    const rect = document.createElementNS(ns, "rect");
    rect.setAttribute("x", x);
    rect.setAttribute("y", y);
    rect.setAttribute("width", w);
    rect.setAttribute("height", h);
    rect.setAttribute("fill", color);
    // No anti-aliased seams between the backgrounds of adjacent SVGs
    rect.setAttribute("shape-rendering", "crispEdges");
    parent.appendChild(rect);
  };

  addBackground(root, 0, 0, width, height, getComputedStyle(el).backgroundColor);

  // Computed url() fills can be absolute (page url + #id); keep only the #id
  const localUrl = value => value.replace(/url\((["']?)[^#)]*#([^)"']+)\1\)/g, "url(#$2)");

  const copyStyles = (source, target, parentStyle) => {
    const computed = getComputedStyle(source);
    svgExportInheritedStyles.forEach(name => {
      const value = computed.getPropertyValue(name);
      if (value && (!parentStyle || parentStyle.getPropertyValue(name) !== value)) {
        target.style.setProperty(name, localUrl(value));
      }
    });
    Object.entries(svgExportOwnStyles).forEach(([name, initial]) => {
      const value = computed.getPropertyValue(name);
      if (value && value !== initial) target.style.setProperty(name, value);
    });
    return computed;
  };

  // Outermost SVGs only; nested ones are copied with their parent. Drawn in
  // z-index order (e.g. links below clusters), as on the page
  const zIndex = svg => parseInt(getComputedStyle(svg).zIndex, 10) || 0;
  const svgs = Array.from(el.querySelectorAll("svg"))
    .filter(svg => !svg.parentElement.closest("svg") && !(exclude && exclude.contains(svg)))
    .map((svg, order) => ({ svg, order, z: zIndex(svg) }))
    .sort((a, b) => a.z - b.z || a.order - b.order)
    .map(item => item.svg);

  svgs.forEach(svg => {
    const svgStyle = getComputedStyle(svg);
    if (svgStyle.display === "none" || svgStyle.visibility === "hidden") return;
    const rect = svg.getBoundingClientRect();
    if (rect.width === 0 || rect.height === 0) return;
    const x = rect.left - elRect.left;
    const y = rect.top - elRect.top;

    addBackground(root, x, y, rect.width, rect.height, svgStyle.backgroundColor);

    const clone = svg.cloneNode(true);
    clone.removeAttribute("style");
    clone.removeAttribute("class");
    clone.setAttribute("x", x);
    clone.setAttribute("y", y);
    clone.setAttribute("width", rect.width);
    clone.setAttribute("height", rect.height);

    // cloneNode keeps the structure, so both lists are in the same order
    const sources = svg.querySelectorAll("*");
    const targets = clone.querySelectorAll("*");
    const computedStyles = new Map([[svg, copyStyles(svg, clone, null)]]);
    sources.forEach((source, i) => {
      const target = targets[i];
      const computed = getComputedStyle(source);
      if (computed.display === "none" && !source.closest("defs")) {
        target.setAttribute("display", "none");
        return;
      }
      if (source.closest("defs")) return;
      target.removeAttribute("class");
      computedStyles.set(source, copyStyles(source, target, computedStyles.get(source.parentNode)));
    });

    // Hidden elements are left out of the file
    clone.querySelectorAll("[display='none']").forEach(node => node.remove());
    root.appendChild(clone);
  });

  return '<?xml version="1.0" encoding="UTF-8"?>\n' + new XMLSerializer().serializeToString(root);
}

function createSaveIcon(widgetId, el, show = true) {

  if (show) {
    var saveMenu = d3.select(el).append("div")
        .attr("id", `saveMenu-${widgetId}`)
        .style("position", "absolute")
        .style("z-index", "1000")
        .style("top", "10px")
        .style("right", "10px")
        .style("display", "none");

    var icon = saveMenu.append("i")
        .attr("class", "fa-solid fa-download")
        .attr("id", `saveIcon-${widgetId}`)
        .style("display", "block")
        .style("text-align", "right")
        .style("font-size", "20px")
        .style("color", "#D3D3D3")
        .style("padding", "5px")
        .style("cursor", "pointer");

    // Format options, shown when hovering the icon
    var options = saveMenu.append("div")
        .attr("id", `saveIconTooltip-${widgetId}`)
        .attr("class", "save-tooltip")
        .style("display", "none")
        .style("background-color", "#333")
        .style("border-radius", "3px")
        .style("font-size", "12px")
        .style("font-family", "Arial, sans-serif")
        .style("overflow", "hidden")
        .style("white-space", "nowrap");

    // Hide the menu so it is not in the PNG, and until the mouse enters again
    var hideMenu = function () {
        saveMenu.style("display", "none");
        options.style("display", "none");
        icon.style("color", "#D3D3D3");
    };

    var formats = [
      {
        label: "Save as SVG",
        save: function () {
          var svg = chartToSVG(el, saveMenu.node());
          saveAs(new Blob([svg], { type: "image/svg+xml;charset=utf-8" }), "graph.svg");
        }
      },
      {
        label: "Save as PNG",
        save: function () {
          html2canvas(el, { backgroundColor: null }).then(canvas => {
            canvas.toBlob(function (blob) {
              saveAs(blob, "graph.png");
            });
          });
        }
      }
    ];

    formats.forEach(function (format) {
      options.append("div")
        .attr("class", "save-option")
        .text(format.label)
        .style("color", "#fff")
        .style("padding", "5px 8px")
        .style("cursor", "pointer")
        .on("mouseenter", function () { d3.select(this).style("background-color", "#555"); })
        .on("mouseleave", function () { d3.select(this).style("background-color", null); })
        .on("click", function () {
          hideMenu();
          format.save();
        });
    });

    // Ensure icon is displayed on hover
    d3.select(el).on("mouseenter", function() {
        saveMenu.style("display", "block");
    }).on("mouseleave", hideMenu);

    // Change icon color on hover and display the format options
    saveMenu.on("mouseenter", function() {
        icon.style("color", "#555555");
        options.style("display", "block");
    }).on("mouseleave", function() {
        icon.style("color", "#D3D3D3");
        options.style("display", "none");
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

    // Keep one entry per link (null if it cannot be drawn) so indexes match the link data
    return links;
};

// Link colours, shared by the links and the colour bar
const defaultLinkColors = {
  normalColor: "#969696",
  invertedColor: "#d62728",
  // Colour of a link with a measure of 0, e.g. the background on a dark chart
  lowColor: "#FFFFFF",
  // Measures of the lightest and the full link colour
  vmin: 0,
  vmax: 100
};

// Colour of a link by its measure: from nearly white (10%
// of the colour) at vmin to the colour at vmax; measures outside are clamped
function linkColorScale(lowColor, color, vmin, vmax) {
  const nearlyWhite = d3.interpolateRgb(lowColor, color)(0.1);
  return d3.scaleLinear()
    .domain([vmin, vmax])
    .range([nearlyWhite, color])
    .interpolate(d3.interpolateRgb)
    .clamp(true);
}

function makeLinks(graphContainer, links, themeOptions = {}) {

    if (!links || links.length === 0) {
        return;
    }
    // Default options for title and subtitle
    const defaultOptions = {
      curve: true,
      ...defaultLinkColors,
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
      .style("z-index", 1);

    links.forEach(function(link) {

        const combinedOptions = mergeOptions(defaultOptions, "linkOptions", link.options, themeOptions)
        const { curve, invertedColor, normalColor, lowColor, vmin, vmax, useGroupColors, color, linkStyle, measure } = combinedOptions;

        const additionalOptionsLinkStyle = extractAdditionalOptions(linkStyle, defaultOptions.linkStyle);

        const linkData = HTMLWidgets.dataframeToD3(link.data)
        const coordinates = getLinkCoordinates(graphContainer, linkData);

      // Add svg paths
      coordinates.forEach(function(coordinate, index) {

                if (!coordinate) {
                    return;
                }

                const baseColor = coordinate[0].strand == coordinate[1].strand ? d3.rgb(normalColor) : d3.rgb(invertedColor)
                var colorScale = linkColorScale(lowColor, baseColor, vmin, vmax);
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

// Which link colours a chart has, over all link sets: like makeLinks(), a link
// is inverted (invertedColor) when its two ends are on different strands
function linkColorTypes(links) {
    let normal = false;
    let inverted = false;

    links.forEach(link => {
        const { start1 = [], end1 = [], start2 = [], end2 = [] } = link.data;
        for (let i = 0; i < start1.length; i++) {
            if ((start1[i] <= end1[i]) === (start2[i] <= end2[i])) {
                normal = true;
            } else {
                inverted = true;
            }
        }
    });

    return { normal, inverted };
}

function makeColorBar(graphContainer, links, themeOptions = {}) {

    // Default options for the color bars (both normal and inverted)
    const defaultOptions = {
        colorBar: true,
        ...defaultLinkColors,
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
              fontFamily: "sans-serif",
              xOffset: 2,
              yOffset: 0
            },
            titleOptions: {
              fontSize: 10,
              fontFamily: "sans-serif",
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

    // Theme link options under the user options, as in makeLinks()
    const linkOptions = mergeOptions(defaultOptions, "linkOptions", links[0].options, themeOptions);
    const { colorBarOptions } = linkOptions;
    const { labelOptions, titleOptions, barOptions } = colorBarOptions;

    if (linkOptions.colorBar === false || linkOptions.measure === 'none') {
      return null;
    }
    if (!links[0].data.hasOwnProperty('identity') && !links[0].data.hasOwnProperty('similarity')) {
      return null;
    }

    // Only draw a gradient for the link colours on the chart
    const { normal: hasNormal, inverted: hasInverted } = linkColorTypes(links);
    if (!hasNormal && !hasInverted) {
      return null;
    }

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

    const defs = g.append("defs");

    // The bar runs from vmin to vmax, like the colours of the links
    const minValue = linkOptions.vmin;
    const maxValue = linkOptions.vmax;

    // Calculate dimensions for drawing the color bar
    const contentWidth = graphRect.width - linkOptions.margin.left - linkOptions.margin.right;
    const contentHeight = graphRect.height - linkOptions.margin.top - linkOptions.margin.bottom;

    const width = parseFloat(colorBarOptions.width);
    const height = parseFloat(colorBarOptions.height);

    const xPosition = contentWidth - width - colorBarOptions.x;
    const yPosition = contentHeight - height - colorBarOptions.y;

    // Draw a gradient bar from lowColor to color (maxValue at the top) at x
    const drawBar = (baseId, color, x) => {
        const gradientId = getUniqueId(baseId);
        const gradient = defs.append("linearGradient")
            .attr("id", gradientId)
            .attr("gradientTransform", "rotate(90)");

        const colorScale = linkColorScale(linkOptions.lowColor, color, minValue, maxValue);

        gradient.append("stop")
            .attr("offset", "0%")
            .attr("stop-color", colorScale(maxValue));

        gradient.append("stop")
            .attr("offset", "100%")
            .attr("stop-color", colorScale(minValue));

        g.append("rect")
            .attr("x", x)
            .attr("y", yPosition)
            .attr("width", width)
            .attr("height", height)
            .style("fill", `url(#${gradientId})`)
            .style("stroke", barOptions.stroke)
            .style("stroke-width", barOptions.strokeWidth)
            .style("opacity", barOptions.opacity)
            .each(function () {
                const currentElement = d3.select(this);
                setStyleFromOptions(currentElement, additionalOptionsBar);
            });
    };

     // Add the title text with rotation
    g.append("text")
        .attr("x", xPosition + width + titleOptions.xOffset)
        .attr("y", yPosition + height / 2 + titleOptions.yOffset + 10)
        .attr("transform", `rotate(-90, ${xPosition + width + titleOptions.xOffset}, ${yPosition + height / 2 + titleOptions.yOffset})`)
        .attr("alignment-baseline", "bottom")
        .attr("text-anchor", "middle")
        .attr("font-size", titleOptions.fontSize)
        .style("font-family", titleOptions.fontFamily)
        .text(linkOptions.measure)
        .each(function () {
            const currentElement = d3.select(this);
            setStyleFromOptions(currentElement, additionalOptionsTitle);
        });

    // The normal bar sits next to the labels, the inverted bar left of it;
    // with only inverted links, the inverted bar takes the normal bar's place
    if (hasNormal) {
        drawBar("linear-gradient", linkOptions.normalColor, xPosition);
    }
    if (hasInverted) {
        drawBar("reverse-gradient", linkOptions.invertedColor, hasNormal ? xPosition - width : xPosition);
    }

    // Draw the min and max labels
    g.append("text")
        .attr("x", xPosition + width + labelOptions.xOffset)
        .attr("y", yPosition - labelOptions.yOffset)
        .attr("alignment-baseline", "middle")
        .attr("font-size", labelOptions.fontSize)
        .style("font-family", labelOptions.fontFamily)
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
        .style("font-family", labelOptions.fontFamily)
        .text(minValue + '%')
        .each(function () {
            const currentElement = d3.select(this);
            setStyleFromOptions(currentElement, additionalOptionsLabel);
        });
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

// Sets the theme ({ geneOptions: {...}, ... }) used by the components drawn
// after it. createContainer() already sets the chart theme.
container.prototype.theme = function (themeOptions) {
  this.themeOptions = themeOptions ?? {};
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
  this.minStart = addScalePadding(this.minStart, this.maxEnd, padding, "start")
  }

  if(end == null){
  this.maxEnd = addScalePadding(this.minStart, this.maxEnd, padding, "end")
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

    const linearScale = d3.scaleLinear()
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

// themeOptionsKey: "titleOptions" for the chart title, "clusterTitleOptions" for a cluster
container.prototype.title = function (title, subtitle, show = true, options = {}, themeOptionsKey = "titleOptions") {

  // Return early if neither title nor subtitle is provided
  if (!title && !subtitle) {
    return this;
  }

  if (!show) {
    return this;
  }

  // Default options for title and subtitle; the position and spacing grow with
  // the text size of the theme
  const size = themeTextScale(this.themeOptions);
  const defaultOptions = {
    x: 0,
    y: 20 * size,
    align: "center",
    spacing: 20 * size, // Default spacing between title and subtitle
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

  const combinedOptions = mergeOptions.call(this, defaultOptions, themeOptionsKey, options);
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
    spacing: 12 * themeTextScale(this.themeOptions), // Default spacing between title and subtitle
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

  // Determine text align and anchor based on the provided align; the footer is
  // drawn in the SVG itself, so the positions include the left margin
  switch (align) {
    case "left":
      xPos = this.margin.left + x;
      textAnchor = "start";
      break;
    case "right":
      xPos = this.width - this.margin.right + x;
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
        setStyleFromOptions(currentElement, additionalOptionsSubtitleFont);
      });
  }

  return this;
};

// The cluster label is drawn in the left or right margin of the cluster. Returns
// that margin as { left: width } or { right: width }, set before the container is
// created. Width: of the label (user) > margin (user, GC_grid) > of the label
// (theme) > margin (theme) > 100px (times the text size of the theme).
function clusterLabelMargin(containerOptions, labelOptions, themeOptions = {}) {
  const themeLabelOptions = themeOptions?.clusterLabelOptions ?? {};
  const position = labelOptions?.position ?? themeLabelOptions.position ?? "left";
  const width = labelOptions?.width ??
    containerOptions?.margin?.[position] ??
    themeLabelOptions.width ??
    themeOptions?.containerOptions?.margin?.[position] ??
    `${100 * themeTextScale(themeOptions)}px`;
  return { [position]: width };
}

container.prototype.clusterLabel = function (title, show = true, options = {}) {
  if (!show) {
    return this;
  }

  // Default options
  const defaultOptions = {
    x: 0,
    y: 0,
    position: 'left',
    align: 'center',
    width: null, // the margin the label is drawn in, see clusterLabelMargin()
    wrapLabel: true,
    wrapOptions: {},
    fontSize: "12px",
    fontStyle: "normal",
    fontWeight: "bold",
    fontFamily: "sans-serif",
    cursor: "default",
    subtitle: null,
    spacing: null, // Between the last line of the title and the subtitle; null: from the font sizes
    subtitleFont: {
      fontSize: "11px",
      fontStyle: "normal",
      fontWeight: "normal",
      fontFamily: "sans-serif",
      cursor: "default"
    }
  };

  // Merge the options using the generic function
  const combinedOptions = mergeOptions.call(this, defaultOptions, 'clusterLabelOptions', options);

  const {
    x,
    y,
    position,
    align,
    wrapLabel,
    wrapOptions,
    fontSize,
    fontStyle,
    fontWeight,
    fontFamily,
    cursor,
    subtitle,
    spacing,
    subtitleFont
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

  // Align the lines of the label to the left, centre or right of the margin
  const alignShare = { left: 0, right: 1 }[align] ?? 0.5;
  const textAnchor = { left: "start", right: "end" }[align] ?? "middle";

  let xposition;
  if (position === 'left') {
    xposition = this.margin.left * alignShare + x;  // title is in the left margin
  } else {  // 'right'
    xposition = this.width - this.margin.right * (1 - alignShare) - x;  // title is in the right margin
  }

  let clusterTitle = this.svg.append("text")
    .attr("x", xposition)
    .attr("y", middleY)
    .attr("text-anchor", textAnchor)
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
        // If wrapLabel is true, wrap the text; wrap() reads the <b>, <i> and <u> tags
        wrap(currentElement, titleWidth, wrapOptions);
      }
      setStyleFromOptions(currentElement, additionalOptions);
    });

  if (subtitle) {
    const additionalOptionsSubtitleFont = extractAdditionalOptions(subtitleFont, defaultOptions.subtitleFont);
    const subtitleStyle = {
      size: subtitleFont.fontSize,
      style: subtitleFont.fontStyle,
      weight: subtitleFont.fontWeight,
      family: subtitleFont.fontFamily,
      cursor: subtitleFont.cursor
    };

    const clusterSubtitle = this.svg.append("text")
      .attr("x", xposition)
      .attr("y", middleY)
      .attr("text-anchor", textAnchor)
      .attr("dominant-baseline", "central")
      .style("font-size", subtitleFont.fontSize)
      .style("font-style", subtitleFont.fontStyle)
      .style("font-weight", subtitleFont.fontWeight)
      .style("font-family", subtitleFont.fontFamily)
      .style("cursor", subtitleFont.cursor)
      .each(function () {
        const currentElement = d3.select(this);

        if (!wrapLabel) {
          parseAndStyleText(subtitle, currentElement, subtitleStyle);
        } else {
          currentElement.text(subtitle);
          wrap(currentElement, titleWidth, wrapOptions);
        }
        setStyleFromOptions(currentElement, additionalOptionsSubtitleFont);
      });

    // The subtitle starts spacing below the last line of the title, and the two
    // are centred together on the middle of the cluster. By default the spacing
    // is 1.2 times the mean font size of the two
    const titleLines = labelLineOffsets(clusterTitle);
    const subtitleLines = labelLineOffsets(clusterSubtitle);
    const lineSpacing = spacing ?? 1.2 * (titleLines.fontSize + subtitleLines.fontSize) / 2;
    const titleY = middleY -
      (titleLines.first + titleLines.last + lineSpacing - subtitleLines.first + subtitleLines.last) / 2;
    setLabelY(clusterTitle, titleY);
    setLabelY(clusterSubtitle, titleY + titleLines.last + lineSpacing - subtitleLines.first);
  }

  return this;
};

// The font size of a label and the offsets in pixels of its first and last
// line from its y position: the lines of a wrapped label are moved with dy (in em)
function labelLineOffsets(label) {
  const fontSize = parseFloat(getComputedStyle(label.node()).fontSize);
  const lines = label.selectAll("tspan.wrap-line").nodes();
  if (!lines.length) {
    return { fontSize, first: 0, last: 0 };
  }
  const offset = line => (parseFloat(line.getAttribute("dy")) || 0) * fontSize;
  return { fontSize, first: offset(lines[0]), last: offset(lines[lines.length - 1]) };
}

// Moves a label and its wrapped lines to a new y position
function setLabelY(label, y) {
  label.attr("y", y);
  label.selectAll("tspan.wrap-line").attr("y", y);
}

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

  var minStart = start ?? d3.min(this.data, d => Math.min(d.start, d.end));
  var maxEnd = end ?? d3.max(this.data, d => Math.max(d.start, d.end));

  if(start == null){
  minStart = addScalePadding(minStart, maxEnd, this.padding, "start")
  }

  if(end == null){
  maxEnd = addScalePadding(minStart, maxEnd, this.padding, "end")
  }

  // Pixel positions of the break markers, each markerStyle.gap px away from the break
  const breakMarkers = this.breaks.map(gap => ({
    xStart: this.xScale(gap.start - 0.001) - markerStyle.gap,
    xEnd: this.xScale(gap.end + 0.001) + markerStyle.gap
  }));

  // Draw baseline with sequenceStyle, left open between the two markers of a break
  const [lineLeft, lineRight] = [this.xScale(minStart), this.xScale(maxEnd)].sort((a, b) => a - b);
  const openings = breakMarkers
    .map(({ xStart, xEnd }) => [xStart + markerStyle.tiltAmount / 2, xEnd - markerStyle.tiltAmount / 2]
      .sort((a, b) => a - b))
    .sort((a, b) => a[0] - b[0]);
  const segments = [];
  let segmentStart = lineLeft;
  for (const [openLeft, openRight] of openings) {
    if (openLeft > segmentStart) segments.push([segmentStart, Math.min(openLeft, lineRight)]);
    segmentStart = Math.max(segmentStart, openRight);
  }
  if (segmentStart < lineRight) segments.push([segmentStart, lineRight]);

  g.selectAll("line.baseline")
    .data(segments)
    .enter()
    .append("line")
    .attr("class", "baseline")
    .attr("x1", d => d[0])
    .attr("y1", this.yScale(y))
    .attr("x2", d => d[1])
    .attr("y2", this.yScale(y))
    .style("stroke", sequenceStyle.stroke)
    .style("stroke-width", sequenceStyle.strokeWidth)
    .each(function () {
      const currentElement = d3.select(this);
      setStyleFromOptions(currentElement, additionalOptionsSequence);
    });

  // Draw break markers with tilted lines
  for (const { xStart, xEnd } of breakMarkers) {
    const yBase = this.yScale(y);
    const yTop = yBase - markerStyle.markerHeight / 2;
    const yBottom = yBase + markerStyle.markerHeight / 2;

    // Draw the tilted line before the gap
    g.append("line")
      .attr("x1", xStart)
      .attr("y1", yTop)
      .attr("x2", xStart + markerStyle.tiltAmount)
      .attr("y2", yBottom)
      .attr("stroke", markerStyle.stroke)
      .attr("stroke-width", markerStyle.strokeWidth);

    // Draw the tilted line after the gap
    g.append("line")
      .attr("x1", xEnd - markerStyle.tiltAmount)
      .attr("y1", yTop)
      .attr("x2", xEnd)
      .attr("y2", yBottom)
      .attr("stroke", markerStyle.stroke)
      .attr("stroke-width", markerStyle.strokeWidth);
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

// Adds the link label of the same gene (e.g. the identity % from links()) to a
// gene label, as a second line below the name, and removes the link label.
// Gene and link labels were drawn at the same place above the gene.
function addLinkLabelLine(label, linkLabel) {
  const text = d3.select(label);
  const name = label.textContent;
  label.textContent = "";
  text.append("tspan").attr("class", "label-name").text(name);
  const measure = text.append("tspan")
    .attr("class", "label-measure")
    .text(linkLabel.textContent);
  // Keep the font and other styles of the link label; the gene label sets the
  // alignment (the link label may have been rotated by its own overlap check)
  for (const property of linkLabel.style) {
    if (property === "text-anchor") continue;
    measure.style(property, linkLabel.style.getPropertyValue(property));
  }
  linkLabel.remove();

  if (name) {
    // Second line below the name; move the label up a line so the link label
    // stays where it was, just above the gene
    measure.attr("x", text.attr("x")).attr("dy", "1.1em");
    const lineHeight = 1.1 * parseFloat(getComputedStyle(measure.node()).fontSize);
    text.attr("y", parseFloat(text.attr("y")) - lineHeight);
  }
}

// A rotated two-line gene label becomes one line ("name 64%"), so rotated
// labels next to each other stay one line thick
function linkLabelLineToInline(label) {
  const text = d3.select(label);
  const measure = text.select(".label-measure");
  if (measure.empty()) return;
  measure.attr("x", null).attr("dy", null)
    .attr("dx", text.select(".label-name").text() ? "0.3em" : null);
}

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

  const combinedOptions = mergeOptions.call(this, defaultOptions, 'labelsOptions', options);
  const { x, y, start, end, adjustLabels, labelAdjustmentOptions, itemStyle, dx, dy, anchor, rotate, fontSize, fontStyle, fontFamily, textAnchor, cursor } = combinedOptions;

  // Extract additional options that are not in defaultOptions
  const additionalOptions = extractAdditionalOptions(combinedOptions, defaultOptions);

  // Create the group
  const g = this.svg.append("g")
    .attr("transform", `translate(${this.margin.left},${this.margin.top})`);

  // Sort the data first by the minimum value of start and end.
  this.data.sort((a, b) => Math.min(a.start, a.end) - Math.min(b.start, b.end));

  const getAttributesForIndex = (d, i) => {
    const style = itemStyle.find(s => s.index === i) || {};
    const currentX = style.x ?? x;
    const currentY = style.y ?? y;

    const currentDx = style.dx ?? dx;
    const currentDy = style.dy ?? dy;
    const currentRotate = style.rotate ?? rotate;
    // Labels of genes on a lower track (prevent_gene_overlap) go below their
    // gene, so they don't run into the genes of the track above
    // (placeLabel() below moves them back above when that side is free)
    const below = d.geneTrack > 1;
    const currentLabelAdjustmentOptions = { ...(style.labelAdjustmentOptions ?? labelAdjustmentOptions) };
    currentLabelAdjustmentOptions.rotation = -Math.abs(currentLabelAdjustmentOptions.rotation);
    const currentAdjustLabels = style.adjustLabels !== undefined ? style.adjustLabels : adjustLabels;

      // Calculate Y position based on geneTrack
    const currentGeneStrandSpacing = (d.strand == "forward" && this.geneStrandSpacing !== 0)
                                 ? this.geneStrandSpacing
                                 : -this.geneStrandSpacing;
    var currentOverlapSpacing = d.geneTrack ? (d.geneTrack - 1) * this.geneOverlapSpacing : 0;

    // Below the gene: the same gap to the gene as a label above it
    const geneY = this.yScale(this.markerY ?? 50);
    const yAbove = this.yScale(currentY) - (this.markerHeight / 2) - currentGeneStrandSpacing + currentOverlapSpacing;
    const yBelow = 2 * geneY - this.yScale(currentY) + (this.markerHeight / 2) - currentGeneStrandSpacing + currentOverlapSpacing;
    const yPos = below ? yBelow : yAbove;
    const xPos = this.xScale((d.start + d.end) / 2) + currentX;

    return {
      xPos,
      yPos,
      dx: currentDx,
      dy: currentDy,
      rotate: currentRotate,
      labelAdjustmentOptions: currentLabelAdjustmentOptions,
      adjustLabels: currentAdjustLabels,
      below,
      yAbove
    };
  };

  // Pixel x ranges of the genes on each track
  const genesByTrack = d3.group(this.data, d => d.geneTrack ?? 1);
  const trackHasGeneIn = (track, left, right) => (genesByTrack.get(track) || []).some(gene => {
    const [geneLeft, geneRight] = [this.xScale(gene.start), this.xScale(gene.end)].sort((a, b) => a - b);
    return geneLeft < right && geneRight > left;
  });

  // A label below its gene that runs into a gene of the next track goes back
  // above its gene when no gene of the track above is in the way there
  const placeLabel = (element, d, attributes) => {
    if (!attributes.below) return;
    const bbox = element.getBBox();
    const left = bbox.x;
    const right = bbox.x + bbox.width;
    if (trackHasGeneIn(d.geneTrack + 1, left, right) && !trackHasGeneIn(d.geneTrack - 1, left, right)) {
      attributes.below = false;
      attributes.yPos = attributes.yAbove;
      d3.select(element)
        .attr("dominant-baseline", null)
        .attr("y", attributes.yPos)
        .attr("transform", `rotate(${attributes.rotate}, ${attributes.xPos}, ${attributes.yPos})`);
    }
  };

  const attributesByIndex = this.data.map((d, i) => getAttributesForIndex(d, i));
  const labelAdjuster = createLabelAdjuster(this, "text.label");
  // Link labels (e.g. identity %) drawn by links() for the genes of this cluster
  const linkLabelsByRow = new Map(
    this.svg.selectAll(".link-text").nodes().map(node => [node.getAttribute("rowID"), node])
  );

  // Adding the Label
  g.selectAll("text.label")
    .data(this.data)
    .enter()
    .append("text")
    .attr("id", (d, i) => `cluster-${sanitizeId(d.cluster)}-label-${i}`)
    .attr("rowID", (d, i) => `${d["rowID"]}`)
    .attr("class", "label")
    .attr("x", (d, i) => attributesByIndex[i].xPos)
    .attr("y", (d, i) => attributesByIndex[i].yPos)
    .attr("dx", (d, i) => attributesByIndex[i].dx)
    .attr("dy", (d, i) => attributesByIndex[i].dy)
    .attr("text-anchor", textAnchor)
    .attr("dominant-baseline", (d, i) => attributesByIndex[i].below ? "hanging" : null)
    .attr("transform", (d, i) => {
      const { xPos, yPos, rotate } = attributesByIndex[i];
      return `rotate(${rotate}, ${xPos}, ${yPos})`;
    })
    .style("font-size", fontSize)
    .style("font-style", fontStyle)
    .style("font-family", fontFamily)
    .style("cursor", cursor)
    .text(d => d[label])
    // One label per gene: a gene with a link label gets it as a second line.
    // Done for all labels before the overlap check, which measures them all at once
    .each(function (d) {
      const linkLabel = linkLabelsByRow.get(String(d.rowID));
      if (linkLabel) addLinkLabelLine(this, linkLabel);
    })
    .each(function (d, i) {
      const attributes = attributesByIndex[i];
      placeLabel(this, d, attributes);
      // Labels below their gene rotate downwards when they overlap
      if (attributes.below) {
        attributes.labelAdjustmentOptions.rotation = Math.abs(attributes.labelAdjustmentOptions.rotation);
      }
    })
    .each(function (d, i) {

      const currentElement = d3.select(this);
      const attributes = attributesByIndex[i];

      if (attributes.adjustLabels && labelAdjuster.adjust(this, attributes.labelAdjustmentOptions)) {
        linkLabelLineToInline(this);
      }
      // Set additional options as attributes
      setStyleFromOptions(currentElement, additionalOptions);
      // Override with itemStyle based on the index
      applyStyleToElement(currentElement, itemStyle, i);
      labelAdjuster.refresh(this);

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

  const combinedOptions = mergeOptions.call(this, defaultOptions, 'tooltipOptions', options);

  // Extract additional options that are not in defaultOptions
  const additionalOptions = extractAdditionalOptions(combinedOptions, defaultOptions);

  // Ensure triggers is an array
  if (typeof combinedOptions.triggers === 'string') {
    combinedOptions.triggers = [combinedOptions.triggers];
  }

  // One tooltip per cluster, inside the widget element, so each cluster keeps
  // its own options and the tooltip is removed when the widget redraws.
  // The arrow (::before in styles.css) takes its colour from --tooltip-bg.
  const host = this.svg.node().closest(".html-widget") || document.body;
  const tooltip = d3.select(host)
    .append("div")
    .attr("class", "cluster-tooltip")
    .style("opacity", combinedOptions.opacity)
    .style("position", combinedOptions.position)
    .style("background-color", combinedOptions.backgroundColor)
    .style("--tooltip-bg", combinedOptions.backgroundColor)
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
  setStyleFromOptions(tooltip, additionalOptions);

  // Place the tooltip centred above the mouse, relative to the widget element
  const moveTooltip = (event) => {
    const [x, y] = d3.pointer(event, host);
    tooltip.style("left", (x - tooltip.node().offsetWidth / 2) + "px")
      .style("top", (y - tooltip.node().offsetHeight - 15) + "px");
  };

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

      const element = d3.select(event.currentTarget);
      element.classed("hovered", true);

      tooltip.transition()
        .duration(200)
        .style("opacity", 1);
      tooltip.html(textAccessor(dataPoint));
      moveTooltip(event);
    });

    // Mousemove event to reposition the tooltip as the mouse moves
    selection.on("mousemove", moveTooltip);

    // Mouseout event to hide the tooltip
    selection.on("mouseout", (event) => {

      const element = d3.select(event.currentTarget);
      element.classed("hovered", false);

      tooltip.transition()
        .duration(500)
        .style("opacity", 0);
    });
  });

  return this; // Return the instance for method chaining
};

// Closed SVG path through points, with the outer corners rounded by radius
// (0: sharp); inner corners, e.g. where the shaft of an arrow meets its head,
// stay sharp. Repeated points are dropped, and the radius is limited to half
// of the shortest side next to a corner, so short genes keep their shape.
function roundedPolygonPath(points, radius) {
    const pts = points.filter((p, i) => {
        const prev = points[(i - 1 + points.length) % points.length];
        return Math.abs(p[0] - prev[0]) > 1e-6 || Math.abs(p[1] - prev[1]) > 1e-6;
    });
    if (!(radius > 0) || pts.length < 3) {
        return `M${pts.map(p => `${p[0]} ${p[1]}`).join(" L")} Z`;
    }
    const n = pts.length;
    const sideLength = (a, b) => Math.hypot(b[0] - a[0], b[1] - a[1]);
    // Point at distance d from a towards b
    const towards = (a, b, d) => {
        const l = sideLength(a, b);
        return [a[0] + (b[0] - a[0]) * d / l, a[1] + (b[1] - a[1]) * d / l];
    };
    // Sign of the turn at a corner: the same as the orientation of the
    // polygon for outer corners
    const turn = (a, p, b) => (p[0] - a[0]) * (b[1] - p[1]) - (p[1] - a[1]) * (b[0] - p[0]);
    const orientation = Math.sign(pts.reduce((sum, p, i) => {
        const next = pts[(i + 1) % n];
        return sum + p[0] * next[1] - next[0] * p[1];
    }, 0));
    const corners = pts.map((p, i) => {
        const prev = pts[(i - 1 + n) % n];
        const next = pts[(i + 1) % n];
        const outer = Math.sign(turn(prev, p, next)) === orientation;
        const r = outer ? Math.min(radius, sideLength(prev, p) / 2, sideLength(p, next) / 2) : 0;
        return { p, start: towards(p, prev, r), end: towards(p, next, r) };
    });
    let path = `M${corners[0].end[0]} ${corners[0].end[1]}`;
    for (let i = 1; i <= n; i++) {
        const { p, start, end } = corners[i % n];
        path += ` L${start[0]} ${start[1]} Q${p[0]} ${p[1]} ${end[0]} ${end[1]}`;
    }
    return path + " Z";
}

// SVG path and size of a marker ("arrow", "box", "rbox", ...) of the given length
function getGenePath(marker, length, markerSize, options = {}) {
    const sizeDefaults = {
        small: { scale: 0.75 },
        medium: { scale: 1 },
        large: { scale: 1.25 }
    };

    const markerDefaults = {
        arrow: { arrowheadWidth: 10, arrowheadHeight: 30, markerHeight: 15 },
        boxarrow: { arrowheadWidth: 10, arrowheadHeight: 30, markerHeight: 30 },
        box: { arrowheadWidth: 0, arrowheadHeight: 30, markerHeight: 30 },
        rbox: { arrowheadWidth: 0, arrowheadHeight: 30, markerHeight: 30, cornerRadius: 5 },
        cbox: { arrowheadWidth: 10, arrowheadHeight: 30, markerHeight: 30 },
        intron: { arrowheadWidth: 0, arrowheadHeight: 0, markerHeight: 20 },  // Add intron default
        // Add other marker types if needed
    };

    const sizeOptions = sizeDefaults[markerSize] || sizeDefaults.medium;
    const markerOptions = markerDefaults[marker] || {};

    const combinedOptions = deepMergeOptions(markerOptions, options);

    // Scale the dimensions based on size
    const { arrowheadWidth, arrowheadHeight, markerHeight, cornerRadius } = combinedOptions;
    const scaledArrowheadWidth = arrowheadWidth * sizeOptions.scale;
    const scaledArrowheadHeight = arrowheadHeight * sizeOptions.scale;
    const scaledMarkerHeight = markerHeight * sizeOptions.scale;

    let shaftLength = length - scaledArrowheadWidth;
    shaftLength = Math.max(0, shaftLength);
    const shaftTop = (scaledArrowheadHeight - scaledMarkerHeight) / 2;
    const shaftBottom = shaftTop + scaledMarkerHeight;

    let path;
    let height = Math.max(scaledMarkerHeight, scaledArrowheadHeight);
    let effectiveCornerRadius;

    switch (marker) {
        case "arrow":
        case "boxarrow":
        case "box":
            path = roundedPolygonPath([
                [0, shaftTop],
                [0, shaftBottom],
                [shaftLength, shaftBottom],
                [shaftLength, scaledArrowheadHeight],
                [length, scaledArrowheadHeight / 2],
                [shaftLength, 0],
                [shaftLength, shaftTop]
            ], cornerRadius || 0);
            break;
        case "rbox":
            effectiveCornerRadius = Math.min(cornerRadius, scaledMarkerHeight / 2, length / 2);
            height = scaledMarkerHeight;
            path = `M ${effectiveCornerRadius},0
                     H ${length - effectiveCornerRadius}
                     A ${effectiveCornerRadius},${effectiveCornerRadius} 0 0 1 ${length},${effectiveCornerRadius}
                     V ${scaledMarkerHeight - effectiveCornerRadius}
                     A ${effectiveCornerRadius},${effectiveCornerRadius} 0 0 1 ${length - effectiveCornerRadius},${scaledMarkerHeight}
                     H ${effectiveCornerRadius}
                     A ${effectiveCornerRadius},${effectiveCornerRadius} 0 0 1 0,${scaledMarkerHeight - effectiveCornerRadius}
                     V ${effectiveCornerRadius}
                     A ${effectiveCornerRadius},${effectiveCornerRadius} 0 0 1 ${effectiveCornerRadius},0
                     Z`;
            break;
        case "cbox":
            effectiveCornerRadius = Math.min(scaledMarkerHeight / 2, length / 2);
            height = scaledMarkerHeight;
            path = `M ${effectiveCornerRadius} 0
                 L ${length - effectiveCornerRadius} 0
                 Q ${length} ${scaledMarkerHeight / 2} ${length - effectiveCornerRadius} ${scaledMarkerHeight}
                 L ${effectiveCornerRadius} ${scaledMarkerHeight}
                 Q 0 ${scaledMarkerHeight / 2} ${effectiveCornerRadius} 0
                 Z`;
            break;
        case "intron":
            // Create a V-shaped path for the intron
            const midPoint = length / 2;
            height = scaledMarkerHeight / 2
            path = `M0 ${height}
                    L${midPoint} 0
                    L${length} ${height}`;
            break;
        default:
            console.warn(`Marker type '${marker}' not recognized.`);
            path = "";
    }

    return { path, length, scaledArrowheadWidth, scaledArrowheadHeight, height };
}

// Path and position of the gene (or exon, intron, UTR) marker of d, the i-th
// item. styles holds the marker options and itemStyles the per-item overrides.
// Also sets the marker height and spacing on the container, used by labels.
function getMarkerAttributes(container, d, i, y, styles, itemStyles) {
  const style = itemStyles.find(s => s.index === i) || {};
  const geneLength = Math.abs(container.xScale(d.end) - container.xScale(d.start));
  const currentY = style.y ?? y;

  const { path, arrowheadWidth, arrowheadHeight, height } = getGenePath(
    style.marker ?? styles.marker,
    geneLength,
    style.markerSize ?? styles.markerSize,
    {
      arrowheadWidth: style.arrowheadWidth ?? styles.arrowheadWidth,
      arrowheadHeight: style.arrowheadHeight ?? styles.arrowheadHeight,
      markerHeight: style.markerHeight ?? styles.markerHeight,
      cornerRadius: style.cornerRadius ?? styles.cornerRadius
    }
  );

  // The labels and track spacing of the cluster follow its own marker, not
  // the size or y of a single item set with itemStyle
  const clusterHeight = getGenePath(styles.marker, geneLength, styles.markerSize, {
    arrowheadWidth: styles.arrowheadWidth,
    arrowheadHeight: styles.arrowheadHeight,
    markerHeight: styles.markerHeight,
    cornerRadius: styles.cornerRadius
  }).height;
  container.markerHeight = clusterHeight;
  container.markerY = y;
  container.geneStrandSpacing = container.separateStrands ? (clusterHeight / 2 + container.strandSpacing) : 0;
  container.geneOverlapSpacing = (clusterHeight + container.overlapSpacing);

  // Calculate Y position based on geneTrack
  const currentGeneStrandSpacing = (d.strand === "forward" && container.geneStrandSpacing !== 0)
    ? container.geneStrandSpacing
    : -container.geneStrandSpacing;
  const currentOverlapSpacing = d.geneTrack ? (d.geneTrack - 1) * container.geneOverlapSpacing : 0;

  const yPos = container.yScale(currentY) - currentGeneStrandSpacing + currentOverlapSpacing;
  const xPos = container.xScale(d.start);

  return { xPos, yPos, path, arrowheadWidth, arrowheadHeight, height };
}

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
    pattern: null,
    cursor: "default",
    marker: "arrow",
    markerSize: "medium",
    itemStyle: [],
    groupStyle: {},
    arrowheadWidth: null,
    arrowheadHeight: null,
    markerHeight: null,
    cornerRadius: null
  };

  const combinedOptions = mergeOptions.call(this, defaultOptions, 'geneOptions', options);
  const { x, y, stroke, strokeWidth, colorScheme, customColors, pattern, cursor, groupStyle } = combinedOptions;

  // Extract additional options that aren't in defaultOptions
  const additionalOptions = extractAdditionalOptions(combinedOptions, defaultOptions);

  const uniqueGroups = [...new Set(this.dataAll.map(d => d[group]))];

  const colorScale = getColorScale(colorScheme, customColors, uniqueGroups);
  this.genesColorScale = colorScale;
  const patternScale = getPatternScale(this.svg, pattern, uniqueGroups, colorScale, "gene");

  var g = this.svg.append("g")
    .attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);

  // Sort the data first by the minimum value of start and end.
  this.data.sort((a, b) => Math.min(a.start, a.end) - Math.min(b.start, b.end));

  // The style of each gene: that of its group (groupStyle), with the style of
  // the gene itself (itemStyle, by index) on top
  const itemStyle = this.data.flatMap((d, i) => {
    const ofGroup = groupStyle[d[group]];
    const ofItem = combinedOptions.itemStyle.find(s => s.index === i);
    return ofGroup || ofItem ? [{ ...ofGroup, ...ofItem, index: i }] : [];
  });

  const getAttributesForIndex = (d, i) => getMarkerAttributes(this, d, i, y, combinedOptions, itemStyle);

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
    .attr("fill", (d) => patternScale.fill(d[group]))
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
    pattern: null,
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

  const combinedOptions = mergeOptions.call(this, defaultOptions, 'transcriptOptions', options);
  const { x, y, colorScheme, customColors, pattern, styleExons, styleIntrons, styleUTRs, itemStyleExons, itemStyleIntrons, itemStyleUTRs, labelOptions } = combinedOptions;

  // Extract additional options that aren't in defaultOptions
  const additionalOptionsExons = extractAdditionalOptions(combinedOptions.styleExons, defaultOptions.styleExons);
  const additionalOptionsIntrons = extractAdditionalOptions(combinedOptions.styleIntrons, defaultOptions.styleIntrons);
  const additionalOptionsUTRs = extractAdditionalOptions(combinedOptions.styleUTRs, defaultOptions.styleUTRs);

  const uniqueGroups = [...new Set(this.dataAll.map(d => d[group]))];

  const colorScale = getColorScale(colorScheme, customColors, uniqueGroups);
  this.genesColorScale = colorScale;
  const patternScale = getPatternScale(this.svg, pattern, uniqueGroups, colorScale, "exon");

  const g = this.svg.append("g")
    .attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);

  // Sort the data first by the minimum value of start and end.
  this.data.sort((a, b) => Math.min(a.start, a.end) - Math.min(b.start, b.end));

  const getAttributesForIndex = (d, i, styles, itemStyles) => getMarkerAttributes(this, d, i, y, styles, itemStyles);

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
      .attr("fill", (d) => patternScale.fill(d[group]))
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
      .style("stroke", (d) => patternScale.stroke(d[group]))
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
      .style("stroke", (d) => patternScale.stroke(d[group]))
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
        .attr("class", "three-prime-label")
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
        .attr("class", "five-prime-label")
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
    align: "left",
    adjustHeight: true,
    order: [],
    cornerRadius: 0,
    legendOptions: {
      cursor: "pointer",
      colorScheme: null,
      customColors: null,
      pattern: null
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
  const { x, y, width, orientation, align, adjustHeight, order, cornerRadius, legendOptions, legendTextOptions } = combinedOptions;

  const additionalLegendOptions = extractAdditionalOptions(legendOptions, defaultOptions.legendOptions);
  const additionalLegendTextOptions = extractAdditionalOptions(legendTextOptions, defaultOptions.legendTextOptions);

  const svgLegend = this.svg;
  const parentWidth = computeSize(width, svgLegend.node().getBoundingClientRect().width) ||
    svgLegend.node().getBoundingClientRect().width;

  let uniqueGroups = [...new Set(this.data.map(d => d[group]))];
  uniqueGroups = uniqueGroups.filter(color => color !== null);

  const colorScale = getColorScale(legendOptions.colorScheme, legendOptions.customColors, uniqueGroups);
  const patternScale = getPatternScale(this.svg, legendOptions.pattern, uniqueGroups, colorScale, "legend");

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
  // Row and horizontal extent of each item, to align the rows afterwards
  const itemExtents = [];

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
        .attr("id", `legend-label-${i}`)
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
        .attr("id", `legend-marker-${i}`)
        .style("cursor", legendOptions.cursor)
        .attr("x", currentX)
        .attr("y", currentY)
        .attr("width", legendSize)
        .attr("height", legendSize)
        .attr("rx", cornerRadius)
        .style("fill", patternScale.fill(d))
        .style("stroke", patternScale.outline ?? (isWhite(patternScale.fill(d)) ? "#bdbdbd" : null))
        .each(() => patternScale.align(d, currentX, currentY))
        .each(function () {
          const currentElement = d3.select(this);
          setStyleFromOptions(currentElement, additionalLegendOptions);
        })

      itemExtents.push({ node: nodes[i], row: currentY, start: currentX,
                         end: currentX + legendSize + legendPadding + textLength });

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

      // Only this legend's items: other charts on the page have their own
      const unselectedLegend = g.selectAll(".unselected").data();
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

  // Align each row (or the whole column of a vertical legend) to the left,
  // centre or right of the available width
  const alignShare = { center: 0.5, right: 1 }[align] ?? 0;
  if (alignShare > 0) {
    const availableWidth = this.width - this.margin.left - this.margin.right;
    const rowOf = item => orientation === "horizontal" ? item.row : 0;
    const rowWidths = d3.rollup(itemExtents,
      items => d3.max(items, item => item.end) - d3.min(items, item => item.start), rowOf);
    itemExtents.forEach(item => {
      const shift = Math.max(0, availableWidth - rowWidths.get(rowOf(item))) * alignShare;
      d3.select(item.node).attr("transform", `translate(${shift}, 0)`);
    });
  }

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
    const labelAdjuster = createLabelAdjuster(this, ".link-text");

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
    .attr("id", (d, i) => `cluster-${sanitizeId(d.cluster)}-link-label-${i}`)
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
      labelAdjuster.adjust(this, labelAdjustmentOptions);
      setStyleFromOptions(currentElement, additionalOptionsLabelStyle);
      labelAdjuster.refresh(this);
    });
  }

  return this;
};
// Annotations

container.prototype.trackMouse = function(track = true, options = {}) {
  if (!track) {
    return this;
  }

  // Styles of the coordinate tooltip
  const defaultOptions = {
    cursor: "crosshair",
    backgroundColor: "rgba(255, 255, 255, 0.9)",
    padding: "8px",
    borderRadius: "4px",
    border: "1px solid rgba(0,0,0,0.1)",
    boxShadow: "0 4px 6px rgba(0, 0, 0, 0.1)",
    fontFamily: "Arial, sans-serif",
    fontSize: "12px",
    color: "#333",
    lineHeight: "1.5",
    zIndex: 1000
  };

  const { cursor, show, ...tooltipStyle } = mergeOptions.call(this, defaultOptions, 'trackMouseOptions', options);

  // Change cursor
  this.svg.style("cursor", cursor);
    this.svg.selectAll("*").each(function() {
    this.style.cssText += `cursor: ${cursor} !important;`;
  });

  // Tooltip for displaying coordinates, inside the widget element so it is
  // removed when the widget redraws
  const host = this.svg.node().closest(".html-widget") || document.body;
  const tooltip = d3.select(host).append("div")
    .attr("class", "coordinate-tooltip")
    .style("pointer-events", "none")
    .style("position", "absolute")
    .style("visibility", "hidden");
  setStyleFromOptions(tooltip, tooltipStyle);

  const xScale = d3.scaleLinear().domain([0 + this.margin.left, this.width - this.margin.right]).range([0, 100]);
  const yScale = d3.scaleLinear().domain([this.height - this.margin.bottom, 0 + this.margin.top]).range([0, 100]);
  const linearScale = d3.scaleLinear().domain([0 - this.margin.left, this.width]).range([0, 100]);

  this.svg.on("mousemove", (event) => {
    const [x, y] = d3.pointer(event);
    const adjustedX = x - this.margin.left
    const loci = Math.round(this.xScale.invert(adjustedX))
    const format = d3.format(",");

    tooltip.html(`x: ${format(loci)} <br>y: ${yScale(y).toFixed(1)}`)
      .style("visibility", "visible")
      .style("left", (d3.pointer(event, host)[0] + 10) + "px")
      .style("top", (d3.pointer(event, host)[1] - 10) + "px");
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
      break;
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

    // Create a marker for each line; the id must be unique on the page
    const markerId = getUniqueId("arrowhead");
    this.svg.append("defs").append("marker")
      .attr("id", markerId)
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
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsArrow);
      });

    // Draw the line with the arrow marker for each set of values
    group.append("line")
      .attr("x1", this.xScale(currentX1))
      .attr("y1", this.yScale(currentY1))
      .attr("x2", this.xScale(currentX2))
      .attr("y2", this.yScale(currentY2 + currentArrowSize / 2))
      .attr("marker-end", `url(#${markerId})`)
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

  // Create a marker for each line; the id must be unique on the page
  const markerId = getUniqueId("arrowhead-marker");
  this.svg.append("defs").append("marker")
    .attr("id", markerId)
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
      line.attr("marker-end", `url(#${markerId})`);
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
  const combinedOptions = mergeOptions.call(this, defaultOptions, "promoterAnnotationOptions", options);
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
