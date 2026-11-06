var geneviewer = (() => {
  var __defProp = Object.defineProperty;
  var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
  var __getOwnPropNames = Object.getOwnPropertyNames;
  var __hasOwnProp = Object.prototype.hasOwnProperty;
  var __export = (target, all) => {
    for (var name in all)
      __defProp(target, name, { get: all[name], enumerable: true });
  };
  var __copyProps = (to, from, except, desc) => {
    if (from && typeof from === "object" || typeof from === "function") {
      for (let key of __getOwnPropNames(from))
        if (!__hasOwnProp.call(to, key) && key !== except)
          __defProp(to, key, { get: () => from[key], enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable });
    }
    return to;
  };
  var __toCommonJS = (mod) => __copyProps(__defProp({}, "__esModule", { value: true }), mod);

  // src/lib/index.js
  var index_exports = {};
  __export(index_exports, {
    chartToPNG: () => chartToPNG,
    chartToSVG: () => chartToSVG,
    compile: () => compile,
    newChart: () => newChart,
    palettes: () => palettes,
    themes: () => builtinThemes,
    version: () => version
  });

  // src/lib/style.js
  function camelToKebab(string) {
    return string.replace(/([a-z0-9]|(?=[A-Z]))([A-Z])/g, "$1-$2").toLowerCase();
  }
  function extractAdditionalOptions(combinedOptions, defaultOptions) {
    const additionalOptions = Object.keys(combinedOptions).reduce((acc, key) => {
      if (!(key in defaultOptions)) {
        acc[key] = combinedOptions[key];
      }
      return acc;
    }, {});
    return additionalOptions;
  }
  var moveKeys = ["moveX", "moveY"];
  var paintStyles = [
    "fill",
    "fillOpacity",
    "stroke",
    "strokeOpacity",
    "strokeWidth",
    "strokeDasharray",
    "opacity",
    "color",
    "cursor",
    "pointerEvents"
  ];
  var notStyles = ["index", "rowID", "text", "labelAdjustmentOptions", ...moveKeys];
  function changesSize(styles) {
    return Object.keys(styles ?? {}).some((key) => !paintStyles.includes(key) && !notStyles.includes(key) && key in document.documentElement.style);
  }
  function setStyleFromOptions(currentElement, additionalOptions) {
    for (const [key, value] of Object.entries(additionalOptions)) {
      if (moveKeys.includes(key)) continue;
      currentElement.style(camelToKebab(key), value);
    }
  }
  function withGroupStyles(items, column, groupStyle, itemStyle = []) {
    return items.flatMap((d, i) => {
      const ofGroup = column && groupStyle ? groupStyle[d[column]] : void 0;
      const ofIndex = itemStyle.find((s) => s.rowID == null && s.index === i);
      const ofRow = itemStyle.find((s) => s.rowID != null && s.rowID === d.rowID);
      return ofGroup || ofIndex || ofRow ? [{ ...ofGroup, ...ofIndex, ...ofRow, index: i }] : [];
    });
  }
  function applyStyleToElement(currentElement, itemStyle, i) {
    const style = itemStyle.find((s) => s.index === i);
    if (style) {
      for (const [key, value] of Object.entries(style)) {
        if (!["index", "rowID", "text", "labelAdjustmentOptions", ...moveKeys].includes(key)) {
          currentElement.style(camelToKebab(key), value);
        }
      }
    }
  }
  function isWhite(color2) {
    const rgb = typeof color2 === "string" ? d3.color(color2)?.rgb() : null;
    return !!rgb && rgb.r === 255 && rgb.g === 255 && rgb.b === 255 && rgb.opacity === 1;
  }
  function partStyle(itemStyle, part) {
    const { part: _part, text, ...style } = (itemStyle ?? []).find((s) => s.part === part) ?? {};
    return { text, style };
  }
  function applyMove(selection, options = {}) {
    selection.each(function() {
      const moveX = Number(options.moveX ?? 0);
      const moveY = Number(options.moveY ?? 0);
      const base = this.getAttribute("transform") || "";
      this.__drawn__ = { ...this.__drawn__, moveX, moveY };
      this.setAttribute("transform", `translate(${moveX}, ${moveY}) ${base}`.trim());
    });
  }

  // src/lib/util.js
  function toRows(data) {
    if (data == null) return [];
    if (Array.isArray(data)) return data;
    const names = Object.keys(data);
    const length = names.length ? data[names[0]].length : 0;
    const rows = [];
    for (let i = 0; i < length; i++) {
      const row = {};
      for (const name of names) row[name] = data[name][i];
      rows.push(row);
    }
    return rows;
  }
  function getUniqueId(baseId) {
    var i = 1;
    while (document.getElementById(baseId + "-" + i)) {
      i++;
    }
    return baseId + "-" + i;
  }
  function sanitizeId(str) {
    return str.replace(/[^a-zA-Z0-9_-]/g, "_");
  }
  function computeSize(inputSize, containerSize) {
    if (typeof inputSize === "undefined" || inputSize === null) {
      return 0;
    }
    if (typeof inputSize === "number") {
      return inputSize;
    }
    var resultSize;
    if (inputSize.includes("%")) {
      var percentageValue = parseFloat(inputSize);
      var fraction = percentageValue / 100;
      resultSize = Math.round(fraction * containerSize);
    } else if (inputSize.includes("px")) {
      resultSize = parseFloat(inputSize);
    } else {
      resultSize = parseFloat(inputSize);
    }
    return Math.floor(resultSize);
  }
  function isPlainObject(value) {
    return value !== null && typeof value === "object" && !Array.isArray(value);
  }
  function deepMergeOptions(base, override) {
    const result = { ...base };
    if (!isPlainObject(override)) {
      return result;
    }
    for (const key in override) {
      const value = override[key];
      if (value === null || value === void 0) {
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
  function scaleSize(value, factor) {
    if (typeof value === "number") {
      return value * factor;
    }
    const match = typeof value === "string" && value.trim().match(/^(-?[\d.]+)([a-z%]*)$/i);
    return match ? `${parseFloat(match[1]) * factor}${match[2]}` : value;
  }
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
  function mergeOptions(defaultOptions, themeOptionsKey, userOptions = {}, themeOptions = this?.themeOptions) {
    const themeOpts = themeOptions?.[themeOptionsKey];
    let options = deepMergeOptions(defaultOptions, themeOpts);
    const textScale = themeTextScale(themeOptions);
    if (textScale !== 1) {
      options = scaleFontSizes(options, textScale);
    }
    return deepMergeOptions(options, userOptions);
  }
  function themeTextScale(themeOptions) {
    const textScale = themeOptions?.textScale;
    return typeof textScale === "number" && textScale > 0 ? textScale : 1;
  }
  var shownWarnings = /* @__PURE__ */ new Set();
  function warnOnce(message) {
    if (shownWarnings.has(message)) return;
    shownWarnings.add(message);
    console.warn(message);
  }

  // src/lib/container.js
  function container(svg, margin, width, height) {
    this.svg = svg;
    this.margin = margin;
    this.width = width;
    this.height = height;
  }
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
    const combinedOptions = mergeOptions(defaultOptions, themeOptionsKey, options, themeOptions);
    const { id: containerId, margin: originalMargin, style, width, height } = combinedOptions;
    const additionalOptionsStyle = extractAdditionalOptions(style, defaultOptions.style);
    const computedMargin = {
      top: computeSize(originalMargin?.top ?? 0, height),
      right: computeSize(originalMargin?.right ?? 0, width),
      bottom: computeSize(originalMargin?.bottom ?? 0, height),
      left: computeSize(originalMargin?.left ?? 0, width)
    };
    var svg = d3.select(targetElementId).append("svg").attr("id", getUniqueId(containerId)).attr("width", width || "100%").attr("height", height || "100%").attr("preserveAspectRatio", "xMinYMin meet").attr("viewBox", `0 0 ${width} ${height}`).classed("geneviewer-svg-content", true).style("box-sizing", "border-box").style("background-color", style.backgroundColor).each(function() {
      const currentElement = d3.select(this);
      setStyleFromOptions(currentElement, additionalOptionsStyle);
    });
    Object.entries(style).forEach(([key, value]) => {
      svg.style(key, value);
    });
    const newContainer = new container(svg, computedMargin, width, height);
    newContainer.themeOptions = themeOptions;
    return newContainer;
  }
  container.prototype.cluster = function(options = {}) {
    const defaultOptions = {
      separateStrands: false,
      strandSpacing: 0,
      preventGeneOverlap: false,
      overlapSpacing: 5
    };
    const combinedOptions = mergeOptions.call(this, defaultOptions, "clusterOptions", options);
    const { separateStrands, strandSpacing, preventGeneOverlap, overlapSpacing } = combinedOptions;
    this.separateStrands = separateStrands;
    this.strandSpacing = strandSpacing;
    this.preventGeneOverlap = preventGeneOverlap;
    this.overlapSpacing = overlapSpacing;
    return this;
  };
  container.prototype.geneData = function(data, clusterData) {
    this.dataAll = data;
    this.data = clusterData.map((item) => {
      var newItem = { ...item };
      newItem.cluster = String(newItem.cluster);
      newItem.direction = "forward";
      if (newItem.start > newItem.end) {
        newItem.direction = "reverse";
      }
      return newItem;
    });
    return this;
  };

  // src/lib/scale.js
  function isInAnyDiscontinuity(value, breaks) {
    for (let gap of breaks) {
      if (value >= gap.start && value <= gap.end) {
        return true;
      }
    }
    return false;
  }
  function createDiscontinuousScale(minStart, maxEnd, width, margin, breaks, reverse = false) {
    breaks = [...breaks].sort((a, b) => a.start - b.start);
    let totalGap = 0;
    for (let gap of breaks) {
      if (gap.start >= minStart && gap.end <= maxEnd) {
        totalGap += gap.end - gap.start;
      }
    }
    let domainStart = reverse ? maxEnd - totalGap : minStart;
    let domainEnd = reverse ? minStart : maxEnd - totalGap;
    const linearScale = d3.scaleLinear().domain([domainStart, domainEnd]).range([0, width - margin.left - margin.right]);
    const scaleProxy = function(value) {
      if (isInAnyDiscontinuity(value, breaks)) {
        return null;
      }
      let cumulativeAdjustment = 0;
      for (let gap of breaks) {
        if (value > gap.end) {
          cumulativeAdjustment += gap.end - gap.start;
        } else {
          break;
        }
      }
      value = value - cumulativeAdjustment;
      return linearScale(value);
    };
    for (let prop in linearScale) {
      if (typeof linearScale[prop] === "function") {
        scaleProxy[prop] = (...args) => {
          const result = linearScale[prop](...args);
          return result === linearScale ? scaleProxy : result;
        };
      } else {
        scaleProxy[prop] = linearScale[prop];
      }
    }
    scaleProxy.invert = function(px) {
      let value = linearScale.invert(px);
      for (let gap of breaks) {
        if (value > gap.start) {
          value += gap.end - gap.start;
        } else {
          break;
        }
      }
      return value;
    };
    return scaleProxy;
  }
  function addScalePadding(startValue, endValue, padding, to, breaks = []) {
    let paddingValue;
    if (typeof padding === "string" && padding.endsWith("%")) {
      paddingValue = parseFloat(padding) / 100;
    } else if (typeof padding === "number" && padding >= 0 && padding <= 100) {
      paddingValue = padding / 100;
    } else {
      warnOnce("Invalid padding format. Padding should be a percentage (as a string or a number between 0 and 100). No padding applied.");
      return to === "start" ? startValue : endValue;
    }
    const [low, high] = [Math.min(startValue, endValue), Math.max(startValue, endValue)];
    const gaps = breaks.reduce((sum, gap) => sum + Math.max(0, Math.min(gap.end, high) - Math.max(gap.start, low)), 0);
    const totalRange = high - low - gaps;
    paddingValue = totalRange * paddingValue;
    if (to === "start") {
      return startValue - paddingValue;
    } else if (to === "end") {
      return endValue + paddingValue;
    } else {
      console.error('Invalid "to" parameter in addScalePadding. Must be "start" or "end".');
      return to === "start" ? startValue : endValue;
    }
  }

  // src/lib/components/scale.js
  container.prototype.scale = function(options = {}) {
    if (!this.data) {
      console.error("No data has been added to this cluster container.");
      return this;
    }
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
    const combinedOptions = mergeOptions.call(this, defaultScaleOptions, "scaleOptions", options);
    const {
      start,
      end,
      xMin,
      xMax,
      padding,
      hidden,
      breaks,
      tickValues,
      reverse,
      axisPosition,
      ticksCount,
      ticksFormat,
      y: initialY,
      tickStyle,
      textStyle,
      lineStyle
    } = combinedOptions;
    const y = initialY !== null ? initialY : axisPosition === "bottom" ? 20 : 80;
    const additionalOptionsTickStyle = extractAdditionalOptions(tickStyle, defaultScaleOptions.tickStyle);
    const additionalOptionsTextStyle = extractAdditionalOptions(textStyle, defaultScaleOptions.textStyle);
    const additionalOptionslineStyle = extractAdditionalOptions(lineStyle, defaultScaleOptions.lineStyle);
    if (start !== null) {
      this.data = this.data.filter((d) => d.start >= start);
    }
    if (end !== null) {
      this.data = this.data.filter((d) => d.end <= end);
    }
    this.data = this.data.filter((d) => {
      return !breaks.some(
        (gap) => d.start >= gap.start && d.start <= gap.end || d.end >= gap.start && d.end <= gap.end
      );
    });
    this.reverse = reverse;
    this.padding = padding;
    this.minStart = start !== null ? start : xMin !== null ? xMin : d3.min(this.data, (d) => Math.min(d.start, d.end));
    this.maxEnd = end !== null ? end : xMax !== null ? xMax : d3.max(this.data, (d) => Math.max(d.start, d.end));
    if (start == null) {
      this.minStart = addScalePadding(this.minStart, this.maxEnd, padding, "start", breaks);
    }
    if (end == null) {
      this.maxEnd = addScalePadding(this.minStart, this.maxEnd, padding, "end", breaks);
    }
    this.xScale = createDiscontinuousScale(this.minStart, this.maxEnd, this.width, this.margin, breaks, reverse);
    this.fullScale = createDiscontinuousScale(this.minStart, this.maxEnd, this.width, this.margin, breaks, reverse);
    this.zoom = null;
    if (options.zoom) {
      const plotWidth = this.width - this.margin.left - this.margin.right;
      const share = (value) => {
        const gap = breaks.find((gap2) => value >= gap2.start && value <= gap2.end);
        return this.fullScale(gap ? gap.end + 1e-9 : value) / plotWidth;
      };
      const { from, to, mode } = options.zoom;
      let [a, b] = (mode === "absolute" ? [share(from), share(to)] : [from, to]).sort((x, y2) => x - y2);
      if (!(b - a > 1e-9)) b = a + 1e-9;
      if (a > 1e-9 || b < 1 - 1e-9) {
        this.zoom = [a, b];
        this.xScale.range([-a * plotWidth / (b - a), (1 - a) * plotWidth / (b - a)]);
      }
    }
    this.yScale = d3.scaleLinear().domain([0, 100]).range([this.height - this.margin.bottom - this.margin.top, 0]);
    this.breaks = breaks.filter((gap) => gap.start >= this.minStart && gap.end <= this.maxEnd);
    const that = this;
    if (!hidden) {
      const adjustedYOffset = this.yScale ? this.yScale(y) : y;
      const axisGroup = this.svg.append("g").attr("data-component", "scale").attr("transform", `translate(${this.margin.left},${this.margin.top + adjustedYOffset})`);
      const plotWidth = this.width - this.margin.left - this.margin.right;
      const shown = this.zoom ? [this.xScale.invert(0), this.xScale.invert(plotWidth)].sort((a, b) => a - b) : [this.minStart, this.maxEnd];
      const linearScale = d3.scaleLinear().domain(shown).range([0, plotWidth]);
      const xAxis = d3.axisBottom(linearScale).tickFormat(d3.format(ticksFormat));
      if (Array.isArray(tickValues) && tickValues.length > 0) {
        xAxis.tickValues(tickValues);
      } else if (typeof tickValues === "number") {
        xAxis.tickValues([tickValues]);
      } else {
        xAxis.ticks(ticksCount);
      }
      const axis = axisGroup.append("g").call(xAxis);
      applyMove(axisGroup, combinedOptions.itemStyle?.[0]);
      axis.selectAll(".tick line").style("stroke", tickStyle.stroke).style("stroke-width", tickStyle.strokeWidth).attr("y2", tickStyle.lineLength).each(function() {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsTickStyle);
      });
      axis.selectAll(".tick text").style("fill", textStyle.fill).style("font-size", textStyle.fontSize).style("font-family", textStyle.fontFamily).style("cursor", textStyle.cursor).each(function() {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsTextStyle);
      });
      axis.selectAll(".tick").each(function(d) {
        let tickValue = d3.select(this).data()[0];
        let newX = that.xScale(tickValue);
        if (newX === null || newX < -0.5 || newX > plotWidth + 0.5) {
          d3.select(this).remove();
        } else {
          d3.select(this).attr("transform", `translate(${newX},0)`);
        }
      });
      axis.select(".domain").style("stroke", lineStyle.stroke).style("stroke-width", lineStyle.strokeWidth).each(function() {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionslineStyle);
      });
    }
    return this;
  };

  // src/lib/text.js
  function wrap(text, width, options = {}) {
    const defaultOptions = {
      dyAdjust: 0,
      lineHeightEms: 1.05,
      lineHeightSquishFactor: 1,
      splitOnHyphen: true,
      centreVertically: true
    };
    const {
      dyAdjust,
      lineHeightEms,
      lineHeightSquishFactor,
      splitOnHyphen,
      centreVertically
    } = { ...defaultOptions, ...options };
    text.each(function() {
      var text2 = d3.select(this), x = text2.attr("x"), y = text2.attr("y");
      var words = [];
      var word = [];
      var endWord = function(space) {
        if (word.length) words.push({ pieces: word, space });
        word = [];
      };
      styledSegments(text2.text()).forEach(function(segment) {
        segment.content.split(/(\s+)/).forEach(function(part) {
          if (/^\s+$/.test(part)) {
            endWord(true);
            return;
          }
          var pieces = splitOnHyphen ? part.split(/(?<=-)/) : [part];
          pieces.forEach(function(piece, i2) {
            if (piece) word.push({ ...segment, content: piece });
            if (i2 < pieces.length - 1) endWord(false);
          });
        });
      });
      endWord(true);
      var setLine = function(line2, lineWords2) {
        line2.selectAll("*").remove();
        lineWords2.forEach(function(lineWord, i2) {
          lineWord.pieces.forEach(function(piece, j) {
            var last = j === lineWord.pieces.length - 1 && i2 < lineWords2.length - 1;
            var space = last && lineWord.space ? " " : "";
            var tspan = line2.append("tspan").text(piece.content + space);
            if (piece.bold) tspan.style("font-weight", "bold");
            if (piece.italic) tspan.style("font-style", "italic");
            if (piece.underline) tspan.style("text-decoration", "underline");
          });
        });
      };
      text2.text(null);
      var line = text2.append("tspan").attr("class", "wrap-line");
      var lineWords = [];
      for (var i = 0; i < words.length; i++) {
        lineWords.push(words[i]);
        setLine(line, lineWords);
        if (line.node().getComputedTextLength() > width && lineWords.length > 1) {
          lineWords.pop();
          setLine(line, lineWords);
          lineWords = [words[i]];
          line = text2.append("tspan").attr("class", "wrap-line");
          setLine(line, lineWords);
        }
      }
      var lines = text2.selectAll("tspan.wrap-line");
      var h = lineHeightEms;
      if (lines.size() > 2)
        for (var i = 0; i < lines.size(); i++) h *= lineHeightSquishFactor;
      lines.each(function(d, i2) {
        var dy = i2 * h + dyAdjust;
        if (centreVertically) dy -= (lines.size() - 1) * h / 2;
        d3.select(this).attr("y", y).attr("x", x).attr("dy", dy + "em");
      });
    });
  }
  function createLabelAdjuster(container2, labelSelector, bounds = null) {
    const defaultOptions = {
      rotation: -65,
      // Rotation angle (in degrees)
      offsetX: 0,
      offsetY: 0,
      dx: "0em",
      // Horizontal adjustment
      dy: "0em"
      // Vertical adjustment
    };
    const overlapPercentage = (rect1, rect2) => {
      const x_overlap = Math.max(0, Math.min(rect1.right, rect2.right) - Math.max(rect1.left, rect2.left));
      const y_overlap = Math.max(0, Math.min(rect1.bottom, rect2.bottom) - Math.max(rect1.top, rect2.top));
      const overlapArea = x_overlap * y_overlap;
      const rect1Area = (rect1.right - rect1.left) * (rect1.bottom - rect1.top);
      return overlapArea / rect1Area * 100;
    };
    const screenCTMs = /* @__PURE__ */ new Map();
    const toScreen = (label, box) => {
      const parent = label.parentNode;
      if (!screenCTMs.has(parent)) {
        const ctm = parent.getScreenCTM?.();
        screenCTMs.set(parent, ctm ? DOMMatrix.fromMatrix(ctm) : null);
      }
      let matrix = screenCTMs.get(parent);
      if (!matrix) return label.getBoundingClientRect();
      const own = label.transform?.baseVal?.consolidate();
      if (own) matrix = matrix.multiply(DOMMatrix.fromMatrix(own.matrix));
      const corners = [
        [box.x, box.y],
        [box.x + box.width, box.y],
        [box.x, box.y + box.height],
        [box.x + box.width, box.y + box.height]
      ].map(([x, y]) => new DOMPoint(x, y).matrixTransform(matrix));
      const xs = corners.map((p) => p.x);
      const ys = corners.map((p) => p.y);
      return { left: Math.min(...xs), right: Math.max(...xs), top: Math.min(...ys), bottom: Math.max(...ys) };
    };
    const boxes = /* @__PURE__ */ new Map();
    const measure = (label) => {
      const box = label.getBBox();
      boxes.set(label, box);
      return toScreen(label, box);
    };
    const pixels = (label, value) => {
      if (value == null || value === "") return 0;
      const match = String(value).trim().match(/^([-+]?[0-9.]+(?:e[-+]?[0-9]+)?)(px|em)?$/i);
      if (!match) return null;
      const number = parseFloat(match[1]);
      if (number === 0 || !match[2] || match[2].toLowerCase() === "px") return number;
      const fontSize = label.style.fontSize.endsWith("px") ? label.style.fontSize : getComputedStyle(label).fontSize;
      return number * parseFloat(fontSize);
    };
    const columnWidth = 50;
    const columns = /* @__PURE__ */ new Map();
    const columnsOf2 = (rect) => {
      if (!Number.isFinite(rect.left) || !Number.isFinite(rect.right)) return [];
      const result = [];
      for (let c = Math.floor(rect.left / columnWidth); c <= Math.floor(rect.right / columnWidth); c++) result.push(c);
      return result;
    };
    const place = (label, rect) => {
      const previous = rects.get(label);
      if (previous) columnsOf2(previous).forEach((c) => columns.get(c).delete(label));
      rects.set(label, rect);
      columnsOf2(rect).forEach((c) => {
        if (!columns.has(c)) columns.set(c, /* @__PURE__ */ new Set());
        columns.get(c).add(label);
      });
    };
    const neighbours = (label) => {
      const result = /* @__PURE__ */ new Set();
      columnsOf2(rects.get(label)).forEach((c) => columns.get(c)?.forEach((other) => result.add(other)));
      result.delete(label);
      return result;
    };
    let labels2 = null;
    const rects = /* @__PURE__ */ new Map();
    const refresh = (label) => {
      if (labels2) place(label, measure(label));
    };
    const adjust = (specificLabel, options = {}) => {
      if (!labels2) {
        labels2 = container2.svg.selectAll(labelSelector).nodes();
        labels2.forEach((label) => place(label, measure(label)));
      }
      const { rotation, offsetX, offsetY, dx, dy, hide = false } = { ...defaultOptions, ...options };
      const bbox = boxes.get(specificLabel) ?? specificLabel.getBBox();
      let centerX = bbox.x + bbox.width / 2;
      let centerY = bbox.y;
      centerX += offsetX;
      centerY += rotation < 0 ? bbox.height : bbox.height / 2;
      centerY += offsetY;
      if (!rects.has(specificLabel)) place(specificLabel, measure(specificLabel));
      const specificLabelRect = rects.get(specificLabel);
      for (const label of neighbours(specificLabel)) {
        if (overlapPercentage(specificLabelRect, rects.get(label)) > 0) {
          if (hide) {
            d3.select(specificLabel).style("display", "none");
            columnsOf2(specificLabelRect).forEach((c) => columns.get(c)?.delete(specificLabel));
            rects.delete(specificLabel);
            return false;
          }
          const oldY = parseFloat(specificLabel.getAttribute("y"));
          const oldDy = pixels(specificLabel, specificLabel.getAttribute("dy"));
          const newDx = pixels(specificLabel, dx);
          const newDy = pixels(specificLabel, dy);
          const radians = (degrees) => Math.abs(degrees) * Math.PI / 180;
          const reach = (degrees) => bbox.width * Math.sin(radians(degrees)) + bbox.height * Math.cos(radians(degrees));
          const anchorY = centerY + (newDy ?? 0);
          const fitsVertically = (degrees) => !bounds || (degrees < 0 ? anchorY - reach(degrees) >= bounds.top : anchorY + reach(degrees) <= bounds.bottom);
          const run = bbox.width * Math.cos(radians(rotation));
          const anchorX = centerX + offsetX + (newDx ?? 0);
          if (!fitsVertically(rotation) || bounds && anchorX + run > bounds.right) {
            d3.select(specificLabel).style("display", "none");
            columnsOf2(specificLabelRect).forEach((c) => columns.get(c)?.delete(specificLabel));
            rects.delete(specificLabel);
            return false;
          }
          const angle = rotation;
          d3.select(specificLabel).style("text-anchor", "start").attr("dx", dx).attr("dy", dy).attr("x", centerX + offsetX).attr("y", centerY).attr("transform", `rotate(${angle}, ${centerX}, ${centerY})`);
          specificLabel.__drawn__ = { ...specificLabel.__drawn__, rotate: angle };
          if (!specificLabel.childElementCount && Number.isFinite(oldY) && [oldDy, newDx, newDy].every(Number.isFinite)) {
            const box = {
              x: centerX + offsetX + newDx,
              y: bbox.y + (centerY + newDy) - (oldY + oldDy),
              width: bbox.width,
              height: bbox.height
            };
            boxes.set(specificLabel, box);
            place(specificLabel, toScreen(specificLabel, box));
          } else {
            refresh(specificLabel);
          }
          return true;
        }
      }
      return false;
    };
    return { adjust, refresh };
  }
  function styledSegments(text) {
    const tagRegex = /<([biu])>(.*?)<\/\1>/g;
    const segments = [];
    const plain = (content) => ({ content, bold: false, italic: false, underline: false });
    let lastIndex = 0;
    let match;
    while ((match = tagRegex.exec(text)) !== null) {
      if (match.index > lastIndex) {
        segments.push(plain(text.substring(lastIndex, match.index)));
      }
      const [, tag, content] = match;
      segments.push({ content, bold: tag === "b", italic: tag === "i", underline: tag === "u" });
      lastIndex = match.index + match[0].length;
    }
    if (lastIndex < text.length) {
      segments.push(plain(text.substring(lastIndex)));
    }
    return segments;
  }
  function parseAndStyleText(text, parentElement, fontOptions) {
    styledSegments(text).forEach(({ content, bold, italic, underline }) => {
      const tspan = parentElement.append("tspan").style("font-weight", bold ? "bold" : fontOptions.weight).style("font-style", italic ? "italic" : fontOptions.style).text(content);
      if (underline) {
        tspan.style("text-decoration", "underline");
      }
    });
  }

  // src/lib/components/title.js
  function titleAlignment(container2, align2, x, inSvg = false) {
    const { width, margin } = container2;
    switch (align2) {
      case "left":
        return { xPos: inSvg ? margin.left + x : x, textAnchor: "start" };
      case "right":
        return {
          xPos: (inSvg ? width - margin.right : width - margin.left - margin.right) + x,
          textAnchor: "end"
        };
      default: {
        const effectiveWidth = width - margin.left - margin.right;
        return {
          xPos: (inSvg ? margin.left + effectiveWidth / 2 : effectiveWidth / 2) + x,
          textAnchor: "middle"
        };
      }
    }
  }
  function styleFontText(selection, text, font, defaultFont) {
    for (const key of Object.keys(defaultFont)) {
      selection.style(camelToKebab(key), font[key]);
    }
    const additionalOptions = extractAdditionalOptions(font, defaultFont);
    selection.each(function() {
      const currentElement = d3.select(this);
      parseAndStyleText(text, currentElement, font);
      setStyleFromOptions(currentElement, additionalOptions);
    });
    applyMove(selection, font);
  }
  container.prototype.title = function(title2, subtitle, show = true, options = {}, themeOptionsKey = "titleOptions") {
    if (!title2 && !subtitle) {
      return this;
    }
    if (!show) {
      return this;
    }
    const size = themeTextScale(this.themeOptions);
    const defaultOptions = {
      x: 0,
      y: 20 * size,
      align: "center",
      spacing: 20 * size,
      // Default spacing between title and subtitle
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
      }
    };
    const combinedOptions = mergeOptions.call(this, defaultOptions, themeOptionsKey, options);
    const { x, y, align: align2, spacing, itemStyle } = combinedOptions;
    const ownTitle = partStyle(itemStyle, "title");
    const ownSubtitle = partStyle(itemStyle, "subtitle");
    const titleFont = { ...combinedOptions.titleFont, ...ownTitle.style };
    const subtitleFont = { ...combinedOptions.subtitleFont, ...ownSubtitle.style };
    title2 = ownTitle.text ?? title2;
    subtitle = ownSubtitle.text ?? subtitle;
    const { xPos, textAnchor } = titleAlignment(this, align2, x);
    var g = this.svg.append("g").attr("data-component", themeOptionsKey === "clusterTitleOptions" ? "clusterTitle" : "title").attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);
    if (title2) {
      g.append("text").attr("data-part", "title").property("__text__", title2).attr("x", xPos).attr("y", y + this.margin.top / 2).attr("text-anchor", textAnchor).call(styleFontText, title2, titleFont, defaultOptions.titleFont);
    }
    if (subtitle) {
      g.append("text").attr("data-part", "subtitle").property("__text__", subtitle).attr("x", xPos).attr("y", y + this.margin.top / 2 + spacing).attr("text-anchor", textAnchor).call(styleFontText, subtitle, subtitleFont, defaultOptions.subtitleFont);
    }
    return this;
  };

  // src/lib/components/footer.js
  container.prototype.footer = function(title2, subtitle, show = true, options = {}) {
    if (!title2 && !subtitle) {
      return this;
    }
    if (!show) {
      return this;
    }
    const defaultOptions = {
      x: 0,
      y: 0,
      align: "left",
      spacing: 12 * themeTextScale(this.themeOptions),
      // Default spacing between title and subtitle
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
      }
    };
    const combinedOptions = mergeOptions.call(this, defaultOptions, "footerOptions", options);
    const { x, y, align: align2, spacing, itemStyle } = combinedOptions;
    const ownTitle = partStyle(itemStyle, "title");
    const ownSubtitle = partStyle(itemStyle, "subtitle");
    const titleFont = { ...combinedOptions.titleFont, ...ownTitle.style };
    const subtitleFont = { ...combinedOptions.subtitleFont, ...ownSubtitle.style };
    title2 = ownTitle.text ?? title2;
    subtitle = ownSubtitle.text ?? subtitle;
    const { xPos, textAnchor } = titleAlignment(this, align2, x, true);
    const titleYPos = this.height - this.margin.bottom + y - 20;
    const subtitleYPos = titleYPos + spacing;
    if (title2) {
      this.svg.append("text").attr("data-component", "clusterFooter").attr("data-part", "title").property("__text__", title2).attr("x", xPos).attr("y", titleYPos).attr("text-anchor", textAnchor).call(styleFontText, title2, titleFont, defaultOptions.titleFont);
    }
    if (subtitle) {
      this.svg.append("text").attr("data-component", "clusterFooter").attr("data-part", "subtitle").property("__text__", subtitle).attr("x", xPos).attr("y", subtitleYPos).attr("text-anchor", textAnchor).call(styleFontText, subtitle, subtitleFont, defaultOptions.subtitleFont);
    }
    return this;
  };

  // src/lib/components/clusterLabel.js
  function clusterLabelMargin(containerOptions, labelOptions, themeOptions = {}) {
    const themeLabelOptions = themeOptions?.clusterLabelOptions ?? {};
    const position = labelOptions?.position ?? themeLabelOptions.position ?? "left";
    const width = labelOptions?.width ?? containerOptions?.margin?.[position] ?? themeLabelOptions.width ?? themeOptions?.containerOptions?.margin?.[position] ?? `${100 * themeTextScale(themeOptions)}px`;
    return { [position]: width };
  }
  container.prototype.clusterLabel = function(title2, show = true, options = {}) {
    if (!show) {
      return this;
    }
    const defaultOptions = {
      x: 0,
      y: 0,
      position: "left",
      align: "center",
      width: null,
      // the margin the label is drawn in, see clusterLabelMargin()
      wrapLabel: true,
      wrapOptions: {},
      fontSize: "12px",
      fontStyle: "normal",
      fontWeight: "bold",
      fontFamily: "sans-serif",
      cursor: "default",
      subtitle: null,
      spacing: null,
      // Between the last line of the title and the subtitle; null: from the font sizes
      subtitleFont: {
        fontSize: "11px",
        fontStyle: "normal",
        fontWeight: "normal",
        fontFamily: "sans-serif",
        cursor: "default"
      }
    };
    const { itemStyle, ...mergedOptions } = mergeOptions.call(this, defaultOptions, "clusterLabelOptions", options);
    const ownTitle = partStyle(itemStyle, "title");
    const ownSubtitle = partStyle(itemStyle, "subtitle");
    const combinedOptions = {
      ...mergedOptions,
      ...ownTitle.style,
      subtitle: ownSubtitle.text ?? mergedOptions.subtitle,
      subtitleFont: { ...mergedOptions.subtitleFont, ...ownSubtitle.style }
    };
    title2 = ownTitle.text ?? title2;
    const {
      x,
      y,
      position,
      align: align2,
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
    const additionalOptions = extractAdditionalOptions(combinedOptions, defaultOptions);
    const titleFont = {
      size: fontSize,
      style: fontStyle,
      weight: fontWeight,
      family: fontFamily,
      cursor
    };
    const adjustedHeight = this.height - this.margin.top - this.margin.bottom;
    const middleY = this.margin.top + adjustedHeight / 2 + y;
    const titleWidth = position === "left" ? this.margin.left - x : this.margin.right - x;
    const alignShare = { left: 0, right: 1 }[align2] ?? 0.5;
    const textAnchor = { left: "start", right: "end" }[align2] ?? "middle";
    let xposition;
    if (position === "left") {
      xposition = this.margin.left * alignShare + x;
    } else {
      xposition = this.width - this.margin.right * (1 - alignShare) - x;
    }
    let clusterTitle2 = this.svg.append("text").attr("data-component", "clusterLabel").attr("data-part", "title").property("__text__", title2).attr("x", xposition).attr("y", middleY).attr("text-anchor", textAnchor).attr("dominant-baseline", "central").style("font-size", fontSize).style("font-style", fontStyle).style("font-weight", fontWeight).style("font-family", fontFamily).style("cursor", cursor).each(function() {
      const currentElement = d3.select(this);
      if (!wrapLabel) {
        parseAndStyleText(title2, currentElement, titleFont);
      } else {
        currentElement.text(title2);
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
      const clusterSubtitle = this.svg.append("text").attr("data-component", "clusterLabel").attr("data-part", "subtitle").property("__text__", subtitle).attr("x", xposition).attr("y", middleY).attr("text-anchor", textAnchor).attr("dominant-baseline", "central").style("font-size", subtitleFont.fontSize).style("font-style", subtitleFont.fontStyle).style("font-weight", subtitleFont.fontWeight).style("font-family", subtitleFont.fontFamily).style("cursor", subtitleFont.cursor).each(function() {
        const currentElement = d3.select(this);
        if (!wrapLabel) {
          parseAndStyleText(subtitle, currentElement, subtitleStyle);
        } else {
          currentElement.text(subtitle);
          wrap(currentElement, titleWidth, wrapOptions);
        }
        setStyleFromOptions(currentElement, additionalOptionsSubtitleFont);
      });
      const titleLines = labelLineOffsets(clusterTitle2);
      const subtitleLines = labelLineOffsets(clusterSubtitle);
      const lineSpacing = spacing ?? 1.2 * (titleLines.fontSize + subtitleLines.fontSize) / 2;
      const titleY = middleY - (titleLines.first + titleLines.last + lineSpacing - subtitleLines.first + subtitleLines.last) / 2;
      setLabelY(clusterTitle2, titleY);
      setLabelY(clusterSubtitle, titleY + titleLines.last + lineSpacing - subtitleLines.first);
    }
    applyMove(clusterTitle2, ownTitle.style);
    this.svg.selectAll('text[data-component="clusterLabel"][data-part="subtitle"]').call(applyMove, ownSubtitle.style);
    return this;
  };
  function labelLineOffsets(label) {
    const fontSize = parseFloat(getComputedStyle(label.node()).fontSize);
    const lines = label.selectAll("tspan.wrap-line").nodes();
    if (!lines.length) {
      return { fontSize, first: 0, last: 0 };
    }
    const offset = (line) => (parseFloat(line.getAttribute("dy")) || 0) * fontSize;
    return { fontSize, first: offset(lines[0]), last: offset(lines[lines.length - 1]) };
  }
  function setLabelY(label, y) {
    label.attr("y", y);
    label.selectAll("tspan.wrap-line").attr("y", y);
  }

  // src/lib/components/sequence.js
  container.prototype.sequence = function(show = true, options = {}) {
    if (!show) {
      return this;
    }
    if (!this.data) {
      console.error("No data has been added to this cluster container. Please use the addGeneData() function before attempting to draw a gene line.");
      return this;
    }
    const defaultOptions = {
      y: 50,
      start: null,
      end: null,
      sequenceStyle: {
        // Adding sequenceStyle
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
    const combinedOptions = mergeOptions.call(this, defaultOptions, "sequenceOptions", options);
    const { y, start, end, markerStyle, sequenceStyle } = combinedOptions;
    const additionalOptionsSequence = extractAdditionalOptions(sequenceStyle, defaultOptions.sequenceStyle);
    var g = this.svg.append("g").attr("data-component", "sequence").attr("transform", `translate(${this.margin.left},${this.margin.top})`);
    var minStart = start ?? d3.min(this.data, (d) => Math.min(d.start, d.end));
    var maxEnd = end ?? d3.max(this.data, (d) => Math.max(d.start, d.end));
    if (start == null) {
      minStart = addScalePadding(minStart, maxEnd, this.padding, "start", this.breaks);
    }
    if (end == null) {
      maxEnd = addScalePadding(minStart, maxEnd, this.padding, "end", this.breaks);
    }
    const breakMarkers = this.breaks.map((gap) => ({
      xStart: this.xScale(gap.start - 1e-3) - markerStyle.gap,
      xEnd: this.xScale(gap.end + 1e-3) + markerStyle.gap
    }));
    const [lineLeft, lineRight] = [this.xScale(minStart), this.xScale(maxEnd)].sort((a, b) => a - b);
    const openings = breakMarkers.map(({ xStart, xEnd }) => [xStart + markerStyle.tiltAmount / 2, xEnd - markerStyle.tiltAmount / 2].sort((a, b) => a - b)).sort((a, b) => a[0] - b[0]);
    const segments = [];
    let segmentStart = lineLeft;
    for (const [openLeft, openRight] of openings) {
      if (openLeft > segmentStart) segments.push([segmentStart, Math.min(openLeft, lineRight)]);
      segmentStart = Math.max(segmentStart, openRight);
    }
    if (segmentStart < lineRight) segments.push([segmentStart, lineRight]);
    g.selectAll("line.baseline").data(segments).enter().append("line").attr("class", "baseline").attr("x1", (d) => d[0]).attr("y1", this.yScale(y)).attr("x2", (d) => d[1]).attr("y2", this.yScale(y)).style("stroke", sequenceStyle.stroke).style("stroke-width", sequenceStyle.strokeWidth).each(function() {
      const currentElement = d3.select(this);
      setStyleFromOptions(currentElement, additionalOptionsSequence);
    });
    for (const { xStart, xEnd } of breakMarkers) {
      const yBase = this.yScale(y);
      const yTop = yBase - markerStyle.markerHeight / 2;
      const yBottom = yBase + markerStyle.markerHeight / 2;
      g.append("line").attr("x1", xStart).attr("y1", yTop).attr("x2", xStart + markerStyle.tiltAmount).attr("y2", yBottom).attr("stroke", markerStyle.stroke).attr("stroke-width", markerStyle.strokeWidth);
      g.append("line").attr("x1", xEnd - markerStyle.tiltAmount).attr("y1", yTop).attr("x2", xEnd).attr("y2", yBottom).attr("stroke", markerStyle.stroke).attr("stroke-width", markerStyle.strokeWidth);
    }
    return this;
  };

  // src/lib/components/coordinates.js
  container.prototype.coordinates = function(show = true, options = {}) {
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
      groupStyle: {},
      styleBy: null,
      itemStyle: [],
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
    const combinedOptions = mergeOptions.call(this, defaultOptions, "coordinatesOptions", options);
    const { rotate, yPositionTop, yPositionBottom, tickValuesBottom, tickValuesTop, ticksFormat, tickStyle, textStyle, groupStyle, styleBy, itemStyle } = combinedOptions;
    const ownStyleOf = (t) => itemStyle.filter((s) => t?.rowID != null && s.rowID === t.rowID && (s.end == null || s.end === t.end)).reduce((style, { rowID, end, ...own }) => ({ ...style, ...own }), {});
    const styleOf = (t) => {
      const group = (styleBy && t?.gene ? groupStyle[t.gene[styleBy]] : void 0) ?? {};
      const own = ownStyleOf(t);
      return { ...group, ...own, tick: { ...group.tick, ...own.tick } };
    };
    const textStyleOf = (t) => {
      const { show: show2, tick, ...text } = styleOf(t);
      return text;
    };
    const additionalOptionsTickStyle = extractAdditionalOptions(tickStyle, defaultOptions.tickStyle);
    const additionalOptionsTextStyle = extractAdditionalOptions(textStyle, defaultOptions.textStyle);
    const g = this.svg.append("g").attr("data-component", "coordinates").attr("transform", "translate(" + this.margin.left + "," + this.margin.top + ")");
    let tickValuesTopFinal = Array.isArray(tickValuesTop) ? tickValuesTop.map((value) => ({ value, rowID: null })) : tickValuesTop != null ? [{ value: tickValuesTop, rowID: null }] : [];
    let tickValuesBottomFinal = Array.isArray(tickValuesBottom) ? tickValuesBottom.map((value) => ({ value, rowID: null })) : tickValuesBottom != null ? [{ value: tickValuesBottom, rowID: null }] : [];
    if (!tickValuesTop && !tickValuesBottom) {
      let allTickValues = this.data.reduce((acc, d) => {
        let tickValueStart = { value: d.start, rowID: d.rowID, end: "start", gene: d };
        let tickValueStop = { value: d.end, rowID: d.rowID, end: "end", gene: d };
        tickValueStart.strand = d.strand;
        tickValueStop.strand = d.strand;
        if ("geneTrack" in d) {
          tickValueStart.geneTrack = d.geneTrack;
          tickValueStop.geneTrack = d.geneTrack;
        }
        acc.push(tickValueStart);
        acc.push(tickValueStop);
        return acc;
      }, []);
      allTickValues = allTickValues.filter((t) => styleOf(t).show !== false);
      allTickValues = allTickValues.filter(
        (obj, index, self2) => index === self2.findIndex((t) => t.value === obj.value)
      );
      allTickValues.sort((a, b) => a.value - b.value);
      if (this.separateStrands) {
        allTickValues.forEach((tickValue) => {
          if (tickValue.strand === "forward") {
            tickValuesTopFinal.push(tickValue);
          } else {
            tickValuesBottomFinal.push(tickValue);
          }
        });
      } else {
        const tickValueThreshold = combinedOptions.overlapThreshold;
        for (let i = 0; i < allTickValues.length; i++) {
          if (i === 0) {
            tickValuesBottomFinal.push(allTickValues[i]);
            continue;
          }
          const diff = this.xScale(allTickValues[i].value) - this.xScale(allTickValues[i - 1].value);
          if (diff < tickValueThreshold) {
            tickValuesTopFinal.push(allTickValues[i]);
            if (i + 1 < allTickValues.length) {
              tickValuesBottomFinal.push(allTickValues[i + 1]);
              i++;
            }
          } else {
            tickValuesBottomFinal.push(allTickValues[i]);
          }
        }
      }
    }
    const self = this;
    const xAxisTop = g.append("g").attr("transform", "translate(0," + this.yScale(yPositionTop) + ")").call(
      d3.axisTop(this.xScale).tickValues(tickValuesTopFinal.map((t) => t.value)).tickFormat(d3.format(ticksFormat))
    );
    xAxisTop.selectAll(".tick").data(tickValuesTopFinal).attr("rowID", (d) => d.rowID).attr("transform", function(d) {
      const xOffset = self.xScale(d.value);
      var currentOverlapSpacing = d.geneTrack ? (d.geneTrack - 1) * self.geneOverlapSpacing : 0;
      return "translate(" + xOffset + "," + -currentOverlapSpacing + ")";
    });
    xAxisTop.select(".domain").attr("stroke", "none");
    xAxisTop.selectAll("text").data(tickValuesTopFinal).attr("class", "coordinate").style("text-anchor", "end").attr("dx", `${-0.8 + textStyle.x}em`).attr("dy", `${0.4 + textStyle.y}em`).attr("transform", "rotate(" + -rotate + ")").style("fill", textStyle.fill).style("font-size", textStyle.fontSize).style("font-family", textStyle.fontFamily).style("cursor", textStyle.cursor).each(function(d) {
      const currentElement = d3.select(this);
      setStyleFromOptions(currentElement, additionalOptionsTextStyle);
      setStyleFromOptions(currentElement, textStyleOf(d));
    });
    xAxisTop.selectAll(".tick line").style("stroke", tickStyle.stroke).style("stroke-width", tickStyle.strokeWidth).attr("y2", -tickStyle.lineLength).each(function() {
      const currentElement = d3.select(this);
      setStyleFromOptions(currentElement, additionalOptionsTickStyle);
      setStyleFromOptions(currentElement, styleOf(d3.select(this.parentNode).datum()).tick ?? {});
    });
    const xAxisBottom = g.append("g").attr("transform", "translate(0," + this.yScale(yPositionBottom) + ")").call(
      d3.axisBottom(this.xScale).tickValues(tickValuesBottomFinal.map((t) => t.value)).tickFormat(d3.format(ticksFormat))
    );
    xAxisBottom.selectAll(".tick").data(tickValuesBottomFinal).attr("rowID", (d) => d.rowID).attr("transform", function(d) {
      const xOffset = self.xScale(d.value);
      var currentOverlapSpacing = d.geneTrack ? -(d.geneTrack - 1) * self.geneOverlapSpacing : 0;
      return "translate(" + xOffset + "," + -currentOverlapSpacing + ")";
    });
    xAxisBottom.select(".domain").attr("stroke", "none");
    xAxisBottom.selectAll("text").data(tickValuesBottomFinal).attr("class", "coordinate").style("text-anchor", "start").attr("dx", `${0.8 + textStyle.x}em`).attr("dy", `${-0.15 + textStyle.y}em`).attr("transform", "rotate(" + -rotate + ")").style("fill", textStyle.fill).style("font-size", textStyle.fontSize).style("font-family", textStyle.fontFamily).style("cursor", textStyle.cursor).each(function(d) {
      const currentElement = d3.select(this);
      setStyleFromOptions(currentElement, additionalOptionsTextStyle);
      setStyleFromOptions(currentElement, textStyleOf(d));
    });
    xAxisBottom.selectAll(".tick line").style("stroke", tickStyle.stroke).style("stroke-width", tickStyle.strokeWidth).attr("y2", tickStyle.lineLength).each(function() {
      const currentElement = d3.select(this);
      setStyleFromOptions(currentElement, additionalOptionsTickStyle);
      setStyleFromOptions(currentElement, styleOf(d3.select(this.parentNode).datum()).tick ?? {});
    });
    return this;
  };

  // src/lib/components/scaleBar.js
  container.prototype.scaleBar = function(show = true, options = {}) {
    if (!show) {
      return this;
    }
    const defaultOptions = {
      title: "1 kb",
      scaleBarUnit: 1e3,
      x: 0,
      // default x offset
      y: 10,
      labelStyle: {
        // default styling for the label
        fontSize: "10px",
        fontFamily: "sans-serif",
        cursor: "default",
        fill: "black",
        // default text color
        labelPosition: "left"
        // moved labelPosition into labelStyle
      },
      textPadding: 0,
      // padding between text and line in x-direction
      scaleBarLineStyle: {
        // default styling for the scale bar line
        stroke: "grey",
        strokeWidth: 1
      },
      scaleBarTickStyle: {
        // default styling for the scale bar ticks
        stroke: "grey",
        strokeWidth: 1
      }
    };
    const combinedOptions = mergeOptions.call(this, defaultOptions, "scaleBarOptions", options);
    const { title: title2, scaleBarUnit, x, y, textPadding, labelStyle, scaleBarLineStyle, scaleBarTickStyle } = combinedOptions;
    const additionalOptionsLine = extractAdditionalOptions(scaleBarLineStyle, defaultOptions.scaleBarLineStyle);
    const additionalOptionsTick = extractAdditionalOptions(scaleBarTickStyle, defaultOptions.scaleBarTickStyle);
    const additionalOptionsLabel = extractAdditionalOptions(labelStyle, defaultOptions.labelStyle);
    const scaleBarLength = Math.abs(this.xScale(scaleBarUnit) - this.xScale(0));
    const g = this.svg.append("g").attr("data-component", "scaleBar").attr("transform", `translate(${this.width - this.margin.right - scaleBarLength - parseInt(labelStyle.fontSize) - 5 + x}, ${this.height - this.margin.bottom})`);
    g.append("line").attr("x1", parseInt(labelStyle.fontSize) + 5 + scaleBarLength).attr("x2", parseInt(labelStyle.fontSize) + 5).attr("y1", -y).attr("y2", -y).style("stroke", scaleBarLineStyle.stroke).style("stroke-width", scaleBarLineStyle.strokeWidth).each(function() {
      const currentElement = d3.select(this);
      setStyleFromOptions(currentElement, additionalOptionsLine);
    });
    [parseInt(labelStyle.fontSize) + 5, parseInt(labelStyle.fontSize) + 5 + scaleBarLength].forEach((d) => {
      g.append("line").attr("x1", d).attr("x2", d).attr("y1", -y - 5).attr("y2", -y + 5).style("stroke", scaleBarTickStyle.stroke).style("stroke-width", scaleBarTickStyle.strokeWidth).each(function() {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsTick);
      });
    });
    const titleX = labelStyle.labelPosition === "left" ? parseInt(labelStyle.fontSize) - textPadding : parseInt(labelStyle.fontSize) + 5 + scaleBarLength + textPadding;
    const textAnchor = labelStyle.labelPosition === "left" ? "end" : "start";
    g.append("text").attr("x", titleX).attr("y", -y).style("text-anchor", textAnchor).style("dominant-baseline", "middle").style("font-size", labelStyle.fontSize).style("font-family", labelStyle.fontFamily).style("cursor", labelStyle.cursor).style("fill", labelStyle.fill).each(function() {
      const currentElement = d3.select(this);
      setStyleFromOptions(currentElement, additionalOptionsLabel);
    }).text(title2);
    applyMove(g, combinedOptions.itemStyle?.[0]);
    return this;
  };

  // src/lib/components/labels.js
  function addLinkLabelLine(label, linkLabel) {
    const text = d3.select(label);
    const name = label.textContent;
    label.textContent = "";
    text.append("tspan").attr("class", "label-name").text(name);
    const measure = text.append("tspan").attr("class", "label-measure").text(linkLabel.textContent);
    for (const property of linkLabel.style) {
      if (property === "text-anchor") continue;
      measure.style(property, linkLabel.style.getPropertyValue(property));
    }
    linkLabel.remove();
    if (name) {
      measure.attr("x", text.attr("x")).attr("dy", "1.1em");
      const lineHeight = 1.1 * parseFloat(getComputedStyle(measure.node()).fontSize);
      text.attr("y", parseFloat(text.attr("y")) - lineHeight);
    }
  }
  function linkLabelLineToInline(label) {
    const text = d3.select(label);
    const measure = text.select(".label-measure");
    if (measure.empty()) return;
    measure.attr("x", null).attr("dy", null).attr("dx", text.select(".label-name").text() ? "0.3em" : null);
  }
  function spreadLabelCentres(wanted, widths, gap, left, right) {
    const order2 = wanted.map((_, i) => i).sort((a, b) => wanted[a] - wanted[b]);
    const blocks = [];
    const place = (block) => {
      block.start = Math.max(left, Math.min(block.total / block.members.length, right - block.width));
    };
    for (const i of order2) {
      let block = { members: [{ i, offset: 0 }], width: widths[i], total: wanted[i] - widths[i] / 2 };
      place(block);
      let previous = blocks[blocks.length - 1];
      while (previous && previous.start + previous.width + gap > block.start) {
        const shift = previous.width + gap;
        block.members.forEach((member) => {
          member.offset += shift;
        });
        previous.members.push(...block.members);
        previous.total += block.total - block.members.length * shift;
        previous.width += gap + block.width;
        block = blocks.pop();
        place(block);
        previous = blocks[blocks.length - 1];
      }
      blocks.push(block);
    }
    const centres = new Array(wanted.length);
    blocks.forEach((block) => block.members.forEach(({ i, offset }) => {
      centres[i] = block.start + offset + widths[i] / 2;
    }));
    return centres;
  }
  function updateLeaderLine(label) {
    const leader = label.__leader__;
    if (!leader || !leader.line.isConnected) return;
    const hidden = !label.isConnected || getComputedStyle(label).display === "none";
    leader.line.style.display = hidden ? "none" : null;
    if (hidden) return;
    const from = label.getScreenCTM?.();
    const to = leader.line.parentNode.getScreenCTM?.();
    const own = label.transform?.baseVal?.consolidate();
    const matrix = from && to ? to.inverse().multiply(from) : own ? own.matrix : new DOMMatrix();
    const box = label.getBBox();
    const top = new DOMPoint(box.x + box.width / 2, box.y).matrixTransform(matrix);
    const bottom = new DOMPoint(box.x + box.width / 2, box.y + box.height).matrixTransform(matrix);
    const above = bottom.y <= (leader.geneTop + leader.geneBottom) / 2;
    const end = above ? bottom : top;
    leader.line.setAttribute("y1", above ? leader.geneTop - leader.gap : leader.geneBottom + leader.gap);
    leader.line.setAttribute("x2", end.x);
    leader.line.setAttribute("y2", above ? end.y + leader.gap : end.y - leader.gap);
  }
  container.prototype.labels = function(label, show = true, options = {}) {
    if (!show) {
      return this;
    }
    if (!label) {
      return this;
    }
    if (!this.data) {
      console.error("No data has been added to this cluster container. Please use the addGeneData() function before attempting to draw genes.");
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
      // adjustLabels "spread": the side of the genes the labels go ("bottom" or
      // "top"), the distance from the genes to the labels, and the style of the
      // lines from the labels to their genes (or false)
      spreadPosition: "bottom",
      leaderLine: {
        length: 20,
        stroke: "#999999",
        strokeWidth: 0.75
      },
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
      itemStyle: [],
      // [{"index": 3,"y": 20}]
      groupStyle: {},
      styleBy: null,
      group: null
    };
    const combinedOptions = mergeOptions.call(this, defaultOptions, "labelsOptions", options);
    const { x, y, start, end, adjustLabels, labelAdjustmentOptions, spreadPosition, leaderLine, groupStyle, styleBy, group, dx, dy, anchor, rotate, fontSize, fontStyle, fontFamily, textAnchor, cursor } = combinedOptions;
    const additionalOptions = extractAdditionalOptions(combinedOptions, defaultOptions);
    const g = this.svg.append("g").attr("data-component", "labels").attr("transform", `translate(${this.margin.left},${this.margin.top})`);
    this.data.sort((a, b) => Math.min(a.start, a.end) - Math.min(b.start, b.end));
    const itemStyle = withGroupStyles(this.data, styleBy ?? group, groupStyle, combinedOptions.itemStyle);
    const styleByIndex = new Map(itemStyle.map((s) => [s.index, s]));
    const getAttributesForIndex = (d, i) => {
      const style = styleByIndex.get(i) || {};
      const currentX = style.x ?? x;
      const currentY = style.y ?? y;
      const currentDx = style.dx ?? dx;
      const currentDy = style.dy ?? dy;
      const currentAdjustLabels = style.adjustLabels !== void 0 ? style.adjustLabels : adjustLabels;
      const spread = currentAdjustLabels === "spread";
      const currentRotate = spread ? 0 : style.rotate ?? rotate;
      const below = !spread && d.geneTrack > 1;
      const currentLabelAdjustmentOptions = { ...style.labelAdjustmentOptions ?? labelAdjustmentOptions };
      currentLabelAdjustmentOptions.rotation = -Math.abs(currentLabelAdjustmentOptions.rotation);
      const currentGeneStrandSpacing = d.strand == "forward" && this.geneStrandSpacing !== 0 ? this.geneStrandSpacing : -this.geneStrandSpacing;
      var currentOverlapSpacing = d.geneTrack ? (d.geneTrack - 1) * this.geneOverlapSpacing : 0;
      const geneY = this.yScale(this.markerY ?? 50);
      const yAbove = this.yScale(currentY) - this.markerHeight / 2 - currentGeneStrandSpacing + currentOverlapSpacing;
      const yBelow = 2 * geneY - this.yScale(currentY) + this.markerHeight / 2 - currentGeneStrandSpacing + currentOverlapSpacing;
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
        spread,
        below,
        yAbove
      };
    };
    const genesByTrack = d3.group(this.data, (d) => d.geneTrack ?? 1);
    const trackHasGeneIn = (track, left, right) => (genesByTrack.get(track) || []).some((gene) => {
      const [geneLeft, geneRight] = [this.xScale(gene.start), this.xScale(gene.end)].sort((a, b) => a - b);
      return geneLeft < right && geneRight > left;
    });
    const placeLabel = (element, d, attributes) => {
      if (!attributes.below) return;
      const bbox = element.getBBox();
      const left = bbox.x;
      const right = bbox.x + bbox.width;
      if (trackHasGeneIn(d.geneTrack + 1, left, right) && !trackHasGeneIn(d.geneTrack - 1, left, right)) {
        attributes.below = false;
        attributes.yPos = attributes.yAbove;
        d3.select(element).attr("dominant-baseline", null).attr("y", attributes.yPos).attr("transform", `rotate(${attributes.rotate}, ${attributes.xPos}, ${attributes.yPos})`);
      }
    };
    const attributesByIndex = this.data.map((d, i) => getAttributesForIndex(d, i));
    const labelAdjuster = createLabelAdjuster(this, "text.label", {
      left: 0,
      right: this.width - this.margin.left - this.margin.right,
      top: -this.margin.top,
      bottom: this.height - this.margin.top
    });
    const linkLabelsByRow = new Map(
      this.svg.selectAll(".link-text").nodes().map((node) => [node.getAttribute("rowID"), node])
    );
    g.selectAll("text.label").data(this.data).enter().append("text").attr("id", (d, i) => `cluster-${sanitizeId(d.cluster)}-label-${i}`).attr("rowID", (d, i) => `${d["rowID"]}`).attr("class", "label").each(function(d, i) {
      const { dx: dx2, dy: dy2, rotate: rotate2 } = attributesByIndex[i];
      this.__drawn__ = { dx: dx2, dy: dy2, rotate: rotate2 };
    }).attr("x", (d, i) => attributesByIndex[i].xPos).attr("y", (d, i) => attributesByIndex[i].yPos).attr("dx", (d, i) => attributesByIndex[i].dx).attr("dy", (d, i) => attributesByIndex[i].dy).attr("text-anchor", textAnchor).attr("dominant-baseline", (d, i) => attributesByIndex[i].below ? "hanging" : null).attr("transform", (d, i) => {
      const { xPos, yPos, rotate: rotate2 } = attributesByIndex[i];
      return `rotate(${rotate2}, ${xPos}, ${yPos})`;
    }).style("font-size", fontSize).style("font-style", fontStyle).style("font-family", fontFamily).style("cursor", cursor).text((d, i) => styleByIndex.get(i)?.text ?? d[label]).each(function(d) {
      const linkLabel = linkLabelsByRow.get(String(d.rowID));
      if (linkLabel) addLinkLabelLine(this, linkLabel);
    }).each(function(d, i) {
      const attributes = attributesByIndex[i];
      placeLabel(this, d, attributes);
      if (attributes.below) {
        attributes.labelAdjustmentOptions.rotation = Math.abs(attributes.labelAdjustmentOptions.rotation);
      }
    });
    const styleLabel = (element, i) => {
      const currentElement = d3.select(element);
      setStyleFromOptions(currentElement, additionalOptions);
      applyStyleToElement(currentElement, styleByIndex.has(i) ? [styleByIndex.get(i)] : [], i);
      if (element.childElementCount || changesSize(additionalOptions) || changesSize(styleByIndex.get(i))) {
        labelAdjuster.refresh(element);
      }
    };
    g.selectAll("text.label").each(function(d, i) {
      if (attributesByIndex[i].spread) styleLabel(this, i);
    });
    const { length: lineLength, ...lineStyle } = { ...defaultOptions.leaderLine, ...isPlainObject(leaderLine) ? leaderLine : {} };
    this.spreadLabels(g, attributesByIndex, {
      length: Number.isFinite(Number(lineLength)) ? Number(lineLength) : defaultOptions.leaderLine.length,
      lineStyle: leaderLine === false ? null : lineStyle,
      onTop: spreadPosition === "top"
    });
    g.selectAll("text.label").each(function(d, i) {
      const attributes = attributesByIndex[i];
      if (attributes.spread) return;
      if (attributes.adjustLabels && labelAdjuster.adjust(this, {
        ...attributes.labelAdjustmentOptions,
        hide: attributes.adjustLabels === "hide"
      })) {
        linkLabelLineToInline(this);
      }
      styleLabel(this, i);
    });
    g.selectAll("text.label").each(function(d, i) {
      applyMove(d3.select(this), styleByIndex.get(i) ?? {});
    });
    this.labels = g.selectAll(".label");
    return this;
  };
  container.prototype.spreadLabels = function(g, attributesByIndex, options) {
    const svg = this.svg.node();
    if (!svg.getClientRects().length) {
      const observer = new ResizeObserver(() => {
        if (!svg.isConnected) return observer.disconnect();
        if (!svg.getClientRects().length) return;
        observer.disconnect();
        this.spreadLabels(g, attributesByIndex, options);
      });
      observer.observe(svg);
      return this;
    }
    const plotWidth = this.width - this.margin.left - this.margin.right;
    const inView = (d) => {
      const [left, right] = [this.xScale(d.start), this.xScale(d.end)].sort((a, b) => a - b);
      const from = Math.max(0, left);
      const to = Math.min(plotWidth, right);
      return from <= to ? [from, to] : null;
    };
    const labels2 = [];
    g.selectAll("text.label").each(function(d, i) {
      if (!attributesByIndex[i].spread || this.textContent.trim() === "") return;
      const view = inView(d);
      if (!view) {
        this.style.display = "none";
        return;
      }
      const { xPos } = attributesByIndex[i];
      labels2.push({ node: this, d, geneX: (view[0] + view[1]) / 2, wanted: Math.min(view[1], Math.max(view[0], xPos)) });
    });
    if (!labels2.length) return this;
    const labelsCTM = g.node().getCTM();
    const labelled = new Set(labels2.map(({ d }) => String(d.rowID)));
    const geneTops = /* @__PURE__ */ new Map();
    const geneBottoms = /* @__PURE__ */ new Map();
    let highest = Infinity;
    let lowest = -Infinity;
    this.svg.selectAll(".gene, .exon, .intron, .utr").each(function() {
      const ctm = this.getCTM();
      if (!ctm || !labelsCTM) return;
      const matrix = labelsCTM.inverse().multiply(ctm);
      const box = this.getBBox();
      let top = Infinity;
      let bottom = -Infinity;
      for (const [px, py] of [
        [box.x, box.y],
        [box.x + box.width, box.y],
        [box.x, box.y + box.height],
        [box.x + box.width, box.y + box.height]
      ]) {
        const y = new DOMPoint(px, py).matrixTransform(matrix).y;
        top = Math.min(top, y);
        bottom = Math.max(bottom, y);
      }
      highest = Math.min(highest, top);
      lowest = Math.max(lowest, bottom);
      const rowID = this.getAttribute("rowID");
      if (!labelled.has(rowID)) return;
      geneTops.set(rowID, Math.min(geneTops.get(rowID) ?? Infinity, top));
      geneBottoms.set(rowID, Math.max(geneBottoms.get(rowID) ?? -Infinity, bottom));
    });
    const geneY = this.yScale(this.markerY ?? 50);
    const half = (this.markerHeight ?? 0) / 2;
    if (!Number.isFinite(highest)) highest = geneY - half;
    if (!Number.isFinite(lowest)) lowest = geneY + half;
    const gap = 6;
    const lineGap = 2;
    const boxes = labels2.map(({ node }) => node.getBBox());
    const centres = spreadLabelCentres(
      labels2.map(({ wanted }) => wanted),
      boxes.map((box) => box.width),
      gap,
      0,
      plotWidth
    );
    const height = boxes.reduce((most, box) => Math.max(most, box.height), 0);
    const room = options.onTop ? highest - height + this.margin.top : this.height - this.margin.top - lowest - height;
    const length = Math.max(2 * lineGap, Math.min(options.length, room));
    labels2.forEach(({ node }, k) => {
      const box = boxes[k];
      const left = centres[k] - box.width / 2;
      if (left < -0.5 || left + box.width > plotWidth + 0.5) {
        node.style.display = "none";
        return;
      }
      const dy = options.onTop ? highest - length - (box.y + box.height) : lowest + length - box.y;
      const { moveX = 0, moveY = 0 } = node.__drawn__ ?? {};
      const move = moveX || moveY ? `translate(${moveX}, ${moveY}) ` : "";
      node.setAttribute("transform", `${move}translate(${centres[k] - (box.x + box.width / 2)}, ${dy})`);
    });
    if (!options.lineStyle) return this;
    const lines = g.insert("g", "text.label").attr("class", "leader-lines").style("pointer-events", "none");
    labels2.forEach(({ node, d, geneX }) => {
      if (node.style.display === "none") return;
      const line = lines.append("line").attr("x1", geneX).attr("x2", geneX);
      setStyleFromOptions(line, options.lineStyle);
      node.__leader__ = {
        line: line.node(),
        gap: lineGap,
        geneTop: geneTops.get(String(d.rowID)) ?? highest,
        geneBottom: geneBottoms.get(String(d.rowID)) ?? lowest
      };
      updateLeaderLine(node);
      new MutationObserver(() => updateLeaderLine(node)).observe(node, { attributes: true, attributeFilter: ["transform", "style"] });
    });
    return this;
  };

  // src/lib/components/tooltip.js
  container.prototype.tooltip = function(show = true, options = {}) {
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
      zIndex: 1e3,
      color: "#333",
      lineHeight: "1.5"
    };
    const combinedOptions = mergeOptions.call(this, defaultOptions, "tooltipOptions", options);
    const additionalOptions = extractAdditionalOptions(combinedOptions, defaultOptions);
    if (typeof combinedOptions.triggers === "string") {
      combinedOptions.triggers = [combinedOptions.triggers];
    }
    const host = this.svg.node().closest(".html-widget") || document.body;
    const tooltip2 = d3.select(host).append("div").attr("class", "cluster-tooltip").style("opacity", combinedOptions.opacity).style("position", combinedOptions.position).style("background-color", combinedOptions.backgroundColor).style("--tooltip-bg", combinedOptions.backgroundColor).style("padding", combinedOptions.padding).style("border-radius", combinedOptions.borderRadius).style("border", combinedOptions.border).style("box-shadow", combinedOptions.boxShadow).style("pointer-events", combinedOptions.pointerEvents).style("font-family", combinedOptions.fontFamily).style("font-size", combinedOptions.fontSize).style("z-index", combinedOptions.zIndex).style("color", combinedOptions.color).style("line-height", combinedOptions.lineHeight);
    setStyleFromOptions(tooltip2, additionalOptions);
    const moveTooltip = (event) => {
      const [x, y] = d3.pointer(event, host);
      tooltip2.style("left", x - tooltip2.node().offsetWidth / 2 + "px").style("top", y - tooltip2.node().offsetHeight - 15 + "px");
    };
    const d3Format = d3.format(",");
    const isMissing = (value) => value === null || value === void 0 || value === "" || typeof value === "number" && isNaN(value);
    const textAccessor = (d) => {
      return combinedOptions.formatter.split(/<br\s*\/?>/i).filter((line) => {
        const fields = [...line.matchAll(/\{(\w+)\}/g)].map((m) => m[1]);
        return fields.length === 0 || !fields.every((field) => isMissing(d[field]));
      }).join("<br>").replace(/\{(\w+)\}/g, (match, p1) => {
        if (typeof d[p1] === "number") {
          return d3Format(d[p1]);
        }
        return d[p1] || "";
      });
    };
    combinedOptions.triggers.forEach((trigger) => {
      if (!this.hasOwnProperty(trigger)) {
        return;
      }
      const selection = this[trigger];
      if (!selection || selection.empty()) {
        return;
      }
      selection.on("mouseover", (event, d) => {
        const dataPoint = this.data.find((item) => item === d);
        const element = d3.select(event.currentTarget);
        element.classed("hovered", true);
        tooltip2.transition().duration(200).style("opacity", 1);
        tooltip2.html(textAccessor(dataPoint));
        moveTooltip(event);
      });
      selection.on("mousemove", moveTooltip);
      selection.on("mouseout", (event) => {
        const element = d3.select(event.currentTarget);
        element.classed("hovered", false);
        tooltip2.transition().duration(500).style("opacity", 0);
      });
    });
    return this;
  };

  // src/lib/color.js
  function getColorScale(colorScheme, customColors, uniqueGroups) {
    const schemeCategory30 = [
      "#1f77b4",
      "#ff7f0e",
      "#2ca02c",
      "#d62728",
      "#9467bd",
      "#8c564b",
      "#e377c2",
      "#7f7f7f",
      "#bcbd22",
      "#17becf",
      "#393b79",
      "#637939",
      "#8c6d31",
      "#843c39",
      "#7b4173",
      "#bd9e39",
      "#ad494a",
      "#d6616b",
      "#31a354",
      "#cedb9c",
      "#9c9ede",
      "#5254a3",
      "#8ca252",
      "#b5cf6b",
      "#e7ba52",
      "#ce6dbd",
      "#e7cb94",
      "#e7969c",
      "#6b6ecf",
      "#a55194"
    ];
    const groups = uniqueGroups.filter((group) => group !== null);
    const schemeRange = (usesScheme = () => true) => {
      if (colorScheme && (Array.isArray(colorScheme) || d3[colorScheme])) {
        const scheme = Array.isArray(colorScheme) ? colorScheme : d3[colorScheme];
        if (groups.some((group, index) => index >= scheme.length && usesScheme(group))) {
          warnOnce(`Warning: More unique groups than colors. Some colors will repeat.`);
        }
        return groups.map((group, index) => scheme[index % scheme.length]);
      }
      if (colorScheme) {
        warnOnce(`Warning: The color scheme "${colorScheme}" does not exist. Defaulting to schemeCategory30.`);
      }
      return groups.map(
        (group, index) => group === "No Hit" ? "#FFF" : schemeCategory30[index % schemeCategory30.length]
      );
    };
    let range;
    if (customColors && typeof customColors === "string") {
      range = groups.map(() => customColors);
    } else if (customColors && Array.isArray(customColors)) {
      range = groups.map((group, index) => customColors[index % customColors.length] || "#FFF");
    } else if (customColors && typeof customColors === "object") {
      const scheme = schemeRange((group) => !customColors[group]);
      range = groups.map((group, index) => customColors[group] || scheme[index]);
    } else {
      range = schemeRange();
    }
    return d3.scaleOrdinal().domain([...groups, null]).range([...range, "#FFF"]);
  }
  var genePatterns = {
    none: () => {
    },
    solid: (p, s, color2) => p.append("rect").attr("width", s).attr("height", s).attr("fill", color2),
    horizontal: (p, s, color2, w) => p.append("path").attr("d", `M0,${s / 2}H${s}`).attr("stroke", color2).attr("stroke-width", w),
    vertical: (p, s, color2, w) => p.append("path").attr("d", `M${s / 2},0V${s}`).attr("stroke", color2).attr("stroke-width", w),
    // The corner segments keep the lines unbroken where tiles meet
    diagonal: (p, s, color2, w) => p.append("path").attr("d", `M0,${s}L${s},0M${-s / 4},${s / 4}L${s / 4},${-s / 4}M${s * 3 / 4},${s * 5 / 4}L${s * 5 / 4},${s * 3 / 4}`).attr("stroke", color2).attr("stroke-width", w),
    antidiagonal: (p, s, color2, w) => p.append("path").attr("d", `M0,0L${s},${s}M${-s / 4},${s * 3 / 4}L${s / 4},${s * 5 / 4}M${s * 3 / 4},${-s / 4}L${s * 5 / 4},${s / 4}`).attr("stroke", color2).attr("stroke-width", w),
    grid: (p, s, color2, w) => p.append("path").attr("d", `M0,${s / 2}H${s}M${s / 2},0V${s}`).attr("stroke", color2).attr("stroke-width", w),
    crosshatch: (p, s, color2, w) => {
      genePatterns.diagonal(p, s, color2, w);
      genePatterns.antidiagonal(p, s, color2, w);
    },
    dots: (p, s, color2) => p.append("circle").attr("cx", s / 2).attr("cy", s / 2).attr("r", s / 5).attr("fill", color2),
    zigzag: (p, s, color2, w) => p.append("path").attr("d", `M0,${s * 0.7}L${s / 2},${s * 0.3}L${s},${s * 0.7}`).attr("fill", "none").attr("stroke", color2).attr("stroke-width", w),
    checkerboard: (p, s, color2) => p.append("path").attr("d", `M0,0H${s / 2}V${s / 2}H0ZM${s / 2},${s / 2}H${s}V${s}H${s / 2}Z`).attr("fill", color2),
    rings: (p, s, color2, w) => p.append("circle").attr("cx", s / 2).attr("cy", s / 2).attr("r", s / 3.2).attr("fill", "none").attr("stroke", color2).attr("stroke-width", w * 0.8),
    // Rows of bricks, each row offset by half a brick
    bricks: (p, s, color2, w) => p.append("path").attr("d", `M0,0H${s}M0,${s / 2}H${s}M${s / 4},0V${s / 2}M${s * 3 / 4},${s / 2}V${s}`).attr("stroke", color2).attr("stroke-width", w),
    plus: (p, s, color2, w) => p.append("path").attr("d", `M${s / 2},${s * 0.2}V${s * 0.8}M${s * 0.2},${s / 2}H${s * 0.8}`).attr("stroke", color2).attr("stroke-width", w),
    // Filled band, so it reads as thick stripes at any strokeWidth
    wideStripes: (p, s, color2) => p.append("path").attr("d", `M0,${s}L${s},0H${s / 2}L0,${s / 2}ZM${s},${s / 2}V${s}H${s / 2}Z`).attr("fill", color2),
    // Background-coloured dots on a solid tile
    inverseDots: (p, s, color2, w, bg) => {
      genePatterns.solid(p, s, color2);
      p.append("circle").attr("cx", s / 2).attr("cy", s / 2).attr("r", s / 4).attr("fill", bg);
    }
  };
  var patternTileScale = { checkerboard: 4 / 3, rings: 7 / 6, bricks: 5 / 3, plus: 4 / 3, wideStripes: 4 / 3 };
  var defaultPatternOrder = [
    "diagonal",
    "dots",
    "horizontal",
    "crosshatch",
    "vertical",
    "zigzag",
    "checkerboard",
    "solid",
    "rings",
    "bricks",
    "inverseDots",
    "grid",
    "wideStripes",
    "plus",
    "antidiagonal"
  ];
  function valuesByGroup(values, groups, fallback) {
    if (values === null || values === void 0) {
      return groups.map(() => fallback);
    }
    if (typeof values === "string") {
      return groups.map(() => values);
    }
    if (Array.isArray(values)) {
      return groups.map((group, index) => values[index % values.length]);
    }
    return groups.map((group) => values[group] ?? fallback);
  }
  function getPatternScale(svg, pattern2, uniqueGroups, colorScale, idPrefix) {
    if (!pattern2) {
      return { fill: colorScale, stroke: colorScale, outline: null, align: () => {
      } };
    }
    const { patterns = null, color: color2 = "black", background = null, size = 6, strokeWidth = 1 } = pattern2;
    const groups = uniqueGroups.filter((group) => group !== null);
    const types = valuesByGroup(patterns, groups, "none");
    if (patterns === null || patterns === void 0) {
      groups.forEach((group, index) => {
        types[index] = defaultPatternOrder[index % defaultPatternOrder.length];
      });
      if (groups.length > defaultPatternOrder.length) {
        warnOnce(`Warning: More unique groups than patterns. Some patterns will repeat.`);
      }
    }
    const backgrounds = valuesByGroup(background, groups, null);
    const colors = valuesByGroup(color2, groups, "black");
    const svgId = svg.attr("id") || "geneviewer";
    const defs = svg.append("defs");
    const fills = /* @__PURE__ */ new Map();
    const tiles = /* @__PURE__ */ new Map();
    groups.forEach((group, index) => {
      let type = types[index];
      if (!genePatterns[type]) {
        warnOnce(`Warning: The pattern "${type}" does not exist. Using "none".`);
        type = "none";
      }
      if (group === "No Hit" && (patterns === null || patterns === void 0)) {
        type = "none";
      }
      const id = `${svgId}-${idPrefix}-pattern-${index}`;
      const tileSize = size * (patternTileScale[type] ?? 1);
      const bg = backgrounds[index] ?? colorScale(group);
      const tile = defs.append("pattern").attr("id", id).attr("patternUnits", "userSpaceOnUse").attr("width", tileSize).attr("height", tileSize);
      tile.append("rect").attr("width", tileSize).attr("height", tileSize).attr("fill", bg);
      genePatterns[type](tile, tileSize, colors[index], strokeWidth, bg);
      fills.set(group, `url(#${id})`);
      tiles.set(group, tile);
    });
    const colorByGroup = new Map(groups.map((group, index) => [group, colors[index]]));
    return {
      fill: (group) => fills.get(group) ?? colorScale(group),
      // Lines (introns, UTR outlines) take the pattern colour when the
      // background replaces the gene colours, and keep the gene colour otherwise
      stroke: (group) => background === null || background === void 0 ? colorScale(group) : colorByGroup.get(group) ?? colors[0],
      // Outline for legend markers, so white and patterned markers stay visible;
      // black when the groups have different pattern colours
      outline: typeof color2 === "string" ? color2 : "black",
      // Starts the tiles of a group at (x, y), e.g. at the corner of its legend marker
      align: (group, x, y) => tiles.get(group)?.attr("x", x).attr("y", y)
    };
  }

  // src/lib/components/genes.js
  function roundedPolygonPath(points, radius) {
    const pts = points.filter((p, i) => {
      const prev = points[(i - 1 + points.length) % points.length];
      return Math.abs(p[0] - prev[0]) > 1e-6 || Math.abs(p[1] - prev[1]) > 1e-6;
    });
    if (!(radius > 0) || pts.length < 3) {
      return `M${pts.map((p) => `${p[0]} ${p[1]}`).join(" L")} Z`;
    }
    const n = pts.length;
    const sideLength = (a, b) => Math.hypot(b[0] - a[0], b[1] - a[1]);
    const towards = (a, b, d) => {
      const l = sideLength(a, b);
      return [a[0] + (b[0] - a[0]) * d / l, a[1] + (b[1] - a[1]) * d / l];
    };
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
      intron: { arrowheadWidth: 0, arrowheadHeight: 0, markerHeight: 20 }
      // Add intron default
      // Add other marker types if needed
    };
    const sizeOptions = sizeDefaults[markerSize] || sizeDefaults.medium;
    const markerOptions = markerDefaults[marker] || {};
    const combinedOptions = deepMergeOptions(markerOptions, options);
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
        const midPoint = length / 2;
        height = scaledMarkerHeight / 2;
        path = `M0 ${height}
                    L${midPoint} 0
                    L${length} ${height}`;
        break;
      default:
        warnOnce(`Marker type '${marker}' not recognized.`);
        path = "";
    }
    const used = { arrowheadWidth, arrowheadHeight, markerHeight, cornerRadius: cornerRadius ?? 0 };
    return { path, length, scaledArrowheadWidth, scaledArrowheadHeight, height, used };
  }
  function setRowAttributes(selection) {
    selection.attr("rowID", (d, i) => `${d["rowID"]}`).attr("start", (d, i) => `${d["start"]}`).attr("end", (d, i) => `${d["end"]}`).attr("strand", (d, i) => `${d["strand"]}`).attr("cluster", (d, i) => `${d["cluster"]}`);
  }
  function markerTransform(reverse, d, { xPos, yPos, height }) {
    const rotation = reverse ? d.direction === "forward" ? 180 : 0 : d.direction === "forward" ? 0 : 180;
    return `rotate(${rotation}, ${xPos}, ${yPos}) translate(${xPos}, ${yPos - height / 2})`;
  }
  function getMarkerAttributes(container2, d, i, y, styles, itemStyles) {
    const style = itemStyles.find((s) => s.index === i) || {};
    const geneLength = Math.abs(container2.xScale(d.end) - container2.xScale(d.start));
    const currentY = style.y ?? y;
    const marker = style.marker ?? styles.marker;
    const markerSize = style.markerSize ?? styles.markerSize;
    const { path, arrowheadWidth, arrowheadHeight, height, used } = getGenePath(
      marker,
      geneLength,
      markerSize,
      {
        arrowheadWidth: style.arrowheadWidth ?? styles.arrowheadWidth,
        arrowheadHeight: style.arrowheadHeight ?? styles.arrowheadHeight,
        markerHeight: style.markerHeight ?? styles.markerHeight,
        cornerRadius: style.cornerRadius ?? styles.cornerRadius
      }
    );
    const clusterHeight = getGenePath(styles.marker, geneLength, styles.markerSize, {
      arrowheadWidth: styles.arrowheadWidth,
      arrowheadHeight: styles.arrowheadHeight,
      markerHeight: styles.markerHeight,
      cornerRadius: styles.cornerRadius
    }).height;
    container2.markerHeight = clusterHeight;
    container2.markerY = y;
    container2.geneStrandSpacing = container2.separateStrands ? clusterHeight / 2 + container2.strandSpacing : 0;
    container2.geneOverlapSpacing = clusterHeight + container2.overlapSpacing;
    const currentGeneStrandSpacing = d.strand === "forward" && container2.geneStrandSpacing !== 0 ? container2.geneStrandSpacing : -container2.geneStrandSpacing;
    const currentOverlapSpacing = d.geneTrack ? (d.geneTrack - 1) * container2.geneOverlapSpacing : 0;
    const yPos = container2.yScale(currentY) - currentGeneStrandSpacing + currentOverlapSpacing;
    const xPos = container2.xScale(d.start);
    const drawn = { marker, markerSize, ...used, y: currentY };
    return { xPos, yPos, path, arrowheadWidth, arrowheadHeight, height, drawn };
  }
  container.prototype.genes = function(group, show = true, options = {}) {
    if (!show) {
      return this;
    }
    if (!this.data) {
      console.error("No data has been added to this cluster container. Please use the geneData() function before attempting to draw arrows.");
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
      styleBy: null,
      arrowheadWidth: null,
      arrowheadHeight: null,
      markerHeight: null,
      cornerRadius: null
    };
    const combinedOptions = mergeOptions.call(this, defaultOptions, "geneOptions", options);
    const { x, y, stroke, strokeWidth, colorScheme, customColors, pattern: pattern2, cursor, groupStyle, styleBy } = combinedOptions;
    const additionalOptions = extractAdditionalOptions(combinedOptions, defaultOptions);
    const uniqueGroups = [...new Set(this.dataAll.map((d) => d[group]))];
    const colorScale = getColorScale(colorScheme, customColors, uniqueGroups);
    this.genesColorScale = colorScale;
    const patternScale = getPatternScale(this.svg, pattern2, uniqueGroups, colorScale, "gene");
    var g = this.svg.append("g").attr("data-component", "genes").attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);
    this.data.sort((a, b) => Math.min(a.start, a.end) - Math.min(b.start, b.end));
    const itemStyle = withGroupStyles(this.data, styleBy ?? group, groupStyle, combinedOptions.itemStyle);
    const getAttributesForIndex = (d, i) => getMarkerAttributes(this, d, i, y, combinedOptions, itemStyle);
    g.selectAll(".gene").data(this.data).enter().append("path").attr("d", (d, i) => {
      return getAttributesForIndex(d, i).path;
    }).attr("transform", (d, i) => markerTransform(this.reverse, d, getAttributesForIndex(d, i))).attr("fill", (d) => patternScale.fill(d[group])).attr("class", "gene").attr("id", (d, i) => `${sanitizeId(d.cluster)}-gene-${i}`).call(setRowAttributes).style("stroke-width", strokeWidth).style("stroke", stroke).style("cursor", cursor).each(function(d, i) {
      const currentElement = d3.select(this);
      setStyleFromOptions(currentElement, additionalOptions);
      applyStyleToElement(currentElement, itemStyle, i);
      this.__drawn__ = getAttributesForIndex(d, i).drawn;
    });
    this.genes = g.selectAll(".gene");
    return this;
  };

  // src/lib/components/transcript.js
  container.prototype.transcript = function(group, show = true, options = {}) {
    if (!show) {
      return this;
    }
    if (!this.data) {
      console.error("No data has been added to this cluster container. Please use the geneData() function before attempting to draw arrows.");
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
        strokeWidth: 1,
        fill: "none",
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
      itemStyleUTRs: [],
      groupStyleExons: {},
      groupStyleIntrons: {},
      groupStyleUTRs: {},
      styleBy: null
    };
    const combinedOptions = mergeOptions.call(this, defaultOptions, "transcriptOptions", options);
    const { x, y, colorScheme, customColors, pattern: pattern2, styleExons, styleIntrons, styleUTRs, labelOptions } = combinedOptions;
    const styleColumn = combinedOptions.styleBy ?? group;
    const additionalOptionsExons = extractAdditionalOptions(combinedOptions.styleExons, defaultOptions.styleExons);
    const additionalOptionsIntrons = extractAdditionalOptions(combinedOptions.styleIntrons, defaultOptions.styleIntrons);
    const additionalOptionsUTRs = extractAdditionalOptions(combinedOptions.styleUTRs, defaultOptions.styleUTRs);
    const uniqueGroups = [...new Set(this.dataAll.map((d) => d[group]))];
    const colorScale = getColorScale(colorScheme, customColors, uniqueGroups);
    this.genesColorScale = colorScale;
    const patternScale = getPatternScale(this.svg, pattern2, uniqueGroups, colorScale, "exon");
    const g = this.svg.append("g").attr("data-component", "transcript").attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);
    this.data.sort((a, b) => Math.min(a.start, a.end) - Math.min(b.start, b.end));
    const getAttributesForIndex = (d, i, styles, itemStyles) => getMarkerAttributes(this, d, i, y, styles, itemStyles);
    const drawParts = (type, styles, groupStyle, itemStyle, additionalOptions, transform, fill, stroke) => {
      const parts = this.data.filter((transcript2) => new RegExp(type, "i").test(transcript2.type));
      const itemStyles = withGroupStyles(parts, styleColumn, groupStyle, itemStyle);
      g.selectAll(`.${type}`).data(parts).enter().append("path").attr("d", (d, i) => {
        return getAttributesForIndex(d, i, styles, itemStyles).path;
      }).attr("transform", (d, i) => transform(d, getAttributesForIndex(d, i, styles, itemStyles))).attr("fill", fill).attr("class", type).attr("id", (d, i) => `${sanitizeId(d.cluster)}-${type}-${i}`).call(setRowAttributes).style("stroke-width", styles.strokeWidth).style("stroke", stroke).style("cursor", styles.cursor).each(function(d, i) {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptions);
        applyStyleToElement(currentElement, itemStyles, i);
      });
      return g.selectAll(`.${type}`);
    };
    const boxTransform = (d, attributes) => markerTransform(this.reverse, d, attributes);
    const geneStroke = (d) => patternScale.stroke(d[group]);
    if (styleExons.show) {
      this.exons = drawParts(
        "exon",
        styleExons,
        combinedOptions.groupStyleExons,
        combinedOptions.itemStyleExons,
        additionalOptionsExons,
        boxTransform,
        (d) => patternScale.fill(d[group]),
        styleExons.stroke
      );
    }
    if (styleIntrons.show) {
      const intronTransform = (d, { xPos, yPos, height }) => {
        const yOffset = d.direction === "forward" ? -height : +height;
        const rotation = this.reverse ? d.direction === "reverse" ? 180 : 0 : d.direction === "forward" ? 0 : 180;
        return `rotate(${rotation}, ${xPos}, ${yPos + yOffset}) translate(${xPos}, ${yPos + yOffset})`;
      };
      this.introns = drawParts(
        "intron",
        styleIntrons,
        combinedOptions.groupStyleIntrons,
        combinedOptions.itemStyleIntrons,
        additionalOptionsIntrons,
        intronTransform,
        styleIntrons.fill,
        geneStroke
      );
    }
    if (styleUTRs.show) {
      this.utrs = drawParts(
        "utr",
        styleUTRs,
        combinedOptions.groupStyleUTRs,
        combinedOptions.itemStyleUTRs,
        additionalOptionsUTRs,
        boxTransform,
        styleUTRs.fill,
        geneStroke
      );
      if (labelOptions.show) {
        const xPosition3Prime = this.data[0].direction === "forward" ? this.xScale(this.maxEnd) + labelOptions.xOffset : this.xScale(this.minStart) - labelOptions.xOffset;
        const xPosition5Prime = this.data[0].direction === "forward" ? this.xScale(this.minStart) - labelOptions.xOffset : this.xScale(this.maxEnd) + labelOptions.xOffset;
        g.append("text").attr("x", xPosition3Prime).attr("y", (d) => this.yScale(y + labelOptions.yOffset)).attr("class", "three-prime-label").attr("text-anchor", "middle").attr("alignment-baseline", "middle").style("font-size", labelOptions.fontSize).style("font-style", labelOptions.fontStyle).style("font-weight", labelOptions.fontWeight).style("font-family", labelOptions.fontFamily).style("cursor", labelOptions.cursor).style("fill", labelOptions.color).text("3'");
        g.append("text").attr("x", xPosition5Prime).attr("y", (d) => this.yScale(y + labelOptions.yOffset)).attr("class", "five-prime-label").attr("text-anchor", "middle").attr("alignment-baseline", "middle").style("font-size", labelOptions.fontSize).style("font-style", labelOptions.fontStyle).style("font-weight", labelOptions.fontWeight).style("font-family", labelOptions.fontFamily).style("cursor", labelOptions.cursor).style("fill", labelOptions.color).text("5'");
      }
    }
    return this;
  };

  // src/lib/links.js
  function getLinkCoordinates(graphContainer, data) {
    const links2 = data.map((item) => {
      const selector1 = `.link-marker[cluster='${item.cluster1}'][linkID='${item.linkID}'][position='${item.start1}']`;
      const selector2 = `.link-marker[cluster='${item.cluster2}'][linkID='${item.linkID}'][position='${item.start2}']`;
      const selector3 = `.link-marker[cluster='${item.cluster1}'][linkID='${item.linkID}'][position='${item.end1}']`;
      const selector4 = `.link-marker[cluster='${item.cluster2}'][linkID='${item.linkID}'][position='${item.end2}']`;
      const element1 = graphContainer.select(selector1).node();
      const element2 = graphContainer.select(selector2).node();
      const element3 = graphContainer.select(selector3).node();
      const element4 = graphContainer.select(selector4).node();
      if (!element1 || !element2 || !element3 || !element4) {
        return null;
      }
      const rect1 = element1.getBoundingClientRect();
      const rect2 = element2.getBoundingClientRect();
      const rect3 = element3.getBoundingClientRect();
      const rect4 = element4.getBoundingClientRect();
      const containerRect = graphContainer.node().getBoundingClientRect();
      const adjustRect = (rect) => ({
        x: rect.left - containerRect.left,
        y: rect.top - containerRect.top
      });
      const strand1 = item.start1 <= item.end1 ? "forward" : "reverse";
      const strand2 = item.start2 <= item.end2 ? "forward" : "reverse";
      const groupColor1 = element1.getAttribute("groupColor");
      const groupColor2 = element2.getAttribute("groupColor");
      return [
        { startPoint: adjustRect(rect1), endPoint: adjustRect(rect2), strand: strand1, groupColor: groupColor1 },
        { startPoint: adjustRect(rect3), endPoint: adjustRect(rect4), strand: strand2, groupColor: groupColor2 }
      ];
    });
    return links2;
  }
  var defaultLinkColors = {
    normalColor: "#969696",
    invertedColor: "#d62728",
    // Colour of a link with a measure of 0, e.g. the background on a dark chart
    lowColor: "#FFFFFF",
    // Measures of the lightest and the full link colour
    vmin: 0,
    vmax: 100
  };
  function linkColorScale(lowColor, color2, vmin, vmax) {
    const nearlyWhite = d3.interpolateRgb(lowColor, color2)(0.1);
    return d3.scaleLinear().domain([vmin, vmax]).range([nearlyWhite, color2]).interpolate(d3.interpolateRgb).clamp(true);
  }
  function makeLinks(graphContainer, links2, themeOptions = {}) {
    if (!links2 || links2.length === 0) {
      return;
    }
    const defaultOptions = {
      curve: true,
      ...defaultLinkColors,
      useGroupColors: false,
      color: "lightgrey",
      measure: "identity",
      label: true,
      // Where the measure is written: "gene", with the label of the gene, or
      // "link", in the middle of the link where it fits
      labelPosition: "gene",
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
    const svgContainer = graphContainer.insert("div", ":first-child").style("position", "relative").style("width", "100%").style("height", "100%");
    var lineSvg = svgContainer.insert("svg", ":first-child").attr("width", graphRect.width).attr("height", graphRect.height).classed("GeneLink", true).style("position", "absolute").style("z-index", 1);
    const linkLabels = [];
    links2.forEach(function(link) {
      const combinedOptions = mergeOptions(defaultOptions, "linkOptions", link.options, themeOptions);
      const { curve, invertedColor, normalColor, lowColor, vmin, vmax, useGroupColors, color: color2, linkStyle, measure } = combinedOptions;
      const additionalOptionsLinkStyle = extractAdditionalOptions(linkStyle, defaultOptions.linkStyle);
      const linkData = toRows(link.data);
      const coordinates2 = getLinkCoordinates(graphContainer, linkData);
      coordinates2.forEach(function(coordinate, index) {
        if (!coordinate) {
          return;
        }
        const baseColor = coordinate[0].strand == coordinate[1].strand ? d3.rgb(normalColor) : d3.rgb(invertedColor);
        var colorScale = linkColorScale(lowColor, baseColor, vmin, vmax);
        const identity = combinedOptions.measure === "none" ? 100 : link.data?.[measure]?.[index] ?? 100;
        const linkColor = useGroupColors ? coordinate[0].groupColor : colorScale(identity);
        lineSvg.append("path").attr("data-component", "links").datum(linkData[index]).attr("d", createLinkerPath(coordinate[0], coordinate[1], curve)).style("fill", linkColor).style("stroke", linkStyle.stroke).style("fill-opacity", linkStyle.fillOpacity).classed("GeneLink", true).each(function() {
          const currentElement = d3.select(this);
          setStyleFromOptions(currentElement, additionalOptionsLinkStyle);
        });
        const value = link.data?.[measure]?.[index];
        if (combinedOptions.label && combinedOptions.labelPosition === "link" && measure !== "none" && value != null && Number.isFinite(+value)) {
          const left = (coordinate[0].startPoint.x + coordinate[0].endPoint.x) / 2;
          const right = (coordinate[1].startPoint.x + coordinate[1].endPoint.x) / 2;
          linkLabels.push({
            x: (left + right) / 2,
            y: (coordinate[0].startPoint.y + coordinate[0].endPoint.y) / 2,
            width: Math.abs(right - left),
            text: `${parseFloat((+value).toFixed(1))}%`,
            // Dark text on a light link, white on a dark one
            fill: d3.hsl(d3.color(linkColor) ?? "#ffffff").l < 0.55 ? "#ffffff" : "#333333",
            datum: linkData[index],
            labelStyle: combinedOptions.labelStyle,
            defaults: defaultOptions.labelStyle
          });
        }
      });
    });
    const labelGroup = lineSvg.append("g").attr("class", "link-measure-labels");
    const placed = [];
    linkLabels.sort((a, b) => b.width - a.width).forEach((item) => {
      const { labelStyle } = item;
      const text = labelGroup.append("text").attr("class", "link-measure").attr("data-component", "links").datum(item.datum).attr("x", item.x).attr("y", item.y).attr("text-anchor", "middle").attr("dominant-baseline", "central").style("font-size", labelStyle.fontSize).style("font-style", labelStyle.fontStyle).style("font-family", labelStyle.fontFamily).style("fill", item.fill).style("pointer-events", "none").text(item.text).each(function() {
        const extra = extractAdditionalOptions(labelStyle, item.defaults);
        delete extra.labelPosition;
        setStyleFromOptions(d3.select(this), extra);
      });
      const box = text.node().getBBox();
      const clear = placed.every((other) => box.x + box.width < other.x || other.x + other.width < box.x || box.y + box.height < other.y || other.y + other.height < box.y);
      if (box.width + 4 > item.width || !clear) {
        text.remove();
      } else {
        placed.push(box);
      }
    });
  }
  function createLinkerPath(link1, link2, curve = true) {
    var path = d3.path();
    if (curve) {
      var midY1 = (link1.startPoint.y + link1.endPoint.y) / 2;
      path.moveTo(link1.startPoint.x, link1.startPoint.y);
      path.bezierCurveTo(link1.startPoint.x, midY1, link1.endPoint.x, midY1, link1.endPoint.x, link1.endPoint.y);
      path.lineTo(link2.endPoint.x, link2.endPoint.y);
      var midY2 = (link2.startPoint.y + link2.endPoint.y) / 2;
      path.bezierCurveTo(link2.endPoint.x, midY2, link2.startPoint.x, midY2, link2.startPoint.x, link2.startPoint.y);
    } else {
      path.moveTo(link1.startPoint.x, link1.startPoint.y);
      path.lineTo(link1.endPoint.x, link1.endPoint.y);
      path.lineTo(link2.endPoint.x, link2.endPoint.y);
      path.lineTo(link2.startPoint.x, link2.startPoint.y);
    }
    path.closePath();
    return path.toString();
  }
  function getClusterLinks(data, cluster2) {
    if (!data || data.length === 0) {
      return [];
    }
    const linksCluster1 = data.filter((item) => item.cluster1 === cluster2);
    const linksCluster2 = data.filter((item) => item.cluster2 === cluster2);
    const clusterLinks = linksCluster1.concat(linksCluster2);
    return clusterLinks;
  }
  function linkColorTypes(links2) {
    let normal = false;
    let inverted = false;
    links2.forEach((link) => {
      const { start1 = [], end1 = [], start2 = [], end2 = [] } = link.data;
      for (let i = 0; i < start1.length; i++) {
        if (start1[i] <= end1[i] === start2[i] <= end2[i]) {
          normal = true;
        } else {
          inverted = true;
        }
      }
    });
    return { normal, inverted };
  }
  function colorBarSettings(links2, themeOptions = {}) {
    const defaultOptions = {
      colorBar: true,
      ...defaultLinkColors,
      measure: "identity",
      margin: { top: 5, right: 25, bottom: 0, left: 50 },
      colorBarOptions: {
        // Where the bar goes: a corner of the plot area or "legend", and
        // whether it stands up or lies down
        position: "bottomright",
        orientation: "vertical",
        // Labels of a lying bar: "inline", on one line with it, or "below",
        // under it with ticks; always inline in the legend
        labels: "inline",
        // In the legend: before the groups ("left") or after them ("right")
        legendSide: "left",
        // From the corner; null: 24 pixels up from the bottom for a
        // standing bar, otherwise 0
        x: 0,
        y: null,
        title: true,
        // null: 10 by 60 pixels standing, 80 by 8 lying inline, 120 by 8
        // lying with the labels below
        width: null,
        height: null,
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
        }
      }
    };
    const linkOptions = mergeOptions(defaultOptions, "linkOptions", links2[0].options, themeOptions);
    const { colorBarOptions } = linkOptions;
    if (linkOptions.colorBar === false || linkOptions.measure === "none") {
      return null;
    }
    if (!links2[0].data.hasOwnProperty("identity") && !links2[0].data.hasOwnProperty("similarity")) {
      return null;
    }
    const { normal: hasNormal, inverted: hasInverted } = linkColorTypes(links2);
    if (!hasNormal && !hasInverted) {
      return null;
    }
    const defaults = defaultOptions.colorBarOptions;
    return {
      linkOptions,
      colorBarOptions,
      // In the legend the bar always lies down
      horizontal: colorBarOptions.position === "legend" || colorBarOptions.orientation === "horizontal",
      hasNormal,
      hasInverted,
      additional: {
        label: extractAdditionalOptions(colorBarOptions.labelOptions, defaults.labelOptions),
        title: extractAdditionalOptions(colorBarOptions.titleOptions, defaults.titleOptions),
        bar: extractAdditionalOptions(colorBarOptions.barOptions, defaults.barOptions)
      }
    };
  }
  function drawColorBar(g, settings, size = {}) {
    const { linkOptions, colorBarOptions, horizontal, hasNormal, hasInverted, additional } = settings;
    const { labelOptions, titleOptions, barOptions } = colorBarOptions;
    const labelsBelow = horizontal && (size.labels ?? colorBarOptions.labels) === "below";
    const width = parseFloat(size.width ?? colorBarOptions.width ?? (horizontal ? labelsBelow ? 120 : 80 : 10));
    const height = parseFloat(size.height ?? colorBarOptions.height ?? (horizontal ? 8 : 60));
    const labelFont = size.fontSize ?? labelOptions.fontSize;
    const titleFont = size.fontSize ?? titleOptions.fontSize;
    const fontFamily = size.fontFamily;
    const stroke = size.outline && (!barOptions.stroke || barOptions.stroke === "none") ? size.outline : barOptions.stroke;
    const strokeWidth = stroke === barOptions.stroke ? barOptions.strokeWidth : 0.5;
    const defs = g.append("defs");
    const minValue = linkOptions.vmin;
    const maxValue = linkOptions.vmax;
    const drawBar = (baseId, color2, x2, y) => {
      const gradientId = getUniqueId(baseId);
      const gradient = defs.append("linearGradient").attr("id", gradientId).attr("gradientTransform", horizontal ? null : "rotate(90)");
      const colorScale = linkColorScale(linkOptions.lowColor, color2, minValue, maxValue);
      gradient.append("stop").attr("offset", "0%").attr("stop-color", colorScale(horizontal ? minValue : maxValue));
      gradient.append("stop").attr("offset", "100%").attr("stop-color", colorScale(horizontal ? maxValue : minValue));
      g.append("rect").attr("x", x2).attr("y", y).attr("width", width).attr("height", height).style("fill", `url(#${gradientId})`).style("stroke", stroke).style("stroke-width", strokeWidth).style("opacity", barOptions.opacity).each(function() {
        setStyleFromOptions(d3.select(this), additional.bar);
      });
    };
    const addText = (text, options, fontSize, style) => g.append("text").attr("font-size", fontSize).style("font-family", fontFamily ?? options.fontFamily).text(text).each(function() {
      setStyleFromOptions(d3.select(this), style);
    });
    if (!horizontal) {
      if (colorBarOptions.title !== false) {
        addText(linkOptions.measure, titleOptions, titleFont, additional.title).attr("x", width + titleOptions.xOffset).attr("y", height / 2 + titleOptions.yOffset + 10).attr("transform", `rotate(-90, ${width + titleOptions.xOffset}, ${height / 2 + titleOptions.yOffset})`).attr("alignment-baseline", "bottom").attr("text-anchor", "middle");
      }
      if (hasNormal) {
        drawBar("linear-gradient", linkOptions.normalColor, 0, 0);
      }
      if (hasInverted) {
        drawBar("reverse-gradient", linkOptions.invertedColor, hasNormal ? -width : 0, 0);
      }
      addText(maxValue + "%", labelOptions, labelFont, additional.label).attr("x", width + labelOptions.xOffset).attr("y", -labelOptions.yOffset).attr("alignment-baseline", "middle");
      addText(minValue + "%", labelOptions, labelFont, additional.label).attr("x", width + labelOptions.xOffset).attr("y", height + labelOptions.yOffset).attr("alignment-baseline", "middle");
      return { width: (hasNormal && hasInverted ? 2 : 1) * width, height };
    }
    const barsHeight = (hasNormal && hasInverted ? 2 : 1) * height;
    const addTitle = () => addText(linkOptions.measure, titleOptions, titleFont, {
      fontWeight: "bold",
      ...additional.title
    });
    const drawBars = (x2, y) => {
      if (hasNormal) {
        drawBar("linear-gradient", linkOptions.normalColor, x2, y);
      }
      if (hasInverted) {
        drawBar("reverse-gradient", linkOptions.invertedColor, x2, y + (hasNormal ? height : 0));
      }
    };
    if (labelsBelow) {
      let top = 0;
      if (colorBarOptions.title !== false) {
        const title2 = addTitle().attr("x", 0).attr("y", titleOptions.yOffset).attr("dominant-baseline", "hanging");
        top = title2.node().getBBox().height + 3;
      }
      drawBars(0, top);
      const scale2 = d3.scaleLinear().domain([minValue, maxValue]).range([0, width]);
      const tickLength = 3;
      const tickColor = stroke && stroke !== "none" ? stroke : "#555555";
      const textWidth = (text) => {
        const node = addText(text, labelOptions, labelFont, additional.label);
        const textLength = node.node().getComputedTextLength();
        node.remove();
        return textLength;
      };
      const fits = (values) => values.length > 1 && values.every((value, i) => i === 0 || scale2(value) - scale2(values[i - 1]) > (textWidth(values[i - 1] + "%") + textWidth(value + "%")) / 2 + 4);
      const ticks = [5, 4, 3, 2].map((count) => scale2.ticks(count)).find(fits) ?? [minValue, maxValue];
      ticks.forEach((value) => {
        g.append("line").attr("x1", scale2(value)).attr("x2", scale2(value)).attr("y1", top + barsHeight).attr("y2", top + barsHeight + tickLength).style("stroke", tickColor).style("stroke-width", 0.5);
        addText(value + "%", labelOptions, labelFont, additional.label).attr("x", scale2(value)).attr("y", top + barsHeight + tickLength + 1 + labelOptions.yOffset).attr("dominant-baseline", "hanging").attr("text-anchor", "middle");
      });
      return { width, height: barsHeight };
    }
    const middle = barsHeight / 2;
    const gap = 4;
    let x = 0;
    const placeOnLine = (node, yOffset) => {
      node.attr("x", x).attr("y", middle + yOffset).attr("dominant-baseline", "central");
      x += node.node().getComputedTextLength() + gap;
    };
    if (colorBarOptions.title !== false) {
      placeOnLine(addTitle(), titleOptions.yOffset);
      x += gap;
    }
    placeOnLine(addText(minValue + "%", labelOptions, labelFont, additional.label), labelOptions.yOffset);
    drawBars(x, 0);
    x += width + gap;
    placeOnLine(addText(maxValue + "%", labelOptions, labelFont, additional.label), labelOptions.yOffset);
    return { width, height: barsHeight };
  }
  function makeColorBar(graphContainer, links2, themeOptions = {}, inLegend = false) {
    const settings = colorBarSettings(links2, themeOptions);
    if (!settings) {
      return null;
    }
    const { linkOptions, colorBarOptions } = settings;
    if (colorBarOptions.position === "legend" && inLegend) {
      return null;
    }
    const position = colorBarOptions.position === "legend" ? "bottomright" : colorBarOptions.position;
    const horizontal = colorBarOptions.orientation === "horizontal";
    const graphRect = graphContainer.node().getBoundingClientRect();
    const svgContainer = graphContainer.insert("div", ":first-child").style("position", "relative").style("width", "100%").style("height", "100%");
    const svg = svgContainer.append("svg").attr("width", graphRect.width).attr("height", graphRect.height).classed("color-bar", true).attr("data-component", "links").style("position", "absolute");
    const g = svg.append("g").attr("transform", `translate(${linkOptions.margin.left}, ${linkOptions.margin.top})`);
    const contentWidth = graphRect.width - linkOptions.margin.left - linkOptions.margin.right;
    const contentHeight = graphRect.height - linkOptions.margin.top - linkOptions.margin.bottom;
    const top = position === "topleft" || position === "topright";
    const left = position === "topleft" || position === "bottomleft";
    const x = colorBarOptions.x ?? 0;
    const y = colorBarOptions.y ?? (top || horizontal ? 0 : 24);
    const bar = g.append("g");
    const { width, height } = drawColorBar(bar, { ...settings, horizontal });
    if (position === "bottomright" && !horizontal) {
      bar.attr("transform", `translate(${contentWidth - width - x}, ${contentHeight - height - y})`);
    } else {
      const box = bar.node().getBBox();
      const tx = left ? x - box.x : contentWidth - x - (box.x + box.width);
      const ty = top ? y - box.y : contentHeight - y - (box.y + box.height);
      bar.attr("transform", `translate(${tx}, ${ty})`);
    }
    return svg;
  }

  // src/lib/components/legend.js
  container.prototype.legendData = function(data) {
    this.data = [...new Set(data)];
    return this;
  };
  container.prototype.legend = function(group, show = true, parentId = null, options = {}, colorBar = null) {
    if (!show) {
      return this;
    }
    const defaultOptions = {
      x: 0,
      y: 0,
      width: null,
      // Default width set to null
      orientation: "horizontal",
      align: "left",
      adjustHeight: true,
      order: [],
      cornerRadius: 0,
      groupStyle: {},
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
    const combinedOptions = mergeOptions.call(this, defaultOptions, "legendOptions", options);
    const { x, y, width, orientation, align: align2, adjustHeight, order: order2, cornerRadius, legendOptions, legendTextOptions, groupStyle, itemStyle = [], editable = false } = combinedOptions;
    const additionalLegendOptions = extractAdditionalOptions(legendOptions, defaultOptions.legendOptions);
    const additionalLegendTextOptions = extractAdditionalOptions(legendTextOptions, defaultOptions.legendTextOptions);
    const svgLegend = this.svg;
    const parentWidth = computeSize(width, svgLegend.node().getBoundingClientRect().width) || svgLegend.node().getBoundingClientRect().width;
    let uniqueGroups = [...new Set(this.data.map((d) => d[group]))];
    uniqueGroups = uniqueGroups.filter((color2) => color2 !== null);
    const colorScale = getColorScale(legendOptions.colorScheme, legendOptions.customColors, uniqueGroups);
    const patternScale = getPatternScale(this.svg, legendOptions.pattern, uniqueGroups, colorScale, "legend");
    const specialGroups = ["Other", "No Hit", "Unknown"];
    const regularGroups = uniqueGroups.filter((group2) => !specialGroups.includes(group2));
    const specialGroupsInData = specialGroups.filter((group2) => uniqueGroups.includes(group2));
    uniqueGroups = [...regularGroups, ...specialGroupsInData];
    if (order2 && order2.length > 0) {
      uniqueGroups = order2.filter((item) => uniqueGroups.includes(item)).concat(uniqueGroups.filter((item) => !order2.includes(item)));
    }
    if (!uniqueGroups.length) {
      console.error(`Error: No labels provided and the group "${group}" does not exist in the data.`);
      return;
    }
    const legendSize = parseFloat(legendTextOptions.fontSize);
    const legendPadding = legendSize / 2;
    let currentX = x;
    let currentY = y;
    const itemExtents = [];
    var g = this.svg.append("g").attr("data-component", "legend").attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);
    const barSide = colorBar?.colorBarOptions.legendSide === "right" ? "right" : "left";
    const addColorBar = () => {
      const barItem = g.append("g").attr("class", "legend-color-bar").attr("data-component", "links");
      const bar = barItem.append("g");
      const both = colorBar.hasNormal && colorBar.hasInverted;
      drawColorBar(bar, colorBar, {
        height: colorBar.colorBarOptions.height ?? legendSize / (both ? 2 : 1),
        fontSize: legendTextOptions.fontSize,
        fontFamily: legendTextOptions.fontFamily,
        outline: "#bdbdbd",
        labels: "inline"
      });
      const box = bar.node().getBBox();
      const gap = 2 * legendSize + legendPadding;
      if (orientation === "horizontal" && barSide === "right" && currentX > x) {
        const availableWidth = this.width - this.margin.left - this.margin.right;
        if (currentX + gap + box.width > availableWidth) {
          currentX = x;
          currentY += legendSize + legendPadding;
        } else {
          currentX += gap;
        }
      }
      bar.attr("transform", `translate(${currentX - box.x}, ${currentY + legendSize / 2 - (box.y + box.height / 2)})`);
      itemExtents.push({ node: barItem.node(), row: currentY, start: currentX, end: currentX + box.width });
      if (orientation === "horizontal") {
        currentX += box.width + gap + legendPadding;
      } else {
        currentY += Math.max(legendSize, box.height) + legendPadding;
      }
    };
    if (colorBar && barSide === "left") {
      addColorBar();
    }
    g.selectAll(".legend").data(uniqueGroups).enter().append("g").attr("class", "legend").each((d, i, nodes) => {
      const legendGroup = d3.select(nodes[i]);
      const { marker: markerStyle = {}, text: groupText, ...textStyle } = groupStyle[d] ?? {};
      const { group: _labelGroup, part: _labelPart, text, ...labelStyle } = itemStyle.find((s) => s.group === d && s.part === "label") ?? {};
      const { group: _markerGroup, part: _markerPart, ...ownMarkerStyle } = itemStyle.find((s) => s.group === d && s.part === "marker") ?? {};
      const textLabel = legendGroup.append("text").attr("class", "legend-label").attr("data-part", "label").attr("id", `legend-label-${i}`).attr("dy", legendTextOptions.dy).style("text-anchor", legendTextOptions.textAnchor).style("font-size", legendTextOptions.fontSize).style("font-family", legendTextOptions.fontFamily).style("cursor", legendTextOptions.cursor).text(text ?? groupText ?? d).each(function() {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalLegendTextOptions);
        setStyleFromOptions(currentElement, textStyle);
        setStyleFromOptions(currentElement, labelStyle);
      });
      const textLength = textLabel.node().getComputedTextLength();
      const availableWidth = this.width - this.margin.left - this.margin.right;
      const totalItemWidth = textLength + legendSize + 2 * legendPadding;
      if (currentX + totalItemWidth > availableWidth) {
        currentX = x;
        currentY += legendSize + legendPadding;
      }
      textLabel.attr("x", currentX + legendSize + legendPadding).attr("y", currentY + legendSize / 2);
      const rect = legendGroup.append("rect").attr("class", "legend-marker").attr("data-part", "marker").attr("id", `legend-marker-${i}`).style("cursor", legendOptions.cursor).attr("x", currentX).attr("y", currentY).attr("width", legendSize).attr("height", legendSize).attr("rx", cornerRadius).style("fill", patternScale.fill(d)).style("stroke", patternScale.outline ?? (isWhite(patternScale.fill(d)) ? "#bdbdbd" : null)).each(() => patternScale.align(d, currentX, currentY)).each(function() {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalLegendOptions);
        setStyleFromOptions(currentElement, markerStyle);
        setStyleFromOptions(currentElement, ownMarkerStyle);
      });
      applyMove(textLabel, labelStyle);
      applyMove(rect, ownMarkerStyle);
      itemExtents.push({
        node: nodes[i],
        row: currentY,
        start: currentX,
        end: currentX + legendSize + legendPadding + textLength
      });
      if (orientation === "horizontal") {
        currentX += textLength + legendSize + 2 * legendPadding;
      } else {
        currentY += legendSize + legendPadding;
      }
    }).on("mouseover", (event, d) => {
      const element = d3.select(event.currentTarget);
      element.classed("hovered", true);
    }).on("mouseout", (event, d) => {
      const element = d3.select(event.currentTarget);
      element.classed("hovered", false);
    }).on("click", (event, d) => {
      if (event.detail > 1 || editable && event.bubbles) return;
      const element = d3.select(event.currentTarget);
      if (element.classed("unselected")) {
        element.classed("unselected", false);
      } else {
        element.classed("unselected", true);
      }
      const unselectedLegend = g.selectAll(".unselected").data();
      const unselectedRowIds = this.data.filter((item) => unselectedLegend.includes(item[group])).map((item) => item.rowID);
      if (parentId && typeof parentId === "string") {
        d3.select("#" + parentId).selectAll("[rowID]").each(function() {
          const currentRowID = +d3.select(this).attr("rowID");
          if (unselectedRowIds.includes(currentRowID)) {
            d3.select(this).style("display", "none");
          } else {
            d3.select(this).style("display", "");
          }
        });
      } else {
        d3.selectAll("[rowID]").each(function() {
          const currentRowID = +d3.select(this).attr("rowID");
          if (unselectedRowIds.includes(currentRowID)) {
            d3.select(this).style("display", "none");
          } else {
            d3.select(this).style("display", "");
          }
        });
      }
    });
    if (colorBar && barSide === "right") {
      addColorBar();
    }
    const alignShare = { center: 0.5, right: 1 }[align2] ?? 0;
    if (alignShare > 0) {
      const availableWidth = this.width - this.margin.left - this.margin.right;
      const rowOf = (item) => orientation === "horizontal" ? item.row : 0;
      const rowWidths = d3.rollup(
        itemExtents,
        (items) => d3.max(items, (item) => item.end) - d3.min(items, (item) => item.start),
        rowOf
      );
      itemExtents.forEach((item) => {
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

  // src/lib/components/links.js
  container.prototype.links = function(links2, clusterKey, options = {}) {
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
        textAnchor: "middle"
      },
      labelAdjustmentOptions: {
        rotation: -65,
        offsetX: 0,
        offsetY: 0,
        dx: "0em",
        dy: "0em"
      }
    };
    if (!links2 || links2.length === 0) {
      return this;
    }
    const combinedOptions = mergeOptions.call(this, defaultOptions, "linkOptions", options);
    const { x, y, cursor, fontSize, fontStyle, fontFamily, textAnchor, showLinks, label, measure, labelStyle, labelAdjustmentOptions, linkWidth } = combinedOptions;
    const additionalOptions = extractAdditionalOptions(options, defaultOptions);
    const additionalOptionsLabelStyle = extractAdditionalOptions(labelStyle, defaultOptions.labelStyle);
    var group = this.svg.append("g").attr("data-component", "links").attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);
    const hasReverseStrand = this.data.some((item) => item.strand === "reverse");
    const clusterStrandSpacing = hasReverseStrand ? this.geneStrandSpacing * 1 : 0;
    if (showLinks) {
      links2.forEach((link) => {
        const widthFactor = (1 - linkWidth) / 2;
        const adjustedStart1 = link.start1 + (link.end1 - link.start1) * widthFactor;
        const adjustedEnd1 = link.end1 - (link.end1 - link.start1) * widthFactor;
        const adjustedStart2 = link.start2 + (link.end2 - link.start2) * widthFactor;
        const adjustedEnd2 = link.end2 - (link.end2 - link.start2) * widthFactor;
        if (link.cluster1 === clusterKey) {
          const groupColor1 = link.groupColor ? this.genesColorScale(link.groupColor) : this.genesColorScale(link.group1);
          group.append("circle").attr("cx", this.xScale(adjustedStart1)).attr("cy", this.yScale(y) + clusterStrandSpacing).attr("position", link.start1).attr("r", 1).attr("cluster", clusterKey).attr("visibility", "hidden").attr("groupColor", groupColor1).attr("linkID", link.linkID).attr("class", "link-marker");
          group.append("circle").attr("cx", this.xScale(adjustedEnd1)).attr("cy", this.yScale(y) + clusterStrandSpacing).attr("position", link.end1).attr("class", "link-marker").attr("groupColor", groupColor1).attr("visibility", "hidden").attr("linkID", link.linkID).attr("cluster", clusterKey).attr("r", 1);
        }
        if (link.cluster2 === clusterKey) {
          const groupColor2 = link.groupColor ? this.genesColorScale(link.groupColor) : this.genesColorScale(link.group2);
          group.append("circle").attr("cx", this.xScale(adjustedStart2)).attr("cy", this.yScale(y) - clusterStrandSpacing).attr("position", link.start2).attr("class", "link-marker").attr("cluster", clusterKey).attr("visibility", "hidden").attr("groupColor", groupColor2).attr("linkID", link.linkID).attr("r", 1);
          group.append("circle").attr("cx", this.xScale(adjustedEnd2)).attr("cy", this.yScale(y) - clusterStrandSpacing).attr("position", link.end2).attr("class", "link-marker").attr("linkID", link.linkID).attr("cluster", clusterKey).attr("visibility", "hidden").attr("groupColor", groupColor2).attr("r", 1);
        }
      });
    }
    if (label && combinedOptions.labelPosition !== "link") {
      const labelAdjuster = createLabelAdjuster(this, ".link-text");
      const labelled = this.svg.selectAll(".link-text").nodes();
      const labelledRows = new Set(labelled.map((node) => node.getAttribute("rowID")));
      const labelData = this.data.filter(
        (d) => !labelledRows.has(`${d["rowID"]}`) && d[measure] != null && d.BlastP != "No Hit" && (d.BlastP === void 0 || d.BlastP != d.protein_id && d.BlastP) && (options.value1 === void 0 && options.value2 === void 0 || options.value1 && options.value1.includes(d[options.group]) || options.value2 && options.value2.includes(d[options.group]))
      );
      group.selectAll(".link-text").data(labelData).enter().append("text").attr("id", (d, i) => `cluster-${sanitizeId(d.cluster)}-link-label-${labelled.length + i}`).attr("rowID", (d, i) => `${d["rowID"]}`).attr("class", "link-text").attr("x", (d, i) => this.xScale((d.start + d.end) / 2)).attr("y", (d, i) => this.yScale(y) - this.markerHeight / 1.5 - clusterStrandSpacing).text((d) => `${parseFloat(d[measure].toFixed(1))}%`).style("font-size", labelStyle.fontSize).style("font-style", labelStyle.fontStyle).style("font-family", labelStyle.fontFamily).style("cursor", labelStyle.cursor).attr("text-anchor", labelStyle.textAnchor).attr("cluster", clusterKey).each(function(d, i) {
        const currentElement = d3.select(this);
        labelAdjuster.adjust(this, labelAdjustmentOptions);
        setStyleFromOptions(currentElement, additionalOptionsLabelStyle);
        if (changesSize(additionalOptionsLabelStyle)) labelAdjuster.refresh(this);
      });
    }
    return this;
  };

  // src/lib/components/trackMouse.js
  container.prototype.trackMouse = function(track = true, options = {}) {
    if (!track) {
      return this;
    }
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
      zIndex: 1e3
    };
    const { cursor, show, ...tooltipStyle } = mergeOptions.call(this, defaultOptions, "trackMouseOptions", options);
    this.svg.style("cursor", cursor);
    this.svg.selectAll("*").each(function() {
      this.style.cssText += `cursor: ${cursor} !important;`;
    });
    const host = this.svg.node().closest(".html-widget") || document.body;
    const tooltip2 = d3.select(host).append("div").attr("class", "coordinate-tooltip").style("pointer-events", "none").style("position", "absolute").style("visibility", "hidden");
    setStyleFromOptions(tooltip2, tooltipStyle);
    const xScale = d3.scaleLinear().domain([0 + this.margin.left, this.width - this.margin.right]).range([0, 100]);
    const yScale = d3.scaleLinear().domain([this.height - this.margin.bottom, 0 + this.margin.top]).range([0, 100]);
    const linearScale = d3.scaleLinear().domain([0 - this.margin.left, this.width]).range([0, 100]);
    this.svg.on("mousemove", (event) => {
      const [x, y] = d3.pointer(event);
      const adjustedX = x - this.margin.left;
      const loci = Math.round(this.xScale.invert(adjustedX));
      const format = d3.format(",");
      tooltip2.html(`x: ${format(loci)} <br>y: ${yScale(y).toFixed(1)}`).style("visibility", "visible").style("left", d3.pointer(event, host)[0] + 10 + "px").style("top", d3.pointer(event, host)[1] - 10 + "px");
    });
    this.svg.on("mouseout", () => {
      tooltip2.style("visibility", "hidden");
    });
    return this;
  };

  // src/lib/components/zoom.js
  var zoomClipped = ["sequence", "genes", "transcript", "links", "coordinates", "labels", "annotations"];
  container.prototype.clipZoom = function() {
    if (!this.zoom) return this;
    const svg = this.svg.node();
    const parts = Array.from(svg.children).filter((node) => zoomClipped.includes(node.getAttribute("data-component")));
    if (!parts.length) return this;
    const plotWidth = this.width - this.margin.left - this.margin.right;
    const clip = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    clip.setAttribute("class", "geneviewer-zoom-clip");
    clip.setAttribute("x", this.margin.left);
    clip.setAttribute("y", 0);
    clip.setAttribute("width", plotWidth);
    clip.setAttribute("height", this.height);
    clip.setAttribute("viewBox", `${this.margin.left} 0 ${plotWidth} ${this.height}`);
    clip.setAttribute("preserveAspectRatio", "none");
    clip.setAttribute("overflow", "hidden");
    svg.insertBefore(clip, parts[0]);
    parts.forEach((node) => clip.appendChild(node));
    return this;
  };

  // src/lib/components/annotations.js
  container.prototype.addAnnotations = function(annotations) {
    if (!annotations || annotations.length === 0) {
      return this;
    }
    var g = this.svg.append("g").attr("data-component", "annotations").attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);
    annotations.forEach((annotation2) => {
      this.createAnnotation(g, annotation2);
    });
    return this;
  };
  container.prototype.createAnnotation = function(group, options) {
    switch (options.type) {
      case "text":
        this.createTextAnnotation(group, options);
        break;
      case "line":
        this.createLineAnnotation(group, options);
        break;
      case "textMarker":
        this.createTextMarkerAnnotation(group, options);
        break;
      case "arrow":
        this.createArrowAnnotation(group, options);
        break;
      case "promoter":
        this.createPromoterAnnotation(group, options);
        break;
      case "terminator":
        this.createTerminatorAnnotation(group, options);
        break;
      case "rectangle":
        this.createRectangleAnnotation(group, options);
        break;
      case "symbol":
        this.createSymbolAnnotation(group, options);
        break;
      case "arc":
        this.createCurveAnnotation(group, options);
        break;
      default:
        warnOnce("Unsupported annotation type: " + options.type);
    }
  };
  container.prototype.createTextAnnotation = function(group, options) {
    const defaultOptions = {
      x: 0,
      y: 60,
      text: "",
      style: {
        fontSize: "10px",
        fontStyle: "normal",
        fontWeight: "normal",
        textDecoration: "none",
        fontFamily: "sans-serif",
        cursor: "default"
      }
    };
    const combinedOptions = mergeOptions.call(this, defaultOptions, "textAnnotationOptions", options);
    let { x, y, text, style } = combinedOptions;
    if (!Array.isArray(x)) x = [x];
    if (!Array.isArray(y)) y = [y];
    if (!Array.isArray(text)) text = [text];
    const additionalOptionsStyle = extractAdditionalOptions(style, defaultOptions.style);
    var group = this.svg.append("g").attr("data-component", "annotations").attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);
    for (let i = 0; i < Math.max(x.length, y.length, text.length); i++) {
      const currentX = x[Math.min(i, x.length - 1)];
      const currentY = y[Math.min(i, y.length - 1)];
      const currentText = text[Math.min(i, text.length - 1)];
      group.append("text").attr("x", this.xScale(currentX)).attr("y", this.yScale(currentY)).style("font-size", style.fontSize).style("font-style", style.fontStyle).style("font-weight", style.fontWeight).style("text-decoration", style.textDecoration).style("font-family", style.fontFamily).style("cursor", style.cursor).each(function() {
        const currentElement = d3.select(this);
        parseAndStyleText(currentText, currentElement, style);
        setStyleFromOptions(currentElement, additionalOptionsStyle);
      });
    }
  };
  container.prototype.createLineAnnotation = function(group, options) {
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
    const combinedOptions = mergeOptions.call(this, defaultOptions, "lineAnnotationOptions", options);
    let { x1, y1, x2, y2, style } = combinedOptions;
    if (!Array.isArray(x1)) x1 = [x1];
    if (!Array.isArray(y1)) y1 = [y1];
    if (!Array.isArray(x2)) x2 = [x2];
    if (!Array.isArray(y2)) y2 = [y2];
    const additionalOptionsStyle = extractAdditionalOptions(style, defaultOptions.style);
    var group = this.svg.append("g").attr("data-component", "annotations").attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);
    for (let i = 0; i < Math.max(x1.length, y1.length, x2.length, y2.length); i++) {
      const currentX1 = x1[Math.min(i, x1.length - 1)];
      const currentY1 = y1[Math.min(i, y1.length - 1)];
      const currentX2 = x2[Math.min(i, x2.length - 1)];
      const currentY2 = y2[Math.min(i, y2.length - 1)];
      group.append("line").attr("x1", this.xScale(currentX1)).attr("y1", this.yScale(currentY1)).attr("x2", this.xScale(currentX2)).attr("y2", this.yScale(currentY2)).style("stroke", style.stroke).style("stroke-width", style.strokeWidth).each(function() {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsStyle);
      });
    }
  };
  container.prototype.createArrowAnnotation = function(group, options) {
    const defaultOptions = {
      x1: 1,
      y1: 70,
      x2: 1,
      y2: 50,
      arrowSize: 8,
      arrowStyle: {
        fill: "black"
      },
      lineStyle: {
        stroke: "black",
        strokeWidth: 1
      }
    };
    const combinedOptions = mergeOptions.call(this, defaultOptions, "arrowAnnotationOptions", options);
    let { x1, y1, x2, y2, arrowSize, arrowStyle, lineStyle } = combinedOptions;
    if (!Array.isArray(x1)) x1 = [x1];
    if (!Array.isArray(y1)) y1 = [y1];
    if (!Array.isArray(x2)) x2 = [x2];
    if (!Array.isArray(y2)) y2 = [y2];
    if (!Array.isArray(arrowSize)) arrowSize = [arrowSize];
    const additionalOptionsLine = extractAdditionalOptions(lineStyle, defaultOptions.lineStyle);
    const additionalOptionsArrow = extractAdditionalOptions(arrowStyle, defaultOptions.arrowStyle);
    var group = this.svg.append("g").attr("data-component", "annotations").attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);
    for (let i = 0; i < Math.max(x1.length, y1.length, x2.length, y2.length, arrowSize.length); i++) {
      const currentX1 = x1[Math.min(i, x1.length - 1)];
      const currentY1 = y1[Math.min(i, y1.length - 1)];
      const currentX2 = x2[Math.min(i, x2.length - 1)];
      const currentY2 = y2[Math.min(i, y2.length - 1)];
      const currentArrowSize = arrowSize[Math.min(i, arrowSize.length - 1)];
      const markerId = getUniqueId("arrowhead");
      this.svg.append("defs").append("marker").attr("id", markerId).attr("viewBox", "-0 -5 10 10").attr("refX", 5).attr("refY", 0).attr("orient", "auto").attr("markerWidth", currentArrowSize).attr("markerHeight", currentArrowSize).attr("xoverflow", "visible").append("path").attr("d", "M 0,-5 L 10 ,0 L 0,5").attr("fill", arrowStyle.fill).each(function() {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsArrow);
      });
      group.append("line").attr("x1", this.xScale(currentX1)).attr("y1", this.yScale(currentY1)).attr("x2", this.xScale(currentX2)).attr("y2", this.yScale(currentY2 + currentArrowSize / 2)).attr("marker-end", `url(#${markerId})`).style("stroke", lineStyle.stroke).style("stroke-width", lineStyle.strokeWidth).each(function() {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsLine);
      });
    }
  };
  container.prototype.createTextMarkerAnnotation = function(group, options) {
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
        fill: "black"
      },
      lineStyle: {
        stroke: "black",
        strokeWidth: 1
      }
    };
    const combinedOptions = mergeOptions.call(this, defaultOptions, "textMarkerAnnotationOptions", options);
    let { textStyle, arrowStyle, lineStyle } = combinedOptions;
    const keys = ["x1", "y1", "x2", "y2", "position", "text", "labelX", "labelY", "showArrow", "arrowSize"];
    keys.forEach((key) => {
      if (!Array.isArray(combinedOptions[key])) {
        combinedOptions[key] = [combinedOptions[key]];
      }
    });
    const additionalOptionsLine = extractAdditionalOptions(lineStyle, defaultOptions.lineStyle);
    const additionalOptionsArrow = extractAdditionalOptions(arrowStyle, defaultOptions.arrowStyle);
    const maxLength = Math.max(...keys.map((key) => combinedOptions[key].length));
    for (let i = 0; i < maxLength; i++) {
      let currentValues = {};
      keys.forEach((key) => {
        currentValues[key] = combinedOptions[key][i % combinedOptions[key].length];
      });
      let offsetY2 = currentValues.showArrow ? currentValues.arrowSize / 2 : 0;
      var group = this.svg.insert("g", ":first-child").attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);
      const currentX1 = Math.round(this.xScale(currentValues.x1 !== null ? currentValues.x1 : currentValues.position));
      const currentX2 = Math.round(this.xScale(currentValues.x2 !== null ? currentValues.x2 : currentValues.position));
      const line = group.append("line").attr("x1", currentX1).attr("y1", this.yScale(currentValues.y1)).attr("x2", currentX2).attr("y2", this.yScale(currentValues.y2 + offsetY2)).style("stroke", lineStyle.stroke).style("stroke-width", lineStyle.strokeWidth).each(function() {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsLine);
      });
      const markerId = getUniqueId("arrowhead-marker");
      this.svg.append("defs").append("marker").attr("id", markerId).attr("viewBox", "-0 -5 10 10").attr("refX", 5).attr("refY", 0).attr("orient", "auto").attr("markerWidth", currentValues.arrowSize).attr("markerHeight", currentValues.arrowSize).attr("xoverflow", "visible").append("path").attr("d", "M 0,-5 L 10 ,0 L 0,5").attr("fill", arrowStyle.fill).each(function() {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsArrow);
      });
      if (currentValues.showArrow) {
        line.attr("marker-end", `url(#${markerId})`);
      }
      if (currentValues.text) {
        group.append("text").attr("x", this.xScale(currentValues.x1 !== null ? currentValues.x1 : currentValues.position) + currentValues.labelX).attr("y", this.yScale(currentValues.y1) - 5 - currentValues.labelY).style("text-anchor", textStyle.textAnchor).style("font-size", textStyle.fontSize).style("font-family", textStyle.fontFamily).style("fill", textStyle.fill).each(function() {
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
    const combinedOptions = mergeOptions.call(this, defaultOptions, "symbolAnnotationOptions", options);
    let { x, y, size, symbol, style, rotation } = combinedOptions;
    if (!Array.isArray(x)) x = [x];
    if (!Array.isArray(y)) y = [y];
    if (!Array.isArray(symbol)) symbol = [symbol];
    if (!Array.isArray(rotation)) rotation = [rotation];
    if (!Array.isArray(size)) size = [size];
    var group = this.svg.append("g").attr("data-component", "annotations").attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);
    const additionalOptionsStyle = extractAdditionalOptions(style, defaultOptions.style);
    const symbolTypes = {
      "circle": d3.symbolCircle,
      "cross": d3.symbolCross,
      "diamond": d3.symbolDiamond,
      "square": d3.symbolSquare,
      "star": d3.symbolStar,
      "triangle": d3.symbolTriangle,
      "wye": d3.symbolWye
    };
    for (let i = 0; i < Math.max(x.length, y.length, symbol.length, rotation.length); i++) {
      const currentX = x[Math.min(i, x.length - 1)];
      const currentY = y[Math.min(i, y.length - 1)];
      const currentSymbol = symbol[Math.min(i, symbol.length - 1)];
      const currentRotation = rotation[Math.min(i, rotation.length - 1)];
      const currentSize = size[Math.min(i, size.length - 1)];
      if (!symbolTypes[currentSymbol]) {
        console.error(`Unsupported symbol type: ${currentSymbol}`);
        continue;
      }
      const d3Symbol = d3.symbol().type(symbolTypes[currentSymbol]).size(currentSize);
      group.append("path").attr("d", d3Symbol).attr("transform", `translate(${this.xScale(currentX)}, ${this.yScale(currentY)}) rotate(${currentRotation})`).style("fill", style.fill).style("stroke", style.stroke).style("stroke-width", style.strokeWidth).each(function() {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsStyle);
      });
    }
    return group;
  };
  container.prototype.createRectangleAnnotation = function(group, options) {
    const defaultOptions = {
      position: [[[9300, 20], [9400, 50]]],
      // Default as a nested array for multiple rectangles
      rotation: [0],
      // Array to support multiple rotations
      style: {
        fill: "#0000",
        stroke: "black",
        strokeWidth: 2
      }
    };
    const combinedOptions = mergeOptions.call(this, defaultOptions, "rectangleAnnotationOptions", options);
    let { position, style, rotation } = combinedOptions;
    if (!Array.isArray(position[0][0])) {
      position = [position];
    }
    if (!Array.isArray(rotation)) rotation = [rotation];
    const additionalOptionsStyle = extractAdditionalOptions(style, defaultOptions.style);
    var group = this.svg.append("g").attr("data-component", "annotations").attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);
    const numRects = Math.max(position.length, rotation.length);
    for (let i = 0; i < numRects; i++) {
      const currentPos = position[Math.min(i, position.length - 1)];
      const currentRot = rotation[Math.min(i, rotation.length - 1)];
      const x1 = Math.min(currentPos[0][0], currentPos[1][0]);
      const x2 = Math.max(currentPos[0][0], currentPos[1][0]);
      const y1 = Math.max(currentPos[0][1], currentPos[1][1]);
      const y2 = Math.min(currentPos[0][1], currentPos[1][1]);
      const width = Math.abs(x1 - x2);
      const height = Math.abs(y2 - y1);
      group.append("rect").attr("x", this.xScale(x1)).attr("y", this.yScale(y1)).attr("width", this.xScale(x2) - this.xScale(x1)).attr("height", this.yScale(y2) - this.yScale(y1)).attr("transform", `rotate(${currentRot}, ${this.xScale(x1 + width / 2)}, ${this.yScale(y1 + height / 2)})`).style("fill", style.fill).style("stroke", style.stroke).style("stroke-width", style.strokeWidth).each(function() {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsStyle);
      });
    }
  };
  container.prototype.createPromoterAnnotation = function(group, options) {
    const defaultOptions = {
      x: 0,
      y: 50,
      direction: "forward",
      rotation: 0,
      scale: 1,
      style: {
        fill: "none",
        stroke: "black",
        strokeWidth: 1
      }
    };
    const combinedOptions = mergeOptions.call(this, defaultOptions, "promoterAnnotationOptions", options);
    let { x, y, direction, rotation, scale: scale2, style } = combinedOptions;
    const additionalOptionsStyle = extractAdditionalOptions(style, defaultOptions.style);
    if (!Array.isArray(x)) x = [x];
    if (!Array.isArray(y)) y = [y];
    if (!Array.isArray(direction)) direction = [direction];
    if (!Array.isArray(rotation)) rotation = [rotation];
    if (!Array.isArray(scale2)) scale2 = [scale2];
    const mirroredPath = "M -8 -17.5 L -13 -14 l 5 3.5 M -13 -14 L 0 -14 v 14";
    const customPath = "M 8 -17.5 L 13 -14 l -5 3.5 M 13 -14 H 0 v 14";
    var group = this.svg.append("g").attr("data-component", "annotations").attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);
    for (let i = 0; i < Math.max(x.length, y.length, direction.length, rotation.length, scale2.length); i++) {
      const currentX = x[Math.min(i, x.length - 1)];
      const currentY = y[Math.min(i, y.length - 1)];
      const currentDirection = direction[Math.min(i, direction.length - 1)];
      const currentRotation = rotation[Math.min(i, rotation.length - 1)];
      const currentScale = scale2[Math.min(i, scale2.length - 1)];
      const pathToUse = currentDirection === "forward" ? customPath : mirroredPath;
      const xPosition = this.xScale(currentX);
      group.append("path").attr("d", pathToUse).attr("transform", `translate(${xPosition}, ${this.yScale(currentY)}) scale(${currentScale}) rotate(${currentRotation})`).style("fill", style.fill).style("stroke", style.stroke).style("stroke-width", style.strokeWidth).each(function() {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsStyle);
      });
    }
  };
  container.prototype.createTerminatorAnnotation = function(group, options) {
    const defaultOptions = {
      x: 0,
      y: 50,
      direction: "forward",
      rotation: 0,
      scale: 1,
      style: {
        fill: "none",
        stroke: "black",
        strokeWidth: 1
      }
    };
    const combinedOptions = mergeOptions.call(this, defaultOptions, "terminatorAnnotationOptions", options);
    let { x, y, direction, rotation, scale: scale2, style } = combinedOptions;
    const additionalOptionsStyle = extractAdditionalOptions(style, defaultOptions.style);
    if (!Array.isArray(x)) x = [x];
    if (!Array.isArray(y)) y = [y];
    if (!Array.isArray(direction)) direction = [direction];
    if (!Array.isArray(rotation)) rotation = [rotation];
    if (!Array.isArray(scale2)) scale2 = [scale2];
    const customPath = "M -8 17.5 L -13 14 l 5 -3.5 M -13 14 H 0 v -14";
    const mirroredPath = "M 8 17.5 L 13 14 l -5 -3.5 M 13 14 L 0 14 v -14";
    var group = this.svg.append("g").attr("data-component", "annotations").attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);
    for (let i = 0; i < Math.max(x.length, y.length, direction.length, rotation.length, scale2.length); i++) {
      const currentX = x[Math.min(i, x.length - 1)];
      const currentY = y[Math.min(i, y.length - 1)];
      const currentDirection = direction[Math.min(i, direction.length - 1)];
      const currentRotation = rotation[Math.min(i, rotation.length - 1)];
      const currentScale = scale2[Math.min(i, scale2.length - 1)];
      const pathToUse = currentDirection === "forward" ? customPath : mirroredPath;
      const xPosition = this.xScale(currentX);
      group.append("path").attr("d", pathToUse).attr("transform", `translate(${xPosition}, ${this.yScale(currentY)}) scale(${currentScale}) rotate(${currentRotation})`).style("fill", style.fill).style("stroke", style.stroke).style("stroke-width", style.strokeWidth).each(function() {
        const currentElement = d3.select(this);
        setStyleFromOptions(currentElement, additionalOptionsStyle);
      });
    }
  };
  container.prototype.createCurveAnnotation = function(group, options) {
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
    const combinedOptions = mergeOptions.call(this, defaultOptions, "curveAnnotationOptions", options);
    const keys = ["x1", "y1", "x2", "y2", "midY", "stroke", "strokeWidth", "text", "labelX", "labelY"];
    keys.forEach((key) => {
      if (!Array.isArray(combinedOptions[key])) {
        combinedOptions[key] = [combinedOptions[key]];
      }
    });
    let { x1, y1, x2, y2, midY, stroke, strokeWidth, lineStyle, text, labelX, labelY, textStyle } = combinedOptions;
    var group = this.svg.append("g").attr("data-component", "annotations").attr("transform", `translate(${this.margin.left}, ${this.margin.top})`);
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
      const lineGenerator = d3.line().curve(d3.curveBasis).x((d) => this.xScale(d.x)).y((d) => this.yScale(d.y));
      const points = [
        { x: currentX1, y: currentY1 },
        { x: midX, y: currentMidY },
        { x: currentX2, y: currentY2 }
      ];
      const path = group.append("path").attr("d", lineGenerator(points)).style("stroke", currentStroke).style("stroke-width", currentStrokeWidth).style("fill", "none");
      setStyleFromOptions(path, extractAdditionalOptions(lineStyle, defaultOptions.lineStyle));
      const textElement = group.append("text").attr("x", this.xScale(midX) + currentLabelX).attr("y", this.yScale(currentMidY) - currentLabelY).text(currentText);
      setStyleFromOptions(textElement, textStyle);
    }
  };

  // src/lib/compile/frame.js
  var Frame = class _Frame {
    /**
     * @param {Object<string, Array<*>>} [columns] Columns by name, in order.
     */
    constructor(columns = {}) {
      this.columns = new Map(Object.entries(columns));
    }
    /**
     * A table from spec data: an array of rows or an object of columns.
     * @param {Array<Object>|Object<string, Array<*>>} data
     * @returns {Frame}
     */
    static from(data) {
      if (data instanceof _Frame) return data.copy();
      if (Array.isArray(data)) {
        const names = [];
        for (const row of data) {
          for (const name of Object.keys(row)) if (!names.includes(name)) names.push(name);
        }
        const frame2 = new _Frame();
        for (const name of names) frame2.columns.set(name, data.map((row) => row[name] === void 0 ? null : row[name]));
        return frame2;
      }
      const frame = new _Frame();
      for (const [name, values] of Object.entries(data || {})) {
        frame.columns.set(name, Array.isArray(values) ? values.slice() : [values]);
      }
      return frame;
    }
    /** @returns {Frame} */
    copy() {
      const frame = new _Frame();
      for (const [name, values] of this.columns) frame.columns.set(name, values.slice());
      return frame;
    }
    /** @returns {number} */
    get nrow() {
      const first = this.columns.values().next();
      return first.done ? 0 : first.value.length;
    }
    /** @returns {string[]} */
    get names() {
      return [...this.columns.keys()];
    }
    /**
     * @param {string} name
     * @returns {boolean}
     */
    has(name) {
      return this.columns.has(name);
    }
    /**
     * @param {string} name
     * @returns {Array<*>|undefined}
     */
    get(name) {
      return this.columns.get(name);
    }
    /**
     * Sets a column: in its place when it exists, otherwise last (R's
     * `data$name <- values`). A single value is recycled over the rows.
     * @param {string} name
     * @param {Array<*>|*} values
     * @returns {Frame} this
     */
    set(name, values) {
      const n = this.nrow;
      this.columns.set(name, Array.isArray(values) ? values : Array.from({ length: n }, () => values));
      return this;
    }
    /**
     * @param {string} name
     * @returns {Frame} this
     */
    drop(name) {
      this.columns.delete(name);
      return this;
    }
    /**
     * The rows at the given 0-based indices, in that order.
     * @param {number[]} indices
     * @returns {Frame}
     */
    rows(indices) {
      const frame = new _Frame();
      for (const [name, values] of this.columns) frame.columns.set(name, indices.map((i) => values[i]));
      return frame;
    }
    /**
     * The rows for which keep(i) is true.
     * @param {(i: number) => boolean} keep
     * @returns {Frame}
     */
    filter(keep) {
      const indices = [];
      for (let i = 0; i < this.nrow; i++) if (keep(i)) indices.push(i);
      return this.rows(indices);
    }
    /**
     * The columns with the given names, in that order.
     * @param {string[]} names
     * @returns {Frame}
     */
    select(names) {
      const frame = new _Frame();
      for (const name of names) if (this.columns.has(name)) frame.columns.set(name, this.columns.get(name).slice());
      return frame;
    }
    /**
     * Row i as an object.
     * @param {number} i
     * @returns {Object<string, *>}
     */
    row(i) {
      const row = {};
      for (const [name, values] of this.columns) row[name] = values[i];
      return row;
    }
    /** @returns {Object<string, Array<*>>} The columns, as htmlwidgets writes a data frame. */
    toJSON() {
      return Object.fromEntries(this.columns);
    }
  };

  // src/lib/compile/r.js
  function asCharacter(x) {
    if (x === null || x === void 0) return null;
    if (typeof x === "boolean") return x ? "TRUE" : "FALSE";
    if (typeof x !== "number") return String(x);
    if (Number.isNaN(x)) return "NaN";
    if (!Number.isFinite(x)) return x > 0 ? "Inf" : "-Inf";
    if (x === 0) return "0";
    const rounded = Number(x.toPrecision(15));
    const value = rounded !== x && Number.isInteger(x) ? x : rounded;
    const [mantissa, exponent] = Math.abs(value).toExponential().split("e");
    const digits = mantissa.replace(".", "");
    const e = Number(exponent);
    const sign = value < 0 ? "-" : "";
    let fixed;
    if (e >= 0) {
      fixed = digits.length > e + 1 ? `${digits.slice(0, e + 1)}.${digits.slice(e + 1)}` : digits + "0".repeat(e + 1 - digits.length);
    } else {
      fixed = `0.${"0".repeat(-e - 1)}${digits}`;
    }
    const scientific = `${mantissa}e${e < 0 ? "-" : "+"}${String(Math.abs(e)).padStart(2, "0")}`;
    return sign + (fixed.length <= scientific.length ? fixed : scientific);
  }
  function asNumeric(x) {
    if (x === null || x === void 0) return null;
    if (typeof x === "number") return x;
    if (typeof x === "boolean") return x ? 1 : 0;
    const text = String(x).trim();
    if (text === "" || !/^[-+]?(\d+\.?\d*|\.\d+)([eE][-+]?\d+)?$|^[-+]?(Inf|NaN)$/.test(text)) return null;
    return Number(text.replace("Inf", "Infinity"));
  }
  function recycleAt(x, i) {
    if (x === null || x === void 0) return void 0;
    if (Array.isArray(x)) {
      if (!x.length) return void 0;
      const value = x[i % x.length];
      return value !== null && typeof value === "object" ? [value] : value;
    }
    if (isNamedList(x)) {
      const keys = Object.keys(x);
      if (!keys.length) return void 0;
      const key = keys[i % keys.length];
      return { [key]: x[key] };
    }
    return x;
  }
  function arg(entry, name, fallback) {
    return Object.prototype.hasOwnProperty.call(entry, name) ? entry[name] : fallback;
  }
  function isNamedList(x) {
    return x !== null && typeof x === "object" && !Array.isArray(x);
  }
  function isEmpty(x) {
    if (x === null || x === void 0) return true;
    if (Array.isArray(x)) return x.length === 0;
    if (isNamedList(x)) return Object.keys(x).length === 0;
    return false;
  }
  function compact(options) {
    const out = {};
    for (const [key, value] of Object.entries(options)) {
      if (value === null || value === void 0) continue;
      if (Array.isArray(value) && value.length === 0) continue;
      if (isNamedList(value) && Object.keys(value).length === 0) continue;
      out[key] = value;
    }
    return out;
  }
  function dropNull(options) {
    const out = {};
    for (const [key, value] of Object.entries(options)) {
      if (value !== null && value !== void 0) out[key] = value;
    }
    return out;
  }
  function modifyList(old, update) {
    const out = isNamedList(old) ? { ...old } : {};
    for (const [key, value] of Object.entries(update || {})) {
      if (value === null || value === void 0) {
        delete out[key];
      } else if (isNamedList(out[key]) && isNamedList(value)) {
        out[key] = modifyList(out[key], value);
      } else {
        out[key] = value;
      }
    }
    return out;
  }
  function mergeOptions2(old, update) {
    return old === null || old === void 0 ? update : modifyList(old, update);
  }
  function computeSize2(value, length = 2) {
    if (typeof value === "number") return value / length;
    if (/px$/.test(value)) return `${asCharacter(asNumeric(value.replace(/px$/, "")) / length)}px`;
    if (/%$/.test(value)) return `${asCharacter(asNumeric(value.replace(/%$/, "")) / length)}%`;
    throw new Error("Unsupported format");
  }
  function getRelativeHeight(baseHeight, relativeHeight) {
    const isPercent = (x) => typeof x === "string" && /%$/.test(x);
    if (isPercent(baseHeight) && isPercent(relativeHeight)) return relativeHeight;
    let base = baseHeight;
    if (typeof base === "string" && /px$/.test(base)) base = asNumeric(base.replace("px", ""));
    if (isPercent(relativeHeight)) {
      const percentage = asNumeric(String(relativeHeight).replace("%", "")) / 100;
      return typeof base === "number" ? base * percentage : null;
    }
    let relative = relativeHeight;
    if (typeof relative === "string" && /px$/.test(relative)) relative = asNumeric(String(relative).replace("px", ""));
    return asNumeric(relative);
  }
  function clone(x) {
    if (x === null || typeof x !== "object") return x;
    if (typeof x.copy === "function") return x.copy();
    if (Array.isArray(x)) return x.map(clone);
    const out = {};
    for (const [key, value] of Object.entries(x)) out[key] = clone(value);
    return out;
  }

  // src/lib/compile/chart.js
  var DEFAULT_FORMATTER = "<b>Start:</b> {start}<br><b>End:</b> {end}";
  var QUERY_FORMATTER = "<b>{protein_id}</b><br>\n        <b>Query gene</b><br>\n        <b>Location:</b> {start} - {end}<br>";
  var HIT_FORMATTER = "<b>{protein_id}</b><br>\n        <b>Best hit:</b> {BlastP}<br>\n        <b>Identity:</b> {identity}%<br>\n        <b>Similarity:</b> {similarity}%<br>\n        <b>Location:</b> {start} - {end}<br>";
  var FORWARD = ["1", "forward", "+", "sense"];
  var REVERSE = ["-1", "0", "reverse", "-", "antisense", "complement"];
  function addStrand(data, strand = null) {
    if (!data || data.nrow === 0) return null;
    if (strand === null && data.has("strand")) strand = "strand";
    if (!data.has("start")) throw new Error("start column not found in data");
    if (!data.has("end")) throw new Error("end column not found in data");
    const start = data.get("start");
    const end = data.get("end");
    if (strand === null) {
      data.set("strand", start.map((s, i) => s === null || end[i] === null ? null : s < end[i] ? "forward" : "reverse"));
      return data;
    }
    if (!data.has(strand)) throw new Error("strand column not found in data");
    const values = data.get(strand).map(asCharacter);
    if (!values.every((v) => FORWARD.includes(v) || REVERSE.includes(v))) {
      throw new Error("Invalid strand values found. Valid values are 1/forward/+/sense or -1/0/reverse/-/antisense/complement.");
    }
    const strands = values.map((v) => FORWARD.includes(v) ? "forward" : "reverse");
    data.set("strand", strands);
    const newStart = start.slice();
    const newEnd = end.slice();
    strands.forEach((s, i) => {
      if (s === "forward" && start[i] > end[i] || s === "reverse" && start[i] < end[i]) {
        newStart[i] = end[i];
        newEnd[i] = start[i];
      }
    });
    data.set("start", newStart);
    data.set("end", newEnd);
    return data;
  }
  function getUpdatedClusters(state, cluster2) {
    const clusters = [...state.x.series.keys()];
    if (cluster2 === null || cluster2 === void 0) return clusters;
    const wanted = Array.isArray(cluster2) ? cluster2 : [cluster2];
    if (wanted.length && wanted.every((c) => typeof c === "number")) {
      if (wanted.some((c) => c > clusters.length || c < 1)) {
        state.warnings.push("Some cluster numbers provided are out of range. Please check the cluster numbers.");
        return [];
      }
      return wanted.map((c) => clusters[Math.trunc(c) - 1]);
    }
    const names = wanted.map((c) => asCharacter(c));
    if (!names.every((c) => clusters.includes(c))) {
      state.warnings.push("Some cluster names provided are not valid. Please check the cluster names.");
      return [];
    }
    return names;
  }
  function chart(spec) {
    const fields = spec.fields || {};
    const start = fields.start ?? "start";
    const end = fields.end ?? "end";
    const cluster2 = fields.cluster ?? null;
    const group = fields.group ?? null;
    const strand = fields.strand ?? null;
    const width = spec.width ?? "100%";
    const height = spec.height ?? "400px";
    const style = spec.style ?? {};
    const buttons = spec.toolbar && !Array.isArray(spec.toolbar) ? spec.toolbar.buttons : void 0;
    const saveButton = !(Array.isArray(buttons) && !buttons.includes("save"));
    const x = {};
    x.params = { start, end };
    if (cluster2 !== null) x.params.cluster = cluster2;
    if (group !== null) x.params.group = group;
    if (strand !== null) x.params.strand = strand;
    x.params.width = width;
    x.params.height = height;
    if (Object.keys(style).length) x.params.style = clone(style);
    x.params.save_button = saveButton;
    let data = Frame.from(spec.data);
    for (const [column, label] of [[start, "start"], [end, "end"], [cluster2, "cluster"], [group, "group"], [strand, "strand"]]) {
      if (column !== null && !data.has(column)) throw new Error(`${label} column not found in data`);
    }
    const columnNames = data.names;
    const startValues = data.get(start);
    const endValues = data.get(end);
    const kept = data.filter((i) => startValues[i] !== null && endValues[i] !== null);
    const warnings = [];
    if (kept.nrow < data.nrow) {
      warnings.push("Rows with NA values in 'start' or 'end' columns have been filtered out.");
    }
    data = kept;
    data.set("rowID", Array.from({ length: data.nrow }, (_, i) => i + 1));
    const original = data.copy();
    data.set("start", original.get(start).slice());
    data.set("end", original.get(end).slice());
    if (cluster2 !== null) data.set(cluster2, data.get(cluster2).map(asCharacter));
    data = addStrand(data, strand);
    x.data = data;
    if (group !== null) x.group = group;
    if (cluster2 !== null) x.cluster = cluster2;
    x.graphContainer = { direction: "column" };
    x.style = style;
    x.title = { style: { width: "100%" } };
    x.legend = { group, show: group !== null, width };
    x.legend.style = { width: "100%", backgroundColor: style.backgroundColor ?? null };
    x.saveButton = saveButton;
    const clusterRows = /* @__PURE__ */ new Map();
    if (cluster2 !== null) {
      data.get(cluster2).forEach((name, i) => {
        const rows = clusterRows.get(name);
        if (rows) rows.push(i);
        else clusterRows.set(name, [i]);
      });
    }
    const clusters = cluster2 === null ? ["cluster"] : [...clusterRows.keys()];
    const blast = columnNames.includes("BlastP") && columnNames.includes("protein_id");
    const series = /* @__PURE__ */ new Map();
    for (const name of clusters) {
      const subset = cluster2 === null ? data.copy() : data.rows(clusterRows.get(name));
      subset.set("cluster", name);
      const clusterHeight = typeof height === "string" && /%$/.test(height) ? computeSize2(height, clusters.length) : Number(getRelativeHeight(height, height)) / clusters.length;
      const starts = subset.get("start");
      const ends = subset.get("end");
      const positions = [...starts, ...ends].filter((v) => v !== null);
      let formatter = DEFAULT_FORMATTER;
      if (blast) {
        const hits = subset.get("BlastP");
        const ids = subset.get("protein_id");
        const query = hits.some((hit, i) => hit !== null && ids[i] !== null && hit === ids[i]) && hits.every((hit) => hit === null || hit === "No Hit" || ids.includes(hit));
        formatter = query ? QUERY_FORMATTER : HIT_FORMATTER;
      }
      series.set(name, {
        clusterName: name,
        data: subset,
        cluster: {},
        container: {
          height: clusterHeight,
          width,
          style: { width: "100%", backgroundColor: style.backgroundColor ?? null }
        },
        genes: { group, show: true },
        sequence: { show: true },
        scale: { xMin: Math.min(...positions), xMax: Math.max(...positions) },
        transcript: { group, show: false },
        labels: { group, show: true },
        coordinates: { show: false },
        scaleBar: {},
        footer: {},
        clusterLabel: {},
        clusterTitle: {},
        annotations: [],
        trackMouse: { show: false },
        tooltip: { show: true, formatter }
      });
    }
    x.series = series;
    return { x, width, height, spec, warnings };
  }

  // src/themes.json
  var themes_default = {
    palettes: {
      okabeIto: [
        "#E69F00",
        "#56B4E9",
        "#009E73",
        "#F0E442",
        "#0072B2",
        "#D55E00",
        "#CC79A7",
        "#999999"
      ],
      classic: [
        "#5470c6",
        "#91cc75",
        "#fac858",
        "#ee6666",
        "#73c0de",
        "#3ba272",
        "#fc8452",
        "#9a60b4",
        "#ea7ccc"
      ],
      vivid: [
        "#4992ff",
        "#7cffb2",
        "#fddd60",
        "#ff6e76",
        "#58d9f9",
        "#05c091",
        "#ff8a45",
        "#8d48e3",
        "#dd79ff"
      ],
      dark: [
        "#dd6b66",
        "#759aa0",
        "#e69d87",
        "#8dc1a9",
        "#ea7e53",
        "#eedd78",
        "#73a373",
        "#73b9bc",
        "#7289ab",
        "#91ca8c",
        "#f49f42"
      ],
      vintage: [
        "#d87c7c",
        "#919e8b",
        "#d7ab82",
        "#6e7074",
        "#61a0a8",
        "#efa18d",
        "#787464",
        "#cc7e63",
        "#724e58",
        "#4b565b"
      ],
      infographic: [
        "#C1232B",
        "#27727B",
        "#FCCE10",
        "#E87C25",
        "#B5C334",
        "#FE8463",
        "#9BCA63",
        "#FAD860",
        "#F3A43B",
        "#60C0DD",
        "#D7504B",
        "#C6E579",
        "#F4E001",
        "#F0805A",
        "#26C0C0"
      ],
      greys: [
        "#252525",
        "#525252",
        "#737373",
        "#969696",
        "#bdbdbd",
        "#d9d9d9"
      ],
      blues: [
        "#08306b",
        "#08519c",
        "#2171b5",
        "#4292c6",
        "#6baed6",
        "#9ecae1",
        "#c6dbef"
      ]
    },
    themes: {
      default: {},
      classic: {
        fontFamily: "sans-serif",
        textColor: "#6E7079",
        lineColor: "#6E7079",
        palette: "classic",
        title: {
          titleFont: {
            fontSize: "18px",
            fill: "#464646"
          },
          subtitleFont: {
            fontSize: "12px"
          }
        },
        clusterTitle: {
          titleFont: {
            fill: "#464646"
          }
        },
        clusterLabel: {
          fill: "#464646"
        },
        genes: {
          stroke: "none",
          corner_radius: 3
        },
        transcript: {
          styleExons: {
            strokeWidth: 0,
            cornerRadius: 3
          },
          styleUTRs: {
            cornerRadius: 3
          }
        },
        labels: {
          fontStyle: "normal",
          fontSize: "12px",
          leaderLine: {
            strokeWidth: 1
          }
        },
        coordinates: {
          tickStyle: {
            lineLength: 5
          }
        },
        scale: {
          textStyle: {
            fontSize: "12px"
          },
          tickStyle: {
            lineLength: 5
          }
        },
        scaleBar: {
          labelStyle: {
            fontSize: "12px"
          }
        },
        legend: {
          corner_radius: 3,
          legendTextOptions: {
            fill: "#464646"
          }
        },
        links: {
          normal_color: "#5470c6",
          inverted_color: "#ee6666",
          linkStyle: {
            fillOpacity: 0.6
          },
          colorBarOptions: {
            barOptions: {
              stroke: "none"
            },
            labelOptions: {
              fontSize: 12
            },
            titleOptions: {
              fontSize: 12
            }
          }
        },
        tooltip: {
          backgroundColor: "#ffffff",
          color: "#6d6e73",
          border: "1px solid #b7b9be",
          borderRadius: "4px",
          fontSize: "12px",
          fontFamily: "sans-serif",
          padding: "8px",
          boxShadow: "1px 2px 10px rgba(0, 0, 0, 0.2)"
        },
        trackMouse: {
          backgroundColor: "#ffffff",
          color: "#6d6e73",
          border: "1px solid #b7b9be",
          borderRadius: "4px",
          fontSize: "12px",
          fontFamily: "sans-serif",
          padding: "8px",
          boxShadow: "1px 2px 10px rgba(0, 0, 0, 0.2)"
        },
        toolbar: {
          color: "#999999",
          hoverColor: "#3E98C5",
          tooltipBackground: "#ffffff",
          tooltipColor: "#6d6e73",
          tooltipBorder: "1px solid #b7b9be",
          tooltipRadius: "4px",
          tooltipShadow: "1px 2px 10px rgba(0, 0, 0, 0.2)",
          fontFamily: "sans-serif"
        }
      },
      dark: {
        fontFamily: "sans-serif",
        background: "#333333",
        textColor: "#eeeeee",
        lineColor: "#eeeeee",
        palette: "dark",
        title: {
          titleFont: {
            fontSize: "18px"
          },
          subtitleFont: {
            fontSize: "12px",
            fill: "#aaaaaa"
          }
        },
        clusterTitle: {
          subtitleFont: {
            fill: "#aaaaaa"
          }
        },
        clusterLabel: {
          subtitleFont: {
            fill: "#aaaaaa"
          }
        },
        genes: {
          stroke: "none",
          corner_radius: 3
        },
        transcript: {
          styleExons: {
            strokeWidth: 0,
            cornerRadius: 3
          },
          styleUTRs: {
            cornerRadius: 3
          }
        },
        labels: {
          fontStyle: "normal",
          fontSize: "12px",
          leaderLine: {
            strokeWidth: 1
          }
        },
        coordinates: {
          tickStyle: {
            lineLength: 5
          }
        },
        scale: {
          textStyle: {
            fontSize: "12px"
          },
          tickStyle: {
            lineLength: 5
          }
        },
        scaleBar: {
          labelStyle: {
            fontSize: "12px"
          }
        },
        legend: {
          corner_radius: 3
        },
        links: {
          normal_color: "#759aa0",
          inverted_color: "#dd6b66",
          low_color: "#333333",
          linkStyle: {
            fillOpacity: 0.6
          },
          colorBarOptions: {
            barOptions: {
              stroke: "none"
            },
            labelOptions: {
              fontSize: 12
            },
            titleOptions: {
              fontSize: 12
            }
          }
        },
        tooltip: {
          backgroundColor: "#ffffff",
          color: "#6d6e73",
          border: "1px solid #b7b9be",
          borderRadius: "4px",
          fontSize: "12px",
          fontFamily: "sans-serif",
          padding: "8px",
          boxShadow: "1px 2px 10px rgba(0, 0, 0, 0.2)"
        },
        trackMouse: {
          backgroundColor: "#ffffff",
          color: "#6d6e73",
          border: "1px solid #b7b9be",
          borderRadius: "4px",
          fontSize: "12px",
          fontFamily: "sans-serif",
          padding: "8px",
          boxShadow: "1px 2px 10px rgba(0, 0, 0, 0.2)"
        },
        toolbar: {
          color: "#999999",
          hoverColor: "#eeeeee",
          tooltipBackground: "#ffffff",
          tooltipColor: "#6d6e73",
          tooltipBorder: "1px solid #b7b9be",
          tooltipRadius: "4px",
          tooltipShadow: "1px 2px 10px rgba(0, 0, 0, 0.2)",
          fontFamily: "sans-serif"
        }
      },
      vintage: {
        background: "#fef8ef",
        textColor: "#3f3a33",
        lineColor: "#787464",
        palette: "vintage",
        title: {
          subtitleFont: {
            fill: "#787464"
          }
        },
        clusterTitle: {
          subtitleFont: {
            fill: "#787464"
          }
        },
        clusterLabel: {
          subtitleFont: {
            fill: "#787464"
          }
        },
        sequence: {
          sequenceStyle: {
            stroke: "#c9c2b5"
          }
        },
        genes: {
          stroke: "none",
          corner_radius: 3
        },
        transcript: {
          styleExons: {
            strokeWidth: 0,
            cornerRadius: 3
          },
          styleUTRs: {
            cornerRadius: 3
          }
        },
        labels: {
          fontStyle: "normal",
          fontSize: "11px",
          leaderLine: {
            stroke: "#c9c2b5",
            strokeWidth: 1
          }
        },
        legend: {
          corner_radius: 3
        },
        links: {
          normal_color: "#61a0a8",
          inverted_color: "#d87c7c",
          low_color: "#fef8ef",
          linkStyle: {
            fillOpacity: 0.6
          },
          colorBarOptions: {
            barOptions: {
              stroke: "none"
            }
          }
        },
        tooltip: {
          backgroundColor: "#ffffff",
          color: "#6d6e73",
          border: "1px solid #b7b9be",
          borderRadius: "4px",
          fontSize: "12px",
          fontFamily: "sans-serif",
          padding: "8px",
          boxShadow: "1px 2px 10px rgba(0, 0, 0, 0.2)"
        },
        trackMouse: {
          backgroundColor: "#ffffff",
          color: "#6d6e73",
          border: "1px solid #b7b9be",
          borderRadius: "4px",
          fontSize: "12px",
          fontFamily: "sans-serif",
          padding: "8px",
          boxShadow: "1px 2px 10px rgba(0, 0, 0, 0.2)"
        },
        toolbar: {
          color: "#c9c2b5",
          hoverColor: "#d87c7c",
          tooltipBackground: "#ffffff",
          tooltipColor: "#6d6e73",
          tooltipBorder: "1px solid #b7b9be",
          tooltipRadius: "4px",
          tooltipShadow: "1px 2px 10px rgba(0, 0, 0, 0.2)",
          fontFamily: "sans-serif"
        }
      },
      infographic: {
        fontFamily: "sans-serif",
        textColor: "#6E7079",
        lineColor: "#6E7079",
        palette: "infographic",
        title: {
          titleFont: {
            fontSize: "18px",
            fill: "#464646"
          },
          subtitleFont: {
            fontSize: "12px"
          }
        },
        clusterTitle: {
          titleFont: {
            fill: "#464646"
          }
        },
        clusterLabel: {
          fill: "#464646"
        },
        genes: {
          stroke: "none",
          corner_radius: 3
        },
        transcript: {
          styleExons: {
            strokeWidth: 0,
            cornerRadius: 3
          },
          styleUTRs: {
            cornerRadius: 3
          }
        },
        labels: {
          fontStyle: "normal",
          fontSize: "12px",
          leaderLine: {
            strokeWidth: 1
          }
        },
        coordinates: {
          tickStyle: {
            lineLength: 5
          }
        },
        scale: {
          textStyle: {
            fontSize: "12px"
          },
          tickStyle: {
            lineLength: 5
          }
        },
        scaleBar: {
          labelStyle: {
            fontSize: "12px"
          }
        },
        legend: {
          corner_radius: 3,
          legendTextOptions: {
            fill: "#464646"
          }
        },
        links: {
          normal_color: "#27727B",
          inverted_color: "#C1232B",
          linkStyle: {
            fillOpacity: 0.6
          },
          colorBarOptions: {
            barOptions: {
              stroke: "none"
            },
            labelOptions: {
              fontSize: 12
            },
            titleOptions: {
              fontSize: 12
            }
          }
        },
        tooltip: {
          backgroundColor: "rgba(50, 50, 50, 0.85)",
          color: "#ffffff",
          border: "none",
          borderRadius: "4px",
          fontSize: "12px"
        },
        trackMouse: {
          backgroundColor: "rgba(50, 50, 50, 0.85)",
          color: "#ffffff",
          border: "none",
          borderRadius: "4px",
          fontSize: "12px"
        },
        toolbar: {
          color: "#C1232B",
          hoverColor: "#E87C25",
          tooltipBackground: "rgba(50, 50, 50, 0.85)",
          tooltipColor: "#ffffff",
          tooltipBorder: "none",
          tooltipRadius: "4px"
        }
      },
      publication: {
        fontFamily: "Arial, Helvetica, sans-serif",
        textColor: "black",
        lineColor: "black",
        palette: "okabeIto",
        title: {
          titleFont: {
            fontSize: "9.33px",
            fill: "black"
          },
          subtitleFont: {
            fontSize: "9.33px"
          }
        },
        clusterTitle: {
          titleFont: {
            fontSize: "9.33px"
          },
          subtitleFont: {
            fontSize: "9.33px"
          }
        },
        clusterFooter: {
          titleFont: {
            fontSize: "9.33px"
          },
          subtitleFont: {
            fontSize: "9.33px"
          }
        },
        clusterLabel: {
          fontSize: "9.33px",
          subtitleFont: {
            fontSize: "9.33px"
          }
        },
        genes: {
          stroke: "black",
          strokeWidth: 0.75,
          corner_radius: 3
        },
        transcript: {
          styleExons: {
            strokeWidth: 0,
            cornerRadius: 3
          },
          styleUTRs: {
            cornerRadius: 3,
            fontSize: "9.33px"
          },
          labelOptions: {
            fontSize: "9.33px"
          }
        },
        labels: {
          fontStyle: "italic",
          fontSize: "9.33px",
          leaderLine: {
            strokeWidth: 0.75
          }
        },
        coordinates: {
          textStyle: {
            fontSize: "9.33px"
          },
          tickStyle: {
            lineLength: 4
          }
        },
        scale: {
          textStyle: {
            fontSize: "9.33px"
          },
          tickStyle: {
            lineLength: 4
          }
        },
        scaleBar: {
          labelStyle: {
            fontSize: "9.33px"
          }
        },
        legend: {
          corner_radius: 2,
          legendOptions: {
            stroke: "black",
            strokeWidth: 0.75
          },
          legendTextOptions: {
            fontSize: "9.33px"
          }
        },
        links: {
          normal_color: "#0072B2",
          inverted_color: "#D55E00",
          linkStyle: {
            fillOpacity: 0.6
          },
          labelStyle: {
            fontSize: "9.33px"
          },
          colorBarOptions: {
            barOptions: {
              stroke: "none"
            },
            labelOptions: {
              fontSize: 9.33
            },
            titleOptions: {
              fontSize: 9.33
            }
          }
        },
        annotations: {
          text: {
            style: {
              fontSize: "9.33px"
            }
          },
          textMarker: {
            textStyle: {
              fontSize: "9.33px"
            }
          },
          arc: {
            textStyle: {
              fontSize: "9.33px"
            }
          }
        },
        tooltip: {
          backgroundColor: "white",
          color: "black",
          border: "1px solid black",
          borderRadius: "0",
          boxShadow: "none",
          fontSize: "9.33px",
          padding: "8px"
        },
        trackMouse: {
          backgroundColor: "white",
          color: "black",
          border: "1px solid black",
          borderRadius: "0",
          boxShadow: "none",
          fontSize: "9.33px",
          padding: "8px"
        },
        toolbar: {
          color: "#999999",
          hoverColor: "black",
          tooltipBackground: "white",
          tooltipColor: "black",
          tooltipBorder: "1px solid black",
          tooltipRadius: "0",
          tooltipShadow: "none"
        }
      },
      bw: {
        fontFamily: "sans-serif",
        textColor: "#555555",
        lineColor: "#555555",
        pattern: {
          color: "black",
          background: "white",
          size: 6,
          strokeWidth: 1
        },
        title: {
          titleFont: {
            fontSize: "18px",
            fill: "black"
          },
          subtitleFont: {
            fontSize: "12px"
          }
        },
        clusterTitle: {
          titleFont: {
            fill: "black"
          }
        },
        clusterLabel: {
          fill: "black"
        },
        genes: {
          stroke: "black",
          strokeWidth: 0.75,
          corner_radius: 3
        },
        transcript: {
          styleExons: {
            stroke: "black",
            strokeWidth: 0.75,
            cornerRadius: 3
          },
          styleUTRs: {
            cornerRadius: 3
          }
        },
        labels: {
          fontStyle: "normal",
          fontSize: "12px",
          leaderLine: {
            strokeWidth: 1
          }
        },
        coordinates: {
          tickStyle: {
            lineLength: 5
          }
        },
        scale: {
          textStyle: {
            fontSize: "12px"
          },
          tickStyle: {
            lineLength: 5
          }
        },
        scaleBar: {
          labelStyle: {
            fontSize: "12px"
          }
        },
        legend: {
          corner_radius: 3,
          legendTextOptions: {
            fill: "black"
          }
        },
        links: {
          normal_color: "#bdbdbd",
          inverted_color: "#525252",
          colorBarOptions: {
            barOptions: {
              stroke: "#555555"
            },
            labelOptions: {
              fontSize: 12
            },
            titleOptions: {
              fontSize: 12
            }
          }
        },
        tooltip: {
          backgroundColor: "#ffffff",
          color: "#6d6e73",
          border: "1px solid #b7b9be",
          borderRadius: "4px",
          fontSize: "12px",
          fontFamily: "sans-serif",
          padding: "8px",
          boxShadow: "1px 2px 10px rgba(0, 0, 0, 0.2)"
        },
        trackMouse: {
          backgroundColor: "#ffffff",
          color: "#6d6e73",
          border: "1px solid #b7b9be",
          borderRadius: "4px",
          fontSize: "12px",
          fontFamily: "sans-serif",
          padding: "8px",
          boxShadow: "1px 2px 10px rgba(0, 0, 0, 0.2)"
        },
        toolbar: {
          color: "#bdbdbd",
          hoverColor: "black",
          tooltipBackground: "#ffffff",
          tooltipColor: "#6d6e73",
          tooltipBorder: "1px solid #b7b9be",
          tooltipRadius: "4px",
          tooltipShadow: "1px 2px 10px rgba(0, 0, 0, 0.2)",
          fontFamily: "sans-serif"
        }
      }
    }
  };

  // src/lib/compile/cluster.js
  function dots(entry, named) {
    const out = {};
    for (const [key, value] of Object.entries(entry)) {
      if (!named.includes(key)) out[key] = value;
    }
    return out;
  }
  function order(n, keys) {
    const indices = Array.from({ length: n }, (_, i) => i);
    return indices.sort((a, b) => {
      for (const key of keys) {
        const x = key(a);
        const y = key(b);
        const xNA = x === null || Number.isNaN(x);
        const yNA = y === null || Number.isNaN(y);
        if (xNA || yNA) {
          if (xNA !== yNA) return xNA ? 1 : -1;
          continue;
        }
        if (x < y) return -1;
        if (x > y) return 1;
      }
      return a - b;
    });
  }
  function addGeneTrack(data, nested = false, overlapMin = 0) {
    if (!data || data.nrow === 0) return null;
    const starts = data.get("start");
    const ends = data.get("end");
    const low = starts.map((s, i) => Math.min(s, ends[i]));
    const high = starts.map((s, i) => Math.max(s, ends[i]));
    const share = typeof overlapMin === "string";
    const limit = share ? Number(overlapMin.replace(/%$/, "")) / 100 : overlapMin;
    const clash = (i, j) => {
      const overlap = Math.min(high[i], high[j]) - Math.max(low[i], low[j]) + 1;
      if (overlap <= 0) return false;
      const threshold = share ? limit * Math.min(high[i] - low[i] + 1, high[j] - low[j] + 1) : limit;
      return overlap > threshold;
    };
    const inside = low.map((_, i) => low.some((__, j) => j !== i && low[j] <= low[i] && high[j] >= high[i] && (low[j] < low[i] || high[j] > high[i] || j < i)));
    const track = [];
    const placed = /* @__PURE__ */ new Map();
    for (let i = 0; i < low.length; i++) {
      if (nested && !inside[i]) {
        track[i] = 1;
        continue;
      }
      let candidate = nested ? 2 : 1;
      while ((placed.get(candidate) || []).some((j) => clash(i, j))) candidate++;
      track[i] = candidate;
      placed.set(candidate, [...placed.get(candidate) || [], i]);
    }
    data.set("geneTrack", track);
    return data;
  }
  var NAMED = [
    "separateStrands",
    "strandSpacing",
    "preventGeneOverlap",
    "overlapMin",
    "overlapSpacing",
    "clusters",
    "style"
  ];
  function cluster(state, entry) {
    let separateStrands = entry.separateStrands ?? null;
    let preventGeneOverlap = entry.preventGeneOverlap ?? null;
    const overlapMin = entry.overlapMin ?? null;
    if (separateStrands === true && preventGeneOverlap !== null && preventGeneOverlap !== false) {
      state.warnings.push("Setting both 'separate_strands' and 'prevent_gene_overlap' to TRUE is not supported. Resetting to NULL.");
      separateStrands = null;
      preventGeneOverlap = null;
    }
    const extra = dots(entry, NAMED);
    for (const name of getUpdatedClusters(state, entry.clusters)) {
      const series = state.x.series.get(name);
      if (preventGeneOverlap !== null && preventGeneOverlap !== false) {
        const data = series.data;
        const starts = data.get("start");
        const ends = data.get("end");
        const rows = order(data.nrow, [
          (i) => -Math.max(starts[i], ends[i]),
          (i) => Math.abs(ends[i] - starts[i])
        ]);
        series.data = addGeneTrack(
          data.rows(rows),
          preventGeneOverlap === "nested",
          overlapMin === null ? 0 : overlapMin
        );
      }
      series.container.style = mergeOptions2(series.container.style, entry.style ?? {});
      const clusterOptions = compact({
        separateStrands,
        strandSpacing: entry.strandSpacing,
        preventGeneOverlap: preventGeneOverlap !== null ? preventGeneOverlap !== false : null,
        overlapSpacing: entry.overlapSpacing,
        ...extra
      });
      series.cluster = mergeOptions2(series.cluster, clusterOptions);
      series.container = mergeOptions2(series.container, compact({ ...extra }));
    }
  }

  // src/lib/compile/rcolors.js
  var R_COLORS = {
    white: "#FFFFFF",
    aliceblue: "#F0F8FF",
    antiquewhite: "#FAEBD7",
    "antiquewhite1": "#FFEFDB",
    "antiquewhite2": "#EEDFCC",
    "antiquewhite3": "#CDC0B0",
    "antiquewhite4": "#8B8378",
    aquamarine: "#7FFFD4",
    "aquamarine1": "#7FFFD4",
    "aquamarine2": "#76EEC6",
    "aquamarine3": "#66CDAA",
    "aquamarine4": "#458B74",
    azure: "#F0FFFF",
    "azure1": "#F0FFFF",
    "azure2": "#E0EEEE",
    "azure3": "#C1CDCD",
    "azure4": "#838B8B",
    beige: "#F5F5DC",
    bisque: "#FFE4C4",
    "bisque1": "#FFE4C4",
    "bisque2": "#EED5B7",
    "bisque3": "#CDB79E",
    "bisque4": "#8B7D6B",
    black: "#000000",
    blanchedalmond: "#FFEBCD",
    blue: "#0000FF",
    "blue1": "#0000FF",
    "blue2": "#0000EE",
    "blue3": "#0000CD",
    "blue4": "#00008B",
    blueviolet: "#8A2BE2",
    brown: "#A52A2A",
    "brown1": "#FF4040",
    "brown2": "#EE3B3B",
    "brown3": "#CD3333",
    "brown4": "#8B2323",
    burlywood: "#DEB887",
    "burlywood1": "#FFD39B",
    "burlywood2": "#EEC591",
    "burlywood3": "#CDAA7D",
    "burlywood4": "#8B7355",
    cadetblue: "#5F9EA0",
    "cadetblue1": "#98F5FF",
    "cadetblue2": "#8EE5EE",
    "cadetblue3": "#7AC5CD",
    "cadetblue4": "#53868B",
    chartreuse: "#7FFF00",
    "chartreuse1": "#7FFF00",
    "chartreuse2": "#76EE00",
    "chartreuse3": "#66CD00",
    "chartreuse4": "#458B00",
    chocolate: "#D2691E",
    "chocolate1": "#FF7F24",
    "chocolate2": "#EE7621",
    "chocolate3": "#CD661D",
    "chocolate4": "#8B4513",
    coral: "#FF7F50",
    "coral1": "#FF7256",
    "coral2": "#EE6A50",
    "coral3": "#CD5B45",
    "coral4": "#8B3E2F",
    cornflowerblue: "#6495ED",
    cornsilk: "#FFF8DC",
    "cornsilk1": "#FFF8DC",
    "cornsilk2": "#EEE8CD",
    "cornsilk3": "#CDC8B1",
    "cornsilk4": "#8B8878",
    cyan: "#00FFFF",
    "cyan1": "#00FFFF",
    "cyan2": "#00EEEE",
    "cyan3": "#00CDCD",
    "cyan4": "#008B8B",
    darkblue: "#00008B",
    darkcyan: "#008B8B",
    darkgoldenrod: "#B8860B",
    "darkgoldenrod1": "#FFB90F",
    "darkgoldenrod2": "#EEAD0E",
    "darkgoldenrod3": "#CD950C",
    "darkgoldenrod4": "#8B6508",
    darkgray: "#A9A9A9",
    darkgreen: "#006400",
    darkgrey: "#A9A9A9",
    darkkhaki: "#BDB76B",
    darkmagenta: "#8B008B",
    darkolivegreen: "#556B2F",
    "darkolivegreen1": "#CAFF70",
    "darkolivegreen2": "#BCEE68",
    "darkolivegreen3": "#A2CD5A",
    "darkolivegreen4": "#6E8B3D",
    darkorange: "#FF8C00",
    "darkorange1": "#FF7F00",
    "darkorange2": "#EE7600",
    "darkorange3": "#CD6600",
    "darkorange4": "#8B4500",
    darkorchid: "#9932CC",
    "darkorchid1": "#BF3EFF",
    "darkorchid2": "#B23AEE",
    "darkorchid3": "#9A32CD",
    "darkorchid4": "#68228B",
    darkred: "#8B0000",
    darksalmon: "#E9967A",
    darkseagreen: "#8FBC8F",
    "darkseagreen1": "#C1FFC1",
    "darkseagreen2": "#B4EEB4",
    "darkseagreen3": "#9BCD9B",
    "darkseagreen4": "#698B69",
    darkslateblue: "#483D8B",
    darkslategray: "#2F4F4F",
    "darkslategray1": "#97FFFF",
    "darkslategray2": "#8DEEEE",
    "darkslategray3": "#79CDCD",
    "darkslategray4": "#528B8B",
    darkslategrey: "#2F4F4F",
    darkturquoise: "#00CED1",
    darkviolet: "#9400D3",
    deeppink: "#FF1493",
    "deeppink1": "#FF1493",
    "deeppink2": "#EE1289",
    "deeppink3": "#CD1076",
    "deeppink4": "#8B0A50",
    deepskyblue: "#00BFFF",
    "deepskyblue1": "#00BFFF",
    "deepskyblue2": "#00B2EE",
    "deepskyblue3": "#009ACD",
    "deepskyblue4": "#00688B",
    dimgray: "#696969",
    dimgrey: "#696969",
    dodgerblue: "#1E90FF",
    "dodgerblue1": "#1E90FF",
    "dodgerblue2": "#1C86EE",
    "dodgerblue3": "#1874CD",
    "dodgerblue4": "#104E8B",
    firebrick: "#B22222",
    "firebrick1": "#FF3030",
    "firebrick2": "#EE2C2C",
    "firebrick3": "#CD2626",
    "firebrick4": "#8B1A1A",
    floralwhite: "#FFFAF0",
    forestgreen: "#228B22",
    gainsboro: "#DCDCDC",
    ghostwhite: "#F8F8FF",
    gold: "#FFD700",
    "gold1": "#FFD700",
    "gold2": "#EEC900",
    "gold3": "#CDAD00",
    "gold4": "#8B7500",
    goldenrod: "#DAA520",
    "goldenrod1": "#FFC125",
    "goldenrod2": "#EEB422",
    "goldenrod3": "#CD9B1D",
    "goldenrod4": "#8B6914",
    gray: "#BEBEBE",
    "gray0": "#000000",
    "gray1": "#030303",
    "gray2": "#050505",
    "gray3": "#080808",
    "gray4": "#0A0A0A",
    "gray5": "#0D0D0D",
    "gray6": "#0F0F0F",
    "gray7": "#121212",
    "gray8": "#141414",
    "gray9": "#171717",
    "gray10": "#1A1A1A",
    "gray11": "#1C1C1C",
    "gray12": "#1F1F1F",
    "gray13": "#212121",
    "gray14": "#242424",
    "gray15": "#262626",
    "gray16": "#292929",
    "gray17": "#2B2B2B",
    "gray18": "#2E2E2E",
    "gray19": "#303030",
    "gray20": "#333333",
    "gray21": "#363636",
    "gray22": "#383838",
    "gray23": "#3B3B3B",
    "gray24": "#3D3D3D",
    "gray25": "#404040",
    "gray26": "#424242",
    "gray27": "#454545",
    "gray28": "#474747",
    "gray29": "#4A4A4A",
    "gray30": "#4D4D4D",
    "gray31": "#4F4F4F",
    "gray32": "#525252",
    "gray33": "#545454",
    "gray34": "#575757",
    "gray35": "#595959",
    "gray36": "#5C5C5C",
    "gray37": "#5E5E5E",
    "gray38": "#616161",
    "gray39": "#636363",
    "gray40": "#666666",
    "gray41": "#696969",
    "gray42": "#6B6B6B",
    "gray43": "#6E6E6E",
    "gray44": "#707070",
    "gray45": "#737373",
    "gray46": "#757575",
    "gray47": "#787878",
    "gray48": "#7A7A7A",
    "gray49": "#7D7D7D",
    "gray50": "#7F7F7F",
    "gray51": "#828282",
    "gray52": "#858585",
    "gray53": "#878787",
    "gray54": "#8A8A8A",
    "gray55": "#8C8C8C",
    "gray56": "#8F8F8F",
    "gray57": "#919191",
    "gray58": "#949494",
    "gray59": "#969696",
    "gray60": "#999999",
    "gray61": "#9C9C9C",
    "gray62": "#9E9E9E",
    "gray63": "#A1A1A1",
    "gray64": "#A3A3A3",
    "gray65": "#A6A6A6",
    "gray66": "#A8A8A8",
    "gray67": "#ABABAB",
    "gray68": "#ADADAD",
    "gray69": "#B0B0B0",
    "gray70": "#B3B3B3",
    "gray71": "#B5B5B5",
    "gray72": "#B8B8B8",
    "gray73": "#BABABA",
    "gray74": "#BDBDBD",
    "gray75": "#BFBFBF",
    "gray76": "#C2C2C2",
    "gray77": "#C4C4C4",
    "gray78": "#C7C7C7",
    "gray79": "#C9C9C9",
    "gray80": "#CCCCCC",
    "gray81": "#CFCFCF",
    "gray82": "#D1D1D1",
    "gray83": "#D4D4D4",
    "gray84": "#D6D6D6",
    "gray85": "#D9D9D9",
    "gray86": "#DBDBDB",
    "gray87": "#DEDEDE",
    "gray88": "#E0E0E0",
    "gray89": "#E3E3E3",
    "gray90": "#E5E5E5",
    "gray91": "#E8E8E8",
    "gray92": "#EBEBEB",
    "gray93": "#EDEDED",
    "gray94": "#F0F0F0",
    "gray95": "#F2F2F2",
    "gray96": "#F5F5F5",
    "gray97": "#F7F7F7",
    "gray98": "#FAFAFA",
    "gray99": "#FCFCFC",
    "gray100": "#FFFFFF",
    green: "#00FF00",
    "green1": "#00FF00",
    "green2": "#00EE00",
    "green3": "#00CD00",
    "green4": "#008B00",
    greenyellow: "#ADFF2F",
    grey: "#BEBEBE",
    "grey0": "#000000",
    "grey1": "#030303",
    "grey2": "#050505",
    "grey3": "#080808",
    "grey4": "#0A0A0A",
    "grey5": "#0D0D0D",
    "grey6": "#0F0F0F",
    "grey7": "#121212",
    "grey8": "#141414",
    "grey9": "#171717",
    "grey10": "#1A1A1A",
    "grey11": "#1C1C1C",
    "grey12": "#1F1F1F",
    "grey13": "#212121",
    "grey14": "#242424",
    "grey15": "#262626",
    "grey16": "#292929",
    "grey17": "#2B2B2B",
    "grey18": "#2E2E2E",
    "grey19": "#303030",
    "grey20": "#333333",
    "grey21": "#363636",
    "grey22": "#383838",
    "grey23": "#3B3B3B",
    "grey24": "#3D3D3D",
    "grey25": "#404040",
    "grey26": "#424242",
    "grey27": "#454545",
    "grey28": "#474747",
    "grey29": "#4A4A4A",
    "grey30": "#4D4D4D",
    "grey31": "#4F4F4F",
    "grey32": "#525252",
    "grey33": "#545454",
    "grey34": "#575757",
    "grey35": "#595959",
    "grey36": "#5C5C5C",
    "grey37": "#5E5E5E",
    "grey38": "#616161",
    "grey39": "#636363",
    "grey40": "#666666",
    "grey41": "#696969",
    "grey42": "#6B6B6B",
    "grey43": "#6E6E6E",
    "grey44": "#707070",
    "grey45": "#737373",
    "grey46": "#757575",
    "grey47": "#787878",
    "grey48": "#7A7A7A",
    "grey49": "#7D7D7D",
    "grey50": "#7F7F7F",
    "grey51": "#828282",
    "grey52": "#858585",
    "grey53": "#878787",
    "grey54": "#8A8A8A",
    "grey55": "#8C8C8C",
    "grey56": "#8F8F8F",
    "grey57": "#919191",
    "grey58": "#949494",
    "grey59": "#969696",
    "grey60": "#999999",
    "grey61": "#9C9C9C",
    "grey62": "#9E9E9E",
    "grey63": "#A1A1A1",
    "grey64": "#A3A3A3",
    "grey65": "#A6A6A6",
    "grey66": "#A8A8A8",
    "grey67": "#ABABAB",
    "grey68": "#ADADAD",
    "grey69": "#B0B0B0",
    "grey70": "#B3B3B3",
    "grey71": "#B5B5B5",
    "grey72": "#B8B8B8",
    "grey73": "#BABABA",
    "grey74": "#BDBDBD",
    "grey75": "#BFBFBF",
    "grey76": "#C2C2C2",
    "grey77": "#C4C4C4",
    "grey78": "#C7C7C7",
    "grey79": "#C9C9C9",
    "grey80": "#CCCCCC",
    "grey81": "#CFCFCF",
    "grey82": "#D1D1D1",
    "grey83": "#D4D4D4",
    "grey84": "#D6D6D6",
    "grey85": "#D9D9D9",
    "grey86": "#DBDBDB",
    "grey87": "#DEDEDE",
    "grey88": "#E0E0E0",
    "grey89": "#E3E3E3",
    "grey90": "#E5E5E5",
    "grey91": "#E8E8E8",
    "grey92": "#EBEBEB",
    "grey93": "#EDEDED",
    "grey94": "#F0F0F0",
    "grey95": "#F2F2F2",
    "grey96": "#F5F5F5",
    "grey97": "#F7F7F7",
    "grey98": "#FAFAFA",
    "grey99": "#FCFCFC",
    "grey100": "#FFFFFF",
    honeydew: "#F0FFF0",
    "honeydew1": "#F0FFF0",
    "honeydew2": "#E0EEE0",
    "honeydew3": "#C1CDC1",
    "honeydew4": "#838B83",
    hotpink: "#FF69B4",
    "hotpink1": "#FF6EB4",
    "hotpink2": "#EE6AA7",
    "hotpink3": "#CD6090",
    "hotpink4": "#8B3A62",
    indianred: "#CD5C5C",
    "indianred1": "#FF6A6A",
    "indianred2": "#EE6363",
    "indianred3": "#CD5555",
    "indianred4": "#8B3A3A",
    ivory: "#FFFFF0",
    "ivory1": "#FFFFF0",
    "ivory2": "#EEEEE0",
    "ivory3": "#CDCDC1",
    "ivory4": "#8B8B83",
    khaki: "#F0E68C",
    "khaki1": "#FFF68F",
    "khaki2": "#EEE685",
    "khaki3": "#CDC673",
    "khaki4": "#8B864E",
    lavender: "#E6E6FA",
    lavenderblush: "#FFF0F5",
    "lavenderblush1": "#FFF0F5",
    "lavenderblush2": "#EEE0E5",
    "lavenderblush3": "#CDC1C5",
    "lavenderblush4": "#8B8386",
    lawngreen: "#7CFC00",
    lemonchiffon: "#FFFACD",
    "lemonchiffon1": "#FFFACD",
    "lemonchiffon2": "#EEE9BF",
    "lemonchiffon3": "#CDC9A5",
    "lemonchiffon4": "#8B8970",
    lightblue: "#ADD8E6",
    "lightblue1": "#BFEFFF",
    "lightblue2": "#B2DFEE",
    "lightblue3": "#9AC0CD",
    "lightblue4": "#68838B",
    lightcoral: "#F08080",
    lightcyan: "#E0FFFF",
    "lightcyan1": "#E0FFFF",
    "lightcyan2": "#D1EEEE",
    "lightcyan3": "#B4CDCD",
    "lightcyan4": "#7A8B8B",
    lightgoldenrod: "#EEDD82",
    "lightgoldenrod1": "#FFEC8B",
    "lightgoldenrod2": "#EEDC82",
    "lightgoldenrod3": "#CDBE70",
    "lightgoldenrod4": "#8B814C",
    lightgoldenrodyellow: "#FAFAD2",
    lightgray: "#D3D3D3",
    lightgreen: "#90EE90",
    lightgrey: "#D3D3D3",
    lightpink: "#FFB6C1",
    "lightpink1": "#FFAEB9",
    "lightpink2": "#EEA2AD",
    "lightpink3": "#CD8C95",
    "lightpink4": "#8B5F65",
    lightsalmon: "#FFA07A",
    "lightsalmon1": "#FFA07A",
    "lightsalmon2": "#EE9572",
    "lightsalmon3": "#CD8162",
    "lightsalmon4": "#8B5742",
    lightseagreen: "#20B2AA",
    lightskyblue: "#87CEFA",
    "lightskyblue1": "#B0E2FF",
    "lightskyblue2": "#A4D3EE",
    "lightskyblue3": "#8DB6CD",
    "lightskyblue4": "#607B8B",
    lightslateblue: "#8470FF",
    lightslategray: "#778899",
    lightslategrey: "#778899",
    lightsteelblue: "#B0C4DE",
    "lightsteelblue1": "#CAE1FF",
    "lightsteelblue2": "#BCD2EE",
    "lightsteelblue3": "#A2B5CD",
    "lightsteelblue4": "#6E7B8B",
    lightyellow: "#FFFFE0",
    "lightyellow1": "#FFFFE0",
    "lightyellow2": "#EEEED1",
    "lightyellow3": "#CDCDB4",
    "lightyellow4": "#8B8B7A",
    limegreen: "#32CD32",
    linen: "#FAF0E6",
    magenta: "#FF00FF",
    "magenta1": "#FF00FF",
    "magenta2": "#EE00EE",
    "magenta3": "#CD00CD",
    "magenta4": "#8B008B",
    maroon: "#B03060",
    "maroon1": "#FF34B3",
    "maroon2": "#EE30A7",
    "maroon3": "#CD2990",
    "maroon4": "#8B1C62",
    mediumaquamarine: "#66CDAA",
    mediumblue: "#0000CD",
    mediumorchid: "#BA55D3",
    "mediumorchid1": "#E066FF",
    "mediumorchid2": "#D15FEE",
    "mediumorchid3": "#B452CD",
    "mediumorchid4": "#7A378B",
    mediumpurple: "#9370DB",
    "mediumpurple1": "#AB82FF",
    "mediumpurple2": "#9F79EE",
    "mediumpurple3": "#8968CD",
    "mediumpurple4": "#5D478B",
    mediumseagreen: "#3CB371",
    mediumslateblue: "#7B68EE",
    mediumspringgreen: "#00FA9A",
    mediumturquoise: "#48D1CC",
    mediumvioletred: "#C71585",
    midnightblue: "#191970",
    mintcream: "#F5FFFA",
    mistyrose: "#FFE4E1",
    "mistyrose1": "#FFE4E1",
    "mistyrose2": "#EED5D2",
    "mistyrose3": "#CDB7B5",
    "mistyrose4": "#8B7D7B",
    moccasin: "#FFE4B5",
    navajowhite: "#FFDEAD",
    "navajowhite1": "#FFDEAD",
    "navajowhite2": "#EECFA1",
    "navajowhite3": "#CDB38B",
    "navajowhite4": "#8B795E",
    navy: "#000080",
    navyblue: "#000080",
    oldlace: "#FDF5E6",
    olivedrab: "#6B8E23",
    "olivedrab1": "#C0FF3E",
    "olivedrab2": "#B3EE3A",
    "olivedrab3": "#9ACD32",
    "olivedrab4": "#698B22",
    orange: "#FFA500",
    "orange1": "#FFA500",
    "orange2": "#EE9A00",
    "orange3": "#CD8500",
    "orange4": "#8B5A00",
    orangered: "#FF4500",
    "orangered1": "#FF4500",
    "orangered2": "#EE4000",
    "orangered3": "#CD3700",
    "orangered4": "#8B2500",
    orchid: "#DA70D6",
    "orchid1": "#FF83FA",
    "orchid2": "#EE7AE9",
    "orchid3": "#CD69C9",
    "orchid4": "#8B4789",
    palegoldenrod: "#EEE8AA",
    palegreen: "#98FB98",
    "palegreen1": "#9AFF9A",
    "palegreen2": "#90EE90",
    "palegreen3": "#7CCD7C",
    "palegreen4": "#548B54",
    paleturquoise: "#AFEEEE",
    "paleturquoise1": "#BBFFFF",
    "paleturquoise2": "#AEEEEE",
    "paleturquoise3": "#96CDCD",
    "paleturquoise4": "#668B8B",
    palevioletred: "#DB7093",
    "palevioletred1": "#FF82AB",
    "palevioletred2": "#EE799F",
    "palevioletred3": "#CD6889",
    "palevioletred4": "#8B475D",
    papayawhip: "#FFEFD5",
    peachpuff: "#FFDAB9",
    "peachpuff1": "#FFDAB9",
    "peachpuff2": "#EECBAD",
    "peachpuff3": "#CDAF95",
    "peachpuff4": "#8B7765",
    peru: "#CD853F",
    pink: "#FFC0CB",
    "pink1": "#FFB5C5",
    "pink2": "#EEA9B8",
    "pink3": "#CD919E",
    "pink4": "#8B636C",
    plum: "#DDA0DD",
    "plum1": "#FFBBFF",
    "plum2": "#EEAEEE",
    "plum3": "#CD96CD",
    "plum4": "#8B668B",
    powderblue: "#B0E0E6",
    purple: "#A020F0",
    "purple1": "#9B30FF",
    "purple2": "#912CEE",
    "purple3": "#7D26CD",
    "purple4": "#551A8B",
    red: "#FF0000",
    "red1": "#FF0000",
    "red2": "#EE0000",
    "red3": "#CD0000",
    "red4": "#8B0000",
    rosybrown: "#BC8F8F",
    "rosybrown1": "#FFC1C1",
    "rosybrown2": "#EEB4B4",
    "rosybrown3": "#CD9B9B",
    "rosybrown4": "#8B6969",
    royalblue: "#4169E1",
    "royalblue1": "#4876FF",
    "royalblue2": "#436EEE",
    "royalblue3": "#3A5FCD",
    "royalblue4": "#27408B",
    saddlebrown: "#8B4513",
    salmon: "#FA8072",
    "salmon1": "#FF8C69",
    "salmon2": "#EE8262",
    "salmon3": "#CD7054",
    "salmon4": "#8B4C39",
    sandybrown: "#F4A460",
    seagreen: "#2E8B57",
    "seagreen1": "#54FF9F",
    "seagreen2": "#4EEE94",
    "seagreen3": "#43CD80",
    "seagreen4": "#2E8B57",
    seashell: "#FFF5EE",
    "seashell1": "#FFF5EE",
    "seashell2": "#EEE5DE",
    "seashell3": "#CDC5BF",
    "seashell4": "#8B8682",
    sienna: "#A0522D",
    "sienna1": "#FF8247",
    "sienna2": "#EE7942",
    "sienna3": "#CD6839",
    "sienna4": "#8B4726",
    skyblue: "#87CEEB",
    "skyblue1": "#87CEFF",
    "skyblue2": "#7EC0EE",
    "skyblue3": "#6CA6CD",
    "skyblue4": "#4A708B",
    slateblue: "#6A5ACD",
    "slateblue1": "#836FFF",
    "slateblue2": "#7A67EE",
    "slateblue3": "#6959CD",
    "slateblue4": "#473C8B",
    slategray: "#708090",
    "slategray1": "#C6E2FF",
    "slategray2": "#B9D3EE",
    "slategray3": "#9FB6CD",
    "slategray4": "#6C7B8B",
    slategrey: "#708090",
    snow: "#FFFAFA",
    "snow1": "#FFFAFA",
    "snow2": "#EEE9E9",
    "snow3": "#CDC9C9",
    "snow4": "#8B8989",
    springgreen: "#00FF7F",
    "springgreen1": "#00FF7F",
    "springgreen2": "#00EE76",
    "springgreen3": "#00CD66",
    "springgreen4": "#008B45",
    steelblue: "#4682B4",
    "steelblue1": "#63B8FF",
    "steelblue2": "#5CACEE",
    "steelblue3": "#4F94CD",
    "steelblue4": "#36648B",
    tan: "#D2B48C",
    "tan1": "#FFA54F",
    "tan2": "#EE9A49",
    "tan3": "#CD853F",
    "tan4": "#8B5A2B",
    thistle: "#D8BFD8",
    "thistle1": "#FFE1FF",
    "thistle2": "#EED2EE",
    "thistle3": "#CDB5CD",
    "thistle4": "#8B7B8B",
    tomato: "#FF6347",
    "tomato1": "#FF6347",
    "tomato2": "#EE5C42",
    "tomato3": "#CD4F39",
    "tomato4": "#8B3626",
    turquoise: "#40E0D0",
    "turquoise1": "#00F5FF",
    "turquoise2": "#00E5EE",
    "turquoise3": "#00C5CD",
    "turquoise4": "#00868B",
    violet: "#EE82EE",
    violetred: "#D02090",
    "violetred1": "#FF3E96",
    "violetred2": "#EE3A8C",
    "violetred3": "#CD3278",
    "violetred4": "#8B2252",
    wheat: "#F5DEB3",
    "wheat1": "#FFE7BA",
    "wheat2": "#EED8AE",
    "wheat3": "#CDBA96",
    "wheat4": "#8B7E66",
    whitesmoke: "#F5F5F5",
    yellow: "#FFFF00",
    "yellow1": "#FFFF00",
    "yellow2": "#EEEE00",
    "yellow3": "#CDCD00",
    "yellow4": "#8B8B00",
    yellowgreen: "#9ACD32"
  };
  var R_PALETTE = ["#000000", "#DF536B", "#61D04F", "#2297E6", "#28E2E5", "#CD0BBC", "#F5C710", "#9E9E9E"];

  // src/lib/compile/genes.js
  function setPath(obj, path, value) {
    const last = path[path.length - 1];
    let node = obj;
    for (const key of path.slice(0, -1)) {
      if (!isNamedList(node[key])) {
        if (value === null || value === void 0) return;
        node[key] = {};
      }
      node = node[key];
    }
    if (value === null || value === void 0) delete node[last];
    else node[last] = value;
  }
  function recycleDots(options, extra, i) {
    for (const [name, value] of Object.entries(extra)) {
      const recycled = recycleAt(value, i);
      if (recycled === void 0) delete options[name];
      else options[name] = recycled;
    }
    return options;
  }
  function setClusterOptions(state, clusters, slot, optionsFor) {
    getUpdatedClusters(state, clusters).forEach((name, i) => {
      const series = state.x.series.get(name);
      series[slot] = optionsFor(i, series);
    });
  }
  var columnsOf = (x) => Array.isArray(x) ? x : [x];
  function checkColumns(state, columns, message) {
    const names = state.x.data.names;
    if (!columnsOf(columns).every((column) => names.includes(column))) throw new Error(message);
  }
  var LABELS = ["label", "show", "clusters", "itemStyle", "groupStyle", "styleBy"];
  function labels(state, entry) {
    let label = entry.label ?? null;
    const show = entry.show ?? true;
    if (label === null && state.x.group === void 0) throw new Error("Please define labels");
    if (label === null) label = state.x.group;
    checkColumns(state, label, "label column not found in data");
    const extra = dots(entry, LABELS);
    getUpdatedClusters(state, entry.clusters).forEach((name, i) => {
      const series = state.x.series.get(name);
      const options = compact({
        label: recycleAt(label, i),
        show: recycleAt(show, i),
        itemStyle: entry.itemStyle,
        groupStyle: entry.groupStyle,
        styleBy: entry.styleBy,
        ...extra
      });
      series.labels = mergeOptions2(series.labels, options);
    });
  }
  function keepColorOptions(options, old) {
    if (options.colorScheme == null && options.customColors == null) {
      setPath(options, ["colorScheme"], old?.colorScheme);
      setPath(options, ["customColors"], old?.customColors);
    }
    if (options.pattern == null) setPath(options, ["pattern"], old?.pattern);
    return options;
  }
  function setLegendColors(state, colorScheme, customColors) {
    if (colorScheme == null && customColors == null) return;
    setPath(state.x, ["legend", "legendOptions", "colorScheme"], colorScheme);
    setPath(state.x, ["legend", "legendOptions", "customColors"], customColors);
  }
  var GENES = [
    "group",
    "marker",
    "markerSize",
    "show",
    "colorScheme",
    "customColors",
    "clusters",
    "itemStyle",
    "groupStyle",
    "styleBy",
    "cornerRadius"
  ];
  function genes(state, entry) {
    let group = entry.group ?? null;
    const show = entry.show ?? true;
    if (group === null && state.x.group !== void 0) group = state.x.group;
    if (group !== null) checkColumns(state, group, "group column not found in data");
    const extra = dots(entry, GENES);
    getUpdatedClusters(state, entry.clusters).forEach((name, i) => {
      const series = state.x.series.get(name);
      const options = compact({
        group: recycleAt(group, i),
        marker: recycleAt(entry.marker, i),
        cornerRadius: entry.cornerRadius,
        markerSize: entry.markerSize,
        show: recycleAt(show, i),
        colorScheme: entry.colorScheme,
        customColors: entry.customColors,
        itemStyle: entry.itemStyle,
        groupStyle: entry.groupStyle,
        styleBy: entry.styleBy,
        ...extra
      });
      series.genes = keepColorOptions(options, series.genes);
    });
    if (entry.clusters == null) setLegendColors(state, entry.colorScheme, entry.customColors);
  }
  function color(state, entry) {
    const colorScheme = entry.colorScheme ?? null;
    const customColors = entry.customColors ?? null;
    if (colorScheme !== null && customColors !== null && !isNamedList(customColors)) {
      state.warnings.push("Both colorScheme and customColors are selected. Use a named list of customColors to set the colours of some groups, or choose only one.");
      return;
    }
    setPath(state.x, ["legend", "legendOptions", "colorScheme"], colorScheme);
    setPath(state.x, ["legend", "legendOptions", "customColors"], customColors);
    for (const series of state.x.series.values()) {
      for (const slot of ["genes", "transcript"]) {
        setPath(series, [slot, "colorScheme"], colorScheme);
        setPath(series, [slot, "customColors"], customColors);
      }
    }
  }
  var PATTERNS = [
    "none",
    "solid",
    "horizontal",
    "vertical",
    "diagonal",
    "antidiagonal",
    "grid",
    "crosshatch",
    "dots",
    "zigzag",
    "checkerboard",
    "rings",
    "bricks",
    "plus",
    "wideStripes",
    "inverseDots"
  ];
  function asCssColor(col) {
    if (col === null || col === void 0) return "#FFFFFF00";
    const text = String(col);
    const hex2 = (n) => n.toString(16).toUpperCase().padStart(2, "0");
    let match = /^#([0-9a-f]{3,4}|[0-9a-f]{6}|[0-9a-f]{8})$/i.exec(text);
    if (match) {
      let digits = match[1];
      if (digits.length <= 4) digits = digits.split("").map((d) => d + d).join("");
      digits = digits.toUpperCase();
      return digits.length === 8 && digits.endsWith("FF") ? `#${digits.slice(0, 6)}` : `#${digits}`;
    }
    const name = text.toLowerCase();
    if (name === "transparent") return "#FFFFFF00";
    if (R_COLORS[name]) return R_COLORS[name];
    if (/^[1-9][0-9]*$/.test(text)) return R_PALETTE[(Number(text) - 1) % R_PALETTE.length];
    return col;
  }
  function patternToJs(pattern2) {
    const named = isNamedList(pattern2.patterns) ? Object.values(pattern2.patterns) : [pattern2.patterns ?? []].flat();
    const unknown = named.filter((p) => p !== null && !PATTERNS.includes(p));
    if (unknown.length) {
      throw new Error(`Unknown pattern(s): ${unknown.join(", ")}. Available patterns: ${PATTERNS.join(", ")}`);
    }
    const toCss = (x) => {
      if (x === null || x === void 0) return void 0;
      if (isNamedList(x)) return Object.fromEntries(Object.entries(x).map(([k, v]) => [k, asCssColor(v)]));
      if (Array.isArray(x)) return x.map(asCssColor);
      return asCssColor(x);
    };
    const out = {};
    for (const [key, value] of Object.entries({
      patterns: pattern2.patterns,
      color: toCss(pattern2.color),
      background: toCss(pattern2.background),
      size: pattern2.size,
      strokeWidth: pattern2.strokeWidth
    })) {
      if (value !== null && value !== void 0) out[key] = value;
    }
    return out;
  }
  function pattern(state, entry) {
    const settings = patternToJs({
      patterns: entry.patterns,
      color: arg(entry, "color", "black"),
      background: arg(entry, "background", "white"),
      size: arg(entry, "size", 6),
      strokeWidth: arg(entry, "strokeWidth", 1)
    });
    setPath(state.x, ["legend", "legendOptions", "pattern"], settings);
    for (const series of state.x.series.values()) {
      series.genes.pattern = settings;
      series.transcript.pattern = settings;
    }
  }
  var LEGEND = [
    "group",
    "show",
    "backgroundColor",
    "order",
    "position",
    "align",
    "style",
    "markerStyle",
    "textStyle",
    "cornerRadius",
    "groupStyle"
  ];
  function legend(state, entry) {
    let group = entry.group ?? null;
    const show = entry.show ?? true;
    if (show === true && group === null && state.x.group === void 0) throw new Error("Please define a group");
    if (group === null && state.x.group !== void 0) group = state.x.group;
    if (group !== null) checkColumns(state, group, "group column not found in data");
    const style = { ...entry.style ?? {} };
    if (entry.backgroundColor != null && style.backgroundColor == null) style.backgroundColor = entry.backgroundColor;
    const options = compact({
      group,
      show,
      order: entry.order,
      position: entry.position,
      align: entry.align,
      cornerRadius: entry.cornerRadius,
      style,
      legendOptions: entry.markerStyle,
      legendTextOptions: entry.textStyle,
      groupStyle: entry.groupStyle,
      ...dots(entry, LEGEND)
    });
    state.x.legend = mergeOptions2(state.x.legend, options);
  }
  var SEQUENCE = ["show", "clusters", "y", "lineStyle", "markerStyle"];
  function shiftSequence(options, shift) {
    if (!options || !shift) return options;
    for (const name of ["start", "end"]) {
      if (typeof options[name] === "number") options[name] -= shift;
    }
    return options;
  }
  function sequence(state, entry) {
    const show = entry.show ?? true;
    const extra = dots(entry, SEQUENCE);
    setClusterOptions(state, entry.clusters, "sequence", (i, series) => {
      const options = {
        show: recycleAt(show, i),
        y: recycleAt(entry.y, i) ?? null,
        sequenceStyle: entry.lineStyle ?? {},
        markerStyle: entry.markerStyle ?? {}
      };
      return shiftSequence(recycleDots(options, extra, i), series.positionShift);
    });
  }
  var COORDINATES = [
    "show",
    "tickValuesTop",
    "tickValuesBottom",
    "ticksFormat",
    "tickStyle",
    "textStyle",
    "clusters",
    "groupStyle",
    "styleBy"
  ];
  function coordinates(state, entry) {
    const show = entry.show ?? true;
    let styleBy = entry.styleBy ?? null;
    if (styleBy === null && state.x.group !== void 0) styleBy = state.x.group;
    if (entry.groupStyle != null && styleBy === null) {
      throw new Error("groupStyle needs a group in GC_chart() or a style_by column");
    }
    const extra = dots(entry, COORDINATES);
    setClusterOptions(state, entry.clusters, "coordinates", (i) => {
      const options = {
        show: recycleAt(show, i),
        ticksFormat: entry.ticksFormat ?? null,
        tickStyle: entry.tickStyle ?? {},
        textStyle: entry.textStyle ?? {}
      };
      if (entry.groupStyle != null) {
        options.groupStyle = entry.groupStyle;
        options.styleBy = styleBy;
      }
      recycleDots(options, extra, i);
      setPath(options, ["tickValuesTop"], entry.tickValuesTop);
      setPath(options, ["tickValuesBottom"], entry.tickValuesBottom);
      return options;
    });
  }
  var TOOLTIP = ["formatter", "show", "clusters"];
  function tooltip(state, entry) {
    const formatter = entry.formatter ?? "<b>Start:</b> {start}<br><b>End:</b> {end}";
    const show = entry.show ?? true;
    const extra = dots(entry, TOOLTIP);
    setClusterOptions(state, entry.clusters, "tooltip", (i) => recycleDots({ formatter, show: recycleAt(show, i) }, extra, i));
  }

  // src/lib/compile/names.js
  var OPTION_NAMES = {
    scaleBar: { unit: "scaleBarUnit" },
    labels: { labelAdjustment: "labelAdjustmentOptions" },
    links: { labelAdjustment: "labelAdjustmentOptions" }
  };
  var THEME_NAMES = {
    legend: { markerStyle: "legendOptions", textStyle: "legendTextOptions" },
    sequence: { lineStyle: "sequenceStyle" },
    scaleBar: { lineStyle: "scaleBarLineStyle", tickStyle: "scaleBarTickStyle", unit: "scaleBarUnit" },
    links: { show: "showLinks", labelAdjustment: "labelAdjustmentOptions" },
    labels: { labelAdjustment: "labelAdjustmentOptions" },
    scale: { autoBreaks: "scaleBreaks", autoBreakThreshold: "scaleBreakThreshold", breakPadding: "scaleBreakPadding" }
  };
  function renameOptions(options, names) {
    if (!names || options === null || typeof options !== "object" || Array.isArray(options)) return options;
    return Object.fromEntries(Object.entries(options).map(([key, value]) => [names[key] ?? key, value]));
  }

  // src/lib/compile/theme.js
  var THEME_COMPONENTS = {
    chart: "chartOptions",
    grid: "containerOptions",
    cluster: "clusterOptions",
    title: "titleOptions",
    clusterTitle: "clusterTitleOptions",
    clusterFooter: "footerOptions",
    clusterLabel: "clusterLabelOptions",
    sequence: "sequenceOptions",
    genes: "geneOptions",
    transcript: "transcriptOptions",
    labels: "labelsOptions",
    coordinates: "coordinatesOptions",
    scale: "scaleOptions",
    scaleBar: "scaleBarOptions",
    legend: "legendOptions",
    links: "linkOptions",
    tooltip: "tooltipOptions",
    trackMouse: "trackMouseOptions",
    toolbar: "toolbarOptions"
  };
  var ANNOTATION_TYPES = {
    text: "textAnnotationOptions",
    line: "lineAnnotationOptions",
    arrow: "arrowAnnotationOptions",
    textMarker: "textMarkerAnnotationOptions",
    symbol: "symbolAnnotationOptions",
    rectangle: "rectangleAnnotationOptions",
    promoter: "promoterAnnotationOptions",
    terminator: "terminatorAnnotationOptions",
    arc: "curveAnnotationOptions"
  };
  var builtinThemes = themes_default.themes;
  var palettes = themes_default.palettes;
  var snakeToCamel = (name) => name.replace(/_([a-z])/g, (_, letter) => letter.toUpperCase());
  function camelCaseNames(options) {
    if (!isNamedList(options)) return options;
    return Object.fromEntries(Object.entries(options).map(([name, value]) => [snakeToCamel(name), value]));
  }
  function themeNames(theme2) {
    const out = { ...theme2 };
    for (const name of Object.keys(out)) {
      if (name === "annotations") {
        if (isNamedList(out[name])) {
          out[name] = Object.fromEntries(Object.entries(out[name]).map(([type, o]) => [type, camelCaseNames(o)]));
        }
      } else if (name in THEME_COMPONENTS) {
        out[name] = camelCaseNames(out[name]);
      }
    }
    return out;
  }
  function mergeThemeSettings(x, y) {
    const out = { ...x };
    for (const [name, value] of Object.entries(y || {})) {
      if (value === null || value === void 0) delete out[name];
      else if (isNamedList(out[name]) && isNamedList(value)) out[name] = mergeThemeSettings(out[name], value);
      else out[name] = value;
    }
    return out;
  }
  function theme(state, entry) {
    const settings = typeof entry === "string" ? { base: entry } : clone(entry);
    const name = settings.base;
    delete settings.base;
    let base;
    if (name == null) {
      base = state.x.theme || {};
    } else {
      if (!(name in builtinThemes)) {
        throw new Error(`name must be one of: ${Object.keys(builtinThemes).map((t) => `"${t}"`).join(", ")}`);
      }
      base = themeNames(clone(builtinThemes[name]));
    }
    for (const name2 of Object.keys(settings)) {
      if (name2 in THEME_NAMES) settings[name2] = renameOptions(settings[name2], THEME_NAMES[name2]);
    }
    const removePattern = settings.pattern === false;
    if (removePattern) delete settings.pattern;
    state.x.theme = mergeThemeSettings(base, settings);
    if (removePattern) delete state.x.theme.pattern;
  }
  function dropEmpty(x) {
    if (!isNamedList(x) && !Array.isArray(x)) return x;
    if (Array.isArray(x)) return x.map(dropEmpty).filter((y) => !((isNamedList(y) || Array.isArray(y)) && !Object.keys(y).length));
    const out = {};
    for (const [key, value] of Object.entries(x)) {
      const y = dropEmpty(value);
      if ((isNamedList(y) || Array.isArray(y)) && Object.keys(y).length === 0) continue;
      out[key] = y;
    }
    return out;
  }
  function globalOptions(theme2) {
    const font = theme2.fontFamily != null ? { fontFamily: theme2.fontFamily } : {};
    const text = { ...font, ...theme2.textColor != null ? { fill: theme2.textColor } : {} };
    const htmlText = { ...font, ...theme2.textColor != null ? { color: theme2.textColor } : {} };
    const line = theme2.lineColor != null ? { stroke: theme2.lineColor } : {};
    let palette = theme2.palette ?? null;
    if (palette !== null && !(typeof palette === "string" && (palette.startsWith("scheme") || palette in palettes))) {
      palette = Array.isArray(palette) ? palette : [palette];
    }
    const colors = palette !== null ? { colorScheme: palette } : {};
    const pattern2 = theme2.pattern != null ? { pattern: patternToJs(theme2.pattern) } : {};
    const background = theme2.background != null ? { backgroundColor: theme2.background } : {};
    return dropEmpty({
      chart: { style: background },
      title: { titleFont: text, subtitleFont: text },
      clusterTitle: { titleFont: text, subtitleFont: text },
      clusterFooter: { titleFont: text, subtitleFont: text },
      clusterLabel: { ...text, subtitleFont: text },
      sequence: { sequenceStyle: line, markerStyle: line },
      genes: { ...colors, ...pattern2 },
      // UTRs are open boxes, filled with the background
      transcript: {
        ...colors,
        ...pattern2,
        labelOptions: htmlText,
        styleUTRs: theme2.background != null ? { fill: theme2.background } : {}
      },
      labels: { ...text, leaderLine: line },
      coordinates: { textStyle: text, tickStyle: line },
      scale: { textStyle: text, tickStyle: line, lineStyle: line },
      scaleBar: { labelStyle: text, scaleBarLineStyle: line, scaleBarTickStyle: line },
      legend: { legendOptions: { ...colors, ...pattern2 }, legendTextOptions: text },
      links: { labelStyle: text, colorBarOptions: { labelOptions: text, titleOptions: text } },
      tooltip: htmlText,
      trackMouse: htmlText,
      toolbar: font,
      annotations: { text: { style: text }, textMarker: { textStyle: text }, arc: { textStyle: text } }
    });
  }
  function themeToJs(theme2) {
    if (!theme2 || Object.keys(theme2).length === 0) return void 0;
    const components = {};
    for (const name of Object.keys(theme2)) {
      if (name in THEME_COMPONENTS) components[name] = camelCaseNames(theme2[name]);
    }
    if (theme2.annotations != null) {
      components.annotations = Object.fromEntries(Object.entries(theme2.annotations).map(([type, options2]) => [type, camelCaseNames(options2)]));
    }
    const options = mergeThemeSettings(globalOptions(theme2), components);
    const js = {};
    for (const [name, key] of Object.entries(THEME_COMPONENTS)) {
      if (name in options) js[key] = options[name];
    }
    for (const type of Object.keys(options.annotations || {})) {
      if (type in ANNOTATION_TYPES) js[ANNOTATION_TYPES[type]] = options.annotations[type];
    }
    if (theme2.textScale != null) js.textScale = theme2.textScale;
    return js;
  }

  // src/lib/compile/render.js
  var SEQUENCE_COLUMNS = ["translation", "sequence"];
  function optionStrings(x, out = []) {
    if (x instanceof Frame) return out;
    if (x instanceof Map) {
      for (const value of x.values()) optionStrings(value, out);
    } else if (Array.isArray(x)) {
      for (const value of x) optionStrings(value, out);
    } else if (x !== null && typeof x === "object") {
      for (const value of Object.values(x)) optionStrings(value, out);
    } else if (typeof x === "string") {
      out.push(x);
    }
    return out;
  }
  function compactData(x) {
    if (!x.data) return;
    const options = { ...x, data: void 0, series: /* @__PURE__ */ new Map() };
    for (const [name, series] of x.series) options.series.set(name, { ...series, data: void 0 });
    const strings = optionStrings(options);
    const names = x.data.names;
    const unused = SEQUENCE_COLUMNS.filter((column) => names.includes(column) && !strings.some((text) => text.includes(column)));
    x.dataColumns = names.filter((name) => !unused.includes(name));
    delete x.data;
    for (const series of x.series.values()) {
      if (series.data instanceof Frame) {
        series.data = series.data.select(series.data.names.filter((name) => !unused.includes(name)));
      }
    }
  }
  function resolvePaletteNames(x) {
    if (x instanceof Map) {
      for (const [key, value] of x) x.set(key, resolvePaletteNames(value));
      return x;
    }
    if (x === null || typeof x !== "object" || x instanceof Frame) return x;
    for (const [key, value] of Object.entries(x)) {
      if (key === "colorScheme" && typeof value === "string" && palettes[value]) {
        x[key] = palettes[value].slice();
      } else if (value !== null && typeof value === "object") {
        x[key] = resolvePaletteNames(value);
      }
    }
    return x;
  }
  function renderTheme(state) {
    const x = state.x;
    x.size = { width: state.width };
    const theme2 = themeToJs(x.theme);
    if (theme2 === void 0) delete x.theme;
    else x.theme = theme2;
    resolvePaletteNames(x);
    compactData(x);
    return x;
  }

  // src/lib/compile/layout.js
  var TITLE = ["title", "subtitle", "style", "subtitleFont", "titleFont", "show", "height", "align", "spacing"];
  function title(state, entry) {
    const show = entry.show ?? true;
    if (!show || entry.title == null && entry.subtitle == null) return;
    const options = compact({
      title: entry.title,
      subtitle: entry.subtitle,
      subtitleFont: entry.subtitleFont,
      titleFont: entry.titleFont,
      style: entry.style,
      height: entry.height,
      show,
      align: entry.align,
      spacing: entry.spacing,
      ...dots(entry, TITLE)
    });
    state.x.title = mergeOptions2(state.x.title, options);
  }
  var CLUSTER_TITLE = [
    "title",
    "subtitle",
    "subtitleFont",
    "titleFont",
    "show",
    "height",
    "clusters",
    "align",
    "spacing"
  ];
  function clusterTitle(state, entry) {
    const show = entry.show ?? true;
    const extra = dots(entry, CLUSTER_TITLE);
    getUpdatedClusters(state, entry.clusters).forEach((name, i) => {
      const series = state.x.series.get(name);
      const options = {
        title: recycleAt(entry.title, i) ?? null,
        subtitle: recycleAt(entry.subtitle, i) ?? null,
        subtitleFont: entry.subtitleFont ?? {},
        titleFont: entry.titleFont ?? {},
        show: recycleAt(show, i)
      };
      setPath(options, ["align"], recycleAt(entry.align, i));
      setPath(options, ["spacing"], recycleAt(entry.spacing, i));
      recycleDots(options, extra, i);
      setPath(series, ["grid", "margin", "top"], recycleAt(entry.height, i));
      series.clusterTitle = options;
    });
  }
  var CLUSTER_LABEL = [
    "title",
    "show",
    "width",
    "clusters",
    "position",
    "align",
    "wrapLabel",
    "wrapOptions",
    "subtitle",
    "subtitleFont",
    "spacing"
  ];
  function clusterLabel(state, entry) {
    const show = entry.show ?? true;
    const extra = dots(entry, CLUSTER_LABEL);
    const clusters = getUpdatedClusters(state, entry.clusters);
    let labelTitle = entry.title ?? null;
    if (state.x.cluster !== void 0 && labelTitle === null) labelTitle = clusters;
    clusters.forEach((name, i) => {
      const series = state.x.series.get(name);
      const options = dropNull({
        title: recycleAt(labelTitle, i),
        show: recycleAt(show, i),
        width: recycleAt(entry.width, i),
        wrapLabel: entry.wrapLabel,
        wrapOptions: entry.wrapOptions ?? {},
        position: recycleAt(entry.position, i),
        align: recycleAt(entry.align, i),
        subtitle: recycleAt(entry.subtitle, i),
        spacing: recycleAt(entry.spacing, i)
      });
      if (!isEmpty(entry.subtitleFont)) options.subtitleFont = entry.subtitleFont;
      series.clusterLabel = recycleDots(options, extra, i);
    });
  }
  var CLUSTER_FOOTER = ["title", "subtitle", "show", "clusters", "subtitleFont", "titleFont", "align", "spacing"];
  function clusterFooter(state, entry) {
    const show = entry.show ?? true;
    const extra = dots(entry, CLUSTER_FOOTER);
    setClusterOptions(state, entry.clusters, "footer", (i) => {
      const options = {
        title: recycleAt(entry.title, i) ?? null,
        subtitle: recycleAt(entry.subtitle, i) ?? null,
        show: recycleAt(show, i),
        subtitleFont: entry.subtitleFont ?? {},
        titleFont: entry.titleFont ?? {}
      };
      setPath(options, ["align"], recycleAt(entry.align, i));
      setPath(options, ["spacing"], recycleAt(entry.spacing, i));
      return recycleDots(options, extra, i);
    });
  }
  function round(x, digits) {
    const factor = 10 ** digits;
    return Math.round(x * factor) / factor;
  }
  function grid(state, entry) {
    const x = state.x;
    const allClusters = [...x.series.keys()];
    const updateClusters = getUpdatedClusters(state, entry.clusters);
    const chartHeight = state.height;
    const perCluster = entry.clusters != null;
    let margin = entry.margin == null ? null : { ...entry.margin };
    const width = entry.width ?? null;
    const height = entry.height ?? null;
    if (width !== null && !perCluster) state.width = getRelativeHeight(state.width, width);
    if ((entry.direction ?? "column") === "row") {
      x.graphContainer.direction = "row";
      if (updateClusters.length > 0) {
        const percent = `${asCharacter(round(100 / updateClusters.length, 3))}%`;
        for (const name of updateClusters) {
          const container2 = x.series.get(name).container;
          container2.style = { ...container2.style || {}, width: percent };
          container2.width = percent;
        }
      }
    }
    if (margin !== null && !perCluster) {
      const paddingTop = margin.top != null ? getRelativeHeight(chartHeight, margin.top) : 0;
      const paddingBottom = margin.bottom != null ? getRelativeHeight(chartHeight, margin.bottom) : 0;
      if (typeof paddingTop !== "number" || typeof paddingBottom !== "number") {
        throw new Error("Use pixel values for the top and bottom margins when the chart height is a percentage.");
      }
      delete margin.top;
      delete margin.bottom;
      x.legend.margin = mergeOptions2(x.legend.margin, margin);
      x.title.margin = mergeOptions2(x.title.margin, margin);
      if (Array.isArray(x.style)) x.style = {};
      if (paddingTop > 0) x.style.paddingTop = `${asCharacter(paddingTop)}px`;
      if (paddingBottom > 0) x.style.paddingBottom = `${asCharacter(paddingBottom)}px`;
      if (paddingTop > 0 || paddingBottom > 0) {
        state.height = "100%";
        for (const name of allClusters) {
          const container2 = x.series.get(name).container;
          if (typeof container2.height === "string" && /%$/.test(container2.height)) continue;
          const current = Number(getRelativeHeight(container2.height, container2.height));
          container2.height = current - (paddingTop + paddingBottom) / allClusters.length;
        }
      }
    }
    updateClusters.forEach((name, i) => {
      const container2 = x.series.get(name).container;
      if (margin !== null) container2.margin = mergeOptions2(container2.margin, margin);
      if (width !== null && perCluster) {
        const current = getRelativeHeight(state.width, recycleAt(width, i));
        container2.style = { ...container2.style || {}, width: current };
        container2.width = current;
      }
      if (height !== null) {
        container2.height = perCluster ? getRelativeHeight(chartHeight, recycleAt(height, i)) : computeSize2(height, allClusters.length);
      }
    });
  }

  // src/lib/compile/scale.js
  function breakPaddingBp(padding) {
    if (!/^\s*[0-9]*\.?[0-9]+\s*bp\s*$/.test(padding)) {
      throw new Error('scale_break_padding must be a percentage, e.g. 1, or base pairs, e.g. "500bp"');
    }
    return Number(padding.replace(/\s*bp\s*$/, ""));
  }
  function getScaleBreaks(genes2, thresholdPercentage = 0, padding = 0) {
    let starts = genes2.get("start").slice();
    let ends = genes2.get("end").slice();
    starts.forEach((s, i) => {
      if (s > ends[i]) [starts[i], ends[i]] = [ends[i], s];
    });
    const entire = Math.max(...ends) - Math.min(...starts) + 1;
    const minGap = entire * thresholdPercentage / 100;
    const sorted = order(starts.length, [(i) => starts[i]]);
    starts = sorted.map((i) => starts[i]);
    ends = sorted.map((i) => ends[i]);
    const merged = [];
    let current = { start: starts[0], end: ends[0] };
    for (let i = 1; i < starts.length; i++) {
      if (starts[i] <= current.end) {
        current.end = Math.max(current.end, ends[i]);
      } else {
        merged.push(current);
        current = { start: starts[i], end: ends[i] };
      }
    }
    merged.push(current);
    let gaps = [];
    for (let i = 0; i < merged.length - 1; i++) {
      const start = merged[i].end + 1;
      const end = merged[i + 1].start - 1;
      if (end - start + 1 >= minGap) gaps.push({ start, end });
    }
    let pad;
    if (typeof padding === "string") {
      pad = breakPaddingBp(padding);
    } else {
      const shown = entire - gaps.reduce((sum, gap) => sum + (gap.end - gap.start + 1), 0);
      pad = shown * padding / 100;
    }
    gaps = gaps.map((gap) => ({ start: gap.start + pad, end: gap.end - pad }));
    return gaps.filter((gap) => gap.start < gap.end);
  }
  function clusterBreaks(breaks, cluster2, shift) {
    if (isNamedList(breaks)) {
      if (!("start" in breaks) || !("end" in breaks)) throw new Error("A data frame of breaks needs a start and an end column");
      const starts = [breaks.start].flat();
      const ends = [breaks.end].flat();
      let rows = starts.map((_, k) => k);
      if ("cluster" in breaks) {
        const clusters = [breaks.cluster].flat();
        rows = rows.filter((k) => asCharacter(clusters[k]) === cluster2);
      }
      breaks = rows.map((k) => ({ start: starts[k], end: ends[k] }));
    }
    if (!shift) return breaks;
    return (breaks || []).map((gap) => ({ ...gap, start: gap.start - shift, end: gap.end - shift }));
  }
  function shiftAnnotation(options, shift) {
    if (!shift) return options;
    const move = (x) => typeof x === "number" ? x - shift : x;
    const moveAll = (x) => Array.isArray(x) ? x.map(move) : move(x);
    for (const name of ["x", "x1", "x2"]) {
      if (name in options) options[name] = moveAll(options[name]);
    }
    if (options.position !== null && options.position !== void 0) {
      if (options.type === "rectangle") {
        const rectangles = Array.isArray(options.position) ? options.position : [options.position];
        options.position = rectangles.map((corners) => {
          if (!Array.isArray(corners)) return [move(corners)];
          return corners.map((point) => Array.isArray(point) ? [move(point[0]), ...point.slice(1)] : move(point));
        });
      } else {
        options.position = moveAll(options.position);
      }
    }
    return options;
  }
  var SCALE = [
    "clusters",
    "start",
    "end",
    "padding",
    "hidden",
    "breaks",
    "tickValues",
    "reverse",
    "axisPosition",
    "axisType",
    "y",
    "autoBreaks",
    "autoBreakThreshold",
    "breakPadding",
    "ticksCount",
    "ticksFormat",
    "tickStyle",
    "textStyle",
    "lineStyle"
  ];
  function scale(state, entry) {
    const x = state.x;
    const axisType = entry.axisType ?? "position";
    const hidden = entry.hidden ?? false;
    const autoBreaks = entry.autoBreaks ?? false;
    const threshold = entry.autoBreakThreshold ?? 20;
    const breakPadding = entry.breakPadding ?? 1;
    let ticksFormat = entry.ticksFormat ?? null;
    if (typeof breakPadding === "string") breakPaddingBp(breakPadding);
    let xMin = null;
    let xMax = null;
    const clusters = getUpdatedClusters(state, entry.clusters);
    if (axisType === "range") {
      if (ticksFormat === null) ticksFormat = ".2s";
      for (const name of clusters) {
        const series = x.series.get(name);
        const data = series.data;
        const minimum = Math.min(...data.get("start"), ...data.get("end"));
        data.set("start", data.get("start").map((v) => v - minimum + 1));
        data.set("end", data.get("end").map((v) => v - minimum + 1));
        const shift = minimum - 1;
        series.sequence = shiftSequence(series.sequence, shift);
        if (series.annotations.length > 0) {
          series.annotations = series.annotations.map((options) => shiftAnnotation(options, shift));
        }
        series.positionShift = series.positionShift == null ? shift : series.positionShift + shift;
        const rowIDs = x.data.get("rowID");
        const allStarts = x.data.get("start");
        const allEnds = x.data.get("end");
        data.get("rowID").forEach((id, k) => {
          const row = rowIDs.indexOf(id);
          allStarts[row] = data.get("start")[k];
          allEnds[row] = data.get("end")[k];
        });
      }
      xMin = 1;
      xMax = Math.max(...clusters.flatMap((name) => {
        const data = x.series.get(name).data;
        return [...data.get("start"), ...data.get("end")];
      }));
    }
    const extra = dots(entry, SCALE);
    clusters.forEach((name, i) => {
      const series = x.series.get(name);
      const hiddenCurrent = axisType === "range" && !hidden && i < clusters.length - 1 ? true : hidden;
      const breaks = autoBreaks ? getScaleBreaks(series.data, threshold, breakPadding) : clusterBreaks(entry.breaks ?? [], name, series.positionShift);
      const options = {
        start: recycleAt(entry.start, i) ?? null,
        end: recycleAt(entry.end, i) ?? null,
        xMin,
        xMax,
        padding: entry.padding ?? null,
        hidden: hiddenCurrent,
        breaks,
        tickValues: entry.tickValues ?? null,
        reverse: recycleAt(entry.reverse ?? false, i),
        axisPosition: entry.axisPosition ?? null,
        axisType,
        scale_breaks: autoBreaks,
        scale_break_threshold: threshold,
        scale_break_padding: breakPadding,
        ticksCount: entry.ticksCount ?? null,
        ticksFormat,
        y: entry.y ?? null,
        tickStyle: entry.tickStyle ?? {},
        textStyle: entry.textStyle ?? {},
        lineStyle: entry.lineStyle ?? {}
      };
      series.scale = recycleDots(options, extra, i);
    });
    applyAlign(state);
  }
  var SCALE_BAR = ["show", "clusters", "lineStyle", "tickStyle", "labelStyle"];
  function scaleBar(state, entry) {
    const show = entry.show ?? true;
    const extra = dots(entry, SCALE_BAR);
    setClusterOptions(state, entry.clusters, "scaleBar", (i) => recycleDots({
      show: recycleAt(show, i),
      scaleBarLineStyle: entry.lineStyle ?? {},
      scaleBarTickStyle: entry.tickStyle ?? {},
      labelStyle: entry.labelStyle ?? {}
    }, extra, i));
  }
  function alignPosition(align2, start, end, reversed) {
    if (align2 === "left" && !reversed || align2 === "right" && reversed) return start;
    if (align2 === "right" && !reversed || align2 === "left" && reversed) return end;
    if (align2 === "center") return (start + end) / 2;
    return start;
  }
  function align(state, entry) {
    const x = state.x;
    if (x.cluster === void 0) {
      state.warnings.push("Could not align selected gene. Please define cluster in the GC_chart function.");
      return;
    }
    if (!x.data.has(entry.idColumn)) {
      state.warnings.push(`Column ${entry.idColumn} not found in the data.`);
      return;
    }
    const id = asCharacter(entry.id);
    if (!x.data.get(entry.idColumn).some((value) => asCharacter(value) === id)) {
      state.warnings.push(`ID ${entry.id} not found in the ${entry.idColumn} column.`);
      return;
    }
    state.align = { idColumn: entry.idColumn, id: entry.id, align: entry.align ?? "left" };
    x.align = { id_column: entry.idColumn, id: entry.id, align: state.align.align };
    applyAlign(state);
  }
  function applyAlign(state) {
    const options = state.align;
    if (!options) return;
    const x = state.x;
    const data = x.data;
    const clusterColumn = x.cluster;
    const clusterOf = data.get(clusterColumn);
    const clusters = [...new Set(clusterOf)];
    const reversed = new Set(clusters.filter((cl) => x.series.get(cl)?.scale?.reverse === true));
    const starts = data.get("start").map((s, i) => Math.min(s, data.get("end")[i]));
    const ends = data.get("start").map((s, i) => Math.max(s, data.get("end")[i]));
    const ids = data.get(options.idColumn);
    const id = asCharacter(options.id);
    const rows = [];
    for (let i = 0; i < data.nrow; i++) if (ids[i] !== null && asCharacter(ids[i]) === id) rows.push(i);
    if (!rows.length) {
      state.warnings.push(`Selected gene coud not be aligned. Gene ${options.id} not found in the ${options.idColumn} column.`);
      return;
    }
    if (new Set(rows.map((i) => clusterOf[i])).size < rows.length) {
      state.warnings.push(`The selected gene ${options.id} appears multiple times in some clusters. Please make sure to use a unique gene id.`);
      return;
    }
    const range = /* @__PURE__ */ new Map();
    clusterOf.forEach((cl, i) => {
      const r = range.get(cl) || { start: Infinity, end: -Infinity };
      range.set(cl, { start: Math.min(r.start, starts[i]), end: Math.max(r.end, ends[i]) });
    });
    const gaps = (from, to, cl) => (x.series.get(cl)?.scale?.breaks || []).reduce((sum, gap) => sum + Math.max(0, Math.min(gap.end, to) - Math.max(gap.start, from)), 0);
    const genes2 = rows.map((i) => {
      const cl = clusterOf[i];
      const isReversed = reversed.has(cl);
      const position = alignPosition(options.align, starts[i], ends[i], isReversed);
      const { start: clusterStart, end: clusterEnd } = range.get(cl);
      const gapsBelow = gaps(clusterStart, position, cl);
      const gapsAbove = gaps(position, clusterEnd, cl);
      const below = position - clusterStart - gapsBelow;
      const above = clusterEnd - position - gapsAbove;
      return {
        cl,
        isReversed,
        position,
        gapsBelow,
        gapsAbove,
        left: isReversed ? above : below,
        right: isReversed ? below : above
      };
    });
    const maxLeft = Math.max(...genes2.map((g) => g.left));
    const maxRight = Math.max(...genes2.map((g) => g.right));
    for (const g of genes2) {
      const series = x.series.get(g.cl);
      series.scale.start = g.position - (g.isReversed ? maxRight : maxLeft) - g.gapsBelow;
      series.scale.end = g.position + (g.isReversed ? maxLeft : maxRight) + g.gapsAbove;
    }
  }

  // src/lib/compile/links.js
  var MEASURES = ["identity", "similarity", "none"];
  var LINKS = [
    "data",
    "group",
    "value1",
    "value2",
    "clusters",
    "curve",
    "measure",
    "show",
    "label",
    "labelPosition",
    "normalColor",
    "invertedColor",
    "lowColor",
    "vmin",
    "vmax",
    "useGroupColors",
    "colorBar",
    "colorBarOptions",
    "linkWidth",
    "linkStyle",
    "labelStyle"
  ];
  var COLOR_BAR_CHOICES = {
    position: ["bottomright", "bottomleft", "topright", "topleft", "legend"],
    orientation: ["vertical", "horizontal"],
    labels: ["inline", "below"],
    legendSide: ["left", "right"]
  };
  function rename(data, from, to) {
    if (!data.has(from)) return;
    data.columns = new Map([...data.columns].map(([name, values]) => [name === from ? to : name, values]));
  }
  function links(state, entry) {
    const measure = entry.measure ?? "identity";
    const labelPosition = entry.labelPosition ?? "gene";
    const colorBarOptions = entry.colorBarOptions ?? {};
    if (!MEASURES.includes(measure)) {
      throw new Error("Invalid measure specified. Choose 'identity', 'similarity', or 'none'.");
    }
    if (!["gene", "link"].includes(labelPosition)) throw new Error(`'label_position' must be one of "gene", "link".`);
    for (const [name, choices] of Object.entries(COLOR_BAR_CHOICES)) {
      const value = colorBarOptions[name];
      if (value != null && !choices.includes(value)) {
        throw new Error(`'colorBarOptions$${name}' must be one of ${choices.map((c) => `"${c}"`).join(", ")}.`);
      }
    }
    const vmin = entry.vmin ?? null;
    const vmax = entry.vmax ?? null;
    if ((vmin !== null || vmax !== null) && (vmin ?? 0) >= (vmax ?? 100)) {
      throw new Error("'vmin' must be smaller than 'vmax'.");
    }
    const group = entry.group ?? null;
    let label = entry.label ?? true;
    if (group === null && labelPosition === "gene") label = false;
    const data = Frame.from(entry.data ?? []);
    if (data.nrow === 0) return;
    if (data.has("identity2")) rename(data, "identity2", "identity");
    if (data.has("similarity2")) rename(data, "similarity2", "similarity");
    rename(data, `${group ?? ""}1`, "group1");
    rename(data, `${group ?? ""}2`, "group2");
    let columns = ["cluster1", "group1", "start1", "end1", "cluster2", "group2", "start2", "end2"];
    if (measure !== "none") columns.push(measure);
    const useGroupColors = entry.useGroupColors ?? false;
    let colorBar = entry.colorBar;
    if (useGroupColors) {
      const groupColor = state.x.group !== void 0 ? state.x.group : group;
      const values = data.get(`${groupColor ?? ""}2`);
      if (values) data.set("groupColor", values.slice());
      columns.push("groupColor");
      colorBar = false;
    }
    let pairs = data.select(columns.filter((name) => data.has(name)));
    const starts = pairs.get("start2") || [];
    pairs = pairs.rows(order(pairs.nrow, [(i) => starts[i]]));
    const options = compact({
      group,
      curve: entry.curve,
      measure,
      showLinks: entry.show ?? true,
      label,
      labelPosition,
      normalColor: entry.normalColor,
      invertedColor: entry.invertedColor,
      lowColor: entry.lowColor,
      vmin,
      vmax,
      useGroupColors,
      value1: entry.value1,
      value2: entry.value2,
      colorBar,
      colorBarOptions,
      linkWidth: entry.linkWidth,
      linkStyle: entry.linkStyle,
      labelStyle: entry.labelStyle,
      ...dots(entry, LINKS)
    });
    const current = state.x.links || [];
    pairs.set("linkID", Array.from({ length: pairs.nrow }, (_, i) => `${current.length}-${i + 1}`));
    state.x.links = [...current, { data: pairs, options }];
  }

  // src/lib/compile/transcript.js
  var TRANSCRIPT = [
    "transcript",
    "type",
    "strand",
    "group",
    "selection",
    "show",
    "colorScheme",
    "customColors",
    "styleExons",
    "styleIntrons",
    "styleUTRs",
    "itemStyleExons",
    "itemStyleIntrons",
    "itemStyleUTRs",
    "groupStyleExons",
    "groupStyleIntrons",
    "groupStyleUTRs",
    "styleBy",
    "labelOptions"
  ];
  function identical(a, b) {
    return JSON.stringify(a) === JSON.stringify(b);
  }
  function keepChartSettings(state, old) {
    for (const key of ["title", "legend", "theme", "events", "toolbar", "zoom"]) {
      if (old.x[key] === void 0) delete state.x[key];
      else state.x[key] = old.x[key];
    }
    state.width = old.width;
    const margins = [...old.x.series.values()].map((series) => series.container.margin);
    const same = margins.length > 0 && margins.every((m) => identical(m, margins[0]));
    let margin = same ? margins[0] : {};
    const style = Array.isArray(old.x.style) ? {} : old.x.style || {};
    if (style.paddingTop != null) margin = { ...margin || {}, top: style.paddingTop };
    if (style.paddingBottom != null) margin = { ...margin || {}, bottom: style.paddingBottom };
    if (margin && Object.keys(margin).length > 0) grid(state, { margin });
    if (old.x.graphContainer?.direction === "row") grid(state, { direction: "row" });
  }
  function transcript(state, entry) {
    if (!state.x.transcript_called) {
      const params = state.x.params;
      const old = { ...state, x: state.x };
      const fields = { start: params.start, end: params.end, cluster: entry.transcript ?? "transcript" };
      if (params.group !== void 0) fields.group = params.group;
      if (entry.strand != null) fields.strand = entry.strand;
      const rebuilt = chart({
        ...state.spec,
        fields,
        width: params.width,
        height: params.height,
        style: params.style
      });
      rebuilt.x.saveButton = params.save_button;
      rebuilt.x.params.save_button = params.save_button;
      state.x = rebuilt.x;
      state.width = rebuilt.width;
      state.height = rebuilt.height;
      keepChartSettings(state, old);
      const transcripts = [...new Set(state.x.data.get("transcript").map(String))];
      for (const name of transcripts) {
        const series = state.x.series.get(name);
        series.genes = { group: null, show: false };
        series.sequence = { show: false };
        series.transcript = { group: null, show: true };
      }
    }
    let group = entry.group ?? null;
    const show = entry.show ?? true;
    if (group === null && state.x.group !== void 0) group = state.x.group;
    if (group !== null && ![group].flat().every((g) => state.x.data.has(g))) {
      throw new Error("group column not found in data");
    }
    const extra = dots(entry, TRANSCRIPT);
    getUpdatedClusters(state, entry.selection).forEach((name, i) => {
      const series = state.x.series.get(name);
      const options = compact({
        group: recycleAt(group, i),
        show: recycleAt(show, i),
        colorScheme: entry.colorScheme,
        customColors: entry.customColors,
        styleExons: entry.styleExons,
        styleIntrons: entry.styleIntrons,
        styleUTRs: entry.styleUTRs,
        itemStyleExons: entry.itemStyleExons,
        itemStyleIntrons: entry.itemStyleIntrons,
        itemStyleUTRs: entry.itemStyleUTRs,
        groupStyleExons: entry.groupStyleExons,
        groupStyleIntrons: entry.groupStyleIntrons,
        groupStyleUTRs: entry.groupStyleUTRs,
        styleBy: entry.styleBy,
        labelOptions: entry.labelOptions,
        ...extra
      });
      series.transcript = keepColorOptions(options, series.transcript);
    });
    if (entry.selection == null) setLegendColors(state, entry.colorScheme, entry.customColors);
    state.x.transcript_called = true;
  }

  // src/lib/compile/annotations.js
  var ANNOTATION_TYPES2 = [
    "text",
    "textMarker",
    "line",
    "arrow",
    "symbol",
    "rectangle",
    "promoter",
    "terminator",
    "arc"
  ];
  function annotation(state, entry) {
    const type = entry.type ?? "text";
    if (typeof type !== "string" || !ANNOTATION_TYPES2.includes(type)) {
      throw new Error(`'type' must be one of ${ANNOTATION_TYPES2.map((t) => `"${t}"`).join(", ")}.`);
    }
    const extra = dots(entry, ["type", "clusters"]);
    for (const name of getUpdatedClusters(state, entry.clusters)) {
      const series = state.x.series.get(name);
      const options = shiftAnnotation(compact({ type, ...clone(extra) }), series.positionShift);
      series.annotations = [...series.annotations || [], options];
    }
  }
  var ROW_COMPONENTS = ["genes", "labels", "coordinates"];
  var CHART_COMPONENTS = ["legend", "title"];
  var SCALE_COMPONENTS = ["scaleBar", "scale"];
  var COMPONENTS = [
    "genes",
    "labels",
    "coordinates",
    "clusterTitle",
    "clusterFooter",
    "clusterLabel",
    "legend",
    "title",
    "scaleBar",
    "scale"
  ];
  function updateOverride(items, same, id, style) {
    const list = Array.isArray(items) ? items : [];
    const previous = list.find(same) || {};
    const kept = list.filter((item) => !same(item));
    const override = modifyList(previous, style);
    for (const [key, value] of Object.entries(id)) override[key] = value;
    return [...kept, override];
  }
  function elementStyle(state, entry) {
    const component = entry.component ?? "genes";
    if (!COMPONENTS.includes(component)) throw new Error(`'component' must be one of ${COMPONENTS.join(", ")}`);
    const { rowID = null, cluster: cluster2 = null, part = null, end = null, group = null } = entry;
    const style = clone(dots(entry, ["component", "rowID", "cluster", "part", "end", "group"]));
    const byRow = ROW_COMPONENTS.includes(component);
    const x = state.x;
    if (CHART_COMPONENTS.includes(component)) {
      if (component === "legend") {
        const column = x.legend?.group;
        if (group === null || column == null || !x.data.get(column)?.some((value) => String(value) === String(group))) {
          throw new Error("'group' must identify one group in the legend.");
        }
      }
      const id2 = component === "legend" ? { group, part } : { part };
      const same2 = (item) => item.part === part && (component === "title" || item.group === group);
      x[component].itemStyle = updateOverride(x[component].itemStyle, same2, id2, style);
      return;
    }
    const key = component === "clusterFooter" ? "footer" : component;
    let id;
    let same;
    let clusters;
    if (byRow) {
      if (typeof rowID !== "number" || !x.data.get("rowID").includes(rowID)) {
        throw new Error("'row_id' must identify one row in the chart's data.");
      }
      id = end === null ? { rowID } : { rowID, end };
      same = (item) => Number(item.rowID) === Number(rowID) && (item.end ?? null) === end;
      clusters = [...x.series.keys()];
    } else {
      id = SCALE_COMPONENTS.includes(component) ? {} : { part };
      same = (item) => (item.part ?? null) === part;
      clusters = getUpdatedClusters(state, cluster2);
      if (!clusters.length) throw new Error("'cluster' must be the name or number of a cluster of the chart.");
    }
    for (const name of clusters) {
      const series = x.series.get(name);
      if (byRow && !series.data.get("rowID").includes(rowID)) continue;
      if (!series[key] || Array.isArray(series[key])) series[key] = {};
      series[key].itemStyle = updateOverride(series[key].itemStyle, same, id, style);
    }
  }

  // src/lib/compile/interaction.js
  function trackMouse(state, entry) {
    const show = entry.show ?? true;
    const extra = dots(entry, ["show", "clusters"]);
    getUpdatedClusters(state, entry.clusters).forEach((name, i) => {
      state.x.series.get(name).trackMouse = { show: recycleAt(show, i), ...clone(extra) };
    });
  }
  function events(state, entry) {
    const settings = { click: entry.click ?? true, highlight: entry.highlight ?? false };
    if (entry.dblclick) settings.dblclick = true;
    if (entry.select) settings.select = true;
    if (entry.drag) settings.drag = true;
    if (entry.legendClick === false) settings.legendClick = false;
    if (entry.zoom) settings.zoom = true;
    if (entry.rendered) settings.rendered = true;
    if (entry.renderId != null) settings.renderId = entry.renderId;
    state.x.events = settings;
  }
  var TOOLBAR = [
    "show",
    "position",
    "buttons",
    "size",
    "color",
    "hoverColor",
    "tooltipBackground",
    "tooltipColor",
    "tooltipBorder",
    "tooltipRadius",
    "tooltipShadow",
    "fontFamily",
    "icons"
  ];
  function toolbar(state, entry) {
    const options = dropNull(Object.fromEntries(TOOLBAR.map((name) => [name, clone(entry[name])])));
    if (options.buttons !== void 0 && !Array.isArray(options.buttons)) options.buttons = [options.buttons];
    state.x.toolbar = modifyList(state.x.toolbar || {}, options);
  }
  var ZOOM = [
    "show",
    "range",
    "mode",
    "min",
    "update",
    "height",
    "overview",
    "color",
    "style",
    "windowStyle",
    "handleStyle"
  ];
  function zoom(state, entry) {
    const options = dropNull(Object.fromEntries(ZOOM.map((name) => [name, clone(entry[name])])));
    if (options.show === void 0) options.show = true;
    const old = { ...state.x.zoom || {} };
    if (options.mode !== void 0 && options.mode !== old.mode) {
      delete old.range;
      delete old.min;
    }
    if (options.range !== void 0) options.range = [options.range].flat().map(Number).sort((a, b) => a - b);
    state.x.zoom = modifyList(old, options);
  }

  // src/lib/compile/index.js
  var handlers = {
    cluster,
    labels,
    genes,
    color,
    pattern,
    legend,
    sequence,
    coordinates,
    tooltip,
    title,
    clusterTitle,
    clusterLabel,
    clusterFooter,
    grid,
    scale,
    scaleBar,
    align,
    links,
    transcript,
    annotation,
    elementStyle,
    theme,
    zoom,
    toolbar,
    events,
    trackMouse
  };
  var SPEC_VERSION = 1;
  var CHART_KEYS = /* @__PURE__ */ new Set(["version", "data", "fields", "width", "height", "style"]);
  function entries(value) {
    if (value === null || value === void 0) return [];
    return Array.isArray(value) ? value : [value];
  }
  function compile(spec) {
    if (!spec || typeof spec !== "object") throw new Error("compile() needs a spec object");
    if (typeof spec.version === "number" && spec.version > SPEC_VERSION) {
      console.warn(`geneviewer: spec version ${spec.version} is newer than this library's (${SPEC_VERSION}); drawing what it knows`);
    }
    spec = clone(spec);
    const state = chart(spec);
    for (const [key, value] of Object.entries(spec)) {
      if (CHART_KEYS.has(key)) continue;
      const apply = handlers[key];
      if (!apply) continue;
      for (const entry of entries(value)) apply(state, renameOptions(entry, OPTION_NAMES[key]));
    }
    return toModel(renderTheme(state));
  }
  function toModel(value) {
    if (value instanceof Frame) return value.toJSON();
    if (value instanceof Map) {
      const out = {};
      for (const [key, item] of value) out[key] = toModel(item);
      return out;
    }
    if (Array.isArray(value)) return value.map(toModel);
    if (value !== null && typeof value === "object") {
      const out = {};
      for (const [key, item] of Object.entries(value)) {
        if (item !== void 0) out[key] = toModel(item);
      }
      return out;
    }
    return value;
  }

  // src/lib/compile/merge.js
  var isObject = (x) => x !== null && typeof x === "object" && !Array.isArray(x);
  function mergeSpec(spec, partial) {
    const out = { ...spec };
    for (const [key, value] of Object.entries(partial || {})) {
      if (value === void 0) continue;
      out[key] = key !== "data" && isObject(out[key]) && isObject(value) ? mergeSpec(out[key], value) : structuredClone(value);
    }
    return out;
  }

  // src/styles.css
  var styles_default = `.geneviewer-container {
  display: flex;
  flex-direction: column; /* or row, depending on the desired layout */
  align-content: stretch;
}

.geneviewer-svg-content {
  flex: 1 1 auto; /* This makes the SVGs grow and shrink as needed */
  min-height: 0; /* This may be needed for correct sizing in some browsers */
}

.geneviewer .hovered {
  cursor: pointer !important;
}

.geneviewer-svg-content {
  z-index: 2;
}

/* Common styles */
.geneviewer .legend .legend-marker,
.geneviewer .legend .legend-label,
.geneviewer .label {
    transition: opacity 0.3s ease-in-out;  /* Fade on hover; only opacity, so rotated labels are not animated */
}

/* Hover styles: Gives a dim effect to the marker and text when hovered */
.geneviewer .legend.hovered .legend-marker,
.geneviewer .legend.hovered .legend-label,
.geneviewer .label.hovered {
    opacity: 0.7;
}

/* Highlighted/Clicked styles: Gives a more dimmed effect to both marker and text when clicked */
.geneviewer .legend.unselected .legend-marker,
.geneviewer .legend.unselected .legend-label,
.geneviewer-move-overlay .unselected > .legend-marker,
.geneviewer-move-overlay .unselected > .legend-label {
    opacity: 0.5;
}

.geneviewer .gene.hovered, .geneviewer .exon.hovered,
.geneviewer .utr.hovered, .geneviewer .intron.hovered {
  filter: brightness(80%);
}

/* Tooltip arrow; each tooltip sets --tooltip-bg to its background colour */
.cluster-tooltip::before {
  content: "";
  position: absolute;
  top: 100%;
  left: 50%;
  transform: translateX(-50%);
  border-width: 5px;
  border-style: solid;
  border-color: var(--tooltip-bg, white) transparent transparent transparent;
}

/* GC_events(highlight = TRUE): show which part of the chart a click selects */
.geneviewer-highlight [data-component] {
  cursor: pointer;
}

.geneviewer-highlight [data-component]:hover,
.geneviewer-highlight .geneviewer-hover {
  filter: drop-shadow(0 0 1.5px rgba(37, 99, 235, 0.95)) drop-shadow(0 0 3px rgba(37, 99, 235, 0.45));
}

/* A link under the mouse, below the SVG of a cluster (see geneviewerwidget.js) */
.geneviewer-highlight.geneviewer-hover-below .geneviewer-svg-content {
  cursor: pointer;
}

/* In selection mode hover identifies one selectable element (a gene, label,
   coordinate, title or subtitle) rather than its whole layer. Other parts of
   the chart keep their layer highlight. */
.geneviewer-selectable.geneviewer-highlight [data-component="genes"]:hover,
.geneviewer-selectable.geneviewer-highlight [data-component="labels"]:hover,
.geneviewer-selectable.geneviewer-highlight [data-component="coordinates"]:hover,
.geneviewer-selectable.geneviewer-highlight [data-component="clusterTitle"]:hover,
.geneviewer-selectable.geneviewer-highlight [data-component="legend"]:hover {
  filter: none;
}

.geneviewer-selectable.geneviewer-highlight .gene:hover,
.geneviewer-selectable.geneviewer-highlight text.label:hover,
.geneviewer-selectable.geneviewer-highlight text.coordinate:hover,
.geneviewer-selectable.geneviewer-highlight [data-part]:hover {
  filter: drop-shadow(0 0 1.5px rgba(37, 99, 235, 0.95)) drop-shadow(0 0 3px rgba(37, 99, 235, 0.45));
}

/* After the hover rules, so a selected item keeps its outline under the mouse */
.geneviewer .geneviewer-selected,
.geneviewer-selectable.geneviewer-highlight .geneviewer-selected:hover,
.geneviewer .gene:focus-visible,
.geneviewer text.label:focus-visible,
.geneviewer text.coordinate:focus-visible,
.geneviewer [data-part]:focus-visible {
  filter: drop-shadow(0 0 1px #2563eb) drop-shadow(0 0 1px #2563eb) drop-shadow(0 0 5px rgba(37, 99, 235, 0.7));
  outline: none;
}

/* Dragging text uses chart axes even when the text itself is rotated. */
.geneviewer-draggable text.label,
.geneviewer-draggable [data-component="title"] text,
.geneviewer-draggable [data-component="clusterTitle"] text,
.geneviewer-draggable [data-component="clusterFooter"] text,
.geneviewer-draggable text[data-component="clusterFooter"],
.geneviewer-draggable text[data-component="clusterLabel"],
.geneviewer-draggable [data-component="scaleBar"],
.geneviewer-draggable [data-component="scaleBar"] *,
.geneviewer-draggable [data-component="scale"],
.geneviewer-draggable [data-component="scale"] *,
.geneviewer-draggable [data-component="legend"] [data-part] {
  cursor: grab !important;
  touch-action: none;
  user-select: none;
}
.geneviewer-dragging, .geneviewer-dragging text,
.geneviewer-dragging [data-component="scaleBar"] *,
.geneviewer-dragging [data-component="scale"] * {
  cursor: grabbing !important;
  user-select: none;
}

/* The chart's controls, in a corner (GC_toolbar()): undo, redo and reset of
   moves, and the save icon. They show while the mouse is over the chart or
   one has keyboard focus, and hide during a drag. Only the buttons take
   clicks: the gaps between them, and the toolbar while hidden, leave clicks
   to the chart. Colours and size are variables, set by GC_toolbar() and
   GC_theme(toolbar = ...). */
.geneviewer-toolbar {
  --gv-toolbar-color: #D3D3D3;
  --gv-toolbar-hover: #555555;
  --gv-toolbar-tip-background: #333333;
  --gv-toolbar-tip-color: #ffffff;
  --gv-toolbar-size: 20px;
  --gv-toolbar-tip-border: none;
  --gv-toolbar-tip-radius: 3px;
  --gv-toolbar-tip-shadow: none;
  --gv-toolbar-font: Arial, sans-serif;
  position: absolute;
  top: 10px;
  right: 10px;
  z-index: 1000;
  display: flex;
  align-items: flex-start;
  opacity: 0;
  pointer-events: none;
  transition: opacity 0.15s;
}
.geneviewer-toolbar-topleft { left: 10px; right: auto; }
.geneviewer-toolbar-bottomright { top: auto; bottom: 10px; align-items: flex-end; }
.geneviewer-toolbar-bottomleft { top: auto; bottom: 10px; left: 10px; right: auto; align-items: flex-end; }
.geneviewer:hover > .geneviewer-toolbar,
.geneviewer-toolbar:focus-within,
.geneviewer-toolbar.geneviewer-toolbar-always {
  opacity: 1;
}
.geneviewer-toolbar-button,
.geneviewer-toolbar > [id^="saveMenu-"] {
  pointer-events: none;
}
.geneviewer:hover > .geneviewer-toolbar .geneviewer-toolbar-button,
.geneviewer:hover > .geneviewer-toolbar > [id^="saveMenu-"],
.geneviewer-toolbar:focus-within .geneviewer-toolbar-button,
.geneviewer-toolbar:focus-within > [id^="saveMenu-"],
.geneviewer-toolbar-always .geneviewer-toolbar-button,
.geneviewer-toolbar-always > [id^="saveMenu-"] {
  pointer-events: auto;
}
.geneviewer.geneviewer-dragging > .geneviewer-toolbar {
  opacity: 0;
}
.geneviewer.geneviewer-dragging > .geneviewer-toolbar * {
  pointer-events: none !important;
}
/* Undo, redo and reset, 4px apart, and a divider before the save icon.
   Disabled buttons keep their place, so none of them shifts. */
.geneviewer-history {
  display: flex;
  align-items: flex-start;
  gap: 4px;
}
.geneviewer-history[hidden] {
  display: none;
}
.geneviewer-toolbar-divider {
  width: 1px;
  height: calc(var(--gv-toolbar-size) * 0.9);
  margin: calc(5px + var(--gv-toolbar-size) * 0.05) 2px 0;
  background-color: var(--gv-toolbar-color);
  opacity: 0.6;
}
.geneviewer-toolbar-button {
  position: relative;
  display: block;
  margin: 0;
  padding: 0;
  border: 0;
  background: none;
  color: var(--gv-toolbar-color);
  cursor: pointer;
}
.geneviewer-toolbar-button:disabled {
  opacity: 0.45;
  cursor: default;
}
/* Glyphs the size of the save icon */
.geneviewer-toolbar-button > i,
.geneviewer-toolbar-button > .geneviewer-icon,
.geneviewer-toolbar .geneviewer-save-icon {
  display: block;
  padding: 5px;
  font-size: var(--gv-toolbar-size);
}
/* The default icons: inline SVG, 1em high, in the text colour */
.geneviewer-icon-svg {
  display: block;
  height: 1em;
  fill: currentColor;
}
.geneviewer-toolbar .geneviewer-save-icon {
  color: var(--gv-toolbar-color);
}
.geneviewer-toolbar-button:enabled:hover,
.geneviewer-toolbar-button:focus-visible,
.geneviewer-toolbar [id^="saveMenu-"]:hover .geneviewer-save-icon,
.geneviewer-toolbar .geneviewer-save-icon:focus-visible {
  color: var(--gv-toolbar-hover);
}
.geneviewer-toolbar-button:focus-visible,
.geneviewer-toolbar [tabindex]:focus-visible {
  outline: 2px solid var(--gv-toolbar-hover);
  outline-offset: -2px;
  border-radius: 3px;
}
/* Tooltips and the formats of the save icon, below the toolbar, or above it
   in a bottom corner, and towards the middle of the chart */
.geneviewer-toolbar-tip,
.geneviewer-toolbar .save-tooltip {
  position: absolute;
  top: 100%;
  right: 0;
  box-sizing: border-box;
  border: var(--gv-toolbar-tip-border);
  border-radius: var(--gv-toolbar-tip-radius);
  box-shadow: var(--gv-toolbar-tip-shadow);
  background-color: var(--gv-toolbar-tip-background);
  color: var(--gv-toolbar-tip-color);
  font-family: var(--gv-toolbar-font);
}
.geneviewer-toolbar-bottomright .geneviewer-toolbar-tip,
.geneviewer-toolbar-bottomright .save-tooltip,
.geneviewer-toolbar-bottomleft .geneviewer-toolbar-tip,
.geneviewer-toolbar-bottomleft .save-tooltip {
  top: auto;
  bottom: 100%;
}
.geneviewer-toolbar-topleft .geneviewer-toolbar-tip,
.geneviewer-toolbar-topleft .save-tooltip,
.geneviewer-toolbar-bottomleft .geneviewer-toolbar-tip,
.geneviewer-toolbar-bottomleft .save-tooltip {
  right: auto;
  left: 0;
}
.geneviewer-toolbar .save-option {
  color: var(--gv-toolbar-tip-color);
}
.geneviewer-toolbar .save-option:hover,
.geneviewer-toolbar .save-option:focus-visible {
  background-color: color-mix(in srgb, var(--gv-toolbar-tip-color) 18%, transparent);
}
/* Tooltips wait a moment, so they do not flicker while the mouse passes over
   the toolbar */
.geneviewer-toolbar-tip {
  padding: 5px 8px;
  font-size: 12px;
  line-height: normal;
  white-space: nowrap;
  pointer-events: none;
  visibility: hidden;
  opacity: 0;
  transition: opacity 0.1s, visibility 0s linear 0.1s;
}
.geneviewer-toolbar-tip kbd {
  margin-left: 8px;
  font: inherit;
  color: inherit;
  opacity: 0.7;
}
.geneviewer-toolbar-button:enabled:hover .geneviewer-toolbar-tip {
  visibility: visible;
  opacity: 1;
  transition: opacity 0.1s linear 0.3s, visibility 0s linear 0.3s;
}
.geneviewer-toolbar-button:focus-visible .geneviewer-toolbar-tip {
  visibility: visible;
  opacity: 1;
  transition: none;
}

/* The box drawn on empty space to select several elements */
.geneviewer-marquee {
  position: absolute;
  z-index: 999;
  border: 1px solid #2563eb;
  background-color: rgba(37, 99, 235, 0.08);
  pointer-events: none;
}

/* Zoom slider (GC_zoom()) below the clusters: the track with an overview of
   the genes, shades over what is not shown, the window and its two handles.
   Translucent greys, so it fits light and dark charts. */
.geneviewer-zoom-row {
  position: relative;
  margin: 6px 0 2px;
  flex: none;
}
.geneviewer-zoom-slider {
  position: absolute;
  top: 0;
  height: 100%;
  box-sizing: border-box;
  border-radius: 3px;
  background-color: rgba(128, 128, 128, 0.12);
  cursor: pointer;
  touch-action: none;
  user-select: none;
  -webkit-user-select: none;
}
.geneviewer-zoom-overview {
  position: absolute;
  left: 0;
  top: 0;
  width: 100%;
  height: 100%;
  display: block;
  pointer-events: none;
}
.geneviewer-zoom-shade {
  position: absolute;
  top: 0;
  bottom: 0;
  background-color: rgba(128, 128, 128, 0.3);
  pointer-events: none;
}
.geneviewer-zoom-shade[data-side="start"] {
  left: 0;
  border-radius: 3px 0 0 3px;
}
.geneviewer-zoom-shade[data-side="end"] {
  right: 0;
  border-radius: 0 3px 3px 0;
}
.geneviewer-zoom-window {
  position: absolute;
  top: 0;
  bottom: 0;
  box-sizing: border-box;
  border: 1px solid rgba(128, 128, 128, 0.85);
  border-radius: 3px;
  cursor: grab;
}
.geneviewer-zoom-window:active {
  cursor: grabbing;
}
.geneviewer-zoom-handle {
  position: absolute;
  top: 50%;
  width: 8px;
  height: 70%;
  min-height: 12px;
  box-sizing: border-box;
  transform: translate(-50%, -50%);
  border: 1px solid rgba(128, 128, 128, 0.95);
  border-radius: 2px;
  background-color: #ffffff;
  cursor: ew-resize;
}
.geneviewer-zoom-slider [data-zoom-part]:focus {
  outline: none;
}
.geneviewer-zoom-slider [data-zoom-part]:focus-visible {
  outline: 2px solid #4a90e2;
  outline-offset: 1px;
}
`;

  // src/lib/styles.js
  function injectStyles(doc) {
    if (!doc || !doc.head || doc.querySelector("style[data-geneviewer]")) return;
    const style = doc.createElement("style");
    style.setAttribute("data-geneviewer", "");
    style.textContent = styles_default;
    doc.head.insertBefore(style, doc.head.firstChild);
  }

  // src/lib/icons.js
  var ICONS = {
    "download": { width: 512, height: 512, path: "M288 32c0-17.7-14.3-32-32-32s-32 14.3-32 32V274.7l-73.4-73.4c-12.5-12.5-32.8-12.5-45.3 0s-12.5 32.8 0 45.3l128 128c12.5 12.5 32.8 12.5 45.3 0l128-128c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0L288 274.7V32zM64 352c-35.3 0-64 28.7-64 64v32c0 35.3 28.7 64 64 64H448c35.3 0 64-28.7 64-64V416c0-35.3-28.7-64-64-64H346.5l-45.3 45.3c-25 25-65.5 25-90.5 0L165.5 352H64zm368 56a24 24 0 1 1 0 48 24 24 0 1 1 0-48z" },
    "rotate-left": { width: 512, height: 512, path: "M48.5 224H40c-13.3 0-24-10.7-24-24V72c0-9.7 5.8-18.5 14.8-22.2s19.3-1.7 26.2 5.2L98.6 96.6c87.6-86.5 228.7-86.2 315.8 1c87.5 87.5 87.5 229.3 0 316.8s-229.3 87.5-316.8 0c-12.5-12.5-12.5-32.8 0-45.3s32.8-12.5 45.3 0c62.5 62.5 163.8 62.5 226.3 0s62.5-163.8 0-226.3c-62.2-62.2-162.7-62.5-225.3-1L185 183c6.9 6.9 8.9 17.2 5.2 26.2s-12.5 14.8-22.2 14.8H48.5z" },
    "rotate-right": { width: 512, height: 512, path: "M463.5 224H472c13.3 0 24-10.7 24-24V72c0-9.7-5.8-18.5-14.8-22.2s-19.3-1.7-26.2 5.2L413.4 96.6c-87.6-86.5-228.7-86.2-315.8 1c-87.5 87.5-87.5 229.3 0 316.8s229.3 87.5 316.8 0c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0c-62.5 62.5-163.8 62.5-226.3 0s-62.5-163.8 0-226.3c62.2-62.2 162.7-62.5 225.3-1L327 183c-6.9 6.9-8.9 17.2-5.2 26.2s12.5 14.8 22.2 14.8H463.5z" },
    "house": { width: 576, height: 512, path: "M575.8 255.5c0 18-15 32.1-32 32.1h-32l.7 160.2c0 2.7-.2 5.4-.5 8.1V472c0 22.1-17.9 40-40 40H456c-1.1 0-2.2 0-3.3-.1c-1.4 .1-2.8 .1-4.2 .1H416 392c-22.1 0-40-17.9-40-40V448 384c0-17.7-14.3-32-32-32H256c-17.7 0-32 14.3-32 32v64 24c0 22.1-17.9 40-40 40H160 128.1c-1.5 0-3-.1-4.5-.2c-1.2 .1-2.4 .2-3.6 .2H104c-22.1 0-40-17.9-40-40V360c0-.9 0-1.9 .1-2.8V287.6H32c-18 0-32-14-32-32.1c0-9 3-17 10-24L266.4 8c7-7 15-8 22-8s15 2 21 7L564.8 231.5c8 7 12 15 11 24z" },
    "magnifying-glass-minus": { width: 512, height: 512, path: "M416 208c0 45.9-14.9 88.3-40 122.7L502.6 457.4c12.5 12.5 12.5 32.8 0 45.3s-32.8 12.5-45.3 0L330.7 376c-34.4 25.2-76.8 40-122.7 40C93.1 416 0 322.9 0 208S93.1 0 208 0S416 93.1 416 208zM136 184c-13.3 0-24 10.7-24 24s10.7 24 24 24H280c13.3 0 24-10.7 24-24s-10.7-24-24-24H136z" }
  };
  function appendIcon(selection, name) {
    const icon = ICONS[name];
    const svg = selection.append("svg").attr("class", "geneviewer-icon-svg").attr("viewBox", `0 0 ${icon.width} ${icon.height}`).attr("aria-hidden", "true").attr("focusable", "false").style("width", `${icon.width / icon.height}em`);
    svg.append("path").attr("d", icon.path);
    return svg;
  }

  // src/lib/export.js
  var svgExportInheritedStyles = [
    "fill",
    "fill-opacity",
    "fill-rule",
    "stroke",
    "stroke-width",
    "stroke-opacity",
    "stroke-dasharray",
    "stroke-linecap",
    "stroke-linejoin",
    "font-family",
    "font-size",
    "font-weight",
    "font-style",
    "text-anchor",
    "dominant-baseline",
    "visibility"
  ];
  var svgExportOwnStyles = { opacity: "1" };
  function chartToSVG(el, exclude = null) {
    const ns = "http://www.w3.org/2000/svg";
    const elRect = el.getBoundingClientRect();
    const width = Math.ceil(elRect.width);
    const zoomRow = el.querySelector(".geneviewer-zoom-row");
    let band = null;
    if (zoomRow) {
      const rowRect = zoomRow.getBoundingClientRect();
      const rowStyle = getComputedStyle(zoomRow);
      const top = rowRect.top - elRect.top - (parseFloat(rowStyle.marginTop) || 0);
      const bottom = rowRect.bottom - elRect.top + (parseFloat(rowStyle.marginBottom) || 0);
      if (bottom > top) band = { top, bottom, height: bottom - top };
    }
    const shiftOf = (y) => band && y >= band.bottom - 0.5 ? band.height : 0;
    const height = Math.ceil(elRect.height - (band ? band.height : 0));
    const root = document.createElementNS(ns, "svg");
    root.setAttribute("xmlns", ns);
    root.setAttribute("width", width);
    root.setAttribute("height", height);
    root.setAttribute("viewBox", `0 0 ${width} ${height}`);
    const addBackground = (parent, x, y, w, h, color2) => {
      if (!color2 || color2 === "transparent" || color2 === "rgba(0, 0, 0, 0)") return;
      const rect = document.createElementNS(ns, "rect");
      rect.setAttribute("x", x);
      rect.setAttribute("y", y);
      rect.setAttribute("width", w);
      rect.setAttribute("height", h);
      rect.setAttribute("fill", color2);
      rect.setAttribute("shape-rendering", "crispEdges");
      parent.appendChild(rect);
    };
    addBackground(root, 0, 0, width, height, getComputedStyle(el).backgroundColor);
    const localUrl = (value) => value.replace(/url\((["']?)[^#)]*#([^)"']+)\1\)/g, "url(#$2)");
    const copyStyles = (source, target, parentStyle) => {
      const computed = getComputedStyle(source);
      svgExportInheritedStyles.forEach((name) => {
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
    const zIndex = (svg) => parseInt(getComputedStyle(svg).zIndex, 10) || 0;
    const svgs = Array.from(el.querySelectorAll("svg")).filter((svg) => !svg.parentElement.closest("svg") && !(exclude && exclude.contains(svg)) && !svg.closest(".geneviewer-zoom-slider")).map((svg, order2) => ({ svg, order: order2, z: zIndex(svg) })).sort((a, b) => a.z - b.z || a.order - b.order).map((item) => item.svg);
    svgs.forEach((svg) => {
      const svgStyle = getComputedStyle(svg);
      if (svgStyle.display === "none" || svgStyle.visibility === "hidden") return;
      const rect = svg.getBoundingClientRect();
      if (rect.width === 0 || rect.height === 0) return;
      const x = rect.left - elRect.left;
      const top = rect.top - elRect.top;
      const y = top - shiftOf(top);
      addBackground(root, x, y, rect.width, rect.height, svgStyle.backgroundColor);
      const clone2 = svg.cloneNode(true);
      clone2.removeAttribute("style");
      clone2.removeAttribute("class");
      clone2.setAttribute("x", x);
      clone2.setAttribute("y", y);
      clone2.setAttribute("width", rect.width);
      clone2.setAttribute("height", rect.height);
      const sources = svg.querySelectorAll("*");
      const targets = clone2.querySelectorAll("*");
      const computedStyles = /* @__PURE__ */ new Map([[svg, copyStyles(svg, clone2, null)]]);
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
      if (band && top < band.bottom - 0.5) {
        Array.from(svg.children).forEach((part, i) => {
          if (part.tagName.toLowerCase() === "defs") return;
          const partTop = part.getBoundingClientRect().top - elRect.top;
          const shift = shiftOf(partTop);
          if (!shift) return;
          const target = clone2.children[i];
          target.setAttribute("transform", `translate(0, ${-shift}) ${target.getAttribute("transform") || ""}`.trim());
        });
      }
      clone2.querySelectorAll("[display='none']").forEach((node) => node.remove());
      root.appendChild(clone2);
    });
    return '<?xml version="1.0" encoding="UTF-8"?>\n' + new XMLSerializer().serializeToString(root);
  }
  function chartToPNG(el, exclude = null) {
    const svg = new Blob([chartToSVG(el, exclude)], { type: "image/svg+xml;charset=utf-8" });
    const url = URL.createObjectURL(svg);
    return new Promise((resolve, reject) => {
      const image = new Image();
      const fail = (error) => {
        URL.revokeObjectURL(url);
        reject(error);
      };
      image.onerror = () => fail(new Error("Could not render the chart SVG as PNG."));
      image.onload = () => {
        try {
          const scale2 = window.devicePixelRatio || 1;
          const canvas = document.createElement("canvas");
          canvas.width = Math.ceil(image.naturalWidth * scale2);
          canvas.height = Math.ceil(image.naturalHeight * scale2);
          canvas.getContext("2d").drawImage(image, 0, 0, canvas.width, canvas.height);
          canvas.toBlob((blob) => {
            URL.revokeObjectURL(url);
            if (blob) resolve(blob);
            else reject(new Error("Could not encode the chart as PNG."));
          }, "image/png");
        } catch (error) {
          fail(error);
        }
      };
      image.src = url;
    });
  }
  function downloadBlob(blob, filename) {
    const url = URL.createObjectURL(blob);
    const link = document.createElement("a");
    link.href = url;
    link.download = filename;
    link.style.display = "none";
    document.body.appendChild(link);
    link.click();
    link.remove();
    setTimeout(() => URL.revokeObjectURL(url), 4e4);
  }
  function createSaveIcon(widgetId, el, container2, iconClass = null, save = downloadBlob) {
    var saveMenu = d3.select(container2).append("div").attr("id", `saveMenu-${widgetId}`).style("position", "relative").style("z-index", "1000").style("display", "block");
    var icon = saveMenu.append(iconClass ? "i" : "span").attr("class", (iconClass ? iconClass + " " : "") + "geneviewer-save-icon").attr("id", `saveIcon-${widgetId}`).attr("tabindex", "0").attr("role", "button").attr("aria-label", "Save chart").attr("aria-haspopup", "menu").style("text-align", "right").style("cursor", "pointer");
    if (!iconClass) appendIcon(icon, "download");
    var options = saveMenu.append("div").attr("id", `saveIconTooltip-${widgetId}`).attr("class", "save-tooltip").attr("role", "menu").style("display", "none").style("font-size", "12px").style("overflow", "hidden").style("white-space", "nowrap");
    var hideMenu = function() {
      options.style("display", "none");
    };
    var formats = [
      {
        label: "Save as SVG",
        save: function() {
          var svg = chartToSVG(el, container2);
          save(new Blob([svg], { type: "image/svg+xml;charset=utf-8" }), "graph.svg");
        }
      },
      {
        label: "Save as PNG",
        save: function() {
          return chartToPNG(el, container2).then((blob) => save(blob, "graph.png"));
        }
      }
    ];
    formats.forEach(function(format) {
      options.append("div").attr("class", "save-option").attr("tabindex", "0").attr("role", "menuitem").text(format.label).style("padding", "5px 8px").style("cursor", "pointer").on("click", function() {
        hideMenu();
        format.save();
      }).on("keydown", function(event) {
        if (event.key === "Enter" || event.key === " ") {
          event.preventDefault();
          this.click();
        }
      });
    });
    saveMenu.on("focusin", function() {
      options.style("display", "block");
    }).on("focusout", function(event) {
      if (saveMenu.node().contains(event.relatedTarget)) return;
      hideMenu();
    });
    d3.select(el).on("mouseleave", hideMenu);
    saveMenu.on("mouseenter", function() {
      options.style("display", "block");
    }).on("mouseleave", hideMenu);
  }

  // src/lib/chart.js
  var version = "0.3.4";
  var renderSequence = 0;
  var chartCount = 0;
  var activeShortcuts = null;
  var ownKeys = function(node) {
    return !node || node.isContentEditable || /^(INPUT|TEXTAREA|SELECT)$/.test(node.tagName) || !!(node.closest && node.closest('[role="combobox"], [role="listbox"], [role="menu"], [role="dialog"], [role="slider"], [role="tablist"], [role="radiogroup"], [role="spinbutton"]'));
  };
  document.addEventListener("keydown", function(event) {
    if (!activeShortcuts || event.defaultPrevented || event.altKey || ownKeys(event.target)) return;
    activeShortcuts(event);
  });
  var onMac = /Mac|iPhone|iPad/.test(navigator.platform || navigator.userAgent);
  var scaleComponents = ["scale", "scaleBar"];
  var isScale = function(component) {
    return scaleComponents.includes(component);
  };
  var scaleNodes = scaleComponents.map(function(c) {
    return '[data-component="' + c + '"]';
  }).join(", ");
  var toolbarButtons = ["undo", "redo", "reset", "zoom", "save"];
  var eventNames = ["click", "dblclick", "select", "selection", "drag", "dragmove", "moves", "zoom", "rendered"];
  var samePosition = function(a, b) {
    return !!a && !!b && Math.abs(a.moveX - b.moveX) < 0.01 && Math.abs(a.moveY - b.moveY) < 0.01;
  };
  var listenWindow = function(handlers2) {
    Object.keys(handlers2).forEach(function(type) {
      window.addEventListener(type, handlers2[type], type === "pointermove" ? { passive: false } : void 0);
    });
    return function() {
      Object.keys(handlers2).forEach(function(type) {
        window.removeEventListener(type, handlers2[type]);
      });
    };
  };
  var createMoveHistory = function() {
    var past = [], future = [];
    var keysOf = function(items) {
      return items.map(function(item) {
        return item.key;
      }).join("\n");
    };
    return {
      canUndo: function() {
        return past.length > 0;
      },
      canRedo: function() {
        return future.length > 0;
      },
      clear: function() {
        past = [];
        future = [];
      },
      record: function(items, merge) {
        items = items.filter(function(item) {
          return item.key !== null && !samePosition(item.before, item.after);
        });
        if (!items.length) return;
        var last = past[past.length - 1], now = performance.now();
        if (merge && last && last.merge && keysOf(last.items) === keysOf(items) && now - last.time < 1e3) {
          last.items.forEach(function(item, i) {
            item.after = items[i].after;
          });
          last.time = now;
        } else {
          past.push({ items, merge, time: now });
          if (past.length > 100) past.shift();
        }
        future = [];
      },
      // The step that undo (or redo) would take, or undefined
      next: function(undo) {
        var from = undo ? past : future;
        return from[from.length - 1];
      },
      // Take that step: gives the positions its elements move to
      take: function(undo) {
        var entry = (undo ? past : future).pop();
        entry.merge = false;
        (undo ? future : past).push(entry);
        return entry.items.map(function(item) {
          return { key: item.key, position: undo ? item.before : item.after };
        });
      },
      // Where each element of a step is now, after the steps taken
      expected: function() {
        var positions = {};
        future.forEach(function(entry) {
          entry.items.forEach(function(item) {
            positions[item.key] = item.before;
          });
        });
        past.forEach(function(entry) {
          entry.items.forEach(function(item) {
            positions[item.key] = item.after;
          });
        });
        return positions;
      }
    };
  };
  function newChart(target, renderModel, options) {
    if (typeof target === "string") {
      var found = document.querySelector(target);
      if (!found) throw new Error("No element matches " + target);
      target = /** @type {HTMLElement} */
      found;
    }
    var el = (
      /** @type {HTMLElement} */
      target
    );
    options = options || {};
    if (options.injectStyles !== false) injectStyles(el.ownerDocument);
    var addedClass = !el.classList.contains("geneviewer");
    el.classList.add("geneviewer");
    var graphContainer, style, data, links2, series, titleOptions, legendOptions, theme2, saveButton, toolbarOptions, size = {}, events2 = {};
    var outputWidth = el.style.width;
    var outputFlex = el.style.flex;
    var sizedWidth = false;
    var sizedFlex = false;
    var widgetId = el.id ? el.id.replace(/[^A-Za-z0-9_-]/g, "_") : "geneviewer-chart-" + ++chartCount;
    var handlers2 = {};
    var destroyed = false;
    var appliedStyleKeys = [];
    var clickData = ["genes", "transcript", "labels", "links", "legend", "coordinates"];
    var selectable = ["genes", "labels", "coordinates", "clusterTitle", "clusterFooter", "clusterLabel", "legend", "title", "scaleBar", "scale"];
    var selectableNodes = ".gene, text.label, text.coordinate, [data-part], " + scaleNodes;
    var draggable = selectable.filter(function(c) {
      return !["genes", "coordinates"].includes(c);
    });
    var draggableNodes = "text.label, [data-part], " + scaleNodes;
    var selection = [];
    var selected = null;
    var setSelection = function(list) {
      selection = list;
      selected = list[list.length - 1] ?? null;
    };
    var dataSignature = null;
    var preserveLegend = true;
    var targetOf = function(event) {
      var target2 = event.target;
      if (!target2.classList || !target2.classList.contains("geneviewer-svg-content") || typeof event.clientX !== "number") return target2;
      var link = document.elementsFromPoint(event.clientX, event.clientY).find(function(node) {
        return node.closest && node.closest('[data-component="links"]') && el.contains(node);
      });
      return link || target2;
    };
    var hex = function(color2) {
      var rgb = /^rgba?\((\d+), (\d+), (\d+)(?:, ([\d.]+))?\)$/.exec(color2);
      if (!rgb || rgb[4] !== void 0 && rgb[4] !== "1") return color2;
      return "#" + rgb.slice(1, 4).map(function(v) {
        return (+v).toString(16).padStart(2, "0");
      }).join("");
    };
    var solidFill = function(fill) {
      var id = /^url\("?#([^")]+)"?\)$/.exec(fill);
      var pattern2 = id && el.querySelector("#" + CSS.escape(id[1]));
      if (!pattern2 || pattern2.children.length !== 1 || pattern2.firstElementChild.tagName !== "rect") return fill;
      return getComputedStyle(pattern2.firstElementChild).fill;
    };
    var drawnStyle = function(component, node) {
      if (isScale(component)) return Object.assign({ moveX: 0, moveY: 0 }, node.__drawn__);
      node = node.closest(component === "genes" ? ".gene" : component === "legend" ? "[data-part]" : "text") || node;
      var css = getComputedStyle(node);
      var style2 = { fill: hex(solidFill(css.fill)), opacity: +css.opacity };
      if (component === "genes" || component === "legend" && node.matches(".legend-marker")) {
        return Object.assign(style2, {
          stroke: hex(css.stroke),
          strokeWidth: parseFloat(css.strokeWidth),
          strokeDasharray: css.strokeDasharray
        }, node.__drawn__);
      }
      Object.assign(style2, {
        text: node.__text__ ?? node.textContent,
        fontSize: parseFloat(css.fontSize),
        fontWeight: css.fontWeight,
        fontStyle: css.fontStyle,
        fontFamily: css.fontFamily
      }, node.__drawn__);
      if (component === "labels") {
        ["dx", "dy"].forEach(function(name) {
          var lengths = node[name].baseVal;
          if (lengths.numberOfItems) style2[name] = lengths.getItem(0).value;
        });
      }
      return style2;
    };
    var eventData = function(event, withStyle) {
      var target2 = targetOf(event);
      var part = target2.closest && target2.closest("[data-component]");
      if (!part || !el.contains(part)) return;
      var component = part.getAttribute("data-component");
      var datum = null;
      for (var node = target2; clickData.includes(component) && node && node !== part.parentNode; node = node.parentNode) {
        if (node.__data__ !== void 0) {
          datum = node.__data__;
          break;
        }
      }
      var cluster2 = part.closest("[data-cluster]");
      var detail = {
        component,
        cluster: cluster2 ? cluster2.getAttribute("data-cluster") : null,
        data: datum
      };
      var textPart = target2.closest("[data-part]");
      if (textPart && part.contains(textPart)) detail.part = textPart.getAttribute("data-part");
      if (withStyle && keyOf(detail) !== null) detail.style = drawnStyle(component, isScale(component) ? part : component === "legend" ? textPart : node);
      return detail;
    };
    var keyOf = function(d) {
      if (!d || !selectable.includes(d.component)) return null;
      if (isScale(d.component)) return [d.component, d.cluster].join("|");
      if (d.component === "legend") {
        return d.data != null && ["label", "marker"].includes(d.part) ? JSON.stringify([d.component, d.data, d.part]) : null;
      }
      if (["genes", "labels", "coordinates"].includes(d.component)) {
        if (!d.data || d.data.rowID == null) return null;
        return [d.component, d.cluster, d.data.rowID, d.data.end ?? ""].join("|");
      }
      return d.part ? [d.component, d.cluster, d.part].join("|") : null;
    };
    var keysOf = function(list) {
      return JSON.stringify(list.map(keyOf));
    };
    var report = function(name, detail) {
      (handlers2[name] || []).slice().forEach(function(handler) {
        handler({ type: name, detail });
      });
      el.dispatchEvent(new CustomEvent("geneviewer:" + name, { bubbles: true, detail }));
    };
    var sameSelection = function(a, b) {
      return keyOf(a) !== null && keyOf(a) === keyOf(b);
    };
    var isSelected = function(detail) {
      var key = keyOf(detail);
      return key !== null && selection.some(function(item) {
        return keyOf(item) === key;
      });
    };
    var copyItem = function(item) {
      return item === null ? null : structuredClone(item);
    };
    var copySelection = function() {
      return selection.map(copyItem);
    };
    var reportSelection = function() {
      report("select", copyItem(selected));
      report("selection", copySelection());
    };
    var applySelection = function(refresh) {
      var previous = JSON.stringify(selection.map(function(item) {
        return item.style;
      }));
      var fresh = {};
      el.querySelectorAll(selectableNodes).forEach(function(node) {
        var detail = eventData({ target: node });
        var key = keyOf(detail);
        if (key === null) return;
        var active = isSelected(detail);
        node.classList.toggle("geneviewer-selected", active);
        if (events2.select === true) {
          node.setAttribute("tabindex", "0");
          node.setAttribute("role", "button");
          node.setAttribute("aria-pressed", String(active));
          node.setAttribute("aria-label", detail.component + ": " + (detail.component === "legend" ? String(detail.data) + " " + detail.part : detail.data?.name ?? node.__text__ ?? node.textContent));
        }
        if (active && !fresh[key]) fresh[key] = eventData({ target: node }, true);
      });
      var next = selection.map(function(item) {
        return fresh[keyOf(item)];
      }).filter(Boolean);
      var lost = next.length !== selection.length;
      setSelection(next);
      if (lost || refresh && selection.length && JSON.stringify(selection.map(function(item) {
        return item.style;
      })) !== previous) {
        reportSelection();
      }
    };
    var mergeSelection = function(base, details, mode) {
      var next = mode === "replace" ? [] : base.slice();
      details.forEach(function(detail) {
        if (keyOf(detail) === null) return;
        var at = next.findIndex(function(item) {
          return keyOf(item) === keyOf(detail);
        });
        if (at >= 0 && mode === "toggle") next.splice(at, 1);
        else if (at < 0) next.push(detail);
      });
      return next;
    };
    var changeSelection = function(details, mode, quiet) {
      var next = mergeSelection(selection, details, mode);
      if (keysOf(next) === keysOf(selection)) return;
      setSelection(next);
      applySelection();
      if (!quiet) reportSelection();
    };
    var selectElement = function(detail) {
      changeSelection(detail ? [detail] : [], "replace");
    };
    var sameValue = function(given, drawn) {
      if (given === null || typeof given !== "object") return given === drawn;
      if (drawn === null || typeof drawn !== "object") return false;
      return Object.keys(given).every(function(name) {
        return sameValue(given[name], drawn[name]);
      });
    };
    var matchItems = function(items) {
      var drawn = [];
      el.querySelectorAll(selectableNodes).forEach(function(node) {
        var detail = eventData({ target: node });
        if (keyOf(detail) !== null) drawn.push(detail);
      });
      var found2 = [];
      items.forEach(function(item) {
        if (!item || typeof item !== "object" || typeof item.component !== "string") {
          throw new Error("select() needs items with a component, as getSelection() gives them");
        }
        drawn.forEach(function(detail) {
          var matches = detail.component === item.component && (item.cluster == null || String(item.cluster) === detail.cluster) && (item.part == null || item.part === detail.part) && (item.data === void 0 || sameValue(item.data, detail.data));
          if (matches) found2.push(detail);
        });
      });
      return found2;
    };
    var suppressClickUntil = 0;
    var onClick = function(event) {
      if (performance.now() < suppressClickUntil) {
        event.preventDefault();
        event.stopPropagation();
        return;
      }
      var detail = eventData(event, true);
      if (events2.select === true && event.detail < 2) {
        var adds = event.shiftKey || event.metaKey || event.ctrlKey;
        if (adds) {
          if (keyOf(detail) !== null) changeSelection([detail], "toggle");
        } else if (detail || event.target.closest("svg") && !event.target.closest(".geneviewer-toolbar") || event.target === el) {
          var only = selection.length === 1 && sameSelection(selected, detail);
          selectElement(only ? null : detail ?? null);
        }
      }
      if (!detail || detail.component === "legend" && events2.legendClick === false) return;
      if (events2.click === true) report("click", detail);
    };
    var onDoubleClick = function(event) {
      if (performance.now() < suppressClickUntil) return;
      var detail = eventData(event, true);
      if (!detail) return;
      if (events2.select === true) selectElement(detail);
      report("dblclick", detail);
    };
    var movableOf = function(target2, component) {
      if (isScale(component)) return target2.closest('[data-component="' + component + '"]');
      return target2.closest(component === "legend" ? "[data-part]" : "text");
    };
    var liftMovable = function(node, detail) {
      if (node.closest(".geneviewer-move-overlay") || !el.clientWidth || !el.clientHeight) return null;
      var parent = node.parentNode;
      var matrix = parent.getScreenCTM();
      if (!matrix) return null;
      var ns = "http://www.w3.org/2000/svg";
      var overlay = el.querySelector(".geneviewer-move-overlay");
      if (!overlay) {
        overlay = document.createElementNS(ns, "svg");
        overlay.setAttribute("class", "geneviewer-move-overlay");
        overlay.setAttribute("width", el.clientWidth);
        overlay.setAttribute("height", el.clientHeight);
        overlay.setAttribute("viewBox", `0 0 ${el.clientWidth} ${el.clientHeight}`);
        Object.assign(overlay.style, {
          position: "absolute",
          left: "0",
          top: "0",
          width: "100%",
          height: "100%",
          pointerEvents: "none",
          zIndex: "4"
        });
        el.appendChild(overlay);
      }
      var placement = { parent, next: node.nextSibling };
      if (detail.component === "legend") node.__home__ = parent.closest(".legend");
      var style2 = getComputedStyle(node);
      [
        "fill",
        "fill-opacity",
        "stroke",
        "stroke-width",
        "stroke-opacity",
        "font-family",
        "font-size",
        "font-weight",
        "font-style",
        "text-anchor",
        "dominant-baseline"
      ].forEach(function(name) {
        if (!node.style.getPropertyValue(name)) node.style.setProperty(name, style2.getPropertyValue(name));
      });
      var frame = document.createElementNS(ns, "g");
      var transform = overlay.getScreenCTM().inverse().multiply(matrix);
      frame.setAttribute("transform", `matrix(${transform.a} ${transform.b} ${transform.c} ${transform.d} ${transform.e} ${transform.f})`);
      if (!isScale(detail.component)) frame.setAttribute("data-component", detail.component);
      if (detail.cluster != null) frame.setAttribute("data-cluster", detail.cluster);
      overlay.appendChild(frame);
      frame.appendChild(node);
      node.style.pointerEvents = "visiblePainted";
      placement.frame = frame;
      syncLegendFrames();
      return placement;
    };
    var syncLegendFrames = function() {
      el.querySelectorAll(".geneviewer-move-overlay [data-part]").forEach(function(node) {
        if (node.__home__) node.parentNode.classList.toggle("unselected", node.__home__.classList.contains("unselected"));
      });
    };
    var liftMovedElements = function() {
      el.querySelectorAll(draggableNodes).forEach(function(node) {
        if (!(node.__drawn__?.moveX || node.__drawn__?.moveY)) return;
        var detail = eventData({ target: node });
        if (detail && draggable.includes(detail.component)) liftMovable(node, detail);
      });
    };
    var emitDragMove = function(detail, position) {
      report("dragmove", Object.assign({}, detail, { position }));
    };
    var dragState = null;
    var queuedRender = null;
    var flushQueuedRender = function() {
      var input = queuedRender;
      queuedRender = null;
      if (input) renderValue(input);
      return !!input;
    };
    var finishDrag = function(commit) {
      if (!dragState) return;
      var state = dragState;
      dragState = null;
      state.stopListening();
      el.classList.remove("geneviewer-dragging");
      if (!state.moved) {
        flushQueuedRender();
        return;
      }
      suppressClickUntil = performance.now() + 350;
      if (!commit) {
        state.items.forEach(function(item) {
          item.node.setAttribute("transform", item.transform);
          if (item.placement) {
            var place = item.placement;
            place.parent.insertBefore(item.node, place.next && place.next.parentNode === place.parent ? place.next : null);
            var overlay = place.frame.parentNode;
            place.frame.remove();
            if (!overlay.firstChild) overlay.remove();
          }
          emitDragMove(item.detail, { moveX: item.moveX, moveY: item.moveY });
        });
        setSelection(state.selection);
        applySelection();
        flushQueuedRender();
        return;
      }
      var details = state.items.map(function(item) {
        item.node.__drawn__ = Object.assign({}, item.node.__drawn__, item.position);
        var detail = eventData({ target: item.node }, true);
        detail.position = item.position;
        detail.action = "drag";
        return detail;
      });
      if (flushQueuedRender()) {
        var nodes = movableNodes();
        state.items.forEach(function(item) {
          var node = nodes[keyOf(item.detail)];
          if (node) placeAt(node, item.position);
        });
      }
      moveHistory.record(state.items.map(function(item) {
        return { key: keyOf(item.detail), before: { moveX: item.moveX, moveY: item.moveY }, after: item.position };
      }), false);
      updateToolbar();
      announce(details, false);
    };
    var cancelDrag = function() {
      finishDrag(false);
    };
    var escapeDrag = function(event) {
      if (event.key === "Escape" && dragState) {
        event.preventDefault();
        cancelDrag();
      }
    };
    var endDrag = function(event) {
      if (dragState && event.pointerId === dragState.id) finishDrag(true);
    };
    var moveDrag = function(event) {
      var state = dragState;
      if (!state || event.pointerId !== state.id) return;
      if (!state.moved && Math.hypot(event.clientX - state.x, event.clientY - state.y) < 4) return;
      if (!state.moved) {
        state.moved = true;
        state.items.forEach(function(item) {
          item.placement = liftMovable(item.node, item.detail);
        });
        if (events2.select === true && state.items.length === 1 && !isSelected(state.items[0].detail)) {
          setSelection([state.items[0].detail]);
          applySelection();
        }
        el.classList.add("geneviewer-dragging");
      }
      event.preventDefault();
      state.items.forEach(function(item) {
        var point = new DOMPoint(event.clientX, event.clientY).matrixTransform(item.inverse);
        item.position = {
          moveX: Math.round(item.moveX + point.x - item.start.x),
          moveY: Math.round(item.moveY + point.y - item.start.y)
        };
        var dx = item.position.moveX - item.moveX, dy = item.position.moveY - item.moveY;
        item.node.setAttribute("transform", `translate(${dx}, ${dy}) ${item.transform}`);
        emitDragMove(item.detail, item.position);
      });
    };
    var dragItem = function(node, detail, event) {
      var matrix = node.parentNode.getScreenCTM();
      if (!matrix) return null;
      var inverse = matrix.inverse();
      return {
        node,
        detail,
        inverse,
        start: new DOMPoint(event.clientX, event.clientY).matrixTransform(inverse),
        transform: node.getAttribute("transform") || "",
        moveX: detail.style?.moveX ?? positionOf(node).moveX,
        moveY: detail.style?.moveY ?? positionOf(node).moveY
      };
    };
    var startDrag = function(event) {
      if (event.button !== 0 || event.isPrimary === false) return;
      var detail = eventData(event, true);
      if (!detail || !draggable.includes(detail.component) || keyOf(detail) === null) {
        startMarquee(event, detail);
        return;
      }
      if (events2.drag !== true) return;
      var node = movableOf(event.target, detail.component);
      if (!node) return;
      var first = dragItem(node, detail, event);
      if (!first) return;
      var items = [first];
      if (events2.select === true && isSelected(detail) && selection.length > 1) {
        var nodes = movableNodes();
        selection.forEach(function(item) {
          var key = keyOf(item);
          if (key === keyOf(detail) || !nodes[key]) return;
          var other = dragItem(nodes[key], eventData({ target: nodes[key] }, true), event);
          if (other) items.push(other);
        });
      }
      dragState = {
        id: event.pointerId,
        items,
        selection: selection.slice(),
        x: event.clientX,
        y: event.clientY,
        moved: false,
        stopListening: listenWindow({
          pointermove: moveDrag,
          pointerup: endDrag,
          pointercancel: cancelDrag,
          blur: cancelDrag,
          keydown: escapeDrag
        })
      };
    };
    var marquee = null;
    var startMarquee = function(event, detail) {
      if (events2.select !== true || keyOf(detail) !== null || event.target.closest(".geneviewer-toolbar") || !el.contains(event.target)) return;
      marquee = {
        id: event.pointerId,
        x: event.clientX,
        y: event.clientY,
        node: null,
        mode: event.shiftKey || event.metaKey || event.ctrlKey ? "add" : "replace",
        selection: selection.slice(),
        stopListening: listenWindow({
          pointermove: moveMarquee,
          pointerup: endMarquee,
          pointercancel: cancelMarquee,
          blur: cancelMarquee,
          keydown: escapeMarquee
        })
      };
    };
    var marqueeHits = function(rect) {
      var hits = {};
      el.querySelectorAll(selectableNodes).forEach(function(node) {
        var detail = eventData({ target: node });
        var key = keyOf(detail);
        if (key === null || hits[key]) return;
        var r = node.getBoundingClientRect();
        if ((r.width || r.height) && r.left >= rect.left && r.right <= rect.right && r.top >= rect.top && r.bottom <= rect.bottom) hits[key] = eventData({ target: node }, true);
      });
      return Object.values(hits);
    };
    var moveMarquee = function(event) {
      if (!marquee || event.pointerId !== marquee.id) return;
      if (!marquee.node && Math.hypot(event.clientX - marquee.x, event.clientY - marquee.y) < 4) return;
      event.preventDefault();
      var rect = {
        left: Math.min(marquee.x, event.clientX),
        right: Math.max(marquee.x, event.clientX),
        top: Math.min(marquee.y, event.clientY),
        bottom: Math.max(marquee.y, event.clientY)
      };
      if (!marquee.node) {
        marquee.node = document.createElement("div");
        marquee.node.className = "geneviewer-marquee";
        el.appendChild(marquee.node);
      }
      var box = el.getBoundingClientRect(), sx = box.width / el.offsetWidth || 1, sy = box.height / el.offsetHeight || 1;
      Object.assign(marquee.node.style, {
        left: (rect.left - box.left) / sx + "px",
        top: (rect.top - box.top) / sy + "px",
        width: (rect.right - rect.left) / sx + "px",
        height: (rect.bottom - rect.top) / sy + "px"
      });
      setSelection(mergeSelection(marquee.selection, marqueeHits(rect), marquee.mode));
      applySelection();
    };
    var finishMarquee = function(commit) {
      if (!marquee) return;
      var state = marquee;
      marquee = null;
      state.stopListening();
      if (!state.node) return;
      state.node.remove();
      suppressClickUntil = performance.now() + 350;
      if (!commit) {
        setSelection(state.selection);
        applySelection();
        return;
      }
      if (keysOf(selection) !== keysOf(state.selection)) reportSelection();
    };
    var endMarquee = function(event) {
      if (marquee && event.pointerId === marquee.id) finishMarquee(true);
    };
    var cancelMarquee = function() {
      finishMarquee(false);
    };
    var escapeMarquee = function(event) {
      if (event.key === "Escape" && marquee) {
        event.preventDefault();
        cancelMarquee();
      }
    };
    var moveHistory = createMoveHistory();
    var awaiting = {};
    var localMoves = {};
    var origin = {};
    var nudgeTimer = null;
    var toolbar2 = null;
    var positionOf = function(node) {
      return { moveX: node.__drawn__?.moveX ?? 0, moveY: node.__drawn__?.moveY ?? 0 };
    };
    var movableNodes = function() {
      var nodes = {};
      el.querySelectorAll(selectableNodes).forEach(function(node) {
        var detail = eventData({ target: node });
        var key = detail && draggable.includes(detail.component) ? keyOf(detail) : null;
        if (key === null || nodes[key]) return;
        nodes[key] = movableOf(node, detail.component);
      });
      return nodes;
    };
    var placeAt = function(node, position) {
      var from = positionOf(node);
      var dx = position.moveX - from.moveX, dy = position.moveY - from.moveY;
      if (dx || dy) {
        var detail = eventData({ target: node });
        if (detail) liftMovable(node, detail);
        node.setAttribute("transform", `translate(${dx}, ${dy}) ${node.getAttribute("transform") || ""}`.trim());
      }
      node.__drawn__ = Object.assign({}, node.__drawn__, { moveX: position.moveX, moveY: position.moveY });
    };
    var detailOf = function(node, action) {
      var detail = eventData({ target: node }, true);
      if (!detail) return null;
      detail.position = positionOf(node);
      detail.action = action;
      return detail;
    };
    var announce = function(details, outline) {
      details.forEach(function(detail) {
        var key = keyOf(detail);
        if (options.server) awaiting[key] = detail.position;
        else localMoves[key] = detail.position;
      });
      if (!details.length) return;
      if (events2.select === true) {
        if (outline && details.length === 1) setSelection([details[0]]);
        applySelection();
        reportSelection();
      }
      if (details.length === 1) report("drag", details[0]);
      else report("moves", { action: details[0].action, moves: details });
    };
    var movedAway = function(nodes) {
      return Object.keys(nodes).filter(function(key) {
        return origin[key] && !samePosition(positionOf(nodes[key]), origin[key]);
      });
    };
    var updateToolbar = function(nodes) {
      if (!toolbar2 || events2.drag !== true) return;
      var canReset = movedAway(nodes || movableNodes()).length > 0;
      toolbar2.select(".geneviewer-history").property("hidden", !moveHistory.canUndo() && !moveHistory.canRedo() && !canReset);
      toolbar2.select(".geneviewer-undo").property("disabled", !moveHistory.canUndo());
      toolbar2.select(".geneviewer-redo").property("disabled", !moveHistory.canRedo());
      toolbar2.select(".geneviewer-reset").property("disabled", !canReset);
    };
    var apply = function(positions, action, outline) {
      var nodes = movableNodes(), details = [];
      positions.forEach(function(target2) {
        var node = nodes[target2.key];
        if (!node) return;
        placeAt(node, target2.position);
        var detail = detailOf(node, action);
        if (detail) details.push(detail);
      });
      clearTimeout(nudgeTimer);
      nudgeTimer = null;
      updateToolbar(nodes);
      announce(details, outline);
    };
    var step = function(undo) {
      var entry = moveHistory.next(undo);
      if (!entry || dragState) return;
      var nodes = movableNodes();
      var fresh = entry.items.every(function(item) {
        return nodes[item.key] && samePosition(positionOf(nodes[item.key]), undo ? item.after : item.before);
      });
      if (!fresh) {
        moveHistory.clear();
        return updateToolbar(nodes);
      }
      apply(moveHistory.take(undo), undo ? "undo" : "redo", entry.items.length === 1);
    };
    var resetMoves = function() {
      if (dragState) return;
      var nodes = movableNodes();
      var items = movedAway(nodes).map(function(key) {
        return { key, before: positionOf(nodes[key]), after: origin[key] };
      });
      if (!items.length) return;
      moveHistory.record(items, false);
      apply(items.map(function(item) {
        return { key: item.key, position: item.after };
      }), "reset", false);
    };
    var checkMoves = function() {
      var nodes = movableNodes();
      Object.keys(nodes).forEach(function(key) {
        if (!origin[key]) origin[key] = positionOf(nodes[key]);
      });
      Object.keys(awaiting).forEach(function(key) {
        if (!nodes[key] || samePosition(positionOf(nodes[key]), awaiting[key])) delete awaiting[key];
      });
      if (nudgeTimer === null && !Object.keys(awaiting).length) {
        var expected = moveHistory.expected();
        var stale = Object.keys(expected).some(function(key) {
          return !nodes[key] || !samePosition(positionOf(nodes[key]), expected[key]);
        });
        if (stale) moveHistory.clear();
      }
      updateToolbar(nodes);
    };
    var restoreLocalMoves = function() {
      var nodes = movableNodes();
      Object.keys(localMoves).forEach(function(key) {
        if (nodes[key]) placeAt(nodes[key], localMoves[key]);
      });
    };
    var nudge = function(dx, dy) {
      var nodes = movableNodes();
      var items = selection.map(function(item) {
        return keyOf(item);
      }).filter(function(key) {
        return key !== null && nodes[key];
      }).map(function(key) {
        var before = positionOf(nodes[key]);
        var after = { moveX: Math.round(before.moveX + dx), moveY: Math.round(before.moveY + dy) };
        placeAt(nodes[key], after);
        emitDragMove(eventData({ target: nodes[key] }), after);
        return { key, before, after };
      });
      if (!items.length) return false;
      moveHistory.record(items, true);
      updateToolbar();
      clearTimeout(nudgeTimer);
      nudgeTimer = setTimeout(function() {
        nudgeTimer = null;
        var current = movableNodes();
        announce(items.map(function(item) {
          return current[item.key] && detailOf(current[item.key], "nudge");
        }).filter(Boolean), false);
      }, 300);
      return true;
    };
    var onShortcut = function(event) {
      if (events2.drag !== true || !el.isConnected) return;
      var key = event.key.toLowerCase();
      if ((event.metaKey || event.ctrlKey) && (key === "z" || key === "y")) {
        event.preventDefault();
        step(!(key === "y" || event.shiftKey));
        return;
      }
      var arrow = { ArrowLeft: [-1, 0], ArrowRight: [1, 0], ArrowUp: [0, -1], ArrowDown: [0, 1] }[event.key];
      if (!arrow || event.metaKey || event.ctrlKey || events2.select !== true || dragState) return;
      if (event.target !== document.body && !el.contains(event.target)) return;
      var size2 = event.shiftKey ? 10 : 1;
      if (nudge(arrow[0] * size2, arrow[1] * size2)) event.preventDefault();
    };
    var markActive = function() {
      activeShortcuts = onShortcut;
    };
    var onCaptureClick = function(event) {
      if (performance.now() < suppressClickUntil && !event.target.closest(".geneviewer-toolbar")) {
        suppressClickUntil = 0;
        event.preventDefault();
        event.stopPropagation();
        return;
      }
      var home = event.target.closest && event.target.closest(".geneviewer-move-overlay [data-part]")?.__home__;
      var legendEdits = events2.select === true || events2.dblclick === true || events2.click === true && events2.legendClick !== false;
      if (home && !legendEdits && home.isConnected) {
        event.stopPropagation();
        home.dispatchEvent(new MouseEvent("click", { bubbles: false, detail: event.detail }));
        syncLegendFrames();
      }
    };
    el.addEventListener("click", onCaptureClick, true);
    el.addEventListener("click", syncLegendFrames);
    el.addEventListener("pointerdown", markActive, true);
    el.addEventListener("focusin", markActive);
    var iconClass = function(icon) {
      return /(^|\s)fa-/.test(icon) ? icon : "fa-solid fa-" + icon;
    };
    var addToolbar = function() {
      toolbar2 = null;
      var opts = mergeOptions(
        {
          show: "hover",
          position: "topright",
          buttons: toolbarButtons,
          icons: {}
        },
        "toolbarOptions",
        toolbarOptions,
        theme2
      );
      var buttons = [].concat(opts.buttons);
      var wanted = function(name) {
        return buttons.includes(name);
      };
      var movable = events2.drag === true && ["undo", "redo", "reset"].some(wanted);
      var save = saveButton && wanted("save");
      var zoomButton = !!zoomOptions && wanted("zoom");
      if (opts.show === "never" || !movable && !save && !zoomButton) return;
      var customIcon = function(name) {
        return opts.icons[name] != null ? iconClass(opts.icons[name]) : null;
      };
      toolbar2 = d3.select(el).append("div").attr("class", `geneviewer-toolbar geneviewer-toolbar-${opts.position}` + (opts.show === "always" ? " geneviewer-toolbar-always" : "")).attr("id", `geneviewer-toolbar-${widgetId}`).attr("role", "toolbar").attr("aria-label", "Chart");
      var px = function(value) {
        return typeof value === "number" ? value + "px" : value;
      };
      [
        ["color", "--gv-toolbar-color"],
        ["hoverColor", "--gv-toolbar-hover"],
        ["tooltipBackground", "--gv-toolbar-tip-background"],
        ["tooltipColor", "--gv-toolbar-tip-color"],
        ["tooltipBorder", "--gv-toolbar-tip-border"],
        ["tooltipRadius", "--gv-toolbar-tip-radius"],
        ["tooltipShadow", "--gv-toolbar-tip-shadow"],
        ["fontFamily", "--gv-toolbar-font"],
        ["size", "--gv-toolbar-size"]
      ].forEach(function(v) {
        if (opts[v[0]] != null) toolbar2.style(v[1], px(opts[v[0]]));
      });
      var addButton = function(group, b) {
        var button = group.append("button").attr("type", "button").attr("class", "geneviewer-toolbar-button geneviewer-" + b.name).attr("aria-label", b.label).attr("aria-keyshortcuts", b.shortcuts).property("disabled", !!b.disabled).on("click", b.onClick);
        if (customIcon(b.name)) {
          button.append("i").attr("class", customIcon(b.name)).attr("aria-hidden", "true");
        } else {
          appendIcon(button.append("span").attr("class", "geneviewer-icon").attr("aria-hidden", "true"), b.icon);
        }
        var tip = button.append("span").attr("class", "geneviewer-toolbar-tip").attr("aria-hidden", "true");
        tip.append("span").text(b.label);
        if (b.kbd) tip.append("kbd").text(b.kbd);
      };
      if (movable) {
        var historyGroup = toolbar2.append("div").attr("class", "geneviewer-history").attr("role", "group").attr("aria-label", "Positions").property("hidden", true);
        [
          {
            name: "undo",
            label: "Undo",
            icon: "rotate-left",
            kbd: onMac ? "⌘Z" : "Ctrl+Z",
            shortcuts: "Control+Z Meta+Z",
            onClick: function() {
              step(true);
            }
          },
          {
            name: "redo",
            label: "Redo",
            icon: "rotate-right",
            kbd: onMac ? "⇧⌘Z" : "Ctrl+Y",
            shortcuts: "Control+Shift+Z Meta+Shift+Z Control+Y",
            onClick: function() {
              step(false);
            }
          },
          {
            name: "reset",
            label: "Reset positions",
            icon: "house",
            kbd: null,
            shortcuts: null,
            onClick: resetMoves
          }
        ].filter(function(b) {
          return wanted(b.name);
        }).forEach(function(b) {
          addButton(historyGroup, Object.assign({ disabled: true }, b));
        });
        if (save || zoomButton) historyGroup.append("span").attr("class", "geneviewer-toolbar-divider");
      }
      if (zoomButton) {
        var zoomGroup = toolbar2.append("div").attr("class", "geneviewer-history geneviewer-zoom-buttons").attr("role", "group").attr("aria-label", "Zoom").property("hidden", zoomRange === null);
        addButton(zoomGroup, {
          name: "zoom",
          label: "Reset zoom",
          icon: "magnifying-glass-minus",
          kbd: null,
          shortcuts: null,
          onClick: resetZoom
        });
        if (save) zoomGroup.append("span").attr("class", "geneviewer-toolbar-divider");
      }
      if (save) createSaveIcon(widgetId, el, toolbar2.node(), customIcon("save"), options.saveAs);
    };
    var hovered = null;
    var setHovered = function(node) {
      if (hovered === node) return;
      if (hovered) hovered.classList.remove("geneviewer-hover");
      hovered = node;
      if (hovered) hovered.classList.add("geneviewer-hover");
      el.classList.toggle("geneviewer-hover-below", !!hovered);
    };
    var onMouseMove = function(event) {
      var target2 = targetOf(event);
      setHovered(target2 === event.target ? null : target2.closest("[data-component]"));
    };
    var onMouseLeave = function() {
      setHovered(null);
    };
    var onKeyDown = function(event) {
      if (event.key === "Escape") {
        if (dragState) cancelDrag();
        else if (marquee) cancelMarquee();
        else selectElement(null);
        return;
      }
      if (event.key === "Enter" && events2.dblclick === true && event.target.matches(selectableNodes)) {
        event.preventDefault();
        onDoubleClick(event);
      }
    };
    var wireEvents = function() {
      cancelDrag();
      var click = events2.click === true;
      el.removeEventListener("click", onClick);
      el.removeEventListener("dblclick", onDoubleClick);
      el.removeEventListener("keydown", onKeyDown);
      el.removeEventListener("pointerdown", startDrag);
      finishMarquee(false);
      el.removeEventListener("mousemove", onMouseMove);
      el.removeEventListener("mouseleave", onMouseLeave);
      hovered = null;
      el.classList.remove("geneviewer-hover-below");
      var highlight = (click || events2.select === true || events2.dblclick === true) && events2.highlight === true;
      if (highlight) {
        el.addEventListener("mousemove", onMouseMove);
        el.addEventListener("mouseleave", onMouseLeave);
      }
      if (click || events2.select === true) el.addEventListener("click", onClick);
      if (events2.dblclick === true) el.addEventListener("dblclick", onDoubleClick);
      if (events2.select === true || events2.drag === true) el.addEventListener("keydown", onKeyDown);
      if (events2.drag === true || events2.select === true) el.addEventListener("pointerdown", startDrag);
      el.classList.toggle("geneviewer-draggable", events2.drag === true);
      if (events2.select !== true && selection.length) {
        setSelection([]);
        reportSelection();
      }
      el.classList.toggle("geneviewer-selectable", events2.select === true);
      el.classList.toggle("geneviewer-highlight", highlight);
      applySelection(true);
    };
    var zoomOptions = null;
    var zoomRange = null;
    var zoomRequest = null;
    var zoomRenderId = null;
    var zoomCharts = [];
    var zoomTrack = [0, 1];
    var legendColorBar = null;
    var zoomBar = null;
    var zoomDrag = null;
    var zoomFrame = null;
    var zoomTimer = null;
    var zoomMode = function() {
      return zoomOptions?.mode === "absolute" ? "absolute" : "relative";
    };
    var initialZoom = function() {
      var range = zoomOptions?.range;
      if (!Array.isArray(range) || range.length !== 2) return null;
      range = range.slice().sort(function(a, b) {
        return a - b;
      });
      return zoomMode() === "relative" && range[0] <= 0 && range[1] >= 1 ? null : range;
    };
    var toTrack = function(value) {
      return (value - zoomTrack[0]) / (zoomTrack[1] - zoomTrack[0]);
    };
    var fromTrack = function(share) {
      return zoomTrack[0] + share * (zoomTrack[1] - zoomTrack[0]);
    };
    var zoomShares = function() {
      if (!zoomRange) return [0, 1];
      return zoomMode() === "absolute" ? zoomRange.map(toTrack) : zoomRange.slice();
    };
    var minShare = function() {
      var min = zoomOptions?.min;
      if (min == null) return 0.01;
      return Math.min(1, zoomMode() === "absolute" ? min / (zoomTrack[1] - zoomTrack[0]) : min);
    };
    var roundZoom = function(value) {
      return zoomMode() === "absolute" ? Math.round(value) : Math.round(value * 1e4) / 1e4;
    };
    var setZoomShares = function(shares) {
      zoomRange = shares[0] <= 1e-6 && shares[1] >= 1 - 1e-6 ? null : shares.map(function(share) {
        return roundZoom(zoomMode() === "absolute" ? fromTrack(share) : share);
      });
    };
    var zoomDetail = function() {
      var absolute = zoomMode() === "absolute";
      var range = zoomRange || (absolute ? zoomTrack : [0, 1]);
      return {
        mode: zoomMode(),
        start: roundZoom(range[0]),
        end: roundZoom(range[1]),
        zoomed: zoomRange !== null,
        // For GC_zoom(range = ): NULL when everything shows
        range: zoomRange ? zoomRange.slice() : null,
        clusters: zoomCharts.map(function(item) {
          var chart3 = item.chart;
          var shown = [chart3.xScale.invert(0), chart3.xScale.invert(chart3.width - chart3.margin.left - chart3.margin.right)].sort(function(a, b) {
            return a - b;
          });
          return { cluster: item.key, start: Math.round(shown[0]), end: Math.round(shown[1]) };
        })
      };
    };
    var lastZoom = null;
    var reportZoom = function(action) {
      clearTimeout(zoomTimer);
      zoomTimer = null;
      if (events2.zoom !== true || !zoomOptions) return;
      var detail = zoomDetail();
      var key = JSON.stringify([detail.mode, detail.range, detail.clusters]);
      if ((action === "render" || action === "set") && key === lastZoom) return;
      lastZoom = key;
      detail.action = action;
      report("zoom", detail);
    };
    var positionZoomBar = function() {
      if (!zoomBar) return;
      var shares = zoomShares();
      var pct = function(share) {
        return share * 100 + "%";
      };
      zoomBar.querySelector('[data-side="start"]').style.width = pct(shares[0]);
      zoomBar.querySelector('[data-side="end"]').style.left = pct(shares[1]);
      var windowNode = zoomBar.querySelector('[data-zoom-part="window"]');
      windowNode.style.left = pct(shares[0]);
      windowNode.style.width = pct(shares[1] - shares[0]);
      var values = zoomRange || (zoomMode() === "absolute" ? zoomTrack : [0, 1]);
      var text = function(value) {
        return zoomMode() === "absolute" ? d3.format(",.0f")(value) : d3.format(".0%")(value);
      };
      ["start", "end"].forEach(function(part, i) {
        var handle = zoomBar.querySelector('[data-zoom-part="' + part + '"]');
        handle.style.left = pct(shares[i]);
        handle.setAttribute("aria-valuenow", Math.round(shares[i] * 100));
        handle.setAttribute("aria-valuetext", text(values[i]));
      });
      windowNode.setAttribute("aria-valuenow", Math.round(shares[0] * 100));
      windowNode.setAttribute("aria-valuetext", text(values[0]) + " to " + text(values[1]));
      toolbar2?.select(".geneviewer-zoom-buttons").property("hidden", zoomRange === null);
    };
    var redrawZoom = function(now) {
      positionZoomBar();
      if (now) {
        if (zoomFrame) cancelAnimationFrame(zoomFrame);
        zoomFrame = null;
        return redraw();
      }
      if (zoomFrame) return;
      zoomFrame = requestAnimationFrame(function() {
        zoomFrame = null;
        redraw();
      });
    };
    var changeZoom = function(shares, now) {
      setZoomShares(shares);
      if (zoomDrag && zoomOptions?.update === "release") positionZoomBar();
      else redrawZoom(now);
    };
    var resetZoom = function() {
      if (zoomRange === null) return;
      changeZoom([0, 1], true);
      reportZoom("reset");
    };
    var startZoomDrag = function(event) {
      event.stopPropagation();
      if (event.button !== 0 || zoomDrag) return;
      event.preventDefault();
      var rect = event.currentTarget.getBoundingClientRect();
      var at = (event.clientX - rect.left) / rect.width;
      var part = event.target.closest("[data-zoom-part]")?.getAttribute("data-zoom-part") ?? "track";
      var shares = zoomShares();
      zoomDrag = {
        id: event.pointerId,
        left: rect.left,
        width: rect.width,
        at,
        moved: false,
        stopListening: listenWindow({
          pointermove: moveZoomDrag,
          pointerup: endZoomDrag,
          pointercancel: endZoomDrag,
          blur: endZoomDrag
        })
      };
      if (part === "track") {
        var half = (shares[1] - shares[0]) / 2;
        var centre = Math.max(half, Math.min(1 - half, at));
        shares = [centre - half, centre + half];
        zoomDrag.moved = true;
        changeZoom(shares);
        part = "window";
      }
      zoomDrag.part = part;
      zoomDrag.shares = shares;
    };
    var moveZoomDrag = function(event) {
      if (!zoomDrag || event.pointerId !== zoomDrag.id) return;
      event.preventDefault();
      var delta = (event.clientX - zoomDrag.left) / zoomDrag.width - zoomDrag.at;
      var s = zoomDrag.shares, min = minShare();
      var next = zoomDrag.part === "start" ? [Math.max(0, Math.min(s[1] - min, s[0] + delta)), s[1]] : zoomDrag.part === "end" ? [s[0], Math.min(1, Math.max(s[0] + min, s[1] + delta))] : (function() {
        var shift = Math.max(-s[0], Math.min(1 - s[1], delta));
        return [s[0] + shift, s[1] + shift];
      })();
      zoomDrag.moved = true;
      changeZoom(next);
    };
    var endZoomDrag = function(event) {
      if (!zoomDrag || event.pointerId !== void 0 && event.pointerId !== zoomDrag.id) return;
      var state = zoomDrag;
      zoomDrag = null;
      state.stopListening();
      if (!state.moved) return;
      redrawZoom(true);
      reportZoom("drag");
    };
    var onZoomKey = function(event) {
      var part = event.target.getAttribute && event.target.getAttribute("data-zoom-part");
      if (!part) return;
      var s = zoomShares(), min = minShare();
      var step2 = { ArrowLeft: -1, ArrowDown: -1, ArrowRight: 1, ArrowUp: 1 }[event.key];
      var next = null;
      if (step2) {
        var delta = step2 * (event.shiftKey ? 0.1 : 0.01);
        next = part === "start" ? [Math.max(0, Math.min(s[1] - min, s[0] + delta)), s[1]] : part === "end" ? [s[0], Math.min(1, Math.max(s[0] + min, s[1] + delta))] : [s[0] + Math.max(-s[0], Math.min(1 - s[1], delta)), s[1] + Math.max(-s[0], Math.min(1 - s[1], delta))];
      } else if (event.key === "Home") {
        next = part === "end" ? [s[0], Math.min(1, s[0] + min)] : part === "start" ? [0, s[1]] : [0, s[1] - s[0]];
      } else if (event.key === "End") {
        next = part === "start" ? [Math.max(0, s[1] - min), s[1]] : part === "end" ? [s[0], 1] : [1 - (s[1] - s[0]), 1];
      }
      if (!next) return;
      event.preventDefault();
      event.stopPropagation();
      changeZoom(next);
      clearTimeout(zoomTimer);
      zoomTimer = setTimeout(function() {
        reportZoom("keyboard");
      }, 300);
    };
    var applyCss = function(node, css) {
      Object.keys(css || {}).forEach(function(key) {
        if (key.includes("-")) node.style.setProperty(key, css[key]);
        else node.style[key] = css[key];
      });
    };
    var addZoomBar = function(height) {
      zoomBar = null;
      if (!zoomOptions || !zoomCharts.length) return;
      var absolute = zoomMode() === "absolute";
      zoomTrack = absolute ? [
        d3.min(zoomCharts, function(item) {
          return item.chart.minStart;
        }),
        d3.max(zoomCharts, function(item) {
          return item.chart.maxEnd;
        })
      ] : [0, 1];
      var first = zoomCharts[0].chart;
      var row = d3.select(el).append("div").attr("class", "geneviewer-zoom-row").style("height", height + "px");
      var elBox = el.getBoundingClientRect(), scale2 = elBox.width / el.offsetWidth || 1;
      var svgBox = first.svg.node().getBoundingClientRect(), svgScale = svgBox.width / first.width || 1;
      var plotWidth = first.width - first.margin.left - first.margin.right;
      var rowBox = row.node().getBoundingClientRect();
      var bar = row.append("div").attr("class", "geneviewer-zoom-slider").attr("role", "group").attr("aria-label", "Zoom").style("left", (svgBox.left + first.margin.left * svgScale - rowBox.left) / scale2 + "px").style("width", plotWidth * svgScale / scale2 + "px").on("pointerdown", startZoomDrag).on("click", function(event) {
        event.stopPropagation();
      }).on("dblclick", function(event) {
        event.stopPropagation();
        resetZoom();
      }).on("keydown", onZoomKey);
      applyCss(bar.node(), zoomOptions.style);
      if (zoomOptions.overview !== false) {
        var overview = bar.append("svg").attr("class", "geneviewer-zoom-overview").attr("viewBox", "0 0 1000 " + height).attr("preserveAspectRatio", "none").attr("aria-hidden", "true");
        var rowHeight = (height - 6) / zoomCharts.length;
        var geneHeight = Math.max(1, Math.min(rowHeight * 0.7, 8));
        zoomCharts.forEach(function(item, i) {
          var chart3 = item.chart;
          var width = chart3.width - chart3.margin.left - chart3.margin.right;
          var share = function(value) {
            if (absolute) return toTrack(value);
            var x = chart3.fullScale(value);
            return x === null ? null : x / width;
          };
          var color2 = function(d) {
            if (zoomOptions.color) return zoomOptions.color;
            return item.group && chart3.genesColorScale ? chart3.genesColorScale(d[item.group]) : "#9e9e9e";
          };
          (chart3.data || []).forEach(function(d) {
            var a = share(d.start), b = share(d.end);
            if (a === null || b === null) return;
            overview.append("rect").attr("x", Math.min(a, b) * 1e3).attr("width", Math.max(Math.abs(b - a) * 1e3, 1)).attr("y", 3 + rowHeight * i + (rowHeight - geneHeight) / 2).attr("height", geneHeight).attr("fill", color2(d));
          });
        });
      }
      bar.append("div").attr("class", "geneviewer-zoom-shade").attr("data-side", "start");
      bar.append("div").attr("class", "geneviewer-zoom-shade").attr("data-side", "end");
      var windowNode = bar.append("div").attr("class", "geneviewer-zoom-window").attr("data-zoom-part", "window");
      applyCss(windowNode.node(), zoomOptions.windowStyle);
      [windowNode].concat(["start", "end"].map(function(part) {
        var handle = bar.append("div").attr("class", "geneviewer-zoom-handle").attr("data-zoom-part", part);
        applyCss(handle.node(), zoomOptions.handleStyle);
        return handle;
      })).forEach(function(node) {
        var part = node.attr("data-zoom-part");
        node.attr("tabindex", 0).attr("role", "slider").attr("aria-label", part === "window" ? "Zoom window" : part === "start" ? "Zoom start" : "Zoom end").attr("aria-valuemin", 0).attr("aria-valuemax", 100);
      });
      zoomBar = bar.node();
      positionZoomBar();
    };
    var clipZoomLinks = function() {
      if (!zoomCharts.some(function(item) {
        return item.chart.zoom;
      })) return;
      var ns = "http://www.w3.org/2000/svg";
      el.querySelectorAll("svg.GeneLink").forEach(function(svg, index) {
        var box = svg.getBoundingClientRect();
        var scale2 = box.width / (+svg.getAttribute("width") || box.width) || 1;
        var id = "geneviewer-zoom-links-" + widgetId + "-" + index;
        var clipPath = document.createElementNS(ns, "clipPath");
        clipPath.setAttribute("id", id);
        zoomCharts.forEach(function(item) {
          var chart3 = item.chart;
          var svgBox = chart3.svg.node().getBoundingClientRect(), svgScale = svgBox.width / chart3.width || 1;
          var rect = document.createElementNS(ns, "rect");
          rect.setAttribute("x", (svgBox.left + chart3.margin.left * svgScale - box.left) / scale2);
          rect.setAttribute("y", 0);
          rect.setAttribute("width", (chart3.width - chart3.margin.left - chart3.margin.right) * svgScale / scale2);
          rect.setAttribute("height", svg.getAttribute("height"));
          clipPath.appendChild(rect);
        });
        var defs = document.createElementNS(ns, "defs");
        defs.appendChild(clipPath);
        var group = document.createElementNS(ns, "g");
        group.setAttribute("clip-path", "url(#" + id + ")");
        Array.from(svg.childNodes).forEach(function(node) {
          group.appendChild(node);
        });
        svg.appendChild(defs);
        svg.appendChild(group);
      });
    };
    var zoomTo = function(range) {
      if (destroyed || !zoomOptions) return;
      zoomRange = Array.isArray(range) && range.length === 2 ? range.slice().sort(function(a, b) {
        return a - b;
      }) : null;
      if (zoomRange && zoomMode() === "relative" && zoomRange[0] <= 0 && zoomRange[1] >= 1) zoomRange = null;
      redrawZoom(true);
      reportZoom("set");
    };
    var keepScroll = function(drawing) {
      var scrolled = [];
      for (var node = el.parentElement; node; node = node.parentElement) {
        if (node.scrollTop || node.scrollLeft) scrolled.push([node, node.scrollTop, node.scrollLeft]);
      }
      var page = el.ownerDocument.scrollingElement;
      if (page && !scrolled.some(function(item) {
        return item[0] === page;
      })) {
        scrolled.push([page, page.scrollTop, page.scrollLeft]);
      }
      try {
        drawing();
      } finally {
        scrolled.forEach(function(item) {
          if (item[0].scrollTop !== item[1]) item[0].scrollTop = item[1];
          if (item[0].scrollLeft !== item[2]) item[0].scrollLeft = item[2];
        });
      }
    };
    var draw = function() {
      keepScroll(drawChart);
    };
    var drawChart = function() {
      cancelDrag();
      zoomCharts = [];
      legendColorBar = null;
      var hiddenGroups = preserveLegend ? Array.from(el.querySelectorAll(".legend.unselected"), function(node) {
        return node.__data__;
      }) : [];
      preserveLegend = true;
      d3.select(el).selectAll("*").remove();
      appliedStyleKeys.forEach(function(key2) {
        el.style[key2] = "";
      });
      if (options.server) {
        var fixedWidth = typeof size.width === "number" ? size.width + "px" : size.width;
        var fixed = typeof fixedWidth === "string" && /^\d+(\.\d+)?px$/.test(fixedWidth);
        if (fixed) {
          el.style.width = fixedWidth;
          sizedWidth = true;
        } else if (sizedWidth) {
          el.style.width = outputWidth;
          sizedWidth = false;
        }
        var parentStyle = el.parentElement && getComputedStyle(el.parentElement);
        var inRow = parentStyle && /^(inline-)?flex$/.test(parentStyle.display) && /^row/.test(parentStyle.flexDirection);
        if (fixed && inRow) {
          el.style.flex = "0 0 auto";
          sizedFlex = true;
        } else if (sizedFlex) {
          el.style.flex = outputFlex;
          sizedFlex = false;
        }
      }
      el.style["height"] = "100%";
      el.style["position"] = "relative";
      el.style["boxSizing"] = "border-box";
      var chartStyle = mergeOptions({ style: {} }, "chartOptions", { style }, theme2).style;
      appliedStyleKeys = Object.keys(chartStyle);
      for (var key in chartStyle) {
        el.style[key] = chartStyle[key];
      }
      var titleHeight = 0;
      if (titleOptions?.show) {
        var titleContainer = d3.select(el).append("div").attr("id", `geneviewer-title-container-${widgetId}`).classed("geneviewer-container", true);
        var defaultTitleHeight = (titleOptions.title && titleOptions.subtitle ? 50 : 40) * themeTextScale(theme2);
        var titleHeightOption = mergeOptions({ height: defaultTitleHeight }, "titleOptions", titleOptions, theme2).height;
        titleHeight = computeSize(titleHeightOption, el.clientHeight);
        var titleOpts = { ...titleOptions, width: el.clientWidth, height: titleHeight };
        var title2 = createContainer(
          `#geneviewer-title-container-${widgetId}`,
          "svg-container",
          "titleOptions",
          titleOpts,
          theme2
        ).title(titleOpts.title, titleOpts.subtitle, titleOpts.show ?? false, titleOpts);
      }
      var legendHeight = legendOptions?.show === false ? 0 : computeSize(legendOptions?.height, el.clientHeight);
      if (legendOptions?.group !== null && legendOptions?.show) {
        d3.select(el).append("div").attr("id", `geneviewer-legend-container-${widgetId}`).classed("geneviewer-container", true);
        var barSettings = Array.isArray(links2) && links2.length ? colorBarSettings(links2, theme2) : null;
        legendColorBar = barSettings && barSettings.colorBarOptions.position === "legend" ? barSettings : null;
        var legendOpts = {
          ...legendOptions,
          width: el.clientWidth,
          height: computeSize(legendOptions.height, el.clientHeight),
          editable: events2.select === true || events2.dblclick === true || events2.click === true && events2.legendClick !== false
        };
        createContainer(
          `#geneviewer-legend-container-${widgetId}`,
          "svg-container",
          "legendOptions",
          legendOpts,
          theme2
        ).legendData(data).legend(
          legendOpts.group ?? false,
          legendOpts.show ?? false,
          el.id,
          legendOpts,
          legendColorBar
        );
        var legendElement = d3.select(`#geneviewer-legend-container-${widgetId}`).node();
        var legendDimensions = legendElement.getBoundingClientRect();
        legendHeight = legendDimensions.height;
      }
      d3.select(el).append("div").attr("id", `geneviewer-graph-container-${widgetId}`).style("flex-direction", graphContainer["direction"]).style("position", "relative").classed("geneviewer-container", true);
      var clusters = Object.keys(series);
      var zoomBarHeight = zoomOptions ? zoomOptions.height ?? 24 : 0;
      var zoomHeight = zoomOptions ? zoomBarHeight + 8 : 0;
      var zoom2 = zoomOptions && zoomRange ? { from: zoomRange[0], to: zoomRange[1], mode: zoomMode() } : null;
      var linkSets = (Array.isArray(links2) ? links2 : []).map((entry) => ({
        data: toRows(entry.data),
        options: entry.options ?? {}
      }));
      clusters.forEach(function(clusterKey) {
        var cluster2 = series[clusterKey], containerOptions = cluster2.container, clusterOptions = cluster2.cluster, clusterData = toRows(series[clusterKey].data), scaleOptions = cluster2.scale, clusterTitleOptions = cluster2.clusterTitle, footerOptions = cluster2.footer, clusterLabelOptions = cluster2.clusterLabel, labelOptions = cluster2.labels, sequenceOptions = cluster2.sequence, geneOptions = cluster2.genes, transcriptOptions = cluster2.transcript, coordinateOptions = cluster2.coordinates, scaleBarOptions = cluster2.scaleBar, annotationOptions = cluster2.annotations, trackMouse2 = cluster2.trackMouse, tooltipOptions = cluster2.tooltip;
        var clonedContainerOptions = JSON.parse(JSON.stringify(containerOptions));
        clonedContainerOptions.height = computeSize(clonedContainerOptions.height, el.clientHeight);
        clonedContainerOptions.height -= titleHeight / clusters.length;
        clonedContainerOptions.height -= legendHeight ? legendHeight / clusters.length : 0;
        clonedContainerOptions.height -= zoomHeight / clusters.length;
        if (typeof containerOptions.height === "string" && containerOptions.height.endsWith("%")) {
          var padding = (parseFloat(chartStyle.paddingTop) || 0) + (parseFloat(chartStyle.paddingBottom) || 0);
          clonedContainerOptions.height -= padding / clusters.length;
        }
        clonedContainerOptions.width = computeSize(clonedContainerOptions.width, el.clientWidth);
        if (clusterLabelOptions?.show) {
          clonedContainerOptions.margin = {
            ...clonedContainerOptions.margin,
            ...clusterLabelMargin(containerOptions, clusterLabelOptions, theme2)
          };
        }
        var chart3 = createContainer(`#geneviewer-graph-container-${widgetId}`, "svg-container", "containerOptions", clonedContainerOptions, theme2).cluster(clusterOptions).title(clusterTitleOptions?.title, clusterTitleOptions?.subtitle, clusterTitleOptions?.show ?? false, clusterTitleOptions, "clusterTitleOptions").footer(footerOptions?.title, footerOptions?.subtitle, footerOptions?.show ?? false, footerOptions).clusterLabel(clusterLabelOptions?.title, clusterLabelOptions?.show ?? false, clusterLabelOptions).geneData(data, clusterData).scale(zoom2 ? Object.assign({}, scaleOptions, { zoom: zoom2 }) : scaleOptions).sequence(sequenceOptions?.show ?? false, sequenceOptions).genes(geneOptions?.group, geneOptions?.show ?? false, geneOptions).transcript(transcriptOptions?.group, transcriptOptions?.show ?? false, transcriptOptions);
        chart3.svg.attr("data-cluster", clusterKey);
        linkSets.forEach((linkSet) => {
          chart3.links(getClusterLinks(linkSet.data, clusterKey), clusterKey, linkSet.options);
        });
        chart3.coordinates(coordinateOptions?.show ?? false, coordinateOptions).labels(labelOptions?.label, labelOptions?.show ?? false, labelOptions).scaleBar(scaleBarOptions?.show ?? false, scaleBarOptions).addAnnotations(annotationOptions).trackMouse(trackMouse2?.show ?? false, trackMouse2).tooltip(tooltipOptions?.show ?? false, tooltipOptions).clipZoom();
        zoomCharts.push({ key: clusterKey, chart: chart3, group: geneOptions?.group });
      });
      addZoomBar(zoomBarHeight);
      var legendPosition = mergeOptions({ position: "bottom" }, "legendOptions", legendOptions, theme2).position;
      if (legendPosition == "bottom" && legendOptions?.show && legendOptions?.group !== null) {
        el.appendChild(legendElement);
      }
      addToolbar();
      el.querySelectorAll(".legend").forEach(function(node) {
        if (hiddenGroups.includes(node.__data__)) {
          node.dispatchEvent(new MouseEvent("click", { bubbles: false }));
        }
      });
    };
    var addLinks = function() {
      if (!links2 || links2.length === 0) {
        return;
      }
      const graphContainer2 = d3.select(`#geneviewer-graph-container-${widgetId}`);
      makeLinks(graphContainer2, links2, theme2);
      makeColorBar(graphContainer2, links2, theme2, legendColorBar !== null);
      clipZoomLinks();
    };
    var redraw = function() {
      var focused = zoomBar && zoomBar.contains(document.activeElement) ? document.activeElement.getAttribute("data-zoom-part") : null;
      keepScroll(function() {
        draw();
        addLinks();
        liftMovedElements();
        restoreLocalMoves();
        wireEvents();
        checkMoves();
      });
      if (focused && zoomBar) zoomBar.querySelector('[data-zoom-part="' + focused + '"]')?.focus({ preventScroll: true });
    };
    var chartData = function(input) {
      if (input.data) return toRows(input.data);
      var columns = input.dataColumns ?? [];
      var rows = [];
      Object.values(input.series ?? {}).forEach(function(cluster2) {
        toRows(cluster2.data).forEach(function(row) {
          var picked = {};
          columns.forEach(function(column) {
            picked[column] = row[column];
          });
          rows.push(picked);
        });
      });
      return rows.sort(function(a, b) {
        return a.rowID - b.rowID;
      });
    };
    var renderValue = function(input) {
      var nextData = chartData(input);
      var nextSignature = JSON.stringify([nextData, input.group, input.cluster, input.legend?.group]);
      if (dragState && nextSignature === dataSignature && input.events?.drag === true) {
        queuedRender = input;
        return;
      }
      queuedRender = null;
      cancelDrag();
      zoomOptions = input.zoom && input.zoom.show !== false ? input.zoom : null;
      var request = zoomOptions ? JSON.stringify([zoomOptions.range ?? null, zoomMode()]) : null;
      var nextRenderId = input.events?.renderId ?? null;
      var newLoad = nextRenderId !== null && nextRenderId !== zoomRenderId;
      zoomRenderId = nextRenderId;
      if (nextSignature !== dataSignature || request !== zoomRequest || newLoad) {
        zoomRange = initialZoom();
        lastZoom = null;
      }
      if (!zoomOptions) lastZoom = null;
      zoomRequest = request;
      if (dataSignature !== null && nextSignature !== dataSignature) {
        preserveLegend = false;
        if (selection.length) {
          setSelection([]);
          reportSelection();
        }
        moveHistory.clear();
        awaiting = {};
        origin = {};
      }
      localMoves = {};
      dataSignature = nextSignature;
      graphContainer = input.graphContainer;
      style = input.style;
      data = nextData;
      links2 = input.links;
      series = input.series;
      saveButton = input.saveButton;
      toolbarOptions = input.toolbar ?? {};
      titleOptions = input.title;
      legendOptions = input.legend;
      theme2 = input.theme ?? {};
      events2 = input.events ?? {};
      size = input.size ?? {};
      draw();
      addLinks();
      liftMovedElements();
      wireEvents();
      checkMoves();
      reportZoom("render");
      if (events2.rendered === true) {
        var detail = {
          id: events2.renderId ?? el.id + "-render-" + ++renderSequence,
          chartId: el.id,
          width: el.clientWidth,
          height: el.clientHeight
        };
        report("rendered", detail);
      }
    };
    var spec = null;
    var drawInput = function(input) {
      if (input && input.version !== void 0) {
        spec = structuredClone(input);
        renderValue(compile(spec));
      } else {
        spec = null;
        renderValue(input);
      }
    };
    var chart2 = {
      update: function(input) {
        if (destroyed) return;
        if (spec !== null) {
          spec = mergeSpec(spec, input);
          renderValue(compile(spec));
        } else {
          drawInput(input);
        }
      },
      setSpec: function(next) {
        if (destroyed) return;
        if (!next || next.version === void 0) throw new Error("setSpec() needs a spec with a version");
        drawInput(next);
      },
      getSpec: function() {
        return spec === null ? null : structuredClone(spec);
      },
      resize: function() {
        if (!destroyed) redraw();
      },
      on: function(name, handler) {
        if (!eventNames.includes(name)) throw new Error("Unknown event: " + name);
        (handlers2[name] = handlers2[name] || []).push(handler);
        return function() {
          chart2.off(name, handler);
        };
      },
      off: function(name, handler) {
        var list = handlers2[name] || [];
        var at = list.indexOf(handler);
        if (at >= 0) list.splice(at, 1);
      },
      zoomTo,
      select: function(items) {
        if (destroyed) return;
        if (events2.select !== true) {
          warnOnce("chart.select() needs events: { select: true }; nothing was selected.");
          return;
        }
        changeSelection(matchItems(items == null ? [] : Array.isArray(items) ? items : [items]), "replace");
      },
      getSelection: function() {
        return copySelection();
      },
      undo: function() {
        if (!destroyed) step(true);
      },
      redo: function() {
        if (!destroyed) step(false);
      },
      download: function(format, filename) {
        if (format !== "svg" && format !== "png") {
          return Promise.reject(new Error('download() needs the format "svg" or "png"'));
        }
        var name = filename ?? "graph." + format;
        var save = options.saveAs || downloadBlob;
        return Promise.resolve().then(function() {
          return format === "svg" ? new Blob([chart2.toSVG()], { type: "image/svg+xml;charset=utf-8" }) : chart2.toPNG();
        }).then(function(blob) {
          return save(blob, name);
        }).then(function() {
        });
      },
      // Without the toolbar, as the save menu does
      toSVG: function() {
        return chartToSVG(el, toolbar2 ? toolbar2.node() : null);
      },
      toPNG: function() {
        return chartToPNG(el, toolbar2 ? toolbar2.node() : null);
      },
      destroy: function() {
        if (destroyed) return;
        destroyed = true;
        queuedRender = null;
        cancelDrag();
        finishMarquee(false);
        if (zoomDrag) zoomDrag.stopListening();
        zoomDrag = null;
        clearTimeout(nudgeTimer);
        clearTimeout(zoomTimer);
        if (zoomFrame) cancelAnimationFrame(zoomFrame);
        if (activeShortcuts === onShortcut) activeShortcuts = null;
        handlers2 = {};
        [
          ["click", onCaptureClick, true],
          ["click", syncLegendFrames],
          ["pointerdown", markActive, true],
          ["focusin", markActive],
          ["click", onClick],
          ["dblclick", onDoubleClick],
          ["keydown", onKeyDown],
          ["pointerdown", startDrag],
          ["mousemove", onMouseMove],
          ["mouseleave", onMouseLeave]
        ].forEach(function(l) {
          el.removeEventListener(l[0], l[1], l[2]);
        });
        d3.select(el).on("mouseleave", null).selectAll("*").remove();
        appliedStyleKeys.forEach(function(key) {
          el.style[key] = "";
        });
        el.classList.remove(
          "geneviewer-draggable",
          "geneviewer-selectable",
          "geneviewer-highlight",
          "geneviewer-dragging",
          "geneviewer-hover-below"
        );
        if (addedClass) el.classList.remove("geneviewer");
      }
    };
    Object.keys(options.on || {}).forEach(function(name) {
      chart2.on(name, options.on[name]);
    });
    drawInput(renderModel);
    return chart2;
  }
  return __toCommonJS(index_exports);
})();
