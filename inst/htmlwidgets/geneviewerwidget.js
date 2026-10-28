(function () {
// Shared by all chart instances, including ones removed and recreated.
var renderSequence = 0;

HTMLWidgets.widget({
  name: 'geneviewer',
  type: 'output',

  factory: function (el, width, height) {
    var
      graphContainer,
      style,
      data,
      links,
      series,
      titleOptions,
      legendOptions,
      theme,
      saveButton,
      size = {},
      events = {};

    // Keep the output's original width for responsive charts, including an
    // explicit GC_chartOutput(width = ...). Restore it after a fixed chart.
    var outputWidth = el.style.width;
    var outputFlex = el.style.flex;
    var sizedWidth = false;
    var sizedFlex = false;

    // Unique per chart on the page; used in container and save button ids
    var widgetId = el.id.replace(/[^A-Za-z0-9_-]/g, '_');

    // Chart styles set on el by the last draw
    var appliedStyleKeys = [];

    // Click events for Shiny (GC_events()): the part of the chart clicked (its
    // data-component), its cluster, and the data of the gene, label, link or
    // coordinate, or the title or subtitle (part) of a cluster title, footer or label
    var clickData = ["genes", "transcript", "labels", "links", "legend", "coordinates"];
    // The parts of the chart that can be selected (GC_events(select = TRUE)),
    // one by one: they can be styled with GC_element_style()
    var selectable = ["genes", "labels", "coordinates", "clusterTitle", "clusterFooter", "clusterLabel", "legend", "title", "scaleBar", "scale"];
    var selectableNodes = '.gene, text.label, text.coordinate, [data-part], [data-component="scaleBar"], [data-component="scale"]';
    var selected = null;
    var dataSignature = null;
    var preserveLegend = true;
    // The element under the mouse. Links are drawn in their own SVG below the
    // clusters, whose SVGs take the mouse over their whole area: where the
    // event lands on a cluster SVG itself, a link below it is the target.
    var targetOf = function (event) {
      var target = event.target;
      if (!target.classList || !target.classList.contains("geneviewer-svg-content") ||
          typeof event.clientX !== "number") return target;
      var link = document.elementsFromPoint(event.clientX, event.clientY).find(function (node) {
        return node.closest && node.closest('[data-component="links"]') && el.contains(node);
      });
      return link || target;
    };
    // A computed colour as hex, as typed in an editor: rgb(31, 119, 180) is
    // #1f77b4. Transparent colours, none and patterns (url(...)) stay as they are.
    var hex = function (color) {
      var rgb = /^rgba?\((\d+), (\d+), (\d+)(?:, ([\d.]+))?\)$/.exec(color);
      if (!rgb || (rgb[4] !== undefined && rgb[4] !== "1")) return color;
      return "#" + rgb.slice(1, 4).map(function (v) { return (+v).toString(16).padStart(2, "0"); }).join("");
    };
    // When some groups have a pattern, the genes of the other groups are
    // filled with a pattern of one rect: their colour is that of the rect
    var solidFill = function (fill) {
      var id = /^url\("?#([^")]+)"?\)$/.exec(fill);
      var pattern = id && el.querySelector("#" + CSS.escape(id[1]));
      if (!pattern || pattern.children.length !== 1 || pattern.firstElementChild.tagName !== "rect") return fill;
      return getComputedStyle(pattern.firstElementChild).fill;
    };
    // How a selectable element is drawn, with the names of its element options:
    // colours, opacity and font from its computed style, the shape settings of
    // a gene (recorded when it is drawn) and the text of a label or title
    var drawnStyle = function (component, node) {
      if (["scale", "scaleBar"].includes(component)) return Object.assign({ moveX: 0, moveY: 0 }, node.__drawn__);
      node = node.closest(component === "genes" ? ".gene" : component === "legend" ? "[data-part]" : "text") || node;
      var css = getComputedStyle(node);
      var style = { fill: hex(solidFill(css.fill)), opacity: +css.opacity };
      if (component === "genes" || (component === "legend" && node.matches(".legend-marker"))) {
        return Object.assign(style, {
          stroke: hex(css.stroke), strokeWidth: parseFloat(css.strokeWidth),
          strokeDasharray: css.strokeDasharray
        }, node.__drawn__);
      }
      Object.assign(style, {
        text: node.__text__ ?? node.textContent, fontSize: parseFloat(css.fontSize),
        fontWeight: css.fontWeight, fontStyle: css.fontStyle, fontFamily: css.fontFamily
      }, node.__drawn__);
      if (component === "labels") {
        // SVG resolves offsets such as 1em to the pixels used by the editor.
        ["dx", "dy"].forEach(function (name) {
          var lengths = node[name].baseVal;
          if (lengths.numberOfItems) style[name] = lengths.getItem(0).value;
        });
      }
      return style;
    };
    var eventData = function (event, withStyle) {
      var target = targetOf(event);
      var part = target.closest && target.closest("[data-component]");
      if (!part || !el.contains(part)) return;
      var component = part.getAttribute("data-component");
      // The data bound to the clicked element: a gene or link row, or a group
      var datum = null;
      for (var node = target; clickData.includes(component) && node && node !== part.parentNode; node = node.parentNode) {
        if (node.__data__ !== undefined) { datum = node.__data__; break; }
      }
      var cluster = part.closest("[data-cluster]");
      var detail = {
        component: component,
        cluster: cluster ? cluster.getAttribute("data-cluster") : null,
        data: datum
      };
      var textPart = target.closest("[data-part]");
      if (textPart && part.contains(textPart)) detail.part = textPart.getAttribute("data-part");
      if (withStyle && keyOf(detail) !== null) detail.style = drawnStyle(component, ["scale", "scaleBar"].includes(component) ? part : component === "legend" ? textPart : node);
      return detail;
    };
    // What identifies a selectable element: a gene, label or coordinate by its
    // row (and a coordinate by its start or end), a title or subtitle by its
    // cluster and part. Null for the other parts of the chart.
    var keyOf = function (d) {
      if (!d || !selectable.includes(d.component)) return null;
      if (["scale", "scaleBar"].includes(d.component)) return [d.component, d.cluster].join("|");
      if (d.component === "legend") {
        return d.data != null && ["label", "marker"].includes(d.part) ?
          JSON.stringify([d.component, d.data, d.part]) : null;
      }
      if (["genes", "labels", "coordinates"].includes(d.component)) {
        if (!d.data || d.data.rowID == null) return null;
        return [d.component, d.cluster, d.data.rowID, d.data.end ?? ""].join("|");
      }
      return d.part ? [d.component, d.cluster, d.part].join("|") : null;
    };

    var report = function (name, detail) {
      if (HTMLWidgets.shinyMode) {
        Shiny.setInputValue(el.id + "_" + name, detail, { priority: "event" });
      }
      el.dispatchEvent(new CustomEvent("geneviewer:" + name, { bubbles: true, detail: detail }));
    };

    var sameSelection = function (a, b) {
      return keyOf(a) !== null && keyOf(a) === keyOf(b);
    };
    var applySelection = function (refresh) {
      var previousStyle = JSON.stringify(selected?.style);
      var found = false;
      el.querySelectorAll(selectableNodes).forEach(function (node) {
        var detail = eventData({ target: node });
        if (keyOf(detail) === null) return;
        var active = !!sameSelection(selected, detail);
        node.classList.toggle("geneviewer-selected", active);
        if (events.select === true) {
          node.setAttribute("tabindex", "0");
          node.setAttribute("role", "button");
          node.setAttribute("aria-pressed", String(active));
          node.setAttribute("aria-label", detail.component + ": " +
            (detail.component === "legend" ? String(detail.data) + " " + detail.part :
              detail.data?.name ?? node.__text__ ?? node.textContent));
        }
        if (active) { selected = eventData({ target: node }, true); found = true; }
      });
      if (selected && !found) { selected = null; report("select", null); }
      else if (refresh && selected && JSON.stringify(selected.style) !== previousStyle) {
        report("select", selected);
      }
    };
    // Only the selectable elements can be selected: any other part of the
    // chart ends the selection, as empty space does
    var selectElement = function (detail) {
      if (keyOf(detail) === null) detail = null;
      if ((!selected && !detail) || sameSelection(selected, detail)) return;
      selected = detail;
      applySelection();
      // The clicked element's payload, with how it is drawn
      report("select", selected && detail);
    };
    var suppressClickUntil = 0;
    var onClick = function (event) {
      if (performance.now() < suppressClickUntil) { event.preventDefault(); event.stopPropagation(); return; }
      var detail = eventData(event, true);
      if (events.select === true && event.detail < 2) {
        // Controls such as the save button are not empty chart space. A click
        // on the selected element ends its selection.
        if (detail || event.target.closest("svg") || event.target === el) {
          selectElement(sameSelection(selected, detail) ? null : detail ?? null);
        }
      }
      if (!detail || (detail.component === "legend" && events.legendClick === false)) return;
      if (HTMLWidgets.shinyMode && events.click === true) {
        Shiny.setInputValue(el.id + "_click", detail, { priority: "event" });
      }
    };
    var onDoubleClick = function (event) {
      if (performance.now() < suppressClickUntil) return;
      var detail = eventData(event, true);
      if (!detail) return;
      if (events.select === true) selectElement(detail);
      report("dblclick", detail);
    };
    var draggable = ["labels", "title", "clusterTitle", "clusterFooter", "clusterLabel", "scaleBar", "scale"];
    // Moved text and scale bars share one chart-wide viewport, above the cluster SVGs.
    // Keep the original parent's coordinate system and logical component.
    var liftText = function (node, detail) {
      if (node.closest(".geneviewer-text-overlay") || !el.clientWidth || !el.clientHeight) return null;
      var parent = node.parentNode;
      var matrix = parent.getScreenCTM();
      if (!matrix) return null;
      var ns = "http://www.w3.org/2000/svg";
      var overlay = el.querySelector(".geneviewer-text-overlay");
      if (!overlay) {
        overlay = document.createElementNS(ns, "svg");
        overlay.setAttribute("class", "geneviewer-text-overlay");
        overlay.setAttribute("width", el.clientWidth);
        overlay.setAttribute("height", el.clientHeight);
        overlay.setAttribute("viewBox", `0 0 ${el.clientWidth} ${el.clientHeight}`);
        Object.assign(overlay.style, { position: "absolute", left: "0", top: "0",
          width: "100%", height: "100%", pointerEvents: "none", zIndex: "4" });
        el.appendChild(overlay);
      }
      var placement = { parent: parent, next: node.nextSibling };
      var style = getComputedStyle(node);
      ["fill", "fill-opacity", "stroke", "stroke-width", "stroke-opacity", "font-family",
        "font-size", "font-weight", "font-style", "text-anchor", "dominant-baseline"].forEach(function (name) {
        if (!node.style.getPropertyValue(name)) node.style.setProperty(name, style.getPropertyValue(name));
      });
      var frame = document.createElementNS(ns, "g");
      var transform = overlay.getScreenCTM().inverse().multiply(matrix);
      frame.setAttribute("transform", `matrix(${transform.a} ${transform.b} ${transform.c} ${transform.d} ${transform.e} ${transform.f})`);
      if (!["scale", "scaleBar"].includes(detail.component)) frame.setAttribute("data-component", detail.component);
      if (detail.cluster != null) frame.setAttribute("data-cluster", detail.cluster);
      overlay.appendChild(frame);
      frame.appendChild(node);
      node.style.pointerEvents = "visiblePainted";
      placement.frame = frame;
      return placement;
    };
    var liftMovedText = function () {
      el.querySelectorAll('text.label, text[data-part], [data-component="scaleBar"], [data-component="scale"]').forEach(function (node) {
        if (!(node.__drawn__?.moveX || node.__drawn__?.moveY)) return;
        var detail = eventData({ target: node });
        if (detail && draggable.includes(detail.component)) liftText(node, detail);
      });
    };
    var dragState = null;
    var queuedRender = null;
    var flushQueuedRender = function () {
      var input = queuedRender;
      queuedRender = null;
      if (input) renderValue(input);
    };
    var finishDrag = function (commit) {
      if (!dragState) return;
      var state = dragState;
      dragState = null;
      window.removeEventListener("pointermove", moveDrag);
      window.removeEventListener("pointerup", endDrag);
      window.removeEventListener("pointercancel", cancelDrag);
      window.removeEventListener("blur", cancelDrag);
      window.removeEventListener("keydown", escapeDrag);
      el.classList.remove("geneviewer-dragging");
      if (!state.moved) { flushQueuedRender(); return; }
      suppressClickUntil = performance.now() + 350;
      if (!commit) {
        state.node.setAttribute("transform", state.transform);
        if (state.placement) {
          var place = state.placement;
          place.parent.insertBefore(state.node, place.next && place.next.parentNode === place.parent ? place.next : null);
          var overlay = place.frame.parentNode;
          place.frame.remove();
          if (!overlay.firstChild) overlay.remove();
        }
        selected = state.selection;
        applySelection();
        el.dispatchEvent(new CustomEvent("geneviewer:dragmove", { bubbles: true,
          detail: Object.assign({}, state.detail, { position: { moveX: state.moveX, moveY: state.moveY } }) }));
        flushQueuedRender();
        return;
      }
      state.node.__drawn__ = Object.assign({}, state.node.__drawn__, state.position);
      var detail = eventData({ target: state.node }, true);
      detail.position = state.position;
      flushQueuedRender();
      if (events.select === true) report("select", detail);
      report("drag", detail);
    };
    var cancelDrag = function () { finishDrag(false); };
    var escapeDrag = function (event) {
      if (event.key === "Escape" && dragState) { event.preventDefault(); cancelDrag(); }
    };
    var endDrag = function (event) {
      if (dragState && event.pointerId === dragState.id) finishDrag(true);
    };
    var moveDrag = function (event) {
      var state = dragState;
      if (!state || event.pointerId !== state.id) return;
      if (!state.moved && Math.hypot(event.clientX - state.x, event.clientY - state.y) < 4) return;
      if (!state.moved) {
        state.moved = true;
        state.placement = liftText(state.node, state.detail);
        if (events.select === true) {
          // Keep server redraws until release; moving the pointer is a preview.
          selected = state.detail;
          applySelection();
        }
        el.classList.add("geneviewer-dragging");
      }
      event.preventDefault();
      var point = new DOMPoint(event.clientX, event.clientY).matrixTransform(state.inverse);
      var dx = point.x - state.start.x, dy = point.y - state.start.y;
      state.position = {
        moveX: Math.round((state.moveX + dx) * 100) / 100,
        moveY: Math.round((state.moveY + dy) * 100) / 100
      };
      state.node.setAttribute("transform", `translate(${dx}, ${dy}) ${state.transform}`);
      el.dispatchEvent(new CustomEvent("geneviewer:dragmove", { bubbles: true,
        detail: Object.assign({}, state.detail, { position: state.position }) }));
    };
    var startDrag = function (event) {
      if (event.button !== 0 || event.isPrimary === false) return;
      var detail = eventData(event, true);
      if (!detail || !draggable.includes(detail.component) || keyOf(detail) === null) return;
      var node = event.target.closest(["scale", "scaleBar"].includes(detail.component) ? '[data-component="' + detail.component + '"]' : "text");
      if (!node) return;
      var matrix = node.parentNode.getScreenCTM();
      if (!matrix) return;
      var inverse = matrix.inverse();
      dragState = {
        id: event.pointerId, node: node, detail: detail, inverse: inverse, selection: selected,
        start: new DOMPoint(event.clientX, event.clientY).matrixTransform(inverse),
        x: event.clientX, y: event.clientY, transform: node.getAttribute("transform") || "",
        moveX: detail.style.moveX ?? 0, moveY: detail.style.moveY ?? 0, moved: false
      };
      window.addEventListener("pointermove", moveDrag, { passive: false });
      window.addEventListener("pointerup", endDrag);
      window.addEventListener("pointercancel", cancelDrag);
      window.addEventListener("blur", cancelDrag);
      window.addEventListener("keydown", escapeDrag);
    };

    // A link below a cluster SVG gets no :hover, so it is highlighted here
    var hovered = null;
    var setHovered = function (node) {
      if (hovered === node) return;
      if (hovered) hovered.classList.remove("geneviewer-hover");
      hovered = node;
      if (hovered) hovered.classList.add("geneviewer-hover");
      el.classList.toggle("geneviewer-hover-below", !!hovered);
    };
    var onMouseMove = function (event) {
      var target = targetOf(event);
      setHovered(target === event.target ? null : target.closest("[data-component]"));
    };
    var onMouseLeave = function () { setHovered(null); };
    var onKeyDown = function (event) {
      if (event.key === "Escape") {
        if (dragState) cancelDrag(); else selectElement(null);
        return;
      }
      if (event.key === "Enter" && events.dblclick === true && event.target.matches(selectableNodes)) {
        event.preventDefault();
        onDoubleClick(event);
      }
    };

    var wireEvents = function () {
      cancelDrag();
      var click = HTMLWidgets.shinyMode && events.click === true;
      el.removeEventListener("click", onClick);
      el.removeEventListener("dblclick", onDoubleClick);
      el.removeEventListener("keydown", onKeyDown);
      el.removeEventListener("pointerdown", startDrag);
      el.removeEventListener("mousemove", onMouseMove);
      el.removeEventListener("mouseleave", onMouseLeave);
      // A redraw removed the hovered link
      hovered = null;
      el.classList.remove("geneviewer-hover-below");
      var highlight = (click || events.select === true || events.dblclick === true) && events.highlight === true;
      if (highlight) {
        el.addEventListener("mousemove", onMouseMove);
        el.addEventListener("mouseleave", onMouseLeave);
      }
      if (click || events.select === true) el.addEventListener("click", onClick);
      if (events.dblclick === true) el.addEventListener("dblclick", onDoubleClick);
      if (events.select === true || events.drag === true) el.addEventListener("keydown", onKeyDown);
      if (events.drag === true) el.addEventListener("pointerdown", startDrag);
      el.classList.toggle("geneviewer-draggable", events.drag === true);
      if (events.select !== true && selected) { selected = null; report("select", null); }
      el.classList.toggle("geneviewer-selectable", events.select === true);
      el.classList.toggle("geneviewer-highlight", highlight);
      applySelection(true);
    };

    var draw = function (width, height) {
      cancelDrag();

      // Keep legend visibility while editing or resizing the same chart.
      var hiddenGroups = preserveLegend ? Array.from(el.querySelectorAll(".legend.unselected"), function (node) { return node.__data__; }) : [];
      preserveLegend = true;
      // Clear out the container if it has anything
      d3.select(el).selectAll('*').remove();

      // Remove the chart styles of an earlier render (e.g. in Shiny) first, so
      // the base styles below are kept when a removed style overrode one of them
      appliedStyleKeys.forEach(function (key) { el.style[key] = ""; });

      if (HTMLWidgets.shinyMode) {
        // htmlwidgets does not apply a rendered widget's width to a Shiny
        // output. Apply pixel widths before measuring the title and legend.
        // Leave standalone and percentage sizing to the htmlwidgets host.
        var fixedWidth = typeof size.width === "number" ? size.width + "px" : size.width;
        var fixed = typeof fixedWidth === "string" && /^\d+(\.\d+)?px$/.test(fixedWidth);
        if (fixed) {
          el.style.width = fixedWidth;
          sizedWidth = true;
        } else if (sizedWidth) {
          el.style.width = outputWidth;
          sizedWidth = false;
        }
        // fillRow() uses a zero flex basis, which otherwise ignores width.
        // Only change horizontal flex sizing; vertical height filling stays
        // with the host. Restore the original flex rule for responsive charts.
        var parentStyle = el.parentElement && getComputedStyle(el.parentElement);
        var inRow = parentStyle && /^(inline-)?flex$/.test(parentStyle.display) &&
          /^row/.test(parentStyle.flexDirection);
        if (fixed && inRow) {
          el.style.flex = "0 0 auto";
          sizedFlex = true;
        } else if (sizedFlex) {
          el.style.flex = outputFlex;
          sizedFlex = false;
        }
      }

      el.style["height"] = "100%"
      el.style["position"] = "relative"
      // Keep padding (GC_grid top/bottom margins) inside the chart height
      el.style["boxSizing"] = "border-box"

      // Apply styles: GC_chart(style) over the chart style of the theme
      var chartStyle = mergeOptions({ style: {} }, "chartOptions", { style: style }, theme).style;
      appliedStyleKeys = Object.keys(chartStyle);
      for (var key in chartStyle) {
          el.style[key] = chartStyle[key];
      }

      // Add Title
      // Sizes are computed into copies of the options on every draw, so a "%"
      // height from R is used again on the next draw (resize)

      var titleHeight = 0;

      if (titleOptions?.show) {

        var titleContainer = d3.select(el)
          .append("div")
          .attr("id", `geneviewer-title-container-${widgetId}`)
          .classed("geneviewer-container", true);

        // Default height: room for the title and the subtitle, with the descenders
        // of an 18px title, at the text size of the theme
        var defaultTitleHeight = (titleOptions.title && titleOptions.subtitle ? 50 : 40) * themeTextScale(theme);
        var titleHeightOption = mergeOptions({ height: defaultTitleHeight }, "titleOptions", titleOptions, theme).height;
        titleHeight = computeSize(titleHeightOption, el.clientHeight);
        var titleOpts = { ...titleOptions, width: el.clientWidth, height: titleHeight };

        var title = createContainer(
          `#geneviewer-title-container-${widgetId}`,
          "svg-container",
          "titleOptions",
          titleOpts,
          theme)
          .title(titleOpts.title, titleOpts.subtitle, titleOpts.show ?? false, titleOpts)
      }

      // Add legend

      var legendHeight = (legendOptions?.show === false) ? 0 : computeSize(legendOptions?.height, el.clientHeight);

      if (legendOptions?.group !== null && legendOptions?.show) {

        d3.select(el)
          .append("div")
          .attr("id", `geneviewer-legend-container-${widgetId}`)
          .classed("geneviewer-container", true);

        // The width of the chart, as for the title: in a standalone page the width
        // htmlwidgets passes is that of the window, also for a chart of fixed width
        var legendOpts = { ...legendOptions, width: el.clientWidth,
                           height: computeSize(legendOptions.height, el.clientHeight),
                           editable: events.select === true || events.dblclick === true ||
                             (events.click === true && events.legendClick !== false) };

        createContainer(`#geneviewer-legend-container-${widgetId}`,
          "svg-container",
          "legendOptions",
          legendOpts,
          theme)
          .legendData(data)
          .legend(legendOpts.group ?? false, legendOpts.show ?? false, el.id, legendOpts);

        var legendElement = d3.select(`#geneviewer-legend-container-${widgetId}`).node();
        var legendDimensions = legendElement.getBoundingClientRect();
        legendHeight = legendDimensions.height;

      }

      d3.select(el)
        .append("div")
        .attr("id", `geneviewer-graph-container-${widgetId}`)
        .style("flex-direction", graphContainer["direction"])
        .style("position", "relative")
        .classed("geneviewer-container", true);

      // Add Clusters
      var clusters = Object.keys(series);

      // Add Links: each set from GC_links() keeps its own options
      var linkSets = (Array.isArray(links) ? links : []).map(entry => ({
        data: HTMLWidgets.dataframeToD3(entry.data),
        options: entry.options ?? {}
      }));

      clusters.forEach(function (clusterKey) {

        var cluster = series[clusterKey],
            containerOptions = cluster.container,
            clusterOptions = cluster.cluster,
            clusterData = HTMLWidgets.dataframeToD3(series[clusterKey].data),
            scaleOptions = cluster.scale,
            clusterTitleOptions = cluster.clusterTitle,
            footerOptions = cluster.footer,
            clusterLabelOptions = cluster.clusterLabel,
            labelOptions = cluster.labels,
            sequenceOptions = cluster.sequence,
            geneOptions = cluster.genes,
            transcriptOptions = cluster.transcript,
            coordinateOptions = cluster.coordinates,
            scaleBarOptions = cluster.scaleBar,
            annotationOptions = cluster.annotations,
            trackMouse = cluster.trackMouse,
            tooltipOptions = cluster.tooltip;

        var clonedContainerOptions = JSON.parse(JSON.stringify(containerOptions));
        clonedContainerOptions.height = computeSize(clonedContainerOptions.height, el.clientHeight);
        clonedContainerOptions.height -= titleHeight / clusters.length;
        clonedContainerOptions.height -= legendHeight ? (legendHeight / clusters.length) : 0;
        // Percentage heights: take the top and bottom margins (padding) from the clusters
        if (typeof containerOptions.height === "string" && containerOptions.height.endsWith("%")) {
          var padding = (parseFloat(chartStyle.paddingTop) || 0) + (parseFloat(chartStyle.paddingBottom) || 0);
          clonedContainerOptions.height -= padding / clusters.length;
        }
        clonedContainerOptions.width = computeSize(clonedContainerOptions.width, el.clientWidth);

        // Room for the cluster label in the left or right margin
        if (clusterLabelOptions?.show) {
          clonedContainerOptions.margin = {
            ...clonedContainerOptions.margin,
            ...clusterLabelMargin(containerOptions, clusterLabelOptions, theme)
          };
        }

        var chart = createContainer(`#geneviewer-graph-container-${widgetId}`, "svg-container", 'containerOptions',  clonedContainerOptions, theme)
          .cluster(clusterOptions)
          .title(clusterTitleOptions?.title, clusterTitleOptions?.subtitle, clusterTitleOptions?.show ?? false, clusterTitleOptions, "clusterTitleOptions")
          .footer(footerOptions?.title, footerOptions?.subtitle, footerOptions?.show ?? false, footerOptions)
          .clusterLabel(clusterLabelOptions?.title, clusterLabelOptions?.show ?? false, clusterLabelOptions)
          .geneData(data, clusterData)
          .scale(scaleOptions)
          .sequence(sequenceOptions?.show ?? false, sequenceOptions)
          .genes(geneOptions?.group, geneOptions?.show ?? false, geneOptions)
          .transcript(transcriptOptions?.group, transcriptOptions?.show ?? false, transcriptOptions);

        chart.svg.attr("data-cluster", clusterKey);

        linkSets.forEach(linkSet => {
          chart.links(getClusterLinks(linkSet.data, clusterKey), clusterKey, linkSet.options);
        });

        chart
          .coordinates(coordinateOptions?.show ?? false, coordinateOptions)
          .labels(labelOptions?.label, labelOptions?.show ?? false, labelOptions)
          .scaleBar(scaleBarOptions?.show ?? false, scaleBarOptions)
          .addAnnotations(annotationOptions)
          .trackMouse(trackMouse?.show ?? false, trackMouse)
          .tooltip(tooltipOptions?.show ?? false, tooltipOptions);

      });

      // Bottom legend: drawn above to measure its height, then moved below the clusters
      var legendPosition = mergeOptions({ position: "bottom" }, "legendOptions", legendOptions, theme).position;
      if (legendPosition == "bottom"&& legendOptions?.show && legendOptions?.group !== null) {
        el.appendChild(legendElement);
      }

      // Save icon
      createSaveIcon(widgetId, el, saveButton);

      el.querySelectorAll(".legend").forEach(function (node) {
        if (hiddenGroups.includes(node.__data__)) {
          node.dispatchEvent(new MouseEvent("click", { bubbles: false }));
        }
      });

    };

    var addLinks = function () {
      if (!links || links.length === 0) {
        return;
      }
      const graphContainer = d3.select(`#geneviewer-graph-container-${widgetId}`);
      makeLinks(graphContainer, links, theme);
      makeColorBar(graphContainer, links, theme);
    };

    var renderValue = function (input) {
        var nextSignature = JSON.stringify([input.data, input.group, input.cluster, input.legend?.group]);
        // A settings response must not remove the text under an active pointer.
        // Data changes and disabling drag still redraw immediately and cancel it.
        if (dragState && nextSignature === dataSignature && input.events?.drag === true) {
          queuedRender = input;
          return;
        }
        queuedRender = null;
        cancelDrag();
        if (dataSignature !== null && nextSignature !== dataSignature) {
          preserveLegend = false;
          if (selected) { selected = null; report("select", null); }
        }
        dataSignature = nextSignature;
        graphContainer = input.graphContainer;
        style = input.style;
        data = HTMLWidgets.dataframeToD3(input.data);
        links = input.links;
        series = input.series;
        saveButton = input.saveButton;
        titleOptions = input.title;
        legendOptions = input.legend;
        theme = input.theme ?? {};
        events = input.events ?? {};
        size = input.size ?? {};
        draw(width, height);
        addLinks();
        liftMovedText();
        wireEvents();
        if (events.rendered === true) {
          var detail = {
            id: events.renderId ?? (el.id + "-render-" + (++renderSequence)),
            chartId: el.id,
            width: el.clientWidth,
            height: el.clientHeight
          };
          if (HTMLWidgets.shinyMode) {
            Shiny.setInputValue(el.id + "_rendered", detail, { priority: "event" });
          }
          el.dispatchEvent(new CustomEvent("geneviewer:rendered", { bubbles: true, detail: detail }));
        }
    };
    return {
      renderValue: renderValue,
      resize: function (width, height) {
        draw(width, height);
        addLinks();
        liftMovedText();
        wireEvents();
      }
    };
  }
});
})();
