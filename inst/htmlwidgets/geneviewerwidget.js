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
    // data-component), its cluster, and the data of the gene, label or link
    var clickData = ["genes", "transcript", "labels", "links", "legend"];
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
    var eventData = function (event) {
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
      return {
        component: component,
        cluster: cluster ? cluster.getAttribute("data-cluster") : null,
        data: datum
      };
    };

    var report = function (name, detail) {
      if (HTMLWidgets.shinyMode) {
        Shiny.setInputValue(el.id + "_" + name, detail, { priority: "event" });
      }
      el.dispatchEvent(new CustomEvent("geneviewer:" + name, { bubbles: true, detail: detail }));
    };

    var sameSelection = function (a, b) {
      return a && b && a.component === b.component && a.cluster === b.cluster &&
        a.data.rowID === b.data.rowID;
    };
    var applySelection = function () {
      var found = false;
      el.querySelectorAll(".gene, text.label").forEach(function (node) {
        var detail = eventData({ target: node });
        var active = !!sameSelection(selected, detail);
        node.classList.toggle("geneviewer-selected", active);
        if (events.select === true) {
          node.setAttribute("tabindex", "0");
          node.setAttribute("role", "button");
          node.setAttribute("aria-pressed", String(active));
          node.setAttribute("aria-label", detail.component + ": " + (detail.data.name ?? detail.data.rowID));
        }
        if (active) { selected = detail; found = true; }
      });
      if (selected && !found) { selected = null; report("select", null); }
    };
    // Only genes and labels can be selected: any other part of the chart ends
    // the selection, as empty space does
    var selectElement = function (detail) {
      if (detail && (!["genes", "labels"].includes(detail.component) || !detail.data)) detail = null;
      if ((!selected && !detail) || sameSelection(selected, detail)) return;
      selected = detail;
      applySelection();
      report("select", selected);
    };
    var onClick = function (event) {
      var detail = eventData(event);
      if (events.select === true && event.detail < 2) {
        // Controls such as the save button are not empty chart space.
        if (detail || event.target.closest("svg") || event.target === el) selectElement(detail ?? null);
      }
      if (!detail || (detail.component === "legend" && events.legendClick === false)) return;
      if (HTMLWidgets.shinyMode && events.click === true) {
        Shiny.setInputValue(el.id + "_click", detail, { priority: "event" });
      }
    };
    var onDoubleClick = function (event) {
      var detail = eventData(event);
      if (!detail || detail.component === "legend") return;
      if (events.select === true) selectElement(detail);
      report("dblclick", detail);
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
      if (event.key === "Escape") { selectElement(null); return; }
      if (event.key === "Enter" && events.dblclick === true && event.target.matches(".gene, text.label")) {
        event.preventDefault();
        onDoubleClick(event);
      }
    };

    var wireEvents = function () {
      var click = HTMLWidgets.shinyMode && events.click === true;
      el.removeEventListener("click", onClick);
      el.removeEventListener("dblclick", onDoubleClick);
      el.removeEventListener("keydown", onKeyDown);
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
      if (events.select === true) el.addEventListener("keydown", onKeyDown);
      if (events.select !== true && selected) { selected = null; report("select", null); }
      el.classList.toggle("geneviewer-selectable", events.select === true);
      el.classList.toggle("geneviewer-highlight", highlight);
      applySelection();
    };

    var draw = function (width, height) {

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
                           height: computeSize(legendOptions.height, el.clientHeight) };

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

    return {
      renderValue: function (input) {
        var nextSignature = JSON.stringify([input.data, input.group, input.cluster, input.legend?.group]);
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
      },
      resize: function (width, height) {
        draw(width, height);
        addLinks();
        wireEvents();
      }
    };
  }
});
})();
