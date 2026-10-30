(function () {
// Shared by all chart instances, including ones removed and recreated.
var renderSequence = 0;
// The keyboard shortcuts of the chart last clicked or focused: its moves are
// undone with Ctrl/Cmd+Z. One listener for all charts, so a chart that is
// removed leaves none behind.
var activeShortcuts = null;
// Fields, menus and other controls keep their own keys
var ownKeys = function (node) {
  return !node || node.isContentEditable || /^(INPUT|TEXTAREA|SELECT)$/.test(node.tagName) ||
    !!(node.closest && node.closest('[role="combobox"], [role="listbox"], [role="menu"], ' +
      '[role="dialog"], [role="slider"], [role="tablist"], [role="radiogroup"], [role="spinbutton"]'));
};
document.addEventListener("keydown", function (event) {
  if (!activeShortcuts || event.defaultPrevented || event.altKey || ownKeys(event.target)) return;
  activeShortcuts(event);
});
var onMac = /Mac|iPhone|iPad/.test(navigator.platform || navigator.userAgent);

// Scale axes and scale bars are selected and moved as a whole
var scaleComponents = ["scale", "scaleBar"];
var isScale = function (component) { return scaleComponents.includes(component); };
var scaleNodes = scaleComponents.map(function (c) { return '[data-component="' + c + '"]'; }).join(", ");
// The buttons of the toolbar (GC_toolbar()), in their order
var toolbarButtons = ["undo", "redo", "reset", "zoom", "save"];
// GC_zoom_to() in Shiny: sets the zoom of the chart with that output ID
var zoomTargets = {};
var zoomHandlerAdded = false;

// Positions of moved elements: moveX and moveY (GC_element_style())
var samePosition = function (a, b) {
  return !!a && !!b && Math.abs(a.moveX - b.moveX) < 0.01 && Math.abs(a.moveY - b.moveY) < 0.01;
};

// Listen on the window while a drag or a selection box lasts. Gives the
// function that stops listening.
var listenWindow = function (handlers) {
  Object.keys(handlers).forEach(function (type) {
    window.addEventListener(type, handlers[type], type === "pointermove" ? { passive: false } : undefined);
  });
  return function () {
    Object.keys(handlers).forEach(function (type) { window.removeEventListener(type, handlers[type]); });
  };
};

// The steps of moves that can be undone and redone (GC_events(drag = TRUE)).
// A step lists its elements, by key, with their position before and after it.
// Steps that merge (arrow-key nudges) take in the next step of the same
// elements within a second. Keeps the last 100 steps.
var createMoveHistory = function () {
  var past = [], future = [];
  var keysOf = function (items) { return items.map(function (item) { return item.key; }).join("\n"); };
  return {
    canUndo: function () { return past.length > 0; },
    canRedo: function () { return future.length > 0; },
    clear: function () { past = []; future = []; },
    record: function (items, merge) {
      items = items.filter(function (item) {
        return item.key !== null && !samePosition(item.before, item.after);
      });
      if (!items.length) return;
      var last = past[past.length - 1], now = performance.now();
      if (merge && last && last.merge && keysOf(last.items) === keysOf(items) && now - last.time < 1000) {
        last.items.forEach(function (item, i) { item.after = items[i].after; });
        last.time = now;
      } else {
        past.push({ items: items, merge: merge, time: now });
        if (past.length > 100) past.shift();
      }
      future = [];
    },
    // The step that undo (or redo) would take, or undefined
    next: function (undo) {
      var from = undo ? past : future;
      return from[from.length - 1];
    },
    // Take that step: gives the positions its elements move to
    take: function (undo) {
      var entry = (undo ? past : future).pop();
      entry.merge = false;
      (undo ? future : past).push(entry);
      return entry.items.map(function (item) {
        return { key: item.key, position: undo ? item.before : item.after };
      });
    },
    // Where each element of a step is now, after the steps taken
    expected: function () {
      var positions = {};
      future.forEach(function (entry) {
        entry.items.forEach(function (item) { positions[item.key] = item.before; });
      });
      past.forEach(function (entry) {
        entry.items.forEach(function (item) { positions[item.key] = item.after; });
      });
      return positions;
    }
  };
};

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
      toolbarOptions,
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
    var selectableNodes = ".gene, text.label, text.coordinate, [data-part], " + scaleNodes;
    // The parts that can be dragged: all of them but genes and coordinates
    var draggable = selectable.filter(function (c) { return !["genes", "coordinates"].includes(c); });
    var draggableNodes = "text.label, [data-part], " + scaleNodes;
    // The selected elements, in the order they were selected; the last one is
    // the primary selection, reported as select
    var selection = [];
    var selected = null;
    var setSelection = function (list) {
      selection = list;
      selected = list[list.length - 1] ?? null;
    };
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
      if (isScale(component)) return Object.assign({ moveX: 0, moveY: 0 }, node.__drawn__);
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
      if (withStyle && keyOf(detail) !== null) detail.style = drawnStyle(component, isScale(component) ? part : component === "legend" ? textPart : node);
      return detail;
    };
    // What identifies a selectable element: a gene, label or coordinate by its
    // row (and a coordinate by its start or end), a title or subtitle by its
    // cluster and part. Null for the other parts of the chart.
    var keyOf = function (d) {
      if (!d || !selectable.includes(d.component)) return null;
      if (isScale(d.component)) return [d.component, d.cluster].join("|");
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
    // The keys of a list of elements, to compare selections
    var keysOf = function (list) { return JSON.stringify(list.map(keyOf)); };

    var report = function (name, detail) {
      if (HTMLWidgets.shinyMode) {
        // A list of elements keeps its elements in R (see geneviewer-shiny.R)
        var type = Array.isArray(detail) ? ":geneviewer.list" : "";
        Shiny.setInputValue(el.id + "_" + name + type, detail, { priority: "event" });
      }
      el.dispatchEvent(new CustomEvent("geneviewer:" + name, { bubbles: true, detail: detail }));
    };

    var sameSelection = function (a, b) {
      return keyOf(a) !== null && keyOf(a) === keyOf(b);
    };
    var isSelected = function (detail) {
      var key = keyOf(detail);
      return key !== null && selection.some(function (item) { return keyOf(item) === key; });
    };
    var reportSelection = function () {
      report("select", selected);
      report("selection", selection);
    };
    var applySelection = function (refresh) {
      var previous = JSON.stringify(selection.map(function (item) { return item.style; }));
      var fresh = {};
      el.querySelectorAll(selectableNodes).forEach(function (node) {
        var detail = eventData({ target: node });
        var key = keyOf(detail);
        if (key === null) return;
        var active = isSelected(detail);
        node.classList.toggle("geneviewer-selected", active);
        if (events.select === true) {
          node.setAttribute("tabindex", "0");
          node.setAttribute("role", "button");
          node.setAttribute("aria-pressed", String(active));
          node.setAttribute("aria-label", detail.component + ": " +
            (detail.component === "legend" ? String(detail.data) + " " + detail.part :
              detail.data?.name ?? node.__text__ ?? node.textContent));
        }
        if (active && !fresh[key]) fresh[key] = eventData({ target: node }, true);
      });
      var next = selection.map(function (item) { return fresh[keyOf(item)]; }).filter(Boolean);
      var lost = next.length !== selection.length;
      setSelection(next);
      if (lost || (refresh && selection.length &&
          JSON.stringify(selection.map(function (item) { return item.style; })) !== previous)) {
        reportSelection();
      }
    };
    // Only the selectable elements can be selected: any other part of the
    // chart ends the selection, as empty space does. mode: "replace" the
    // selection, "toggle" the elements in it, or "add" them to it.
    var mergeSelection = function (base, details, mode) {
      var next = mode === "replace" ? [] : base.slice();
      details.forEach(function (detail) {
        if (keyOf(detail) === null) return;
        var at = next.findIndex(function (item) { return keyOf(item) === keyOf(detail); });
        if (at >= 0 && mode === "toggle") next.splice(at, 1);
        else if (at < 0) next.push(detail);
      });
      return next;
    };
    var changeSelection = function (details, mode, quiet) {
      var next = mergeSelection(selection, details, mode);
      if (keysOf(next) === keysOf(selection)) return;
      setSelection(next);
      applySelection();
      if (!quiet) reportSelection();
    };
    var selectElement = function (detail) {
      changeSelection(detail ? [detail] : [], "replace");
    };
    var suppressClickUntil = 0;
    var onClick = function (event) {
      if (performance.now() < suppressClickUntil) { event.preventDefault(); event.stopPropagation(); return; }
      var detail = eventData(event, true);
      if (events.select === true && event.detail < 2) {
        // Controls such as the save button are not empty chart space. Shift,
        // Ctrl or Cmd adds an element to the selection or takes it out; a
        // click on the only selected element ends its selection.
        var adds = event.shiftKey || event.metaKey || event.ctrlKey;
        if (adds) {
          if (keyOf(detail) !== null) changeSelection([detail], "toggle");
        } else if (detail || event.target.closest("svg") || event.target === el) {
          var only = selection.length === 1 && sameSelection(selected, detail);
          selectElement(only ? null : detail ?? null);
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
    // The node a drag moves: a whole scale, a legend label or marker on its
    // own, or the text of a label or title
    var movableOf = function (target, component) {
      if (isScale(component)) return target.closest('[data-component="' + component + '"]');
      return target.closest(component === "legend" ? "[data-part]" : "text");
    };
    // Moved elements (text, legend parts and whole scales) share one
    // chart-wide viewport, above the cluster SVGs. Each keeps the coordinate
    // system of its original parent and its logical component.
    var liftMovable = function (node, detail) {
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
        Object.assign(overlay.style, { position: "absolute", left: "0", top: "0",
          width: "100%", height: "100%", pointerEvents: "none", zIndex: "4" });
        el.appendChild(overlay);
      }
      var placement = { parent: parent, next: node.nextSibling };
      // A legend label or marker keeps its legend item, which toggles its group
      if (detail.component === "legend") node.__home__ = parent.closest(".legend");
      var style = getComputedStyle(node);
      ["fill", "fill-opacity", "stroke", "stroke-width", "stroke-opacity", "font-family",
        "font-size", "font-weight", "font-style", "text-anchor", "dominant-baseline"].forEach(function (name) {
        if (!node.style.getPropertyValue(name)) node.style.setProperty(name, style.getPropertyValue(name));
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
    // Moved legend parts are dimmed with the rest of a hidden group
    var syncLegendFrames = function () {
      el.querySelectorAll(".geneviewer-move-overlay [data-part]").forEach(function (node) {
        if (node.__home__) node.parentNode.classList.toggle("unselected", node.__home__.classList.contains("unselected"));
      });
    };
    var liftMovedElements = function () {
      el.querySelectorAll(draggableNodes).forEach(function (node) {
        if (!(node.__drawn__?.moveX || node.__drawn__?.moveY)) return;
        var detail = eventData({ target: node });
        if (detail && draggable.includes(detail.component)) liftMovable(node, detail);
      });
    };
    // Where an element is while it is dragged or nudged, for live editors
    var emitDragMove = function (detail, position) {
      el.dispatchEvent(new CustomEvent("geneviewer:dragmove", { bubbles: true,
        detail: Object.assign({}, detail, { position: position }) }));
    };
    var dragState = null;
    var queuedRender = null;
    // Gives whether there was a render to draw
    var flushQueuedRender = function () {
      var input = queuedRender;
      queuedRender = null;
      if (input) renderValue(input);
      return !!input;
    };
    // A drag moves one element, or all movable selected elements when it
    // starts on one of them. Each keeps its own coordinate system.
    var finishDrag = function (commit) {
      if (!dragState) return;
      var state = dragState;
      dragState = null;
      state.stopListening();
      el.classList.remove("geneviewer-dragging");
      if (!state.moved) { flushQueuedRender(); return; }
      suppressClickUntil = performance.now() + 350;
      if (!commit) {
        state.items.forEach(function (item) {
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
      var details = state.items.map(function (item) {
        item.node.__drawn__ = Object.assign({}, item.node.__drawn__, item.position);
        var detail = eventData({ target: item.node }, true);
        detail.position = item.position;
        detail.action = "drag";
        return detail;
      });
      // A settings redraw held back during the drag was sent before the server
      // knew of it: it draws the elements where they were, so they go back to
      // where they were dropped. The step is recorded after the redraw, which
      // would otherwise find it stale.
      if (flushQueuedRender()) {
        var nodes = movableNodes();
        state.items.forEach(function (item) {
          var node = nodes[keyOf(item.detail)];
          if (node) placeAt(node, item.position);
        });
      }
      moveHistory.record(state.items.map(function (item) {
        return { key: keyOf(item.detail), before: { moveX: item.moveX, moveY: item.moveY }, after: item.position };
      }), false);
      updateToolbar();
      announce(details, false);
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
        state.items.forEach(function (item) { item.placement = liftMovable(item.node, item.detail); });
        // Dragging an element that is not selected selects it alone. Server
        // redraws wait until release; moving the pointer is a preview.
        if (events.select === true && state.items.length === 1 && !isSelected(state.items[0].detail)) {
          setSelection([state.items[0].detail]);
          applySelection();
        }
        el.classList.add("geneviewer-dragging");
      }
      event.preventDefault();
      state.items.forEach(function (item) {
        var point = new DOMPoint(event.clientX, event.clientY).matrixTransform(item.inverse);
        // Whole pixels: a scaled chart makes fractions of a pixel that no one
        // means, and the offsets stay readable in code
        item.position = {
          moveX: Math.round(item.moveX + point.x - item.start.x),
          moveY: Math.round(item.moveY + point.y - item.start.y)
        };
        var dx = item.position.moveX - item.moveX, dy = item.position.moveY - item.moveY;
        item.node.setAttribute("transform", `translate(${dx}, ${dy}) ${item.transform}`);
        emitDragMove(item.detail, item.position);
      });
    };
    // What a drag would move, for each element: its node and where it starts
    var dragItem = function (node, detail, event) {
      var matrix = node.parentNode.getScreenCTM();
      if (!matrix) return null;
      var inverse = matrix.inverse();
      return {
        node: node, detail: detail, inverse: inverse,
        start: new DOMPoint(event.clientX, event.clientY).matrixTransform(inverse),
        transform: node.getAttribute("transform") || "",
        moveX: detail.style?.moveX ?? positionOf(node).moveX, moveY: detail.style?.moveY ?? positionOf(node).moveY
      };
    };
    var startDrag = function (event) {
      if (event.button !== 0 || event.isPrimary === false) return;
      var detail = eventData(event, true);
      if (!detail || !draggable.includes(detail.component) || keyOf(detail) === null) {
        startMarquee(event, detail);
        return;
      }
      if (events.drag !== true) return;
      var node = movableOf(event.target, detail.component);
      if (!node) return;
      var first = dragItem(node, detail, event);
      if (!first) return;
      var items = [first];
      // On a selected element: all selected elements that can move
      if (events.select === true && isSelected(detail) && selection.length > 1) {
        var nodes = movableNodes();
        selection.forEach(function (item) {
          var key = keyOf(item);
          if (key === keyOf(detail) || !nodes[key]) return;
          var other = dragItem(nodes[key], eventData({ target: nodes[key] }, true), event);
          if (other) items.push(other);
        });
      }
      dragState = {
        id: event.pointerId, items: items, selection: selection.slice(),
        x: event.clientX, y: event.clientY, moved: false,
        stopListening: listenWindow({ pointermove: moveDrag, pointerup: endDrag,
                                      pointercancel: cancelDrag, blur: cancelDrag, keydown: escapeDrag })
      };
    };

    // Box select (GC_events(select = TRUE)): dragging on empty space selects
    // the elements entirely inside the box; Shift, Ctrl or Cmd adds them
    var marquee = null;
    var startMarquee = function (event, detail) {
      if (events.select !== true || keyOf(detail) !== null ||
          event.target.closest(".geneviewer-toolbar") || !el.contains(event.target)) return;
      marquee = { id: event.pointerId, x: event.clientX, y: event.clientY, node: null,
                  mode: event.shiftKey || event.metaKey || event.ctrlKey ? "add" : "replace",
                  selection: selection.slice(),
                  stopListening: listenWindow({ pointermove: moveMarquee, pointerup: endMarquee,
                                                pointercancel: cancelMarquee, blur: cancelMarquee,
                                                keydown: escapeMarquee }) };
    };
    var marqueeHits = function (rect) {
      var hits = {};
      el.querySelectorAll(selectableNodes).forEach(function (node) {
        var detail = eventData({ target: node });
        var key = keyOf(detail);
        if (key === null || hits[key]) return;
        var r = node.getBoundingClientRect();
        if ((r.width || r.height) && r.left >= rect.left && r.right <= rect.right &&
            r.top >= rect.top && r.bottom <= rect.bottom) hits[key] = eventData({ target: node }, true);
      });
      return Object.values(hits);
    };
    var moveMarquee = function (event) {
      if (!marquee || event.pointerId !== marquee.id) return;
      if (!marquee.node && Math.hypot(event.clientX - marquee.x, event.clientY - marquee.y) < 4) return;
      event.preventDefault();
      var rect = { left: Math.min(marquee.x, event.clientX), right: Math.max(marquee.x, event.clientX),
                   top: Math.min(marquee.y, event.clientY), bottom: Math.max(marquee.y, event.clientY) };
      if (!marquee.node) {
        marquee.node = document.createElement("div");
        marquee.node.className = "geneviewer-marquee";
        el.appendChild(marquee.node);
      }
      // In the chart's own pixels, also when the chart is scaled
      var box = el.getBoundingClientRect(), sx = box.width / el.offsetWidth || 1, sy = box.height / el.offsetHeight || 1;
      Object.assign(marquee.node.style, {
        left: (rect.left - box.left) / sx + "px", top: (rect.top - box.top) / sy + "px",
        width: (rect.right - rect.left) / sx + "px", height: (rect.bottom - rect.top) / sy + "px"
      });
      // The selection follows the box while it is drawn, and is reported once
      setSelection(mergeSelection(marquee.selection, marqueeHits(rect), marquee.mode));
      applySelection();
    };
    var finishMarquee = function (commit) {
      if (!marquee) return;
      var state = marquee;
      marquee = null;
      state.stopListening();
      if (!state.node) return;
      state.node.remove();
      // The click that ends the box does not clear what it selected
      suppressClickUntil = performance.now() + 350;
      if (!commit) {
        setSelection(state.selection);
        applySelection();
        return;
      }
      if (keysOf(selection) !== keysOf(state.selection)) reportSelection();
    };
    var endMarquee = function (event) {
      if (marquee && event.pointerId === marquee.id) finishMarquee(true);
    };
    var cancelMarquee = function () { finishMarquee(false); };
    var escapeMarquee = function (event) {
      if (event.key === "Escape" && marquee) { event.preventDefault(); cancelMarquee(); }
    };

    // Undo, redo and reset of moves (GC_events(drag = TRUE)). Each drag is
    // a step, arrow-key nudges of one element within a second are one step,
    // and a reset is one step of all moved elements. Undo, redo and reset
    // report the new positions as a drag (one element) or as moves (more), so
    // an app that keeps drags with GC_element_style() keeps them as well.
    var moveHistory = createMoveHistory();
    // Positions reported to Shiny that no redraw has shown yet: until then a
    // redraw may still show the older position, which is not an outside change
    var awaiting = {};
    // Outside Shiny nothing keeps a move: redraws on resize restore them here
    var localMoves = {};
    // Where each element was when it was first drawn with this data: reset
    // moves it back here, so positions set in R are kept
    var origin = {};
    var nudgeTimer = null;
    var toolbar = null;
    var positionOf = function (node) {
      return { moveX: node.__drawn__?.moveX ?? 0, moveY: node.__drawn__?.moveY ?? 0 };
    };
    // The nodes that move, by element: their text, or a whole scale
    var movableNodes = function () {
      var nodes = {};
      el.querySelectorAll(selectableNodes).forEach(function (node) {
        var detail = eventData({ target: node });
        var key = detail && draggable.includes(detail.component) ? keyOf(detail) : null;
        if (key === null || nodes[key]) return;
        nodes[key] = movableOf(node, detail.component);
      });
      return nodes;
    };
    // Move a node to a position, as a drag does
    var placeAt = function (node, position) {
      var from = positionOf(node);
      var dx = position.moveX - from.moveX, dy = position.moveY - from.moveY;
      if (dx || dy) {
        var detail = eventData({ target: node });
        if (detail) liftMovable(node, detail);
        node.setAttribute("transform", `translate(${dx}, ${dy}) ${node.getAttribute("transform") || ""}`.trim());
      }
      node.__drawn__ = Object.assign({}, node.__drawn__, { moveX: position.moveX, moveY: position.moveY });
    };
    var detailOf = function (node, action) {
      var detail = eventData({ target: node }, true);
      if (!detail) return null;
      detail.position = positionOf(node);
      detail.action = action;
      return detail;
    };
    // Report where elements are now: one element as a drag, more as moves
    var announce = function (details, outline) {
      details.forEach(function (detail) {
        var key = keyOf(detail);
        if (HTMLWidgets.shinyMode) awaiting[key] = detail.position;
        else localMoves[key] = detail.position;
      });
      if (!details.length) return;
      if (events.select === true) {
        // Undo and redo of one element select it, to show what changed
        if (outline && details.length === 1) setSelection([details[0]]);
        applySelection();
        reportSelection();
      }
      if (details.length === 1) report("drag", details[0]);
      else report("moves", { action: details[0].action, moves: details });
    };
    // Elements that are not where they were first drawn
    var movedAway = function (nodes) {
      return Object.keys(nodes).filter(function (key) {
        return origin[key] && !samePosition(positionOf(nodes[key]), origin[key]);
      });
    };
    var updateToolbar = function (nodes) {
      if (!toolbar || events.drag !== true) return;
      var canReset = movedAway(nodes || movableNodes()).length > 0;
      // The buttons show once there is a step or a move to reset; the ones
      // that cannot act are disabled, so none of them shifts
      toolbar.select(".geneviewer-history")
        .property("hidden", !moveHistory.canUndo() && !moveHistory.canRedo() && !canReset);
      toolbar.select(".geneviewer-undo").property("disabled", !moveHistory.canUndo());
      toolbar.select(".geneviewer-redo").property("disabled", !moveHistory.canRedo());
      toolbar.select(".geneviewer-reset").property("disabled", !canReset);
    };
    // Move elements to positions, and report them
    var apply = function (positions, action, outline) {
      var nodes = movableNodes(), details = [];
      positions.forEach(function (target) {
        var node = nodes[target.key];
        if (!node) return;
        placeAt(node, target.position);
        var detail = detailOf(node, action);
        if (detail) details.push(detail);
      });
      clearTimeout(nudgeTimer);
      nudgeTimer = null;
      updateToolbar(nodes);
      announce(details, outline);
    };
    var step = function (undo) {
      var entry = moveHistory.next(undo);
      if (!entry || dragState) return;
      var nodes = movableNodes();
      // Moved or removed by something else since: the steps no longer apply
      var fresh = entry.items.every(function (item) {
        return nodes[item.key] && samePosition(positionOf(nodes[item.key]), undo ? item.after : item.before);
      });
      if (!fresh) {
        moveHistory.clear();
        return updateToolbar(nodes);
      }
      apply(moveHistory.take(undo), undo ? "undo" : "redo", entry.items.length === 1);
    };
    var resetMoves = function () {
      if (dragState) return;
      var nodes = movableNodes();
      var items = movedAway(nodes).map(function (key) {
        return { key: key, before: positionOf(nodes[key]), after: origin[key] };
      });
      if (!items.length) return;
      moveHistory.record(items, false);
      apply(items.map(function (item) { return { key: item.key, position: item.after }; }), "reset", false);
    };
    // After a redraw: remember where new elements are first drawn, and drop
    // steps whose element is no longer where they left it, e.g. when an app
    // moved it back itself
    var checkMoves = function () {
      var nodes = movableNodes();
      Object.keys(nodes).forEach(function (key) {
        if (!origin[key]) origin[key] = positionOf(nodes[key]);
      });
      Object.keys(awaiting).forEach(function (key) {
        if (!nodes[key] || samePosition(positionOf(nodes[key]), awaiting[key])) delete awaiting[key];
      });
      if (nudgeTimer === null && !Object.keys(awaiting).length) {
        var expected = moveHistory.expected();
        var stale = Object.keys(expected).some(function (key) {
          return !nodes[key] || !samePosition(positionOf(nodes[key]), expected[key]);
        });
        if (stale) moveHistory.clear();
      }
      updateToolbar(nodes);
    };
    var restoreLocalMoves = function () {
      var nodes = movableNodes();
      Object.keys(localMoves).forEach(function (key) {
        if (nodes[key]) placeAt(nodes[key], localMoves[key]);
      });
    };
    // Arrow keys move the selected texts and scales, reported once they stop
    var nudge = function (dx, dy) {
      var nodes = movableNodes();
      var items = selection.map(function (item) { return keyOf(item); }).filter(function (key) {
        return key !== null && nodes[key];
      }).map(function (key) {
        var before = positionOf(nodes[key]);
        // Whole pixels, as drags
        var after = { moveX: Math.round(before.moveX + dx), moveY: Math.round(before.moveY + dy) };
        placeAt(nodes[key], after);
        emitDragMove(eventData({ target: nodes[key] }), after);
        return { key: key, before: before, after: after };
      });
      if (!items.length) return false;
      moveHistory.record(items, true);
      updateToolbar();
      clearTimeout(nudgeTimer);
      nudgeTimer = setTimeout(function () {
        nudgeTimer = null;
        var current = movableNodes();
        announce(items.map(function (item) {
          return current[item.key] && detailOf(current[item.key], "nudge");
        }).filter(Boolean), false);
      }, 300);
      return true;
    };
    var onShortcut = function (event) {
      if (events.drag !== true || !el.isConnected) return;
      var key = event.key.toLowerCase();
      if ((event.metaKey || event.ctrlKey) && (key === "z" || key === "y")) {
        event.preventDefault();
        step(!(key === "y" || event.shiftKey));
        return;
      }
      var arrow = { ArrowLeft: [-1, 0], ArrowRight: [1, 0], ArrowUp: [0, -1], ArrowDown: [0, 1] }[event.key];
      if (!arrow || event.metaKey || event.ctrlKey || events.select !== true || dragState) return;
      if (event.target !== document.body && !el.contains(event.target)) return;
      var size = event.shiftKey ? 10 : 1;
      if (nudge(arrow[0] * size, arrow[1] * size)) event.preventDefault();
    };
    var markActive = function () { activeShortcuts = onShortcut; };
    // A drag ends with a click, which must not also select, open or toggle
    // what it ends on, e.g. hide the genes of a legend group. Only that one
    // click: the chart's own buttons, and later clicks, work as usual.
    el.addEventListener("click", function (event) {
      if (performance.now() < suppressClickUntil && !event.target.closest(".geneviewer-toolbar")) {
        suppressClickUntil = 0;
        event.preventDefault();
        event.stopPropagation();
        return;
      }
      // A moved legend part toggles its group like the rest of its item,
      // unless legend clicks edit
      var home = event.target.closest && event.target.closest(".geneviewer-move-overlay [data-part]")?.__home__;
      var legendEdits = events.select === true || events.dblclick === true ||
        (events.click === true && events.legendClick !== false);
      if (home && !legendEdits && home.isConnected) {
        event.stopPropagation();
        home.dispatchEvent(new MouseEvent("click", { bubbles: false, detail: event.detail }));
        syncLegendFrames();
      }
    }, true);
    el.addEventListener("click", syncLegendFrames);
    el.addEventListener("pointerdown", markActive, true);
    el.addEventListener("focusin", markActive);

    // The chart's controls, in a corner (GC_toolbar()): undo, redo and reset
    // of moves, then the save button. By default they show while the mouse is
    // over the chart or a control has keyboard focus, and hide during a drag;
    // undo, redo and reset once there is a move. Gaps between them do not
    // take clicks from the chart.
    // A Font Awesome icon by name ("camera"), or by its classes
    var iconClass = function (icon) {
      return /(^|\s)fa-/.test(icon) ? icon : "fa-solid fa-" + icon;
    };
    var addToolbar = function () {
      toolbar = null;
      var opts = mergeOptions({ show: "hover", position: "topright",
                                buttons: toolbarButtons, icons: {} },
                              "toolbarOptions", toolbarOptions, theme);
      // One button from R arrives as a string
      var buttons = [].concat(opts.buttons);
      var wanted = function (name) { return buttons.includes(name); };
      var movable = events.drag === true && ["undo", "redo", "reset"].some(wanted);
      var save = saveButton && wanted("save");
      var zoomButton = !!zoomOptions && wanted("zoom");
      if (opts.show === "never" || (!movable && !save && !zoomButton)) return;
      var icon = function (name, fallback) { return iconClass(opts.icons[name] ?? fallback); };
      toolbar = d3.select(el).append("div")
        .attr("class", `geneviewer-toolbar geneviewer-toolbar-${opts.position}` +
                       (opts.show === "always" ? " geneviewer-toolbar-always" : ""))
        .attr("id", `geneviewer-toolbar-${widgetId}`)
        .attr("role", "toolbar")
        .attr("aria-label", "Chart");
      // Colours and size, over those of the stylesheet
      var px = function (value) { return typeof value === "number" ? value + "px" : value; };
      [["color", "--gv-toolbar-color"], ["hoverColor", "--gv-toolbar-hover"],
       ["tooltipBackground", "--gv-toolbar-tip-background"], ["tooltipColor", "--gv-toolbar-tip-color"],
       ["tooltipBorder", "--gv-toolbar-tip-border"], ["tooltipRadius", "--gv-toolbar-tip-radius"],
       ["tooltipShadow", "--gv-toolbar-tip-shadow"], ["fontFamily", "--gv-toolbar-font"], ["size", "--gv-toolbar-size"]]
        .forEach(function (v) { if (opts[v[0]] != null) toolbar.style(v[1], px(opts[v[0]])); });
      // icon: the default icon; kbd: the shortcut shown in the tooltip;
      // shortcuts: the same for assistive technology
      var addButton = function (group, b) {
        var button = group.append("button")
          .attr("type", "button")
          .attr("class", "geneviewer-toolbar-button geneviewer-" + b.name)
          .attr("aria-label", b.label)
          .attr("aria-keyshortcuts", b.shortcuts)
          .property("disabled", !!b.disabled)
          .on("click", b.onClick);
        button.append("i").attr("class", icon(b.name, b.icon)).attr("aria-hidden", "true");
        var tip = button.append("span").attr("class", "geneviewer-toolbar-tip").attr("aria-hidden", "true");
        tip.append("span").text(b.label);
        if (b.kbd) tip.append("kbd").text(b.kbd);
      };
      if (movable) {
        var historyGroup = toolbar.append("div")
          .attr("class", "geneviewer-history")
          .attr("role", "group")
          .attr("aria-label", "Positions")
          .property("hidden", true);
        [{ name: "undo", label: "Undo", icon: "rotate-left", kbd: onMac ? "\u2318Z" : "Ctrl+Z",
           shortcuts: "Control+Z Meta+Z", onClick: function () { step(true); } },
         { name: "redo", label: "Redo", icon: "rotate-right", kbd: onMac ? "\u21e7\u2318Z" : "Ctrl+Y",
           shortcuts: "Control+Shift+Z Meta+Shift+Z Control+Y", onClick: function () { step(false); } },
         { name: "reset", label: "Reset positions", icon: "house", kbd: null, shortcuts: null,
           onClick: resetMoves }]
          .filter(function (b) { return wanted(b.name); })
          .forEach(function (b) { addButton(historyGroup, Object.assign({ disabled: true }, b)); });
        // Moving elements and saving the chart are apart, so a click that
        // misses one does not do the other
        if (save || zoomButton) historyGroup.append("span").attr("class", "geneviewer-toolbar-divider");
      }
      // Shows everything again (GC_zoom()), while zoomed in
      if (zoomButton) {
        var zoomGroup = toolbar.append("div")
          .attr("class", "geneviewer-history geneviewer-zoom-buttons")
          .attr("role", "group")
          .attr("aria-label", "Zoom")
          .property("hidden", zoomRange === null);
        addButton(zoomGroup, { name: "zoom", label: "Reset zoom", icon: "magnifying-glass-minus",
                               kbd: null, shortcuts: null, onClick: resetZoom });
        if (save) zoomGroup.append("span").attr("class", "geneviewer-toolbar-divider");
      }
      if (save) createSaveIcon(widgetId, el, toolbar.node(), icon("save", "download"));
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
        // An active drag or selection box is cancelled: the selection before
        // it stays
        if (dragState) cancelDrag();
        else if (marquee) cancelMarquee();
        else selectElement(null);
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
      finishMarquee(false);
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
      // Dragging moves elements, or on empty space draws a selection box
      if (events.drag === true || events.select === true) el.addEventListener("pointerdown", startDrag);
      el.classList.toggle("geneviewer-draggable", events.drag === true);
      if (events.select !== true && selection.length) { setSelection([]); reportSelection(); }
      el.classList.toggle("geneviewer-selectable", events.select === true);
      el.classList.toggle("geneviewer-highlight", highlight);
      applySelection(true);
    };

    // Zoom slider (GC_zoom()): a window on the range of the clusters, below
    // them. Dragging a handle resizes the window, dragging the window moves
    // it, a click on the track centres it there and a double-click shows
    // everything. The chart is drawn again for each window, as on resize.
    var zoomOptions = null;
    // The window in the units of its mode: shares of each cluster's width
    // ("relative") or base pairs ("absolute"); null shows everything
    var zoomRange = null;
    // The range and mode last set in R: a new one replaces the window
    var zoomRequest = null;
    // The clusters of the last draw, and the range of the slider track
    var zoomCharts = [];
    var zoomTrack = [0, 1];
    var zoomBar = null;
    var zoomDrag = null;
    var zoomFrame = null;
    var zoomTimer = null;
    var zoomMode = function () { return zoomOptions?.mode === "absolute" ? "absolute" : "relative"; };
    var initialZoom = function () {
      var range = zoomOptions?.range;
      if (!Array.isArray(range) || range.length !== 2) return null;
      range = range.slice().sort(function (a, b) { return a - b; });
      return zoomMode() === "relative" && range[0] <= 0 && range[1] >= 1 ? null : range;
    };
    // Shares of the track of the window, and back
    var toTrack = function (value) { return (value - zoomTrack[0]) / (zoomTrack[1] - zoomTrack[0]); };
    var fromTrack = function (share) { return zoomTrack[0] + share * (zoomTrack[1] - zoomTrack[0]); };
    var zoomShares = function () {
      if (!zoomRange) return [0, 1];
      return zoomMode() === "absolute" ? zoomRange.map(toTrack) : zoomRange.slice();
    };
    // The narrowest window, as a share of the track (default 1%)
    var minShare = function () {
      var min = zoomOptions?.min;
      if (min == null) return 0.01;
      return Math.min(1, zoomMode() === "absolute" ? min / (zoomTrack[1] - zoomTrack[0]) : min);
    };
    // Whole base pairs, or shares to 0.0001: values to write in code
    var roundZoom = function (value) {
      return zoomMode() === "absolute" ? Math.round(value) : Math.round(value * 10000) / 10000;
    };
    var setZoomShares = function (shares) {
      zoomRange = shares[0] <= 1e-6 && shares[1] >= 1 - 1e-6 ? null
        : shares.map(function (share) { return roundZoom(zoomMode() === "absolute" ? fromTrack(share) : share); });
    };
    // What the window shows: its range, and the base pairs of each cluster
    var zoomDetail = function () {
      var absolute = zoomMode() === "absolute";
      var range = zoomRange || (absolute ? zoomTrack : [0, 1]);
      return {
        mode: zoomMode(),
        start: roundZoom(range[0]),
        end: roundZoom(range[1]),
        zoomed: zoomRange !== null,
        // For GC_zoom(range = ): NULL when everything shows
        range: zoomRange ? zoomRange.slice() : null,
        clusters: zoomCharts.map(function (item) {
          var chart = item.chart;
          var shown = [chart.xScale.invert(0), chart.xScale.invert(chart.width - chart.margin.left - chart.margin.right)]
            .sort(function (a, b) { return a - b; });
          return { cluster: item.key, start: Math.round(shown[0]), end: Math.round(shown[1]) };
        })
      };
    };
    // Every change of the window is reported, so an app always knows it:
    // "drag", "keyboard" and "reset" by the user, "set" by GC_zoom_to(), and
    // "render" when a render changes it (new data, or a range or mode from
    // R). The last two only when the window changed.
    var lastZoom = null;
    var reportZoom = function (action) {
      clearTimeout(zoomTimer);
      zoomTimer = null;
      if (events.zoom !== true || !zoomOptions) return;
      var detail = zoomDetail();
      var key = JSON.stringify([detail.mode, detail.range, detail.clusters]);
      if ((action === "render" || action === "set") && key === lastZoom) return;
      lastZoom = key;
      detail.action = action;
      report("zoom", detail);
    };
    // The window, shades and handles at the shares of the track
    var positionZoomBar = function () {
      if (!zoomBar) return;
      var shares = zoomShares();
      var pct = function (share) { return (share * 100) + "%"; };
      zoomBar.querySelector('[data-side="start"]').style.width = pct(shares[0]);
      zoomBar.querySelector('[data-side="end"]').style.left = pct(shares[1]);
      var windowNode = zoomBar.querySelector('[data-zoom-part="window"]');
      windowNode.style.left = pct(shares[0]);
      windowNode.style.width = pct(shares[1] - shares[0]);
      var values = zoomRange || (zoomMode() === "absolute" ? zoomTrack : [0, 1]);
      var text = function (value) {
        return zoomMode() === "absolute" ? d3.format(",.0f")(value) : d3.format(".0%")(value);
      };
      ["start", "end"].forEach(function (part, i) {
        var handle = zoomBar.querySelector('[data-zoom-part="' + part + '"]');
        handle.style.left = pct(shares[i]);
        handle.setAttribute("aria-valuenow", Math.round(shares[i] * 100));
        handle.setAttribute("aria-valuetext", text(values[i]));
      });
      windowNode.setAttribute("aria-valuenow", Math.round(shares[0] * 100));
      windowNode.setAttribute("aria-valuetext", text(values[0]) + " to " + text(values[1]));
      toolbar?.select(".geneviewer-zoom-buttons").property("hidden", zoomRange === null);
    };
    // Draw the chart again for the window, at most once a frame
    var redrawZoom = function (now) {
      positionZoomBar();
      if (now) {
        if (zoomFrame) cancelAnimationFrame(zoomFrame);
        zoomFrame = null;
        return redraw();
      }
      if (zoomFrame) return;
      zoomFrame = requestAnimationFrame(function () { zoomFrame = null; redraw(); });
    };
    var changeZoom = function (shares, now) {
      setZoomShares(shares);
      if (zoomDrag && zoomOptions?.update === "release") positionZoomBar();
      else redrawZoom(now);
    };
    var resetZoom = function () {
      if (zoomRange === null) return;
      changeZoom([0, 1], true);
      reportZoom("reset");
    };
    var startZoomDrag = function (event) {
      event.stopPropagation();
      if (event.button !== 0 || zoomDrag) return;
      event.preventDefault();
      var rect = event.currentTarget.getBoundingClientRect();
      var at = (event.clientX - rect.left) / rect.width;
      var part = event.target.closest("[data-zoom-part]")?.getAttribute("data-zoom-part") ?? "track";
      var shares = zoomShares();
      zoomDrag = { id: event.pointerId, left: rect.left, width: rect.width, at: at, moved: false,
                   stopListening: listenWindow({ pointermove: moveZoomDrag, pointerup: endZoomDrag,
                                                 pointercancel: endZoomDrag, blur: endZoomDrag }) };
      if (part === "track") {
        // The window moves to centre on the click, then follows the pointer
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
    var moveZoomDrag = function (event) {
      if (!zoomDrag || event.pointerId !== zoomDrag.id) return;
      event.preventDefault();
      var delta = (event.clientX - zoomDrag.left) / zoomDrag.width - zoomDrag.at;
      var s = zoomDrag.shares, min = minShare();
      var next = zoomDrag.part === "start" ? [Math.max(0, Math.min(s[1] - min, s[0] + delta)), s[1]]
        : zoomDrag.part === "end" ? [s[0], Math.min(1, Math.max(s[0] + min, s[1] + delta))]
        : (function () {
          var shift = Math.max(-s[0], Math.min(1 - s[1], delta));
          return [s[0] + shift, s[1] + shift];
        })();
      zoomDrag.moved = true;
      changeZoom(next);
    };
    var endZoomDrag = function (event) {
      if (!zoomDrag || (event.pointerId !== undefined && event.pointerId !== zoomDrag.id)) return;
      var state = zoomDrag;
      zoomDrag = null;
      state.stopListening();
      if (!state.moved) return;
      redrawZoom(true);
      reportZoom("drag");
    };
    // Arrow keys move a handle or the window by 1% of the track (Shift: 10%),
    // Home and End to the ends
    var onZoomKey = function (event) {
      var part = event.target.getAttribute && event.target.getAttribute("data-zoom-part");
      if (!part) return;
      var s = zoomShares(), min = minShare();
      var step = { ArrowLeft: -1, ArrowDown: -1, ArrowRight: 1, ArrowUp: 1 }[event.key];
      var next = null;
      if (step) {
        var delta = step * (event.shiftKey ? 0.1 : 0.01);
        next = part === "start" ? [Math.max(0, Math.min(s[1] - min, s[0] + delta)), s[1]]
          : part === "end" ? [s[0], Math.min(1, Math.max(s[0] + min, s[1] + delta))]
          : [s[0] + Math.max(-s[0], Math.min(1 - s[1], delta)), s[1] + Math.max(-s[0], Math.min(1 - s[1], delta))];
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
      zoomTimer = setTimeout(function () { reportZoom("keyboard"); }, 300);
    };
    var applyCss = function (node, css) {
      Object.keys(css || {}).forEach(function (key) {
        if (key.includes("-")) node.style.setProperty(key, css[key]);
        else node.style[key] = css[key];
      });
    };
    // The slider, below the clusters, as wide as the plot area of the first
    var addZoomBar = function (height) {
      zoomBar = null;
      if (!zoomOptions || !zoomCharts.length) return;
      var absolute = zoomMode() === "absolute";
      zoomTrack = absolute
        ? [d3.min(zoomCharts, function (item) { return item.chart.minStart; }),
           d3.max(zoomCharts, function (item) { return item.chart.maxEnd; })]
        : [0, 1];
      var first = zoomCharts[0].chart;
      var row = d3.select(el).append("div")
        .attr("class", "geneviewer-zoom-row")
        .style("height", height + "px");
      // In the chart's own pixels, also when the chart is scaled
      var elBox = el.getBoundingClientRect(), scale = elBox.width / el.offsetWidth || 1;
      var svgBox = first.svg.node().getBoundingClientRect(), svgScale = svgBox.width / first.width || 1;
      var plotWidth = first.width - first.margin.left - first.margin.right;
      var rowBox = row.node().getBoundingClientRect();
      var bar = row.append("div")
        .attr("class", "geneviewer-zoom-slider")
        .attr("role", "group")
        .attr("aria-label", "Zoom")
        .style("left", (svgBox.left + first.margin.left * svgScale - rowBox.left) / scale + "px")
        .style("width", plotWidth * svgScale / scale + "px")
        .on("pointerdown", startZoomDrag)
        .on("click", function (event) { event.stopPropagation(); })
        .on("dblclick", function (event) { event.stopPropagation(); resetZoom(); })
        .on("keydown", onZoomKey);
      applyCss(bar.node(), zoomOptions.style);

      // A miniature of the genes of each cluster, one row per cluster
      if (zoomOptions.overview !== false) {
        var overview = bar.append("svg")
          .attr("class", "geneviewer-zoom-overview")
          .attr("viewBox", "0 0 1000 " + height)
          .attr("preserveAspectRatio", "none")
          .attr("aria-hidden", "true");
        var rowHeight = (height - 6) / zoomCharts.length;
        var geneHeight = Math.max(1, Math.min(rowHeight * 0.7, 8));
        zoomCharts.forEach(function (item, i) {
          var chart = item.chart;
          var width = chart.width - chart.margin.left - chart.margin.right;
          var share = function (value) {
            if (absolute) return toTrack(value);
            var x = chart.fullScale(value);
            return x === null ? null : x / width;
          };
          var color = function (d) {
            if (zoomOptions.color) return zoomOptions.color;
            return item.group && chart.genesColorScale ? chart.genesColorScale(d[item.group]) : "#9e9e9e";
          };
          (chart.data || []).forEach(function (d) {
            var a = share(d.start), b = share(d.end);
            if (a === null || b === null) return;
            overview.append("rect")
              .attr("x", Math.min(a, b) * 1000)
              .attr("width", Math.max(Math.abs(b - a) * 1000, 1))
              .attr("y", 3 + rowHeight * i + (rowHeight - geneHeight) / 2)
              .attr("height", geneHeight)
              .attr("fill", color(d));
          });
        });
      }
      bar.append("div").attr("class", "geneviewer-zoom-shade").attr("data-side", "start");
      bar.append("div").attr("class", "geneviewer-zoom-shade").attr("data-side", "end");
      var windowNode = bar.append("div")
        .attr("class", "geneviewer-zoom-window")
        .attr("data-zoom-part", "window");
      applyCss(windowNode.node(), zoomOptions.windowStyle);
      [windowNode].concat(["start", "end"].map(function (part) {
        var handle = bar.append("div")
          .attr("class", "geneviewer-zoom-handle")
          .attr("data-zoom-part", part);
        applyCss(handle.node(), zoomOptions.handleStyle);
        return handle;
      })).forEach(function (node) {
        var part = node.attr("data-zoom-part");
        node.attr("tabindex", 0)
          .attr("role", "slider")
          .attr("aria-label", part === "window" ? "Zoom window" : part === "start" ? "Zoom start" : "Zoom end")
          .attr("aria-valuemin", 0)
          .attr("aria-valuemax", 100);
      });
      zoomBar = bar.node();
      positionZoomBar();
    };
    // Zoomed, links between clusters are clipped to the plot areas
    var clipZoomLinks = function () {
      if (!zoomCharts.some(function (item) { return item.chart.zoom; })) return;
      var ns = "http://www.w3.org/2000/svg";
      el.querySelectorAll("svg.GeneLink").forEach(function (svg, index) {
        var box = svg.getBoundingClientRect();
        var scale = box.width / (+svg.getAttribute("width") || box.width) || 1;
        var id = "geneviewer-zoom-links-" + widgetId + "-" + index;
        var clipPath = document.createElementNS(ns, "clipPath");
        clipPath.setAttribute("id", id);
        zoomCharts.forEach(function (item) {
          var chart = item.chart;
          var svgBox = chart.svg.node().getBoundingClientRect(), svgScale = svgBox.width / chart.width || 1;
          var rect = document.createElementNS(ns, "rect");
          rect.setAttribute("x", (svgBox.left + chart.margin.left * svgScale - box.left) / scale);
          rect.setAttribute("y", 0);
          rect.setAttribute("width", (chart.width - chart.margin.left - chart.margin.right) * svgScale / scale);
          rect.setAttribute("height", svg.getAttribute("height"));
          clipPath.appendChild(rect);
        });
        var defs = document.createElementNS(ns, "defs");
        defs.appendChild(clipPath);
        var group = document.createElementNS(ns, "g");
        group.setAttribute("clip-path", "url(#" + id + ")");
        Array.from(svg.childNodes).forEach(function (node) { group.appendChild(node); });
        svg.appendChild(defs);
        svg.appendChild(group);
      });
    };
    // GC_zoom_to(): a range in the units of the mode, or NULL for everything
    zoomTargets[el.id] = function (range) {
      if (!zoomOptions) return;
      zoomRange = Array.isArray(range) && range.length === 2
        ? range.slice().sort(function (a, b) { return a - b; }) : null;
      if (zoomRange && zoomMode() === "relative" && zoomRange[0] <= 0 && zoomRange[1] >= 1) zoomRange = null;
      redrawZoom(true);
      reportZoom("set");
    };
    if (HTMLWidgets.shinyMode && !zoomHandlerAdded && window.Shiny?.addCustomMessageHandler) {
      zoomHandlerAdded = true;
      Shiny.addCustomMessageHandler("geneviewer-zoom", function (message) {
        var target = zoomTargets[message.id];
        if (target) target(message.range);
      });
    }

    var draw = function (width, height) {
      cancelDrag();
      zoomCharts = [];

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
      // Room for the zoom slider below the clusters, with a gap above it
      var zoomBarHeight = zoomOptions ? (zoomOptions.height ?? 24) : 0;
      var zoomHeight = zoomOptions ? zoomBarHeight + 8 : 0;
      var zoom = zoomOptions && zoomRange ? { from: zoomRange[0], to: zoomRange[1], mode: zoomMode() } : null;

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
        clonedContainerOptions.height -= zoomHeight / clusters.length;
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
          .scale(zoom ? Object.assign({}, scaleOptions, { zoom: zoom }) : scaleOptions)
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
          .tooltip(tooltipOptions?.show ?? false, tooltipOptions)
          .clipZoom();

        zoomCharts.push({ key: clusterKey, chart: chart, group: geneOptions?.group });

      });

      addZoomBar(zoomBarHeight);

      // Bottom legend: drawn above to measure its height, then moved below the clusters
      var legendPosition = mergeOptions({ position: "bottom" }, "legendOptions", legendOptions, theme).position;
      if (legendPosition == "bottom"&& legendOptions?.show && legendOptions?.group !== null) {
        el.appendChild(legendElement);
      }

      // Undo, redo and the save icon
      addToolbar();

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
      clipZoomLinks();
    };
    // A redraw empties the chart for a moment, so a page or panel that
    // scrolls it would jump back: keep where they are scrolled to
    var keepScroll = function (drawing) {
      var scrolled = [];
      for (var node = el.parentElement; node; node = node.parentElement) {
        if (node.scrollTop || node.scrollLeft) scrolled.push([node, node.scrollTop, node.scrollLeft]);
      }
      var page = document.scrollingElement;
      if (page && !scrolled.some(function (item) { return item[0] === page; })) {
        scrolled.push([page, page.scrollTop, page.scrollLeft]);
      }
      try {
        drawing();
      } finally {
        scrolled.forEach(function (item) {
          if (item[0].scrollTop !== item[1]) item[0].scrollTop = item[1];
          if (item[0].scrollLeft !== item[2]) item[0].scrollLeft = item[2];
        });
      }
    };
    // Draw again with the same settings, as on resize: moves kept here stay
    var redraw = function () {
      // A slider handle with keyboard focus keeps it
      var focused = zoomBar && zoomBar.contains(document.activeElement)
        ? document.activeElement.getAttribute("data-zoom-part") : null;
      keepScroll(function () {
        draw();
        addLinks();
        liftMovedElements();
        restoreLocalMoves();
        wireEvents();
        checkMoves();
      });
      if (focused && zoomBar) zoomBar.querySelector('[data-zoom-part="' + focused + '"]')?.focus({ preventScroll: true });
    };

    // The rows of all clusters in the columns of the chart data (dataColumns),
    // in the order of the data in R: the order of the groups sets the colours
    var chartData = function (input) {
        if (input.data) return HTMLWidgets.dataframeToD3(input.data);
        var columns = input.dataColumns ?? [];
        var rows = [];
        Object.values(input.series ?? {}).forEach(function (cluster) {
          HTMLWidgets.dataframeToD3(cluster.data).forEach(function (row) {
            var picked = {};
            columns.forEach(function (column) { picked[column] = row[column]; });
            rows.push(picked);
          });
        });
        return rows.sort(function (a, b) { return a.rowID - b.rowID; });
    };
    var renderValue = function (input) {
        var nextData = chartData(input);
        var nextSignature = JSON.stringify([nextData, input.group, input.cluster, input.legend?.group]);
        // A settings response must not remove the text under an active pointer.
        // Data changes and disabling drag still redraw immediately and cancel it.
        if (dragState && nextSignature === dataSignature && input.events?.drag === true) {
          queuedRender = input;
          return;
        }
        queuedRender = null;
        cancelDrag();
        // A new window from R, or new data, starts at the window set in R
        zoomOptions = input.zoom && input.zoom.show !== false ? input.zoom : null;
        var request = zoomOptions ? JSON.stringify([zoomOptions.range ?? null, zoomMode()]) : null;
        if (nextSignature !== dataSignature || request !== zoomRequest) {
          zoomRange = initialZoom();
          // An app may have reset what it knows of the window: report it
          lastZoom = null;
        }
        if (!zoomOptions) lastZoom = null;
        zoomRequest = request;
        if (dataSignature !== null && nextSignature !== dataSignature) {
          preserveLegend = false;
          if (selection.length) { setSelection([]); reportSelection(); }
          moveHistory.clear();
          awaiting = {};
          origin = {};
        }
        // New settings are where everything is, also for moves kept here
        localMoves = {};
        dataSignature = nextSignature;
        graphContainer = input.graphContainer;
        style = input.style;
        data = nextData;
        links = input.links;
        series = input.series;
        saveButton = input.saveButton;
        toolbarOptions = input.toolbar ?? {};
        titleOptions = input.title;
        legendOptions = input.legend;
        theme = input.theme ?? {};
        events = input.events ?? {};
        size = input.size ?? {};
        draw(width, height);
        addLinks();
        liftMovedElements();
        wireEvents();
        checkMoves();
        reportZoom("render");
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
      resize: function () {
        redraw();
      }
    };
  }
});
})();
