// The htmlwidgets binding of the R package: draws each chart's spec with
// geneviewer.newChart() (geneviewer.core.js) and, in Shiny, sends its events to
// Shiny inputs. The R package may load this file before geneviewer.core.js, so
// geneviewer is only used once a chart is drawn.
(function () {
// The events sent to Shiny, as the inputs <output id>_<name>
var shinyEvents = ["click", "dblclick", "select", "selection", "drag", "moves", "zoom", "rendered"];
var zoomHandlerAdded = false;

var addZoomHandler = function () {
  if (zoomHandlerAdded || !window.Shiny?.addCustomMessageHandler) return;
  zoomHandlerAdded = true;
  Shiny.addCustomMessageHandler("geneviewer-zoom", function (message) {
    // GC_zoom_to(): the chart of the output, found when the message arrives.
    // This binding keeps no list of charts: a chart lives as long as its
    // element, so removing a Shiny output frees it. Nothing here calls
    // destroy(); that is for pages that use the library's newChart() themselves.
    var el = document.getElementById(message.id);
    var widget = el && HTMLWidgets.getInstance(el);
    var chart = widget && widget.getChart();
    if (chart) chart.zoomTo(message.range);
  });
};

HTMLWidgets.widget({
  name: 'geneviewer',
  type: 'output',

  factory: function (el, width, height) {
    var chart = null;
    // Shiny mode is read when it is used, not once: the R package's browser
    // tests draw a chart, then switch shinyMode on and draw it again
    var on = {};
    shinyEvents.forEach(function (name) {
      on[name] = function (event) {
        if (!HTMLWidgets.shinyMode) return;
        var detail = event.detail;
        // A list of elements keeps its elements in R (see geneviewer-shiny.R).
        // Priority "event": the same event twice still reaches the server.
        var type = Array.isArray(detail) ? ":geneviewer.list" : "";
        Shiny.setInputValue(el.id + "_" + name + type, detail, { priority: "event" });
      };
    });
    var options = {
      on: on,
      get server() { return !!HTMLWidgets.shinyMode; }
    };
    if (HTMLWidgets.shinyMode) addZoomHandler();

    return {
      // x is { spec }. A redraw from Shiny gives the whole chart again, so it
      // replaces the chart's spec (setSpec), which update() would merge into.
      renderValue: function (x) {
        if (chart) chart.setSpec(x.spec);
        else chart = geneviewer.newChart(el, x.spec, options);
      },
      resize: function () {
        if (chart) chart.resize();
      },
      // The chart (geneviewer.newChart()), e.g. for chart.toSVG():
      // HTMLWidgets.find("#" + id).getChart(); null before the first draw
      getChart: function () {
        return chart;
      }
    };
  }
});
})();
