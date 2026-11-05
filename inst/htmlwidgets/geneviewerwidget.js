// The htmlwidgets binding of the R package: draws each chart's spec with
// geneviewer.newChart() (geneviewer.js) and, in Shiny, sends its events to
// Shiny inputs. The R package may load this file before geneviewer.js, so
// geneviewer is only used once a chart is drawn.
(function () {
// The events sent to Shiny, as the inputs <output id>_<name>
var shinyEvents = ["click", "dblclick", "select", "selection", "drag", "moves", "zoom", "rendered"];
// GC_zoom_to(): the chart of each output id
var charts = {};
var zoomHandlerAdded = false;

var addZoomHandler = function () {
  if (zoomHandlerAdded || !window.Shiny?.addCustomMessageHandler) return;
  zoomHandlerAdded = true;
  Shiny.addCustomMessageHandler("geneviewer-zoom", function (message) {
    var chart = charts[message.id];
    if (chart) chart.zoomTo(message.range);
  });
};

// Until the R package moves to the spec (M6): tests in the R package and the
// app export a chart with the globals chartToSVG(el) and chartToPNG(el),
// which geneviewer.js defined before it was bundled
window.chartToSVG = function (el, exclude) { return geneviewer.chartToSVG(el, exclude); };
window.chartToPNG = function (el, exclude) { return geneviewer.chartToPNG(el, exclude); };

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
      // x is { spec } from the R package 0.3.0 on; before, a render model.
      // A redraw from Shiny gives the whole chart again, so a spec replaces
      // the chart's spec (setSpec), which update() would merge into.
      renderValue: function (x) {
        var spec = x && x.spec;
        if (chart) {
          if (spec) chart.setSpec(spec);
          else chart.update(x);
          return;
        }
        chart = geneviewer.newChart(el, spec || x, options);
        charts[el.id] = chart;
      },
      resize: function () {
        if (chart) chart.resize();
      }
    };
  }
});
})();
