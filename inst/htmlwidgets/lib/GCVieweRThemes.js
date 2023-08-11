const themes = {
  vintage: {
    titleOptions: {
      x: 0,
      y: 0,
      font: {
        size: "18px",
        style: "normal",
        weight: "bold",
        decoration: "none",
        family: "'Arial', sans-serif",
        color: "#000000"
      },
      subtitleFont: {
        size: "16px",
        style: "normal",
        weight: "normal",
        decoration: "none",
        family: "'Arial', sans-serif",
        color: "#333333"
      },
      position: "center",
      padding: 15,
    },
    sequenceOptions: {
      y: 50,
      stroke: {
        color: "grey",
        width: 1,
        dashArray: null
      }
    },
    genesOptions: {
    y: 50,  // default y value
    start: null,
    stop: null,
    colorScheme: null,
    customColors: null,
    marker: "arrowHead",
    markerSize: 10,
    strokeWidth: 1,
    opacity: 1
    }
  },
};


function createMarker(svg, data, colorScale, colour, name, size = 20) {
const markers = {
    "arrowHead": {
        orient: "auto",
        markerUnits: "strokeWidth",
        markerWidth: size,
        markerHeight: size,
        refX: 1,
        refY: 5,
        viewBox: "0 0 10 10",
        pathD: "M 0 0 L 8 5 L 0 10 Z",
        pathClass: "triangleArrow"
    },
    "doubleArrow": {
        orient: "auto",
        markerUnits: "strokeWidth",
        markerWidth: size,
        markerHeight: size,
        refX: 1,
        refY: 5,
        viewBox: "0 0 10 10",
        pathD: "M 0 0 L 8 5 L 0 10 L 2.5 5 L 0 0",
        pathClass: "doubleArrow"
    }
};

    const markerDefinition = markers[name];
    if (!markerDefinition) {
        console.error(`Marker "${name}" not found.`);
        return;
    }

    svg.append("defs")
       .selectAll("marker")
       .data(data)
       .enter()
       .append("marker")
       .attr("id", (d) => d.name)
       .attr("orient", markerDefinition.orient)
       .attr("markerWidth", markerDefinition.markerWidth)
       .attr("markerHeight", markerDefinition.markerHeight)
       .attr("refX", markerDefinition.refX)
       .attr("refY", markerDefinition.refY)
       .attr("viewBox", markerDefinition.viewBox)
       .append("path")
       .attr("d", markerDefinition.pathD)
       .attr("class", markerDefinition.pathClass)
       .attr("fill", (d, i) => colorScale(colour[i]));
}
