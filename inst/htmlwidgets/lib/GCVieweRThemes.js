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
      y: 50,
      marker: "triangleArrow",
      markerSize: 1.3,
      strokeWidth: 20,
      shadow: false,
      border: false,
      opacity: 1,
      cluster: {
        colorScheme: null,
        customColors: null
      }
    }
  },
};


function createMarker(svg, data, colorScale, group, name, size = 20) {
    const markers = {
        "arrowHead": {
            orient: "auto",
            markerUnits: "userSpaceOnUse",
            markerWidth: size,
            markerHeight: size,
            refX: 0,
            refY: 0,
            viewBox: "0 -5 10 10",
            pathD: "M0,-5L10,0L0,5",
            pathClass: "arrowHead"
        },
        "doubleArrow": {
            orient: "auto",
            markerUnits: "userSpaceOnUse",
            markerWidth: size,
            markerHeight: size,
            refX: 0,
            refY: 0,
            viewBox: "0 0 20 10",
            pathD: "M0,0L10,5L0,10L5,5L0,0",
            pathClass: "doubleArrow"
        },
        "triangleArrow": {
            orient: "auto",
            markerUnits: "userSpaceOnUse",
            markerWidth: size,
            markerHeight: size,
            refX: 0,
            refY: 3,
            viewBox: "0 0 6 6",
            pathD: "M0,0L6,3L0,6Z",
            pathClass: "triangleArrow"
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
       .attr("fill", (d) => colorScale(d[group]));
}
