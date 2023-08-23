const themes = {
  preset: {
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
  },
};

function getMarker(markerName, xPos, yPos, size) {
    switch (markerName) {
        case "arrow":
            return `M ${xPos} ${yPos - size/2} L ${xPos + size/2} ${yPos + size/2} L ${xPos - size/2} ${yPos + size/2} Z`;
        default:
            return ""; // Default empty path
    }
}
