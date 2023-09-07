const themes = {
  preset: {
    titleOptions: {
      titleFont: {
      //fill: "green"
      }
    },
    footerOptions: {
      titleFont: {
      //fill: "green"
      }
    },
    clusterLabelOptions: {
      //fill: "green"
      },
    sequenceOptions: {
      //stroke: "green"
    },
    markerOptions: {
    },
    genesOptions: {
      //customColors: ["green"]
    },
    tooltipOptions: {
      //backgroundColor: "green"
    }
  }
};

function getMarker(markerName, xPos, yPos, size, height = null) {

    height = height === null ? size : height;

    switch (markerName) {
        case "arrow":
            return `M ${xPos} ${yPos - height/2} L ${xPos + size/2} ${yPos + height/2} L ${xPos - size/2} ${yPos + height/2} Z`;
        default:
            return ""; // Default empty path
    }
}
