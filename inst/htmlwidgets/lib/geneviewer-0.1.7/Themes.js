const themes = {
  preset: {
    titleOptions: {
      titleFont: {
        size: "16px",
        style: "normal",
        weight: "bold",
        fill: "black",
        decoration: "normal",
        family: "sans-serif",
        cursor: "default",
      },
    subtitleFont: {
       fontSize: "12px",
        fontWeight: "bold",
        fontFamily: "sans-serif",
        cursor: "default",
        fill: "black"
      }
    },
    footerOptions: {
    },
    clusterLabelOptions: {
      x: 0,
      y: 0,
      position: 'left',
      wrapLabel: true,
      wrapOptions: {},
      fontSize: "12px",
      fontStyle: "normal",
      fontWeight: "bold",
      fontFamily: "sans-serif",
      cursor: "default"
    },
    sequenceOptions: {
      stroke: "grey",
      y: 50,
      start: null,
      stop: null,
      strokeWidth: 1,
      marker: {
        markerHeight: 10,
        stroke: "grey",
        strokeWidth: 1,
        //tiltAmount: -5,
        gap: 0
      }
    },
    markerOptions: {
      x: 1,
      y: 50,
      start: null,
      stop: null,
      size: 15,
      colorScheme: null,
      customColors: null,
      marker: "arrow",
      cursor: "default",
      stroke: "black",
      itemStyle: []
    },
    genesOptions: {
      x: 10,
      y: 50,
      start: null,
      stop: null,
      colorScheme: null,
      strokeWidth: 1,
      cursor: "default",
      itemStyle: []
    },
    tooltipOptions: {
      backgroundColor: "white",
      triggers: ["markers", "genes", "labels"],
      formatter: "<b>Start:</b> {start}<br><b>Stop: {stop}</b>",
      opacity: 0,
      position: "absolute",
      padding: "8px",
      borderRadius: "4px",
      border: "1px solid rgba(0,0,0,0.1)",
      boxShadow: "0 4px 6px rgba(0, 0, 0, 0.1)",
      pointerEvents: "none",
      fontFamily: "Arial, sans-serif",
      fontSize: "12px",
      zIndex: 1000,
      color: "#333",
      lineHeight: "1.5"
    }
  },
  genome: {
    labelsOptions: {
      fontSize: "10px",
      show: true
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
        intron: { arrowheadWidth: 0, arrowheadHeight: 0, markerHeight: 20 },  // Add intron default
        // Add other marker types if needed
    };

    const sizeOptions = sizeDefaults[markerSize] || sizeDefaults.medium;
    const markerOptions = markerDefaults[marker] || {};

    const combinedOptions = mergeOptions(markerOptions, "genePathOptions", options);

    // Scale the dimensions based on size
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
            path = `M0 ${shaftTop}
                L0 ${shaftBottom}
                L${shaftLength} ${shaftBottom}
                L${shaftLength} ${scaledArrowheadHeight}
                L${length} ${(scaledArrowheadHeight / 2)}
                L${shaftLength} 0
                L${shaftLength} ${shaftTop} Z`;
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
            // Create a V-shaped path for the intron
            const midPoint = length / 2;
            height = scaledMarkerHeight / 2
            path = `M0 ${height}
                    L${midPoint} 0
                    L${length} ${height}`;
            break;
        default:
            console.warn(`Marker type '${marker}' not recognized.`);
            path = "";
    }

    return { path, length, scaledArrowheadWidth, scaledArrowheadHeight, height };
}

