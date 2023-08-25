const themes = {
  preset: {
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
