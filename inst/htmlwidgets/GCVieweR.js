HTMLWidgets.widget({
  name: 'GCVieweR',
  type: 'output',

  factory: function(el, width, height) {
    var data,
        GC_legend,
        GC_title,
        GC_genes,
        GC_labels,
        GC_coordinates,
        GC_scaleBar,
        GC_footer,
        GC_clusterLabel,
        GC_sequence,
        GC_grid;
    var draw = function(width, height, backgroundColor) {
      // Clear out the container if it has anything
      d3.select(el).selectAll('*').remove();

      var legendHeight = (GC_legend?.options?.show === false) ? 0 : computeSize(GC_legend?.options?.height, height);
      var groupedData = d3.flatGroup(data, (d) => d.cluster);
      console.log(GC_legend)
      if (GC_legend?.options?.group !== null && GC_legend?.options?.show) {

      var legendContainer = d3.select(el)
        .append("div")
        .attr("id", "GCvieweR-legend-container")
        .classed("GCVieweR-container", true);

      var legend = createLegendContainer("#GCvieweR-legend-container",
      {
        width:  width,
        height: legendHeight,
        backgroundColor: GC_legend.options.backgroundColor,
        margin: GC_legend.options.margin
      })
       .legendData(data)
       .legend(GC_legend?.options?.group ?? false, GC_legend?.options?.show ?? false, GC_legend?.options);

       var legendElement = d3.select("#GCvieweR-legend-container").node();
       var legendDimensions = legendElement.getBoundingClientRect();
       legendHeight = legendDimensions.height;
       console.log(legendHeight)
      }

      var graph = d3.select(el)
      .append("div")
      .attr("id", "GCvieweR-graph-container")
      .classed("GCVieweR-container", true);

      groupedData.forEach(function(item) {

        var cluserHeight = Math.floor(el.clientHeight - legendHeight)
        var clusterOptions = {
            width: width,
            height: cluserHeight / groupedData.length
            };

        if (Object.keys(GC_grid).length > 0) {
          clusterOptions.margin = GC_grid;
        }

        var cluster = createClusterContainer("#GCvieweR-graph-container", clusterOptions )
            .theme("preset")
            .title(GC_title?.options?.title, GC_title?.options?.subtitle, GC_title?.options?.show ?? false, GC_title?.options)
            .footer(GC_footer?.options?.title, GC_footer?.options?.subtitle, GC_footer?.options)
            .clusterLabel(GC_clusterLabel?.options?.title, GC_clusterLabel?.options?.show ?? false, GC_clusterLabel?.options)
            .geneData(item[1])
            .sequence(GC_sequence?.options?.show ?? false, GC_sequence?.options)
            .genes(GC_genes?.options?.group, GC_genes?.options?.show ?? false, GC_genes.options)
            .coordinates(GC_coordinates?.options?.show ?? false, GC_coordinates?.options)
            .labels(GC_labels?.options?.label, GC_labels.options)
            .scaleBar(GC_scaleBar?.options?.show ?? false, GC_scaleBar?.options);
      });

      if (GC_legend?.options?.position == "bottom" && GC_legend?.options?.show && GC_legend?.options?.group !== null) {

      d3.select("#GCvieweR-legend-container").remove();

      var legendContainer = d3.select(el)
        .append("div")
        .attr("id", "GCvieweR-legend-container")
        .classed("GCVieweR-container", true);
      var legend = createLegendContainer("#GCvieweR-legend-container",
      {
        width:  width,
        height: legendHeight,
        backgroundColor: GC_legend.options.backgroundColor,
        margin: GC_legend.options.margin
      })
       .legendData(data)
       .legend(GC_legend?.options?.group ?? false, GC_legend?.options?.show ?? false, GC_legend?.options);

      }

    };

    return {
      renderValue: function(input) {
        data = HTMLWidgets.dataframeToD3(input.data);
        GC_legend = input.GC_legend
        GC_title = input.GC_title
        GC_genes = input.GC_genes
        GC_labels = input.GC_labels
        GC_coordinates = input.GC_coordinates
        GC_scaleBar = input.GC_scaleBar
        GC_footer = input.GC_footer
        GC_clusterLabel = input.GC_clusterLabel
        GC_sequence = input.GC_sequence
        GC_grid = input.GC_grid
        draw(width, height);
      },
      resize: function(newWidth, newHeight) {
        draw(newWidth, newHeight);
      }
    };
  }
});
