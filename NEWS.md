# geneviewer (development version)

### New features:
- `GC_scale(scale_break_padding = )` also takes the padding around automatic scale breaks in base pairs, e.g. `"500bp"`, for the same room around every break; a number is still a percentage. A gap too short for the padding gets no break.
- `GC_scale(breaks = )` also takes a data frame with `start` and `end` columns and, for breaks per cluster, a `cluster` column, e.g. `data.frame(cluster = c("A", "B"), start = c(5000, 8000), end = c(9000, 12000))`, to set different breaks for each cluster in one call.
- `GC_labels(groupStyle = )` styles the labels of all genes of a group at once, e.g. `groupStyle = list(tnsA = list(fill = "white"))` for white labels on dark genes. The `itemStyle` of a label takes precedence over the style of its group.
- `GC_genes(groupStyle = )` styles all genes of a group at once, with the options of `itemStyle`, e.g. `groupStyle = list(promoter = list(marker = "box", fill = "grey"))`. The `itemStyle` of a gene takes precedence over the style of its group.
- `GC_theme()` sets the default look of the whole chart: start from a built-in theme (`"default"`, `"classic"`, `"dark"`, `"vintage"` and `"infographic"` with the colours of the ECharts default (version 5), dark, vintage and infographic themes, `"publication"` for journal figures, after common journal figure guidelines (Arial, 7 pt text, black text and axes, italic gene labels, colour-blind safe colours), or `"bw"`: black and white with a pattern per group, in the same style), set the font, text and line colours, background, gene colours and patterns of all components at once, and change the options of any component (e.g. `labels = list(fontSize = "10px")`). `textScale` scales all text at once (e.g. `textScale = 1.5`), with the title and the default cluster label width growing with it. Options set with the other `GC_` functions always take precedence over the theme, whatever the order of the calls. `geneviewer_theme()` returns the settings of a built-in theme.
- `GC_pattern()` fills genes, exons and legend markers with a pattern per group, for black and white figures. There are 16 patterns (`"diagonal"`, `"dots"`, `"crosshatch"`, `"checkerboard"`, `"bricks"`, `"inverseDots"` and more). Patterns, pattern colours and backgrounds can be set per group, and `background = NULL` draws the patterns on top of the gene colours. They are kept in the PNG from the save button.
- The save button can now save charts as SVG, with all clusters, links, legends and titles in one vector file for publications. Hover the download icon and pick "Save as SVG" or "Save as PNG".
- Gene colours can be set by the name of a geneviewer palette, besides the d3 colour schemes: `"okabeIto"` (colour-blind safe), `"classic"` (clear colours), `"vivid"` (bright colours for dark backgrounds), `"dark"` (soft colours for dark backgrounds), `"vintage"` (muted colours), `"infographic"` (strong colours), `"greys"` and `"blues"`, in `GC_theme(palette = )` and the `colorScheme` of `GC_genes()`, `GC_transcript()` and `GC_color()`. `geneviewer_palette()` returns their colours.
- `GC_links(low_color = )` sets the colour of links with the lowest identity (white by default), e.g. the background colour on a dark chart.
- `GC_genes(corner_radius = )` rounds the corners of the "arrow", "boxarrow" and "box" markers (e.g. `corner_radius = 3` for soft corners; the inner corners of arrows stay sharp) and sets those of "rbox". Exons take `cornerRadius` in `styleExons`, and themes in `genes = list(corner_radius = )`. `GC_legend(corner_radius = )` rounds the corners of the legend markers.
- `GC_links(vmin = , vmax = )` set the measures of the lightest and the full link colour, and the ends of the colour bar (0 and 100 by default): e.g. `vmin = floor(min(alignment$identity))` spreads the colours over the identities of an alignment, so differences are easier to see. Measures outside are drawn in the colour of the nearest end.
- `GC_legend(align = )` aligns the legend to the left, centre or right, like `GC_title()`: `"left"` (default), `"center"` or `"right"`. Each row of a legend that wraps is aligned on its own, and `x` moves the legend from that position. Themes take it in `legend = list(align = )`.
- `GC_title()`, `GC_clusterTitle()` and `GC_clusterFooter()` now have a documented `align` argument (`"left"`, `"center"` or `"right"`), which is checked; it was only taken through `...`. The cluster title and footer take one per cluster, e.g. `align = c("left", "right")`.
- `GC_clusterLabel(align = )` aligns the lines of the cluster labels to the left, centre (default) or right of their margin, e.g. `align = "left"` so the names and the species below them start at the same place. `x` moves the labels from that position.
- `GC_clusterLabel(subtitle = )` adds a second line below each cluster label, e.g. the species, styled with `subtitleFont` (11px and not bold by default). `spacing` sets the pixels between the last line of the title and the subtitle (by default 1.2 times their mean font size), and the two are centred together on the cluster. Themes colour the subtitle like the other text, and the dark, vintage and publication themes like their other subtitles.
- `GC_title()`, `GC_clusterTitle()` and `GC_clusterFooter()` now have a documented `spacing` argument for the pixels between the title and the subtitle, which is checked; it was only taken through `...`. The cluster title and footer take one per cluster.
- `GC_trackMouse()` takes the styles of the coordinate tooltip (e.g. `backgroundColor`, `fontSize`) and `cursor`.

### Changes:
- The padding around automatic scale breaks (`GC_scale(scale_breaks = TRUE, scale_break_padding = )`) is now a share of the region shown, without the breaks, so all breaks get the same room around them; long breaks used to get much more padding than short ones.
- With `GC_scale(axis_type = "range")`, scale breaks are given in the positions of the data, like the annotations and the line, and move with the genes; they were taken as positions on the shifted axis.
- `GC_align()` leaves scale breaks out of the shared range: a cluster with breaks gets a range that much longer, so all clusters keep the same scale. The padding of the scale and the line is now a share of the range shown, without its breaks.
- `GC_align()` now shifts the clusters instead of stretching them, so all clusters keep the same scale: genes of the same length are drawn at the same width, also when the clusters differ in length. The alignment is kept when `GC_scale()` is called afterwards, also with `axis_type = "range"`; `GC_scale()` used to undo it.
- Cluster footers aligned "left" (the default) or "right" now line up with the genes and the cluster title: "left" started at the edge of the chart, in the left margin of the cluster (under a cluster label), and "right" ended one left margin short of the last gene. Footers that are not centred move right by the left margin of the cluster (50 pixels by default); an `x` used to line them up with the genes (e.g. `x = 50`) can be removed. The examples in the vignettes are updated.
- Links now fade to nearly white (10% of the link colour) at the lowest measure instead of white, so they stay visible on the background. The colour bar now runs from `vmin` to `vmax` (0% to 100% by default) instead of from the lowest to the highest measure in the data.
- The title and labels of the link colour bar are now in sans-serif, like the other text; they used the serif font of the browser.
- With a theme background, the UTRs of transcripts are filled with the background instead of white.
- White legend markers (e.g. "No Hit" or a white custom colour) now get a light grey outline, so they stay visible on a white background.
- A cluster margin set with `GC_grid(margin = )` is now kept by `GC_clusterLabel()` without `width`, whatever the order of the calls; `GC_clusterLabel()` called after `GC_grid()` set the margin to 100px. `GC_clusterLabel(width = )` still sets the margin.
- Options given as a list (e.g. `labelAdjustmentOptions`, `colorBarOptions = list(labelOptions = )`) now keep the defaults of the options they leave out, at every level.
- The geneviewer vignette has a new "Themes" section on `GC_theme()`: the built-in themes, the global settings and palettes, `textScale`, options per component and making a theme of your own with `geneviewer_theme()`.
- The "Example charts" article has a new example: the *bla*KPC-2 plasmid pQEB1 and four variants, with links that show inversions and inserted transposons. Its data are in the new datasets `pQEB1_plasmids` (genes) and `pQEB1_alignment` (a nucmer alignment), made by `data-raw/pQEB1_plasmids.R` from the GenBank files.

### Bug fixes:
- The `start` and `end` of the line of a cluster (`GC_sequence()`) now stay with the genes with `GC_scale(axis_type = "range")`, like the annotations, whether they are set before or after `GC_scale()`; they were taken as positions on the shifted axis.
- The line of a cluster now stops at a scale break (`GC_scale(breaks = )` or `scale_breaks = TRUE`) and starts again after it, so the break marks show a real gap; the line ran on between them.
- A gene made taller or moved with `GC_genes(itemStyle = )` (e.g. `markerHeight`, `arrowheadHeight` or `y`) no longer moves the labels of the other genes in its cluster. The labels and track spacing used the size and position of whichever gene was drawn last.
- Annotations now stay on their genes with `GC_scale(axis_type = "range")`: the range axis moves the genes of each cluster to start at 1, and now moves the annotations of the cluster (`x`, `x1`, `x2` and `position`) with them, whether they are added before or after `GC_scale()`. They used to be drawn too far to the right.
- A wrapped cluster label (`GC_clusterLabel()`) keeps its `<b>`, `<i>` and `<u>` styles when the tagged text runs over two lines, e.g. `"pQEB1 <i>E. cloacae ECL-21</i>"`; the tags were shown as text. The lines are now also measured without the tags, so they no longer break too early.
- The legend of a chart with a fixed width (e.g. `GC_chart(width = "900px")`) no longer gets smaller in a wider browser window: it was laid out at the width of the window and then scaled down into the chart, e.g. in a page saved with `htmlwidgets::saveWidget()`.
- The bottom of letters such as g, y and p in a title without a subtitle is no longer cut off: the default title height is now 40 pixels instead of 30, so `GC_title(height = )` is no longer needed for titles of 16 or 18 pixels.
- A named list of `customColors` (in `GC_color()`, `GC_genes()` and `GC_transcript()`) that leaves out groups no longer makes those groups white: they keep the colours of `colorScheme`, the theme palette or the default colours, e.g. `GC_color(customColors = list(Other = "grey"))` only changes "Other". `GC_color()` now takes `colorScheme` together with a named list of `customColors`.
- `GC_transcript()` no longer drops the settings of the whole chart made before it: the title, legend, theme and `GC_grid()` margins and direction are kept. Settings per cluster are for the old clusters and are set after `GC_transcript()`.
- `GC_legend(backgroundColor = )` now sets the legend background; it had no effect.
- When a chart is redrawn in Shiny, styles of `GC_chart(style = )` that are no longer set are removed; e.g. a background colour stayed after it was removed.
- `GC_annotation(type = "symbol")` no longer also draws a curve from x = 0 to x = 100.
- The default gene colours now give 30 different colours. Groups 12 and 22, 13 and 26, 15 and 29, and 20 and 25 had the same colour; groups 22, 25, 26 and 29 now get new colours.
- Legend colours now match the gene colours when some genes have an `NA` group. Genes with an `NA` group are now always white, also with `colorScheme` or `customColors`.
- `GC_annotation(type = "arrow", arrowStyle = ...)` now applies all `arrowStyle` options (e.g. `stroke`, `strokeWidth`, `fillOpacity`) to the arrow head; only `fill` was used.
- Arrow heads of `GC_annotation(type = "arrow")` and `type = "textMarker"` no longer take the size and colour of an arrow at the same position in another cluster or chart.
- `GC_scale(breaks = ...)` now works with breaks in any order; breaks not sorted by start put genes and axis ticks at the wrong position.
- `GC_coordinates()` no longer stops the chart from drawing when a cluster has no genes in view (e.g. after `GC_scale(start = , end = )`); clusters below it and the bottom legend were missing.
- The tooltip `mouseout` handler no longer relies on the deprecated global `window.event`.
- Each chart now uses its own `GC_tooltip()` options; with several charts on a page, tooltips took the colours and font of another chart. Tooltips are now created inside the chart, so redraws (e.g. on resize) no longer add `<style>` tags to the page or (with `GC_trackMouse()`) extra tooltip elements.
- Clicking a legend item no longer hides genes that were clicked off in the legend of another chart on the same page.
- `GC_trackMouse()` now shows the right position to the right of scale breaks (it was too small by the width of the breaks on the left).
- Charts no longer create the global JavaScript variables `to` and `linearScale`, which could overwrite variables of the same name on the page.
- Gene labels from `GC_labels()` and identity labels from `GC_links()` no longer share element ids. The overlap check of a gene label used the identity label with the same id, so identity labels were rotated instead of the gene labels.
- `GC_genes()` and `GC_transcript()` no longer drop the colours from `GC_color()` and the patterns from `GC_pattern()` when called after them. Their `colorScheme` and `customColors` now also set the legend colours (when no cluster or selection is given), so the legend matches the genes.
- Identity (or similarity) labels from `GC_links()` and gene labels from `GC_labels()` no longer overlap. Each gene now has one label with the gene name and, below it, its identity; when labels are rotated to avoid overlap, both are shown on one line (e.g. "eryK 64.1%").
- With `GC_cluster(prevent_gene_overlap = TRUE)`, labels of genes on the lower tracks are now drawn below their gene and rotate downwards when they overlap, instead of above the gene, under (and rotated into) the genes of the track above. A label stays above its gene when a gene of the next track is below it and the space above is free.
- `itemStyle` values of 0 are no longer ignored in `GC_labels()`, `GC_genes()` and `GC_transcript()` (e.g. `rotate = 0`, `y = 0` or `cornerRadius = 0` for one gene); they fell back to the general option.
- `GC_sequence(start = 0)` and `end = 0` are no longer ignored; the line started at the first gene instead.
- `GC_sequence(markerStyle = list(gap = ))` is now a distance in pixels between each break marker and the break, the same for every break. It was a percentage of the marker's x position, so breaks further right opened wider. Charts that set `gap` get a narrower gap.
- A percentage `GC_title(height = )` or `GC_legend(height = )` now follows the chart height when the chart is resized; it kept the pixel height of the first draw.
- Smaller fixes: legend items fade on hover again (an invalid CSS selector dropped the transition; it now only animates the opacity, so rotated gene labels no longer swing into place on every draw), legend items have unique ids, the 3' and 5' transcript labels have their own classes (`three-prime-label`, `five-prime-label`), and unused or invalid styles were removed from the footer subtitle and the link layer.
- The `GC_links()` colour bar now shows a gradient for each link colour on the chart: the inverted gradient only when at least one link (in any `GC_links()` set) joins opposite strands, and the normal gradient only when at least one link joins the same strand. Charts with only inverted links showed only the normal gradient, and charts with forward and reverse normal links showed an inverted gradient. The link data is no longer modified.

# geneviewer 0.2.0

### Enhancements:
- `read_fasta()` is about 4-5 times faster on large FASTA files.

### Bug fixes:
- `GC_chart()` no longer errors when `height` is a percentage (e.g. `height = "100%"`).
- `GC_cluster(prevent_gene_overlap = TRUE, cluster = ...)` now stacks overlapping genes in the selected clusters instead of the first ones.
- `read_gbk()` no longer creates fake features from wrapped qualifier lines that start with a feature name (e.g. a `/note` line starting with "gene") or from feature keys that start with another key (e.g. `CDS_motif` read as `CDS`).
- `read_gbk()` now keeps qualifier values intact (e.g. closing brackets in `"... (PPTase)"`) and no longer errors on empty values (`/pseudo=""`) or values that contain `/key=`.
- `read_bed()` no longer adds 1 to `chromEnd`, `thickEnd` and `blockSizes` (BED ends are already 1-based), matching `rtracklayer::import.bed()`. Values in these columns are now 1 lower than before. Block columns are now always lists, so single-row BED12 files no longer error and files where every row has the same number of blocks are read correctly.
- `protein_blast()` now works when the cluster column has a name other than `"cluster"` (set with the `cluster` argument).
- `read_gff()` now URL-decodes attribute values (e.g. `%2C` to `,`), keeps values that contain `=`, trims spaces around keys, keeps `#` inside lines, stops at a `##FASTA` section and no longer errors on rows with empty or missing attributes.
- `read_fasta()` now reads files with a single sequence that has a `location`, keeps header values with spaces or square brackets intact (e.g. `[protein=3-oxoacyl-[acyl-carrier-protein] reductase]`, `OS=Homo sapiens`), and uses the full span of `join()` locations for `start` and `end`.
- `GC_scale(scale_breaks = TRUE)` no longer errors on clusters with a single gene or with only overlapping genes.
- `GC_scale(axis_type = "range")` now shifts genes when `hidden = TRUE`, only shifts the selected clusters (a single selected cluster no longer gets the genes of all clusters), keeps gene tracks from `GC_cluster(prevent_gene_overlap = TRUE)`, and passes `...` on to the scale options.
- `GC_normalize(custom_gaps = ...)` now measures the gap after a gene from its edges, so genes on the reverse strand get the right gap, and applies the gap after every gene with the given name.
- `GC_clusterLabel(cluster = ...)` without a `title` now labels each selected cluster with its own name instead of the names of the first clusters.
- Arguments with one value per cluster no longer error in `if()` checks: e.g. `GC_labels(label = c("a", "b"))`, `GC_genes(group = ...)` and `show = c(TRUE, FALSE)` in `GC_labels()`, `GC_clusterLabel()`, `GC_clusterTitle()`, `GC_clusterFooter()`, `GC_coordinates()`, `GC_scaleBar()` and `GC_tooltip()`. Arguments that take a single value (`GC_title(show)`, `GC_align(id, id_column)`, `GC_transcript(type, transcript)`, `GC_normalize(group)`) now give a clear error.
- `GC_legend()` now gives an error for a `group` column that is not in the data, and when a legend is shown without any group. The checks tested `labels`, which is not an argument (it found `base::labels`), so they never ran.
- Links no longer take the colour of another link when a link cannot be drawn. Removed unused link label code that referred to an undefined `identityLabel`.
- `GC_grid(margin = list(top = , bottom = ))` now works for charts with a percentage height (it errored), and the margins stay inside the chart height on standalone pages.
- `genbank_to_fasta()` now converts GenBank files with several records (one FASTA record per GenBank record); it errored before.
- `mummer_alignment()` no longer deletes unrelated files whose name contains `<cluster1>_<cluster2>` when `remove_files = TRUE`; it now removes only the files MUMmer wrote. Cluster files are matched by exact file name (names like `BGC1+2` work), file paths are quoted in the MUMmer commands, and names with spaces give a clear error.
- `protein_blast(parallel = TRUE)` now works on machines with one core (it tried to start 0 workers) and always stops its worker processes, also after an error. It uses at most 2 workers when `_R_CHECK_LIMIT_CORES_` is set (as in `R CMD check`).
- `read_gbk()` no longer adds `translation = NA` to every feature without a translation (e.g. `gene` and `source`).
- Removed `.DS_Store` files from the package and the unused internal functions `cleanList()` and `adjust_to_range()`.
- Several geneviewer charts in one Shiny app (output IDs without a `-`, or Shiny modules) no longer draw into each other's containers. The widget now uses its full element ID.
- Charts on the same page no longer share settings through global JavaScript variables (e.g. `save_button` of one chart was used by another chart after a resize).
- Removed deprecated `.data$` use in tidyselect calls (`select()`, `pivot_wider()`), which gave warnings with tidyselect >= 1.2.0.

# geneviewer 0.1.11

### Enhancements:
- Fix for setting tooltip and legend to FALSE
- Fix for links displayed behind graph
- Fix for getting proper strand directions in .gbk files
- Fix for passing additional CSS styles
- Add github actions to build Wasm assets for Shinylive 

# geneviewer 0.1.10

### Enhancements:
- Several fixes to comply with CRAN policies.

# geneviewer 0.1.9

### Enhancements:
- Added tests for all core functions.
- Fix for applying custom colors in GC_color.
- Fix for setting direction in GC_grid.
- Prepared the package for CRAN release

# geneviewer 0.1.8

### Enhancements:
- Option to display transcripts using the GC_transcripts function.

# geneviewer 0.1.7

### Enhancements:
- Option to perform MUMmer alignments.
- Add option to add a color bar.

# geneviewer 0.1.6

### Enhancements:
- Option to perform BlastP between gene clusters.
- Add option to add links between genes in clusters.

# geneviewer 0.1.5

### Enhancements:
- Add preset markers for genes: arrow, boxarrow, box, cbox and rbox.
- Add options to load cluster information from GenBank and FASTA files.

# geneviewer 0.1.4

### Enhancements:
- Add option to add gene links.
- Change package name from GCVieweR to geneviewer.

# geneviewer 0.1.3

### Enhancements:
- Option to align a specified gene across clusters.

# geneviewer 0.1.2

### Enhancements:
- Options to add text, line, arrow, rectangle, symbol, terminator and 
promoter annotations to a GC_chart.

# geneviewer 0.1.1

### Enhancements:
- Introduced customizable margin settings for charts, clusters, titles, and legends.
- Added styling options for the containers of charts, clusters, titles, and legends.
- Implemented a new feature to display clusters side by side.
- Updated and expanded the documentation to reflect new features and usability 
improvements.


# geneviewer 0.1.0

* Initial Beta release.
