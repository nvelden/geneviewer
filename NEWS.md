# geneviewer (development version)

### New features:
- `GC_theme()` sets the default look of the whole chart: start from a built-in theme (`"default"`, `"minimal"`, `"classic"` with a colour-blind-safe palette, `"dark"`, or `"bw"`: black and white with a pattern per group), set the font, text and line colours, background, gene colours and patterns of all components at once, and change the options of any component (e.g. `labels = list(fontSize = "10px")`). Options set with the other `GC_` functions always take precedence over the theme, whatever the order of the calls. `geneviewer_theme()` returns the settings of a built-in theme.
- `GC_pattern()` fills genes, exons and legend markers with a pattern per group, for black and white figures. There are 16 patterns (`"diagonal"`, `"dots"`, `"crosshatch"`, `"checkerboard"`, `"bricks"`, `"inverseDots"` and more). Patterns, pattern colours and backgrounds can be set per group, and `background = NULL` draws the patterns on top of the gene colours. They are kept in the PNG from the save button.
- The save button can now save charts as SVG, with all clusters, links, legends and titles in one vector file for publications. Hover the download icon and pick "Save as SVG" or "Save as PNG".
- `GC_trackMouse()` takes the styles of the coordinate tooltip (e.g. `backgroundColor`, `fontSize`) and `cursor`.

### Changes:
- A cluster margin set with `GC_grid(margin = )` is now kept by `GC_clusterLabel()` without `width`, whatever the order of the calls; `GC_clusterLabel()` called after `GC_grid()` set the margin to 100px. `GC_clusterLabel(width = )` still sets the margin.
- Options given as a list (e.g. `labelAdjustmentOptions`, `colorBarOptions = list(labelOptions = )`) now keep the defaults of the options they leave out, at every level.

### Bug fixes:
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
