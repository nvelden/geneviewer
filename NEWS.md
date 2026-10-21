# geneviewer (development version)

### Bug fixes:
- `GC_annotation(type = "symbol")` no longer also draws a curve from x = 0 to x = 100.
- The default gene colours now give 30 different colours. Groups 12 and 22, 13 and 26, 15 and 29, and 20 and 25 had the same colour; groups 22, 25, 26 and 29 now get new colours.
- Legend colours now match the gene colours when some genes have an `NA` group. Genes with an `NA` group are now always white, also with `colorScheme` or `customColors`.

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
