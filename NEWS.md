# geneviewer (development version)

### Bug fixes:
- `GC_chart()` no longer errors when `height` is a percentage (e.g. `height = "100%"`).
- `GC_cluster(prevent_gene_overlap = TRUE, cluster = ...)` now stacks overlapping genes in the selected clusters instead of the first ones.
- `read_gbk()` no longer creates fake features from wrapped qualifier lines that start with a feature name (e.g. a `/note` line starting with "gene") or from feature keys that start with another key (e.g. `CDS_motif` read as `CDS`).
- `read_gbk()` now keeps qualifier values intact (e.g. closing brackets in `"... (PPTase)"`) and no longer errors on empty values (`/pseudo=""`) or values that contain `/key=`.

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
