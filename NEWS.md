# geneviewer 0.2.1

### Changes:
- Charts are drawn by the geneviewer JavaScript library (<https://github.com/nvelden/geneviewer-js>), which the package ships in `inst/htmlwidgets/lib/`. The `GC_` functions describe the chart as a chart spec and the library draws it. Charts look and behave the same: the same functions, arguments, defaults, checks and messages, the same Shiny inputs, and the same drawing, checked against the charts of the examples, vignettes and tests of 0.2.0.
- Code that read the internals of a chart, `widget$x`, must use `widget$x$spec`: `widget$x` is now `list(spec = )`, the chart spec, which holds the arguments of the `GC_` functions as given, under the names of the spec. The R code that built the chart for the browser is gone.
- The default icons of the toolbar are inline SVG copies of the same Font Awesome glyphs, so they show without an icon font. Custom icons by Font Awesome name (`GC_toolbar(icons = )`) still work: the package still loads Font Awesome.
- `geneviewer_theme()` and `geneviewer_palette()` read the built-in themes and palettes from the library (its `themes.json`); they give the same settings and colours. The options of a theme's components have the names of the chart spec: `cornerRadius` (was `corner_radius`), `normalColor`, `invertedColor` and `lowColor` (were `normal_color`, ...), `textStyle` and `markerStyle` in `legend` (were `legendTextOptions` and `legendOptions`), and `lineStyle` in `sequence` (was `sequenceStyle`). `GC_theme()` still takes the old names, also mixed with the new ones in one theme.
- Functions that only style a chart (`GC_genes()`, `GC_labels()`, `GC_title()`, ...) no longer prepare the whole dataset to check their arguments, which adds up on large data.
- FileSaver.js is no longer bundled: the library saves images with the browser's own download. jsonlite is now imported.

### Bug fixes:
- Links made with `GC_links(group = )` before `GC_normalize()` move with their genes; they stayed at the old positions, so they pointed at the wrong place. Links given as `data` (e.g. from `mummer_alignment()`) keep their positions, with a warning to add them after `GC_normalize()`.
- `read_fasta()` gives every sequence with a `location` a `strand`, also in files whose locations are all forward. Before, such a file had no `strand` column, and in a folder with other files its rows got `NA`, so `GC_chart(strand = "strand")` stopped with "Invalid strand values found".
- `GC_cluster(separate_strands = TRUE, prevent_gene_overlap = TRUE)` warns that neither is applied, which is what happens; it said "Resetting to NULL". The help of `GC_grid()` says that a number for `width` is pixels (it said a percentage), and the example of `GC_genes()` no longer passes `prevent_overlap` and `gene_overlap_spacing`, which are not options.
- Tooltips (`GC_tooltip()`) show the values of the data as text: markup in a value, e.g. in a qualifier of a GenBank file from someone else, ran in the page of who viewed the chart. The `formatter` is still HTML, so put tags such as `<b>` or links there, not in the data. (Needs the next geneviewer JavaScript library.)
- `mummer_alignment()` reads a folder whose path has spaces. The files are copied to a working folder in `output_dir` first, so only that path must be without spaces.
- `GC_zoom(range = )` and `GC_zoom(min = )` are checked in the `mode` of an earlier `GC_zoom()` call when the call gives none: `GC_zoom(mode = "absolute") %>% GC_zoom(range = c(100, 2000))` stopped with the error for a relative range.
- `show = FALSE` in `GC_labels()`, `GC_coordinates()`, `GC_scaleBar()`, `GC_clusterTitle()`, `GC_clusterLabel()` and `GC_clusterFooter()` hides what an earlier call shows, e.g. `GC_coordinates() %>% GC_coordinates(show = FALSE, cluster = 2)`; before, it changed nothing. Hidden labels need no label column. With nothing shown before, the chart is unchanged, as before. (`GC_labels(show = FALSE)` on a chart without a group needs the next geneviewer JavaScript library to draw.)
- A chart of data without rows, e.g. after a Shiny filter that leaves nothing, takes the `GC_` functions: `GC_labels("name")` stopped with "label column not found in data", and `GC_transcript()` with an internal error. The chart has no clusters. (Drawing it needs the next geneviewer JavaScript library: the vendored 0.3.10 stops with "Cannot read properties of null".)
- `genbank_to_fasta()` writes records without a `VERSION` line, named by their `ACCESSION`, else their `LOCUS` name, else the file name; without a `DEFINITION`, the header is the name only. Such records were left out without a message, so the FASTA file could be empty and `mummer_alignment()` failed later with another error.
- `read_gbk()` reads features whose key does not start with a letter, such as `5'UTR`, `3'UTR` and `-10_signal`. Their lines were added to the feature before them, so a CDS followed by a `5'UTR` got a second `gene` (`gene.1` in `gbk_features_to_df()`). Qualifiers without a value, such as `/pseudo`, are kept, with the value `""`; they were dropped. A feature without qualifiers gets its `region`; it was `NA`.
- `read_gbk()` reads the whole sequence (`ORIGIN`) of records of more than a million bases, such as bacterial genomes. From base 1,000,001 the lines of the sequence were read as the start of a new section, so the sequence ended at base 1,000,020, without a message.
- `GC_links(group = )` works on data with a column named `cluster` when the clusters of the chart are another column, e.g. `GC_chart(data, cluster = "genome")`. When that column came before the cluster column, it was read in place of the clusters, so links were missing or the call stopped with "No links found". The column is now left out of the links. Links by that column itself (`group = "cluster"`) stop with a message to rename it.
- `GC_normalize()` spaces the genes over the whole range of their cluster, as documented. It shared the total gap between as many gaps as there are genes, one too many, so every gap was too small and the cluster ended before its last gene did: genes at 10-40, 90-120 and 130-160 became 10-40, 60-90 and 110-140, and are now 10-40, 70-100 and 130-160. Genes that are already evenly spaced stay where they are. A given `gap` is now the gap drawn; it was (n - 1) / n of it for n genes. Charts made with `GC_normalize()` change: genes after the first move to the right, the last gene of a cluster by the size of one gap.
- A second `GC_toolbar(buttons = )` or `GC_legend(order = )` call replaces the buttons or order of the first, as other options do; before, the first call's list was kept.
- `read_gff()` reads GTF and GFF2 files: attributes written as `key "value"` become columns without the quotes, and folders are searched for `.gtf` and `.gff2` files too. A key repeated in one row (e.g. `tag` in GTF, or `Dbxref` twice) keeps its values joined with `","`, as GFF3 writes several values of one key; before, every attribute column of the file became a list column, with a warning. `read_fasta()` joins repeated `[key=value]` fields the same way.
- `gbk_features_to_df()` and `read_fasta()` now give the full span of spliced genes whose exons are listed from right to left, as in `join(complement(5441..5600),complement(5095..5373))` (common in fungal and other eukaryotic GenBank files). `gbk_features_to_df()` took the first and the last number, so the gene was drawn reversed and too short, and `read_fasta()` kept only the first exon. A location crossing the origin of a circular genome now keeps its first part in `gbk_features_to_df()` too, as in `read_fasta()`.
- `read_bed()` reads BED12 files in which every row has one block; it stopped with "non-character argument".
- `read_fasta()` skips an empty FASTA file with the warning of a file without keys, instead of an internal error message.
- `protein_blast()` with only the query cluster (e.g. one GenBank file) gives each gene as its own hit; before, R crashed, because pwalign was given nothing to align.
- `GC_legend(order = )` with one group name, e.g. `order = "B"`, orders the legend; before, the chart was blank.
- `protein_blast()` can be run on its own result, e.g. again with another query cluster: the columns `BlastP`, `identity`, `similarity` and `score` of the data are replaced. Before, it stopped with "Column `score` not found".
- `read_gff()` and `read_bed()` combine the files of a folder when a column holds numbers in one file and text in another, e.g. chromosomes named `1` and `X`. The text columns (`seqid`, `source`, `type`, `strand` and `phase` of GFF files; `chrom`, `name`, `strand` and `itemRgb` of BED files) are now always character, and `score` is always numeric, with `NA` for a score of `"."`. Before, the files could not be combined.
- `GC_normalize()` keeps a column named `cluster` when the clusters of the chart are another column, e.g. `GC_chart(data, cluster = "operon")`; before, it was overwritten with the cluster names.
- `GC_links()` without `group` or `data` stops with "Either 'group' or 'data' must be provided."; before, the error was "argument is of length zero".
- `GC_title(show = FALSE)` hides a title set by an earlier `GC_title()` call; before, it changed nothing.
- Options given as named vectors, e.g. `GC_pattern(color = c(A = "white"))`, are sent as named lists, so they no longer depend on a deprecated behaviour of jsonlite and its message.
- `GC_zoom_to()` says that it needs shiny when shiny is not installed.

# geneviewer 0.2.0

### New features:
- `GC_labels(adjustLabels = "spread")` puts the gene labels on one line below the lowest gene, moved apart where they would overlap, with a line from each label to its gene, as in gene order figures. `leaderLine` sets the distance from the genes to the labels (`length`) and the style of the lines, e.g. `leaderLine = list(length = 30, stroke = "grey")`, or `leaderLine = FALSE` for no lines. `spreadPosition = "top"` puts the labels above the highest gene instead. Lines follow labels that are dragged, and start at the top of the gene when a label is dragged above it, and the labels are spread again for the genes in view when zoomed in. Themes style the lines too: they take the `lineColor` of `GC_theme()`, can be set with `GC_theme(labels = list(leaderLine = list(...)))`, and each built-in theme draws them to match its other lines.
- `GC_zoom()` adds a zoom slider below the clusters. Drag a handle to change the window, drag the window to move it, click the track to move the window there, and double-click the slider or use the new reset zoom button of the toolbar to show everything again; the arrow keys, Home and End work on a focused handle or window. Genes, labels, links, coordinates and annotations follow the window and are cut off at the edges of the plot area, and the axis shows the ticks of the window. `mode = "relative"` (default) shows the same share of every cluster, `mode = "absolute"` the same base pairs; `range` sets the window shown first, `min` the narrowest window, `update = "release"` draws only when the slider is let go, and `overview` shows the genes of each cluster in the slider. Saved images leave out the slider and the room it takes, so they look as if the chart had none. In Shiny, `GC_events(zoom = TRUE)` reports the window as `input$<outputId>_zoom` on the first render and after every change, with the base pairs each cluster shows and a `range` to pass back to `GC_zoom(range = )`, and `GC_zoom_to()` sets it from the server. Redraws from Shiny with the same data, range and mode keep the window the user picked; a new `GC_events(render_id = )` marks a new load, which starts at `range` again.
- `GC_toolbar()` sets the chart's toolbar: when it shows (`"hover"`, `"always"` or `"never"`), its corner, which buttons it has, the size and colours of its icons and tooltips, and the icons themselves, by Font Awesome name (e.g. `icons = list(reset = "arrows-to-circle")`). Its tooltips and save menu take a border, corner radius, shadow and font. Colours, size and the tooltip style can also be set for a theme with `GC_theme(toolbar = list(...))`. The built-in themes style the toolbar to fit: their tooltip style for its tooltips and menu, and icon colours of the theme (the ECharts toolbox colours for the ECharts-based themes, muted grey that lights up on dark, black and grey for print), so the icons stay visible on the dark theme.
- `GC_events(select = TRUE)` selects several elements: Shift-, Ctrl- or Cmd-click adds or removes one, and dragging on empty space draws a box that selects the elements inside it. `input$<outputId>_selection` lists all selected elements; `input$<outputId>_select` stays the one selected last. With `drag = TRUE`, dragging a selected element or pressing the arrow keys moves all selected labels, titles, scales and legend parts together, as one step to undo.
- `GC_events(drag = TRUE)` can move legend labels and markers, each on its own. Save their positions with `GC_element_style("legend", group = ..., part = "label"` or `"marker", moveX = ..., moveY = ...)`. A drag no longer counts as a click, so dragging a legend item does not hide its genes.
- With `GC_events(drag = TRUE)`, moves can be undone, redone and reset: undo, redo and reset icons appear next to the download icon once something has been moved, and Ctrl/Cmd+Z, Ctrl/Cmd+Shift+Z and Ctrl+Y work after clicking the chart. Reset moves every element back to where it was first drawn, keeping positions set with `GC_element_style()`, as one step that can be undone. With `select = TRUE`, the arrow keys move the selected text or scale (Shift: 10 pixels). Steps of one element report `input$<outputId>_drag` like a drag, with a new `action` field (`"drag"`, `"nudge"`, `"undo"` or `"redo"`); steps of more elements report `input$<outputId>_moves` once. The download icon and its formats can now also be used with the keyboard. Drags now move by whole pixels, also when the chart is scaled.
- `GC_events(drag = TRUE)` can move a whole scale axis or scale bar from its line, ticks or text. Save per-cluster positions with `GC_element_style("scale", cluster = ..., moveX = ..., moveY = ...)` or the `"scaleBar"` component.
- `GC_events(rendered = TRUE, render_id = )` reports successful chart updates through Shiny's `input$<outputId>_rendered` and a bubbling `geneviewer:rendered` browser event. The event carries the update identifier, chart DOM ID (`chartId`) and widget dimensions. An omitted `render_id` generates a unique ID per update across charts on the page. It fires after drawing and click setup, and does not fire on resize or failed draws.
- `GC_events()` makes a chart report clicks to Shiny: `input$<outputId>_click` gives the part of the chart that was clicked (`"genes"`, `"labels"`, `"scale"`, `"clusterTitle"`, ...), its cluster, and the row of the clicked gene or link or the legend group, e.g. to show the details of a gene. `highlight = TRUE` outlines the part under the mouse. Charts are not clickable unless it is set. Every part of the chart is now marked with a `data-component` attribute and each cluster with `data-cluster`, for CSS and JavaScript.
- `GC_events()` can report double-clicks independently of clicks, enable individual gene and label selection with keyboard access, and exclude legend clicks from settings handlers. Selection and hidden legend groups survive redraws and resize for the same data. The payloads of a gene or label include `style`, how it is drawn (colours, opacity, border, shape, text and font), so an editor can show the current values.
- `GC_element_style()` edits or resets one element: a gene, gene label or coordinate by row ID (and `end = "start"` or `"end"` for a coordinate), so sorting and repeated names do not redirect an edit, or the title or subtitle of a cluster title, footer or label by `cluster` and `part`. Labels, titles and subtitles can also change their displayed text, and coordinates can be hidden. Generated R code can record these calls to reproduce visual edits. With `GC_events(select = TRUE)` all of these elements can be selected one by one.
- `GC_legend(groupStyle = )` styles the legend entry of a group: its text, e.g. `list(tnsA = list(fontStyle = "italic"))` for italic gene names, and its marker through `marker`, e.g. `list(A = list(marker = list(stroke = "black")))`.
- `GC_transcript(groupStyleExons = , groupStyleIntrons = , groupStyleUTRs = )` style all exons, introns or UTRs of a group, with the options of the matching `itemStyle`; `style_by` sets the column they are matched against, e.g. `style_by = "transcript"` to style one transcript.
- `GC_coordinates(groupStyle = )` styles the coordinates of the genes of a group: `show = FALSE` leaves them out, `tick` styles their ticks and the other options their text, e.g. `list(other = list(show = FALSE))` to show the coordinates of the named genes only. `style_by` sets the column they are matched against.
- `GC_genes(style_by = )` and `GC_labels(style_by = )` set the column `groupStyle` is matched against, to style genes or labels by another column than the one that sets their colours, e.g. labels by their text: `GC_labels(label = "name", style_by = "name", groupStyle = list(tnsA = list(fill = "white")))`.
- `GC_scale(scale_break_padding = )` also takes the padding around automatic scale breaks in base pairs, e.g. `"500bp"`, for the same room around every break; a number is still a percentage. A gap too short for the padding gets no break.
- `GC_scale(breaks = )` also takes a data frame with `start` and `end` columns and, for breaks per cluster, a `cluster` column, e.g. `data.frame(cluster = c("A", "B"), start = c(5000, 8000), end = c(9000, 12000))`, to set different breaks for each cluster in one call.
- `GC_labels(groupStyle = )` styles the labels of all genes of a group at once, e.g. `groupStyle = list(tnsA = list(fill = "white"))` for white labels on dark genes. The `itemStyle` of a label takes precedence over the style of its group.
- `GC_genes(groupStyle = )` styles all genes of a group at once, with the options of `itemStyle`, e.g. `groupStyle = list(promoter = list(marker = "box", fill = "grey"))`. The `itemStyle` of a gene takes precedence over the style of its group.
- `GC_theme()` sets the default look of the whole chart: start from a built-in theme (`"default"`, `"classic"`, `"dark"`, `"vintage"` and `"infographic"` with the colours of the ECharts default (version 5), dark, vintage and infographic themes, `"publication"` for journal figures, after common journal figure guidelines (Arial, 7 pt text, black text and axes, italic gene labels, colour-blind safe colours), or `"bw"`: black and white with a pattern per group, in the same style), set the font, text and line colours, background, gene colours and patterns of all components at once, and change the options of any component (e.g. `labels = list(fontSize = "10px")`). `textScale` scales all text at once (e.g. `textScale = 1.5`), with the title and the default cluster label width growing with it. Options set with the other `GC_` functions always take precedence over the theme, whatever the order of the calls. `geneviewer_theme()` returns the settings of a built-in theme.
- `GC_pattern()` fills genes, exons and legend markers with a pattern per group, for black and white figures. There are 16 patterns (`"diagonal"`, `"dots"`, `"crosshatch"`, `"checkerboard"`, `"bricks"`, `"inverseDots"` and more). Patterns, pattern colours and backgrounds can be set per group, and `background = NULL` draws the patterns on top of the gene colours. They are kept in the PNG from the save button.
- The save button can now save charts as SVG, with all clusters, links, legends and titles in one vector file for publications. Hover the download icon and pick "Save as SVG" or "Save as PNG".
- Gene colours can be set by the name of a geneviewer palette, besides the d3 colour schemes: `"okabeIto"` (colour-blind safe), `"classic"` (clear colours), `"vivid"` (bright colours for dark backgrounds), `"dark"` (soft colours for dark backgrounds), `"vintage"` (muted colours), `"infographic"` (strong colours), `"greys"` and `"blues"`, in `GC_theme(palette = )` and the `colorScheme` of `GC_genes()`, `GC_transcript()` and `GC_color()`. `geneviewer_palette()` returns their colours.
- `GC_links(label_position = "link")` writes the measure of each link (e.g. its identity) in the middle of the link instead of with the gene label: only where it fits within its link and clear of the others, white on dark links. Zoomed in with `GC_zoom()`, links widen and more measures are written. Links from `data` (e.g. an alignment) can be labelled this way too.
- `GC_cluster(prevent_gene_overlap = "nested")` moves only the genes that lie inside another gene, e.g. an ORF in another reading frame, to the tracks below; the other genes stay on the first track, also where they overlap. `GC_cluster(overlap_min = )` keeps genes that overlap by no more than a number of base pairs, or a share of the shorter gene (e.g. `"10%"`), on the same track, so genes that share a base or a stop codon are not split. With `style_by = "geneTrack"`, `GC_genes()` and `GC_labels()` style the genes and labels of each track, e.g. `groupStyle = list("2" = list(markerHeight = 8))` to draw the genes of the second track smaller.
- `GC_labels(adjustLabels = "hide")` hides labels that overlap another one instead of turning them; zoomed in, they come back when there is room.
- `GC_links(colorBarOptions = )` places the color bar: `position` puts it in a corner of the plot area (`"bottomright"`, the default, `"bottomleft"`, `"topright"` or `"topleft"`) or, with `"legend"`, in the legend, before the groups or, with `legendSide = "right"`, after them, lying down in the legend's font, as tall as the legend markers and set apart from the groups; `orientation = "horizontal"` lays it down as one line with the title, the lowest measure, the bar and the highest, or with `labels = "below"`, the title above the bar and ticks below it, as many as fit. The title of a lying bar is bold. Charts with inverted links stack the two bars. A bar meant for the legend stands in the bottom right corner when the chart has no legend. By default the bar stands bottom right, as before.
- `GC_links(low_color = )` sets the colour of links with the lowest identity (white by default), e.g. the background colour on a dark chart.
- `GC_genes(corner_radius = )` rounds the corners of the "arrow", "boxarrow" and "box" markers (e.g. `corner_radius = 3` for soft corners; the inner corners of arrows stay sharp) and sets those of "rbox". Exons take `cornerRadius` in `styleExons`, and themes in `genes = list(corner_radius = )`. `GC_legend(corner_radius = )` rounds the corners of the legend markers.
- `GC_links(vmin = , vmax = )` set the measures of the lightest and the full link colour, and the ends of the colour bar (0 and 100 by default): e.g. `vmin = floor(min(alignment$identity))` spreads the colours over the identities of an alignment, so differences are easier to see. Measures outside are drawn in the colour of the nearest end.
- `GC_legend(align = )` aligns the legend to the left, centre or right, like `GC_title()`: `"left"` (default), `"center"` or `"right"`. Each row of a legend that wraps is aligned on its own, and `x` moves the legend from that position. Themes take it in `legend = list(align = )`.
- `GC_title()`, `GC_clusterTitle()` and `GC_clusterFooter()` now have a documented `align` argument (`"left"`, `"center"` or `"right"`), which is checked; it was only taken through `...`. The cluster title and footer take one per cluster, e.g. `align = c("left", "right")`.
- `GC_clusterLabel(align = )` aligns the lines of the cluster labels to the left, centre (default) or right of their margin, e.g. `align = "left"` so the names and the species below them start at the same place. `x` moves the labels from that position.
- `GC_clusterLabel(subtitle = )` adds a second line below each cluster label, e.g. the species, styled with `subtitleFont` (11px and not bold by default). `spacing` sets the pixels between the last line of the title and the subtitle (by default 1.2 times their mean font size), and the two are centred together on the cluster. Themes colour the subtitle like the other text, and the dark, vintage and publication themes like their other subtitles.
- `GC_title()`, `GC_clusterTitle()` and `GC_clusterFooter()` now have a documented `spacing` argument for the pixels between the title and the subtitle, which is checked; it was only taken through `...`. The cluster title and footer take one per cluster.
- `GC_trackMouse()` takes the styles of the coordinate tooltip (e.g. `backgroundColor`, `fontSize`) and `cursor`.

### Enhancements:
- `read_fasta()` is about 4-5 times faster on large FASTA files.
- Charts are drawn faster and are much smaller in HTML pages, Shiny apps and R Markdown documents. Gene labels are placed without laying out the page again for each label, and are only checked against the labels near them: a cluster of 4,000 labelled genes is drawn about 10 times faster, also on resize. The gene data is sent to the browser once instead of twice, and protein sequences (the `translation` and `sequence` columns) are left out unless an option names them, e.g. a tooltip `formatter` with `{translation}`, so a chart of the erythromycin BlastP example is 32 KB instead of 261 KB. They stay in the chart in R, and the row data of Shiny events no longer includes them unless they are shown.
- `protein_blast()` is about 3 to 9 times faster and gives the same results. The aligned sequences are now read from all alignments at once and identity and similarity are counted for all pairs at once, so parallel processing is no longer needed: the `parallel` argument is ignored and the `parallel` package is no longer a dependency.

### Changes:
- The documentation of `protein_blast()` now describes what it does: a global (Needleman-Wunsch) pairwise alignment of every protein pair, not BLASTP. Identity and similarity are counted over the full alignment, gaps included, so they can be lower than BLASTP values. The package description cites Needleman and Wunsch (1970) instead of the BLAST paper, and tooltips of its results say "Best hit" instead of "BlastP hit with". The function, its `BlastP` column and the `erythromycin_BlastP` data keep their names.
- `read_fasta()` no longer needs Biostrings: it reads FASTA files with base R and gives the same result, including gzip-compressed files. It now also reads files with old Mac (`\r`) line endings.
- `genbank_to_fasta()` now writes to `tempdir()` by default instead of next to the input file (use `output_dir = NULL` for the old behaviour), and returns the path of the FASTA file invisibly.
- knitr, rmarkdown, tidyr and biomaRt are no longer suggested: they are only used by the articles of the package website, which are not part of the package.
- MUMmer is now listed under `SystemRequirements`, and the examples of `read_fasta()`, `read_gff()`, `read_bed()`, `gbk_features_to_df()` and `protein_blast()` run on example data.
- FASTA and GFF readers now spread metadata with an internal helper instead of tidyr. Removing the tidyr dependency also avoids its stringr, stringi and purrr dependencies when they are not required by other packages, reducing Shinylive startup downloads.
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
- New tutorial "Reproducing a published figure: Tn7-like transposons", which rebuilds Fig. 3 of Peters et al. (2017) step by step: scale breaks, lining up on a gene, group styles for genes, labels and the legend, and saving as SVG. The "Example charts" article shows the finished chart, which also replaces an example chart in the README. Its data are in the new dataset `tn7_crispr_elements`, made by `data-raw/tn7_crispr_elements.R` from the GenBank records.
- The "Example charts" article has a new example for `GC_zoom()`: the genomes of eleven coronaviruses from the four genera, after Fig. 1 of Chazal (2021) and ordered by subgenus, with the ORFs that lie inside another gene on a lower track and links between homologous genes. Its data are in the new dataset `coronavirus_genomes`, with the homologous genes of closely related neighbouring genomes and their protein identity, made by `data-raw/coronavirus_genomes.R` from the GenBank records.
- The "Example charts" article has a new example for `GC_labels(adjustLabels = "spread")`: the mitochondrial gene order of the locust and human, after Fig. 3 of Jameson et al. (2003), with each circular genome rotated to start at COX1 and the gene labels on one line below the genes. Its data are in the new dataset `mitochondrial_gene_order`, made by `data-raw/mitochondrial_gene_order.R` from the RefSeq records.

### Bug fixes:
- `read_fasta()` reads the FASTA headers again with Biostrings 2.81 and later, which moved them out of the row names of `as.data.frame()`; it found no keys and returned no rows.
- Labels turned to avoid overlap (`GC_labels(adjustLabels = TRUE)`) are no longer cut off: a label that would turn out of its cluster, e.g. above it or past the right end of the plot area, is now hidden, and comes back when there is room for it, e.g. when zoomed in.
- `GC_annotation()` draws a text annotation by default, as documented; its default type was `"textAnnotation"`, which was not drawn. An unknown `type` is now an error that lists the types.
- `GC_transcript()` keeps the `GC_events()` and `GC_toolbar()` settings of the chart; they were lost when they were set before it.
- The stylesheet of the chart no longer styles elements outside it: its rules for the `hovered`, `label`, `legend` and gene classes only apply inside a chart.
- Links can be clicked and highlighted with `GC_events()`: they are drawn below the SVGs of the clusters, which took every click over the chart, so a click on a link was never reported. Drawing, `GC_trackMouse()` and saved SVG and PNG files are unchanged.
- `mummer_alignment()` now finds MUMmer on Windows; it looked for it with a Unix-only shell command and always reported it as not installed.
- `mummer_alignment()` now runs in a unique working subdirectory, preserving existing input and output files and cleaning up its own files on success or failure. With `remove_files = FALSE`, files are retained in that subdirectory of `output_dir`.
- Bundled D3 and FileSaver.js now include their upstream license notices and copyright attribution.
- FASTA tests now skip when the optional Biostrings package is unavailable.
- `read_bed()` no longer drops records when feature names contain "track", and preserves records across multiple track declarations.
- `get_introns()` only adds positive gaps between covered regions, including when exons and UTRs overlap, nest or are adjacent.
- Tooltips of `protein_blast()` results: genes of the query cluster are shown as "Query gene" instead of a hit with themselves (or "No Hit") at 100% identity, and genes without a hit no longer show empty "Identity: %" and "Similarity: %" lines. In `GC_tooltip()` formatters, a line whose fields all have no value is now left out.
- `GC_links()` leaves the chart unchanged when supplied an empty alignment.
- GFF and FASTA metadata named `id` no longer collides with internal record identifiers.
- PNG export now rasterizes the same SVG snapshot as SVG export, so it works inside apps whose surrounding styles use modern CSS colors such as `oklch()`. Backgrounds, patterns and the device pixel ratio are retained without parsing the app's stylesheets. The unused html2canvas dependency is no longer bundled.
- In Shiny, fixed pixel chart widths now size the whole widget before drawing, so the title, clusters and legend share the requested width instead of shrinking the clusters into the output container. Switching back to a responsive chart restores the original output width; standalone and height sizing are unchanged.
- When `GC_links()` is called more than once, the link labels of each call now use the options of that call (`label`, `measure`, `labelStyle`, `value1`, `value2`, ...); all link sets used those of the first call. A gene in more than one link set gets one label.
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
