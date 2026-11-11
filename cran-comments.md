## Submission comments

This is an update from 0.2.0 to 0.2.1. See NEWS.md for details.

* Charts are now drawn by the geneviewer JavaScript library, which the package
  ships in inst/htmlwidgets/lib/geneviewer-0.3.10/ (MIT, by the package
  author). The R functions, their arguments and the charts they draw are
  unchanged.
* The internals of a chart widget changed: `widget$x` is now `list(spec = )`,
  the chart spec, instead of the options built in R. This is noted in NEWS.md
  for code that read `widget$x` directly.
* It fixes bugs in `protein_blast()`, `read_gff()`, `read_bed()`,
  `GC_legend()`, `GC_normalize()`, `GC_links()` and `GC_title()`.
* The bundled third-party code is D3 (ISC), with its upstream license file,
  and Font Awesome Free icons (CC BY 4.0) drawn as inline SVG in the
  geneviewer library. Their authors are listed in Authors@R and the details
  are in inst/COPYRIGHTS.
* `mummer_alignment()` uses the external MUMmer programs, declared in
  SystemRequirements. Its examples stay in `\dontrun{}` because they need
  MUMmer installed.

## Test environments

* Local macOS 15 (arm64), R 4.4.0

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.
