## Submission comments

This is an update from 0.1.11 to 0.2.0. It adds new chart features (themes,
patterns, SVG export, click events for Shiny, styling by group) and fixes a
number of bugs in the file readers and charts. See NEWS.md for details.

* The bundled D3 (ISC) and FileSaver.js (MIT) libraries now ship with their
  upstream license files; their authors are listed as `ctb`/`cph` in
  Authors@R and the details are in inst/COPYRIGHTS.
* tidyr is no longer a dependency.
* `mummer_alignment()` uses the external MUMmer programs, now declared in
  SystemRequirements. Its examples stay in `\dontrun{}` because they need
  MUMmer installed; all other examples now run on data bundled with the package.

## Test environments

* Local macOS 15 (arm64), R 4.4.0
* win-builder, R-devel
* win-builder, R-release (R 4.6.1)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.
