## Submission comments

This is an update from 0.1.11 to 0.2.0. It adds new chart features (themes,
patterns, SVG export, click events for Shiny, styling by group) and fixes a
number of bugs in the file readers and charts. See NEWS.md for details.

* The bundled D3 (ISC) and FileSaver.js (MIT) libraries now ship with their
  upstream license files; their authors are listed as `ctb`/`cph` in
  Authors@R and the details are in inst/COPYRIGHTS.
* tidyr is no longer a dependency, and the suggested packages knitr,
  rmarkdown and biomaRt were removed: they were only used by website articles
  that are not part of the package.
* The description and documentation of `protein_blast()` now describe the
  method it uses, a global (Needleman-Wunsch) pairwise protein alignment, and
  cite it, instead of BLASTP.
* `genbank_to_fasta()` now writes to `tempdir()` by default instead of next to
  the input file.
* `mummer_alignment()` uses the external MUMmer programs, now declared in
  SystemRequirements. Its examples stay in `\dontrun{}` because they need
  MUMmer installed; all other examples now run on data bundled with the package.

## Test environments

* Local macOS 15 (arm64), R 4.4.0
* win-builder, R-devel (TODO: run on the final tarball and record the result)
* win-builder, R-release (TODO: run on the final tarball and record the result)

## R CMD check results

0 errors | 0 warnings | 1 note (local)

* checking for future file timestamps ... NOTE: unable to verify current time.
  This comes from the time server being unreachable during the local check.

## Reverse dependencies

There are no reverse dependencies.
