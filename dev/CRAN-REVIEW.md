# geneviewer CRAN readiness review

Reviewed 2026-10-26, version 0.2.0, commit bf43813. Original verdict:
**not ready for submission**. All seven findings are now fixed locally; see below
for validation and remaining release checks.

The original review changed no package implementation, tests, metadata, or
submission comments. The existing untracked `dev/BUGLIST.md` was left unchanged.

## Fix progress (2026-10-26)

All seven findings below have now been addressed, in order:

1. MUMmer uses a unique working subdirectory, never overwrites existing files,
   and removes only that directory on success or failure. Retained outputs live
   in that subdirectory when `remove_files = FALSE`; documentation was updated.
2. Full upstream D3 and FileSaver licenses are shipped alongside the libraries,
   with `inst/COPYRIGHTS` provenance and `Authors@R` attribution. Both licenses
   were verified to be present in the built tarball.
3. The FASTA test skips when Biostrings is not installed.
4. BED headers are filtered as complete directive/comment lines. Feature names
   containing `track` and records across multiple track declarations are kept.
5. Intron gaps use the running covered end of exon/UTR intervals within each
   transcript. Adjacent, overlapping and nested intervals create no false gaps.
6. Empty alignment data leaves a chart and its existing links unchanged.
7. Reader bookkeeping IDs are returned separately from metadata; lowercase `id`
   fields are preserved in both GFF and FASTA inputs.

Targeted regressions cover cleanup on errors, existing-file preservation,
retained output, BED data loss, nested intervals and reverse strands, empty
alignments, and reader metadata. Testthat now requires 3.2.0 for the mocking
API used by the cleanup tests. NEWS records each fix.

Validation after fixes:

- The full development suite passed with no skipped tests, including browser
  rendering, PNG export, FASTA parsing, and parallel protein alignment.
- The source build passed, and both upstream license files and COPYRIGHTS are
  included in the tarball.
- The final archive passed `R CMD check --as-cran --no-manual` with **0 ERRORs,
  0 WARNINGs, 2 NOTEs**. Its installed-package tests reported **890 passes,
  0 failures, 0 warnings, 26 skips**; the skips are the tests marked off CRAN.
- The two remaining NOTEs are unavailable suggested package biomaRt and inability
  to verify the current time. Biostrings 2.74.1 and pwalign 1.2.0 were installed
  into a temporary library for validation; the project library was unchanged.
- The initial post-fix check included CRAN incoming checks. After updating the
  minimum testthat version, the final archive was checked again with incoming
  checks disabled; both checks completed without errors or warnings.
- `git diff --check` passed.

New artifacts are in `/private/tmp/geneviewer-cran-fixes/`, including
`tests.log` (without optional Bioconductor dependencies),
`tests-with-bioconductor.log` (full suite), `build.log`, the source tarball,
and `final/geneviewer.Rcheck/00check.log` (final archive check).

The original findings and baseline check results below are retained as history.

## Findings

### 1. High: default MUMmer cleanup deletes unrelated sequence files

Location: `R/mummer_alignment.R:82-88` and `R/mummer_alignment.R:190-195`.

After copying inputs into `output_dir`, the function lists every matching
GenBank/FASTA file there. With the default `output_dir = tempdir()` and
`remove_files = TRUE`, cleanup deletes that entire list, including pre-existing
files unrelated to the requested clusters. Copying also uses `overwrite = TRUE`,
so existing files with matching basenames can be overwritten before alignment.

Reproduced in an isolated R process: put `unrelated_review.fasta` in `tempdir()`,
put two identical 3,000-base FASTAs in a separate input directory, then run
`mummer_alignment(input, cluster = c("review_A", "review_B"), seqtype = "nucleotide")`.
The alignment returned one row and `file.exists(unrelated_file)` returned FALSE.

Use a unique working subdirectory per call and track only files created by that
call. Never remove or overwrite pre-existing files as incidental cleanup.

### 2. High: bundled JavaScript lacks upstream license notices

Locations: `inst/htmlwidgets/lib/D3-7.8.5/d3.min.js:1`,
`inst/htmlwidgets/lib/FileSaver-2.0.4/FileSaver.min.js:1`, and `DESCRIPTION:5`.

The built source archive contains no upstream license files or COPYRIGHTS file.
D3 includes a copyright header but no permission notice. FileSaver includes
neither its copyright nor its permission notice. The package's own MIT license
does not supply those notices for these upstream works.

Ship the full upstream notices alongside the libraries or in an installed
third-party notices file. Record the upstream attribution and provenance; review
the Authors@R/copyright metadata accordingly. This is a submission blocker even
though R CMD check does not diagnose it.

Sources:

- [D3 7.8.5 license](https://raw.githubusercontent.com/d3/d3/v7.8.5/LICENSE)
- [FileSaver 2.0.4 license](https://raw.githubusercontent.com/eligrey/FileSaver.js/v2.0.4/LICENSE.md)
- [CRAN source package policy](https://cran.r-project.org/web/packages/policies.html)

### 3. High: tests fail when an optional dependency is absent

Location: `tests/testthat/test-load_data.R:14-18`.

Biostrings is in Suggests, and `read_fasta()` deliberately requires it. This test
calls the function unconditionally. With Biostrings unavailable and
`_R_CHECK_FORCE_SUGGESTS_=false`, both the development tests and the installed
package check fail here. Other Biostrings tests already skip appropriately.

Add `skip_if_not_installed("Biostrings")` to this test and retain a separate CI
run with all suggested packages installed so the FASTA functionality is tested.

### 4. Medium: BED import silently discards valid feature rows

Location: `R/read_bed.R:84-88`.

`grep("track", lines)` treats any occurrence as a track declaration. A feature
name containing `track` therefore causes all rows through that feature to be
dropped. Multiple declarations also make the subsequent colon expression invalid
or ambiguous. Browser declarations and comments before the data are not handled
consistently either.

Reproduction:

```r
f <- tempfile(fileext = ".bed")
writeLines(c("chr1\t0\t10\tfirst",
             "chr1\t20\t30\ttrack_gene",
             "chr1\t40\t50\tlast"), f)
read_bed(f)
# Returns only the last row, instead of all three.
```

Filter actual header/directive lines with anchored patterns instead of slicing
after an arbitrary substring match.

### 5. Medium: intron calculation invents intervals inside adjacent/overlapping exons

Location: `R/transcript.R:47-59`.

Every pair of consecutive exon/UTR records generates an intron, even if there is
no gap. For starts `c(1, 11, 15)` and ends `c(10, 20, 30)`, `get_introns()` adds
introns `11..10` and `21..14`. These are invalid genomic intervals, and
GC_transcript uses this helper directly. Nested/overlapping UTR and exon records
can cause the same problem.

Merge covered intervals within each transcript before computing positive gaps.
At minimum, do not emit an intron unless its start is no greater than its end;
merging is needed to handle nested intervals correctly.

### 6. Medium: empty alignment data crashes GC_links

Location: `R/chart_links.R:372-382`.

`1:nrow(links_data)` returns `c(1, 0)` for an empty data frame, causing
`replacement has 2 rows, data has 0`. Empty alignments are an expected result
when sequences have no matching regions.

Reproduced by supplying an empty data frame with columns `cluster1`, `cluster2`,
`start1`, `end1`, `start2`, `end2`, and `identity` to `GC_links(chart, data = x)`.

Use `seq_len(nrow(links_data))` in both branches and explicitly support an empty
link set or return the chart unchanged.

### 7. Medium: a metadata key named id collides with internal bookkeeping

Location: `R/widen_attributes.R:32-33`; GFF call site `R/read_gff.R:117-124`.

The helper stores the source record identifier in a column named `id`, then
adds parsed metadata with the same names as user-supplied keys. A lowercase
`id` attribute produces duplicate column names and an error. This differs from
the standard uppercase `ID`; lowercase custom attributes are still valid input.
The same helper is used for FASTA metadata.

Reproduced with this GFF3 feature:

```text
chr1\ttest\tgene\t1\t10\t.\t+\t.\tid=abc;Name=gene1
```

Keep row bookkeeping outside user metadata, or choose an internal identifier
that cannot collide and remove only that internal identifier afterwards.

## Validation performed

- `R CMD build --no-manual .`: passed; source archive approximately 1.4 MB.
- `R CMD check --as-cran --no-manual` with
  `_R_CHECK_FORCE_SUGGESTS_=false`: **1 ERROR, 0 WARNINGs, 2 NOTEs**.
- Installed-package test results: **817 passes, 1 failure, 31 skips**.
- Installation, package/namespace load and unload, R syntax, static code checks,
  dependency use, Rd documentation, examples, and `--run-donttest` examples passed.
- Both first-party JavaScript files passed `node --check`.
- Findings 1 and 4-7 were reproduced against the source in a separate script;
  finding 2 was checked against archive contents and the upstream licenses.
- A development test run reproduced the same FASTA failure. Browser tests could
  not run in that library because chromote could not be loaded.

Environment: macOS 15.7.4, ARM64, R 4.4.0. The project renv autoloader stalled;
checks used installed system/user R libraries with
`RENV_CONFIG_AUTOLOADER_ENABLED=false`. The final check ran with network access.
Biostrings, pwalign, and biomaRt were unavailable in that library. The two NOTEs
were unavailable suggested packages and inability to verify the current time.
The missing optional dependency explains the observed test failure; it is not
evidence that FASTA import fails when Biostrings is installed.

Raw artifacts for this session:

- `/private/tmp/geneviewer-cran-review/build.log`
- `/private/tmp/geneviewer-cran-review/geneviewer_0.2.0.tar.gz`
- `/private/tmp/geneviewer-cran-review/geneviewer.Rcheck/00check.log`
- `/private/tmp/geneviewer-cran-review/geneviewer.Rcheck/tests/testthat.Rout.fail`
- `/private/tmp/geneviewer-cran-review/tests.log`
- `/private/tmp/geneviewer-cran-review/reproduce.R`
- `/private/tmp/geneviewer-cran-review/reproduce.log`

## Remaining submission preparation

1. Done: resolved all seven findings and added targeted regression tests.
2. Run the final tarball on current R release and R-devel, including Windows and
   Linux, with suggested packages present; also retain a reduced-dependency run.
   The existing GitHub Actions matrix specifies release only, despite the older
   cran-comments claiming devel coverage. No current remote CI run was verified.
3. Keep browser tests running with working Chrome/chromote on CI. They passed in
   the full local development suite after these fixes, but deliberately skip
   on CRAN, so a successful CRAN check alone cannot validate rendering/export.
4. Build/check the PDF manual on a machine with LaTeX. This review used
   `--no-manual` because pdflatex is unavailable.
5. Replace the old dated `cran-comments.md` with actual results for the release
   being submitted. Do not retain its claim of zero errors/warnings/notes unless
   supported by the final checks.
6. All current vignette sources are excluded from the source tarball by
   .Rbuildignore; none were built or checked. This is not itself a CRAN error,
   but decide whether the release should ship a self-contained vignette. The
   website-only BLASTP article contains a local path and would need work before
   inclusion as a package vignette.
7. Consider declaring a minimum dplyr version matching the APIs used, notably
   `across()` and `all_of()`, instead of allowing arbitrarily old installations.

These checks establish concrete problems and a path to submission; they do not
certify that every R/JavaScript execution path is correct.
