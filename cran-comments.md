02.01.2024

## Submission comments

* Added appropriate references to the `Description` field for methods utilized in the package.
* Revised the `Title` field to remove the redundant "in R" at the end.
* Updated `read_gbk` and `genbank_to_fasta` to utilize package data and replaced `\dontrun{}` with `\donttest{}`.
* Updated `mummer_alignment` to issue a warning instead of printing a message and adjusted the function to write output to `tempdir()` by default.

## Test environments
* macOS (macos-latest), R devel (via GitHub Actions)
* Windows (windows-latest), R devel (via GitHub Actions)
* Ubuntu (ubuntu-latest), R devel (via GitHub Actions)
* Local R installation, R 4.4.0 on macOS Ventura

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

19.12.2024

## Submission comments

* Added the `Authors@R` field in the DESCRIPTION file to comply with CRAN requirements.
* Revised the `Description` field in the DESCRIPTION file to avoid starting with the package name or phrases like 'This package'.

## Test environments
* macOS (macos-latest), R devel (via GitHub Actions)
* Windows (windows-latest), R devel (via GitHub Actions)
* Ubuntu (ubuntu-latest), R devel (via GitHub Actions)
* Local R installation, R 4.4.0 on macOS Ventura

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.
