<p align="center">
  <img src="man/figures/logo.png" class="pkgdown-hide" height="150px" align="right">
  <h1><strong>geneviewer</strong> - Gene Cluster Visualizations in R</h1>
</p>

<!-- badges: start -->

[![R-CMD-check](https://github.com/nvelden/geneviewer/workflows/R-CMD-check/badge.svg)](https://github.com/nvelden/geneviewer/actions) [![CRAN status](https://www.r-pkg.org/badges/version/geneviewer)](https://CRAN.R-project.org/package=geneviewer) ![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)

<!-- badges: end -->

## geneviewer

**geneviewer** is an R package for plotting gene clusters and transcripts. It imports data from GenBank, FASTA, and GFF files, performs BlastP and MUMmer alignments, and displays results on gene arrow maps. The package offers extensive customization options, including legends, labels, annotations, scales, colors, tooltips, and more. To explore all features visit the [package website](https://nvelden.github.io/geneviewer/articles/geneviewer.html).

## Installation

**geneviewer** is still in the development stage which might lead to breaking changes and thus not yet released on CRAN. You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nvelden/geneviewer")
```

## Usage

The below example demonstrates using **geneviewer** to plot a gene cluster on a genomic sequence, using the start and end positions of each gene. The genes are grouped by class and labels are added using the `GC_labels` function.

``` r
library(geneviewer)

# Data
gene_cluster <- data.frame(
  name = c("ophB1", "ophC", "ophA", "ophD", "ophB2", "ophP", "ophE"),
  start = c(2522, 5286, 9536, 12616, 13183, 19346, 20170),
  end = c(4276, 4718, 10904, 11859, 15046, 16016, 21484),
  class = c("Monooxygenase", "NTF2-like", "Methyltransferase", 
  "O-acyltransferase", "Monooxygenase", "Prolyloligopeptidase", 
  "F-box/RNHI-like")
)

# Chart
GC_chart(gene_cluster, group = "class", height = "100px") %>%
  GC_labels("name")
```

<img src="man/figures/ophA_gene_cluster.png"/>

## Examples

For additional examples and the corresponding code to create the plots, please visit the [Examples](https://nvelden.github.io/geneviewer/articles/Examples.html) section.

<img src="man/figures/erythromycin_BlastP.png"/>

<hr>

<img src="man/figures/BRCA1_splice_variants.png"/>

<hr>

<img src="man/figures/MUMmer.png"/>

<hr>

<img src="man/figures/erythromycin_link.png"/>

<hr>

<img src="man/figures/ophA_clusters.png"/>

<hr>

<img src="man/figures/ophA_gene_links.png"/>

<hr>

<img src="man/figures/erythromycin_cluster.png"/>

<hr>

<img src="man/figures/human_hox_genes.png"/>

## Issues

If you encounter any issues or have feature requests, please open an [Issue](https://github.com/nvelden/geneviewer/issues).
