<p align="center">
  <img src="man/figures/logo.png" height="150px" align="right">
  <h1><strong>GCVieweR</strong> - Gene Cluster Visualizations in R</h1>
</p>

<!-- badges: start -->
[![R-CMD-check](https://github.com/nvelden/GCVieweR/workflows/R-CMD-check/badge.svg)](https://github.com/nvelden/GCVieweR/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/GCVieweR)](https://CRAN.R-project.org/package=GCVieweR)
![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)
<!-- badges: end -->

## GCVieweR

**GCVieweR** is an R package designed for drawing gene arrow maps. It allows side by side visualization of multiple gene clusters and has options to add a legend, labels, annotations, customized scales, colors, tooltips and much more.

## Installation

**GCVieweR** is still in the development stage and not yet released on CRAN. You can install the development version of **GCVieweR** from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nvelden/GCVieweR")
```

## Usage

The below example demonstrates using **GCVieweR** to plot a gene cluster on a genomic sequence, using the start and end positions of each gene. The genes are grouped by class and labels are added using the `GC_labels` function.

``` r
library(GCVieweR)

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
GC_chart(gene_cluster, group = "class", height = "100px", width = "800px") %>%
  GC_labels("name")
```

<img src="man/figures/ophA_gene_cluster.png" class="screenshot"/>


## Examples

For additional examples and the corresponding code to create the plots, please visit the [Examples]() section.  

<img src="man/figures/LacZ_operon.png" class="screenshot"/>

<hr>

<img src="man/figures/ophA_clusters.png" class="screenshot"/>

<hr>

<img src="man/figures/erythromycin_cluster.png" class="screenshot"/>

<hr>

<img src="man/figures/human_hox_genes.png" class="screenshot"/>

## Issues

If you encounter any issues or have feature requests, please open an [Issue](https://github.com/nvelden/GCVieweR/issues).    
