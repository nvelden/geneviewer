#' ophA Gene Cluster from Omphalotus olearius
#'
#' This dataset represents the ophA gene cluster from Omphalotus olearius and
#' Dendrothele bispora, involved in the production of omphalotin, a cyclic
#' N-methylated peptide.
#'
#' @format A data frame with 17 rows and 5 columns:
#' \describe{
#'   \item{name}{Gene names. A character vector.}
#'   \item{start}{Start positions of the genes. An integer vector.}
#'   \item{end}{End positions of the genes. An integer vector.}
#'   \item{class}{Classifications of the genes. A character vector indicating
#'   the type of protein or function associated with each gene.}
#'   \item{cluster}{Cluster identification as a character vector, specifying whether
#'   the gene belongs to the ophA or dbophA gene cluster.}
#' }
#' @references van der Velden NS et al. Autocatalytic backbone N-methylation in
#' a family of ribosomal peptide natural products. Nat Chem Biol. 2017
#' Aug;13(8):833-835. doi: 10.1038/nchembio.2393. Epub 2017 Jun 5. PMID:
#' 28581484.
"ophA_clusters"

#' Human HOX Gene Cluster Data
#'
#' This dataset represents the positions and clusters of various human HOX
#' genes. HOX genes are a group of related genes that control the body plan of
#' an embryo along the head-tail axis. The dataset includes genes from HOXA,
#' HOXB, HOXC, and HOXD clusters.
#'
#' @format A data frame with 39 rows and 6 columns:
#' \describe{
#'   \item{symbol}{HGNC symbols for the HOX genes. A character vector.}
#'   \item{chromosome_name}{Chromosome number where each gene is located. A numeric vector.}
#'   \item{start}{Start positions of the genes on the chromosome. A numeric vector.}
#'   \item{end}{End positions of the genes on the chromosome. A numeric vector.}
#'   \item{strand}{-1 for reverse and 1 for forward.}
#'   \item{cluster}{Cluster identification as a character vector,
#'   specifying the HOX gene cluster (HOXA, HOXB, HOXC, HOXD).}
#'   \item{name}{Simplified names for the HOX genes. A character vector.}
#' }
"human_hox_genes"

#' Human Dystrophin Transcripts Data
#'
#' This dataset contains the Exon positions on human Dystrophin transcripts
#' 'Dp427p2', 'Dp260-2', 'Dp140', 'Dp116', and 'Dp71'.
#'
#' @format A data frame with 202 rows and 5 columns:
#' \describe{
#'   \item{transcript}{Transcript names. A character vector representing
#'   the names of the transcripts.}
#'   \item{type}{Transcript type.}
#'   \item{start}{Start positions. An integer vector showing the starting
#'   positions of each Exon.}
#'   \item{end}{End positions. An integer vector showing the ending
#'   positions of each Exon.}
#'   \item{length}{Transcript lengths. An integer vector
#'   indicating the length of each transcript.}
#' }
"hs_dystrophin_transcripts"

#' Erythromycin Gene Cluster Data
#'
#' This dataset contains information about various genes involved in the
#' biosynthesis of Erythromycin, an antibiotic produced by the bacterium
#' Saccharopolyspora erythraea. It includes details such as gene identifiers,
#' start and end positions of genes, and their associated functions and products.
#'
#' @format A data frame with 23 rows and 6 columns:
#' \describe{
#'   \item{Identifiers}{Unique identifiers for the genes. A character vector.}
#'   \item{Start}{Start positions of the genes on the chromosome. A numeric vector.}
#'   \item{End}{End positions of the genes on the chromosome. A numeric vector.}
#'   \item{Strand}{Strand orientation of the gene, either "+" or "-". A character vector.}
#'   \item{Product}{Description of the gene products. A character vector.}
#'   \item{Functions}{Functional categorization of the genes. A character vector.}
#' }
"erythromycin_cluster"


