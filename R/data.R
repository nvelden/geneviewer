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

#' Erythromycin BlastP results
#'
#' This dataset contains detailed information on genes involved in the
#' biosynthesis of Erythromycin, an antibiotic produced by the bacterium
#' Saccharopolyspora erythraea and several homologous gene clusters identified
#' by antiSMASH. It includes gene identifiers, chromosomal positions,
#' orientations, and annotations regarding their products and functions, as well
#' as similarity and identity scores from BlastP analysis.
#'
#' @format A data frame with 148 observations and 16 variables:
#' \describe{
#'   \item{protein_id}{Unique protein identifiers. A character vector.}
#'   \item{region}{The chromosomal region of the gene, indicating start and end positions and strand. A character vector.}
#'   \item{translation}{Amino acid sequence of the protein encoded by the gene. A character vector.}
#'   \item{cluster}{Identifier of the gene cluster to which the gene belongs. A character vector.}
#'   \item{strand}{The strand orientation ("forward" or "complement") of the gene. A character vector.}
#'   \item{start}{The start position of the gene on the chromosome. A numeric vector.}
#'   \item{end}{The end position of the gene on the chromosome. A numeric vector.}
#'   \item{rowID}{A unique identifier for each row in the dataset. An integer vector.}
#'   \item{identity}{The identity score from BlastP analysis, representing the percentage of identical matches. A numeric vector.}
#'   \item{similarity}{The similarity score from BlastP analysis, often reflecting the functional or structural similarity. A numeric vector.}
#'   \item{BlastP}{Reference to the protein_id after BlastP comparison, or NA if not applicable. A character vector.}
#'   \item{score}{Score assigned based on the BlastP analysis, quantifying the match quality. A numeric vector.}
#'   \item{Gene}{Gene name or identifier if available, otherwise NA. A character vector.}
#'   \item{Position}{Formatted string indicating the gene's position and orientation on the chromosome. A character vector.}
#'   \item{Product}{Description of the gene product. A character vector.}
#'   \item{Functions}{Functional categorization of the gene. A character vector.}
#' }
#' @source Derived from BlastP analysis of Saccharopolyspora erythraea genes
#'   involved in Erythromycin production.
"erythromycin_BlastP"

#' BRCA1 Splice Variants
#'
#' This dataset contains detailed information on five different splice variants
#' of the BRCA1 gene. It includes data on the types of transcript features
#' (exon, intron, UTR), chromosomal positions, strand orientations, and
#' transcript lengths.
#'
#' @format A data frame with 218 observations and 6 variables:
#' \describe{
#'   \item{ensembl_transcript_id}{Unique Ensembl transcript identifiers. A character vector.}
#'   \item{type}{Type of the transcript feature, indicating whether it is an "exon", "intron", or "UTR". A character vector.}
#'   \item{start}{The start position of the feature on the chromosome. A numeric vector.}
#'   \item{end}{The end position of the feature on the chromosome. A numeric vector.}
#'   \item{strand}{The strand orientation of the feature, with -1 indicating the reverse strand. An integer vector.}
#'   \item{transcript_length}{The total length of the transcript in base pairs. An integer vector.}
#' }
#' @source Derived from the Ensembl website
"BRCA1_splice_variants"
