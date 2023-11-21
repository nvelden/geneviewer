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
#'   \item{stop}{Stop positions of the genes. An integer vector.}
#'   \item{class}{Classifications of the genes. A character vector indicating
#'   the type of protein or function associated with each gene.}
#'   \item{cluster}{Cluster identification as a character vector, specifying whether
#'   the gene belongs to the ophA or dbophA gene cluster.}
#' }
#' @references
#' van der Velden NS et al. Autocatalytic backbone N-methylation
#' in a family of ribosomal peptide natural products.
#' Nat Chem Biol. 2017 Aug;13(8):833-835. doi: 10.1038/nchembio.2393.
#' Epub 2017 Jun 5. PMID: 28581484.
#' @export
ophA_clusters <- data.frame(
  name = c("ophB1", "ophC", "ophA", "ophD", "ophB2", "ophP", "ophE",
           "dbophB1", "dbophB2", "dbophC", "dbophD1", "dbophB3", "dbophA", "dbophD2", "dbophB4", "dbophP", "dbophE"),
  start = c(2522, 5286, 9536, 12616, 13183, 19346, 20170,
            40768, 38089, 37262, 34955, 32392, 28937, 27371, 25288, 21283, 19236),
  stop = c(4276, 4718, 10904, 11859, 15046, 16016, 21484,
           43005, 40469, 37791, 36331, 34679, 30516, 28885, 27217, 24607, 20583),
  class = c("Monooxygenase", "NTF2-like", "Methyltransferase", "O-acyltransferase", "Monooxygenase", "Prolyloligopeptidase", "F-box/RNHI-like",
            "Monooxygenase", "Monooxygenase", "NTF2-like", "O-acyltransferase", "Monooxygenase", "Methyltransferase","O-acyltransferase", "Monooxygenase", "Prolyloligopeptidase", "F-box/RNHI-like"),
  cluster = c(rep("ophA", 7), rep("dbophA", 10))
)
