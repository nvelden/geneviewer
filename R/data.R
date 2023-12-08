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
           "dbophB1", "dbophB2", "dbophC", "dbophD1", "dbophB3", "dbophA",
           "dbophD2", "dbophB4", "dbophP", "dbophE"),
  start = c(2522, 5286, 9536, 12616, 13183, 19346, 20170,
            40768, 38089, 37262, 34955, 32392, 28937,
            27371, 25288, 21283, 19236),
  stop = c(4276, 4718, 10904, 11859, 15046, 16016, 21484,
           43005, 40469, 37791, 36331, 34679, 30516,
           28885, 27217, 24607, 20583),
  class = c("Monooxygenase", "NTF2-like", "Methyltransferase", "O-acyltransferase",
            "Monooxygenase", "Prolyloligopeptidase", "F-box/RNHI-like",
            "Monooxygenase", "Monooxygenase", "NTF2-like", "O-acyltransferase",
            "Monooxygenase", "Methyltransferase","O-acyltransferase",
            "Monooxygenase", "Prolyloligopeptidase", "F-box/RNHI-like"),
  cluster = c(rep("ophA", 7), rep("dbophA", 10))
)

#' Human HOX Gene Cluster Data
#'
#' This dataset represents the positions and clusters of various human HOX genes.
#' HOX genes are a group of related genes that control the body plan of an embryo along the head-tail axis.
#' The dataset includes genes from HOXA, HOXB, HOXC, and HOXD clusters.
#'
#' @format A data frame with 39 rows and 5 columns:
#' \describe{
#'   \item{symbol}{HGNC symbols for the HOX genes. A character vector.}
#'   \item{chromosome_name}{Chromosome number where each gene is located. A numeric vector.}
#'   \item{start}{Start positions of the genes on the chromosome. A numeric vector.}
#'   \item{stop}{End positions of the genes on the chromosome. A numeric vector.}
#'   \item{cluster}{Cluster identification as a character vector, specifying the HOX gene cluster (HOXA, HOXB, HOXC, HOXD).}
#'   \item{name}{Simplified names for the HOX genes. A character vector.}
#' }
#' @export
human_hox_genes <- data.frame(
  symbol = c("HOXA1", "HOXA10", "HOXA11", "HOXA13", "HOXA2", "HOXA3", "HOXA4",
             "HOXA5", "HOXA6", "HOXA7", "HOXA9","HOXB1", "HOXB13", "HOXB2",
             "HOXB3", "HOXB4", "HOXB5", "HOXB6", "HOXB7", "HOXB8", "HOXB9",
              "HOXC10", "HOXC11", "HOXC12", "HOXC13", "HOXC4", "HOXC5",
             "HOXC6", "HOXC8", "HOXC9","HOXD1", "HOXD10", "HOXD11", "HOXD12",
             "HOXD13", "HOXD3", "HOXD4", "HOXD8", "HOXD9"),
  chromosome_name = c(rep(7, 11), rep(17, 10), rep(12, 9), rep(2, 9)),
  start = c(27092993, 27170605, 27181157, 27193503, 27100354, 27106184, 27128507,
            27141052, 27145396, 27153716, 27162438, 48528526, 48724763, 48540894,
            48548870, 48575507, 48591257, 48595751, 48607232, 48611377, 48621156,
            53985065, 53973126, 53954903, 53938831, 54016931, 54033050, 53990624,
            54008985, 53994895, 176188668, 176108790, 176104216, 176099795,
            176092721, 176136612, 176151550, 176129694, 176122719),
  stop = c(27096000, 27180261, 27185232, 27200091, 27102686, 27152583, 27130780,
           27143681, 27150603, 27157936, 27175180, 48531011, 48728750, 48545109,
           48604912, 48578350, 48593779, 48604992, 48633572, 48615292, 48626358,
           53990279, 53977643, 53958956, 53946544, 54056030, 54035361, 54030823,
           54012769, 54003337,176190907, 176119937, 176109754, 176102489,
           176095944, 176173102, 176153226, 176132695, 176124937),
  cluster = c(rep("HOXA", 11), rep("HOXB", 10), rep("HOXC", 9), rep("HOXD", 9)),
  name = c("HOX1", "HOX10", "HOX11", "HOX13", "HOX2", "HOX3", "HOX4", "HOX5",
           "HOX6", "HOX7", "HOX9","HOX1", "HOX13", "HOX2", "HOX3", "HOX4", "HOX5",
           "HOX6", "HOX7", "HOX8", "HOX9", "HOX10", "HOX11", "HOX12", "HOX13",
           "HOX4", "HOX5", "HOX6", "HOX8", "HOX9", "HOX1", "HOX10", "HOX11", "HOX12",
           "HOX13", "HOX3", "HOX4", "HOX8", "HOX9")
)
