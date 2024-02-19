
#' Generate Protein Combinations Between Two Clusters
#'
#' This function takes a dataset containing protein information and a pair of
#' clusters. It generates all possible combinations of proteins between these
#' two clusters and merges the corresponding translations for each protein from
#' each cluster.
#'
#' @param data A dataframe containing at least the columns `cluster`, `rowID`,
#'   and `translation`, where each row represents a protein.
#' @param cluster_pair A vector of length 2 specifying the two clusters for
#'   which protein combinations are to be generated.
#'
#' @return A dataframe of all possible protein combinations between the two
#'   specified clusters. Each row in the returned dataframe represents a pair of
#'   proteins, one from each cluster, including their `rowID`s (as `rowID.x` and
#'   `rowID.y` for the first and second cluster, respectively), translations,
#'   and the names of their respective clusters (`cluster1` and `cluster2`).
#'
#' @noRd
get_protein_combinations <- function(data, cluster_pair) {

  cluster1_data <- subset(data, cluster == cluster_pair[1])
  cluster2_data <- subset(data, cluster == cluster_pair[2])

  protein_combinations <- expand.grid(rowID.x= cluster1_data$rowID,
                                      rowID.y = cluster2_data$rowID,
                                      stringsAsFactors = FALSE)

  # Merge translations for Query
  protein_combinations <- merge(protein_combinations, cluster1_data[, c("rowID", "translation")],
                                by.x = "rowID.x", by.y = "rowID", all.x = TRUE, suffixes = c("1","2"))

  # Merge translations for Target
  protein_combinations <- merge(protein_combinations, cluster2_data[, c("rowID", "translation")],
                                by.x = "rowID.y", by.y = "rowID", all.x = TRUE, suffixes = c("1","2"))

  # Add cluster names
  protein_combinations$cluster1 <- cluster_pair[1]
  protein_combinations$cluster2 <- cluster_pair[2]

  return(protein_combinations)
}

#' Compute Identity and Similarity Between Amino Acid Sequences
#'
#' This function calculates the percentage of identity and similarity between
#' pairs of amino acid sequences. Identity is defined as the percentage of exact
#' matches between the sequences, while similarity takes into account predefined
#' groups of similar amino acids.
#'
#' @param patterns A vector or list of amino acid sequences to be compared.
#' @param subjects A vector or list of amino acid sequences against which the
#'   patterns are compared. The length and order of `subjects` must correspond
#'   to `patterns`.
#'
#' @return A dataframe with two columns: `identity` and `similarity`,
#'   representing the percentage of identity and similarity for each pair of
#'   sequences. The percentages are rounded to three decimal places.
#'
#' @examples
#' patterns <- c("ACDEFGHIKLMNPQRSTVWY", "ACDGFHIKLMNPQRSTVWY")
#' subjects <- c("ACDEFGHIKLMNPQRSTVWY", "TCDGFHIKLMNPQRSTVWY")
#' result <- compute_identity(patterns, subjects)
#' print(result)
#'
#' @noRd
compute_identity <- function(patterns, subjects) {
  # Define groups of similar amino acids
  similar_acids <- list(
    c("G", "A", "V", "L", "I"),
    c("F", "Y", "W"),
    c("C", "M"),
    c("S", "T"),
    c("K", "R", "H"),
    c("D", "E", "N", "Q"),
    c("P")
  )

  # Precompute a lookup table for acid groups
  acid_group <- rep(NA, 26) # Using NA for acids not in the list
  names(acid_group) <- LETTERS
  for (group_id in seq_along(similar_acids)) {
    for (acid in similar_acids[[group_id]]) {
      acid_group[acid] <- group_id
    }
  }

  # Check if the inputs are lists and have the same length
  if (length(patterns) != length(subjects)) {
    stop("The lists of patterns and subjects must be of the same length")
  }

  # Apply the compute_identity logic to each pair of pattern and subject
  results <- mapply(function(pattern, subject) {
    if (nchar(pattern) != nchar(subject)) {
      stop("The two sequences must be of the same length")
    }

    pattern_chars <- strsplit(pattern, "")[[1]]
    subject_chars <- strsplit(subject, "")[[1]]

    sequence_length <- nchar(pattern)
    matches <- 0
    similar <- 0

    for (i in 1:sequence_length) {
      acid1 <- pattern_chars[i]
      acid2 <- subject_chars[i]

      if (acid1 == acid2) {
        if (acid1 != "-" && acid1 != ".") {
          matches <- matches + 1
        } else {
          sequence_length <- sequence_length - 1
        }
      } else if (!is.na(acid_group[acid1]) && !is.na(acid_group[acid2]) && acid_group[acid1] == acid_group[acid2]) {
        similar <- similar + 1
      }
    }

    identity <- round(matches / sequence_length, 3) * 100
    similarity <- round((matches + similar) / sequence_length, 3) * 100

    return(data.frame(identity = identity, similarity = similarity))
  }, patterns, subjects, SIMPLIFY = FALSE)

  # Combine the results into a data frame
  results_df <- do.call(rbind, results)
  rownames(results_df) <- NULL
  return(results_df)
}

#' Perform Protein BLAST Analysis Within Specified Clusters
#'
#' This function conducts a BLAST analysis for protein sequences within
#' specified clusters. It generates all possible protein combinations between a
#' query cluster and other clusters, performs pairwise alignments, calculates
#' sequence identity and similarity, and filters results based on a minimum
#' identity threshold. The function updates the provided data with BLAST
#' results, including identity and similarity scores for each protein pair, and
#' ensures that the query cluster is placed first.
#'
#' @param data A dataframe containing protein data including sequences or
#'   translations and identifiers.
#' @param id_column The name of the column that contains the gene identifiers.
#' @param query The name of the query cluster to be used for BLAST comparisons.
#' @param identity Minimum identity threshold for BLAST hits to be considered
#'   significant. Defaults to 0.3.
#'
#' @return A modified version of the input `data` dataframe, including
#'   additional columns for BLAST results (identity, similarity).
#'
#' @examples
#' # Assuming 'data' is your dataframe and 'ClusterA' is your query cluster
#' data_updated <- protein_blast(
#'                          GC_chart$x$data,
#'                          clusters = c("ClusterA", "ClusterB"),
#'                          id_column = "geneID",
#'                          query = "ClusterA",
#'                          identity = 0.3
#'                          )
#'
#' @importFrom Biostrings pairwiseAlignment
#' @importFrom dplyr bind_rows group_by slice_max ungroup left_join
#' @importFrom stats setNames
#'
#' @note This function relies on the Biostrings package for sequence alignment
#'   and the dplyr package for data manipulation. Ensure these packages are
#'   installed and loaded into your R session.
#'
#' @noRd
protein_blast <- function(data, clusters, id_column, query, identity) {

  # Check if Biostrings package is installed
  if (!requireNamespace("Biostrings", quietly = TRUE)) {
    stop('Biostrings package is not installed. Please install it using BiocManager::install("Biostrings").')
  }

  # Perform alignment for cluster combinations
  cluster_pairs <- lapply(clusters, function(target) c(query, target))

  protein_combinations_all <- do.call(rbind, lapply(cluster_pairs, function(pair) get_protein_combinations(data, pair)))

  protein_combinations_alignment <- protein_combinations_all[protein_combinations_all$cluster1 != protein_combinations_all$cluster2, ]
  protein_combinations_query <- protein_combinations_all[protein_combinations_all$rowID.x == protein_combinations_all$rowID.y, ]

  alignments <- Biostrings::pairwiseAlignment(pattern = protein_combinations_alignment$translation1,
                                             subject = protein_combinations_alignment$translation2,
                                             scoreOnly = FALSE)

  pattern <- sapply(alignments, function(alignment) as.character(Biostrings::pattern(alignment)))
  subject <- sapply(alignments, function(alignment) as.character(Biostrings::subject(alignment)))

  identity_df <- compute_identity(pattern, subject)

  protein_combinations_alignment$identity <- identity_df$identity
  protein_combinations_alignment$similarity <- identity_df$similarity
  protein_combinations_alignment <- protein_combinations_alignment[protein_combinations_alignment$identity >= identity, ]

  protein_combinations_query$identity <- 100
  protein_combinations_query$similarity <- 100

  protein_combinations_all <- dplyr::bind_rows(protein_combinations_alignment, protein_combinations_query)
  protein_combinations_all <- subset(protein_combinations_all, select = c("rowID.y", "rowID.x", "identity", "similarity"))

  # Filter to keep only the rows with the highest identity for each rowID.y
  protein_combinations_all <- protein_combinations_all %>%
    dplyr::group_by(rowID.y) %>%
    dplyr::slice_max(identity, n = 1) %>%
    dplyr::ungroup()

  # Bind gene Ids
  data_groups <- data[, c("rowID", id_column)]
  names(data_groups) <- c("rowID", "BlastP")

  protein_combinations_all <- dplyr::left_join(protein_combinations_all, data_groups, by = c("rowID.x" = "rowID"))

  # If there is a hit there should be 2 genes in the same group.
  # Therefore set unique groups to NA such that they won't be displayed in the graph
  protein_combinations_all$BlastP[ave(protein_combinations_all$BlastP, protein_combinations_all$BlastP, FUN=length) == 1] <- NA

  # Bind data and put query cluster on top
  data <- dplyr::left_join(data, protein_combinations_all, by = c("rowID" = "rowID.y"))
  data <- data[order(data$cluster != query, data$cluster), ]
  data$rowID <- seq_len(nrow(data))
  data$rowID.x <- NULL

  return(data)
}

#'Perform Gene Cluster BLAST Analysis
#'
#'This function performs a BLAST analysis on gene clusters provided in a GC
#'chart object. It checks for sequence or translation data, performs protein
#'BLASTs, and updates the GC chart with BLAST results including identity,
#'similarity.
#'
#'@param GC_chart A list object representing the GC chart, which must contain
#'  data and configuration for clusters.
#'@param id_column The name of the column that contains the gene identifiers.
#'@param query The name of the query cluster to be used for BLAST comparisons.
#'@param identity Minimum identity threshold for BLAST hits to be considered
#'  significant. Defaults to 0.3.
#'@param lables A named list where names are gene identifiers to be replaced, and
#'  values are the new names. Default is NULL.
#'@param ... Additional arguments passed to the `protein_blast` function.
#'
#'@return Returns the modified GC_chart object with updated BLAST analysis data.
#'@note This function requires the Biostrings package to be installed. It also
#'  updates the GC_chart object in place, adding BlastP results to the chart
#'  configuration.
#' @examples
#' \dontrun{
#' GC_chart(
#'    data,
#'    cluster = "cluster",
#'    group = "protein_id",
#'    height = "900px") %>%
#'    GC_blast(
#'    query = "V792X672X442M18_1",
#'    id_column = "protein_id",
#'    identity = 0.3
#'    ) %>%
#'    GC_labels() %>%
#'    GC_links("BlastP")
#'}
#'
#'@export
GC_blast <- function(
    GC_chart,
    id_column,
    query,
    identity = 0.3,
    labels = NULL,
    ...) {

  data <- GC_chart$x$data

  if (!('sequence' %in% names(data)) && !('translation' %in% names(data))) {
    stop("No 'sequence' or 'translation' column found in chart data.")
    return(NULL)
  }

  if ('sequence' %in% names(data) && !('translation' %in% names(data))) {
    names(data)[names(data) == "sequence"] <- "translation"
  }

  # Check if 'cluster' column exists in the data
  cluster_column <- GC_chart$x$cluster
  if (is.null(cluster_column)) {
    stop("Please define cluster in the GC_chart function.")
    return(NULL)
  }

  # Rename cluster column
  colnames(data)[colnames(data) == cluster_column] <- "cluster"
  clusters <- unique(data$cluster)

  if (!(query %in% data$cluster)) {
    stop("Cluster name not found in cluster columnm.")
    return(NULL)
  }

  data <- protein_blast(data, clusters, id_column, query, identity)

  # Rename 'BlastP' column values based on 'names' argument
  if (!is.null(labels) && "BlastP" %in% colnames(data)) {
    for (label in names(labels)) {
      data$BlastP[data$BlastP == label] <- labels[[label]]
    }
  }

  # Update chart data
  GC_chart$x$data <- data
  GC_chart$x$group <- "BlastP"
  GC_chart$x$legend$group <- "BlastP"

  #Re order series
  GC_chart$x$series <- GC_chart$x$series[unique(data$cluster)]

  for (clust in clusters) {

    subset_data <- data[data[[cluster_column]] == clust, ]

    GC_chart$x$series[[clust]]$data <- subset_data
    GC_chart$x$series[[clust]]$genes <- list(group = "BlastP", show = TRUE)
    if(clust == query){
    GC_chart$x$series[[clust]]$tooltip <-
     list(
      show = TRUE,
      formatter =
        sprintf("<b>{%s}</b><br>
        <b>Location:</b> {start} - {end}<br>",
        id_column
      )
     )
    } else {
      GC_chart$x$series[[clust]]$tooltip <-
        list(
          show = TRUE,
          formatter =
            sprintf("<b>{%s}</b><br>
        <b>BlastP hit with:</b> {BlastP}<br>
        <b>Identity:</b> {identity}%%<br>
        <b>Similarity:</b> {similarity}%%<br>
        <b>Location:</b> {start} - {end}<br>",
                    id_column
            )
        )
    }
  }

  return(GC_chart)

}