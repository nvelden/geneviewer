file_path <- "~/Documents/2023 github/geneviewer/genbank_files/"

gbk <- geneviewer::read_gbk(file_path)


gbk_df <- geneviewer::gbk_features_to_df(gbk, feature = "CDS", keys = c("protein_id", "region", "translation"))

data <- gbk_df[!is.na(gbk_df$protein_id), ]

cluster_combinations <- combn(unique(data$cluster), 2, simplify = FALSE)

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

    identity <- round(matches / sequence_length, 3)
    similarity <- round((matches + similar) / sequence_length, 3)

    return(data.frame(identity = identity, similarity = similarity))
  }, patterns, subjects, SIMPLIFY = FALSE)

  # Combine the results into a data frame
  results_df <- do.call(rbind, results)
  rownames(results_df) <- NULL
  return(results_df)
}

cluster_combinations <- combn(unique(data$cluster), 2, simplify = FALSE)

# Function to get protein combinations with translations for a pair of clusters
get_protein_combinations <- function(cluster_pair) {
  cluster1_data <- subset(data, cluster == cluster_pair[1])
  cluster2_data <- subset(data, cluster == cluster_pair[2])

  protein_combinations <- expand.grid(Query = cluster1_data$protein_id,
                                      Target = cluster2_data$protein_id,
                                      stringsAsFactors = FALSE)

  # Merge translations for Query
  protein_combinations <- merge(protein_combinations, cluster1_data[, c("protein_id", "translation")],
                                by.x = "Query", by.y = "protein_id", all.x = TRUE)

  # Rename the translation column for Query
  names(protein_combinations)[names(protein_combinations) == "translation"] <- "Query_translation"

  # Merge translations for Target
  protein_combinations <- merge(protein_combinations, cluster2_data[, c("protein_id", "translation")],
                                by.x = "Target", by.y = "protein_id", all.x = TRUE)

  # Rename the translation column for Target
  names(protein_combinations)[names(protein_combinations) == "translation"] <- "Target_translation"

  # Add cluster names
  protein_combinations$cluster1 <- cluster_pair[1]
  protein_combinations$cluster2 <- cluster_pair[2]

  return(protein_combinations)
}

profvis::profvis({
# Apply the function to each cluster pair and combine results
protein_combinations <- do.call(rbind, lapply(cluster_combinations, get_protein_combinations))

alignment <- Biostrings::pairwiseAlignment(pattern = protein_combinations$Query_translation,
                                           subject = protein_combinations$Target_translation,
                                           scoreOnly = FALSE)

pattern <- as.character(Biostrings::pattern(alignment))
subject <- as.character(Biostrings::subject(alignment))

identity_df <- compute_identity(pattern, subject)

protein_combinations$identity <- identity_df$identity
protein_combinations$similarity <- identity_df$similarity
})



