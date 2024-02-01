file_path <- "~/Documents/2023 github/geneviewer/genbank_files/"
section_names <- c("locus", "definition", "accession", "version", "keywords", "source", "organism", "comment", "feature", "origin")
lines <- readLines(file_path)

gbk <- geneviewer::read_gbk(file_path)

data <- geneviewer::gbk_features_to_df(gbk, feature = "CDS", keys = c("protein_id", "region", "translation"))

data <- gbk_df[!is.na(gbk_df$protein_id), ]

cluster_combinations <- combn(unique(data$cluster), 2, simplify = FALSE)

compute_identity <- function(pattern, subject) {
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

  if (nchar(pattern) != nchar(subject)) {
    stop("The two sequences must be of the same length")
  }

  length <- nchar(pattern)
  matches <- 0
  similar <- 0

  for (i in 1:length) {
    acid1 <- substr(pattern, i, i)
    acid2 <- substr(subject, i, i)

    if (acid1 == acid2) {
      if (acid1 != "-" && acid1 != ".") {
        matches <- matches + 1
      } else {
        length <- length - 1
      }
    } else {
      is_similar <- any(sapply(similar_acids, function(group) acid1 %in% group && acid2 %in% group))
      if (is_similar) {
        similar <- similar + 1
      }
    }
  }

  identity <- round(matches / length, 3)
  similarity <- round((matches + similar) / length, 3)

  return(c(identity, similarity))
}

# Pre-allocate a large enough dataframe
max_combinations <- sum(sapply(cluster_combinations, function(x) nrow(data[data$cluster == x[1],]) * nrow(data[data$cluster == x[2],])))
results_df <- data.frame(Query = character(max_combinations),
                         Target = character(max_combinations),
                         Identity = numeric(max_combinations),
                         Similarity = numeric(max_combinations),
                         Cluster1 = character(max_combinations),
                         Cluster2 = character(max_combinations),
                         stringsAsFactors = FALSE)

# Define a function to process each pair
process_pair <- function(pair, data) {
  cluster1_data <- data[data$cluster == pair[1], ]
  cluster2_data <- data[data$cluster == pair[2], ]

  protein_combinations <- expand.grid(Query = cluster1_data$protein_id,
                                      Target = cluster2_data$protein_id,
                                      stringsAsFactors = FALSE)

  results_list <- list()

  for (i in 1:nrow(protein_combinations)) {
    query_protein <- protein_combinations$Query[i]
    target_protein <- protein_combinations$Target[i]

    query_translation <- cluster1_data$translation[cluster1_data$protein_id == query_protein]
    target_translation <- cluster2_data$translation[cluster2_data$protein_id == target_protein]

    if (length(query_translation) > 0 && length(target_translation) > 0) {
      alignment <- Biostrings::pairwiseAlignment(pattern = query_translation,
                                                 subject = target_translation,
                                                 scoreOnly = FALSE)
      pattern <- as.character(Biostrings::pattern(alignment))
      subject <- as.character(Biostrings::subject(alignment))

      identity_scores <- compute_identity(pattern, subject)

      results_list[[i]] <- c(query_protein, target_protein, identity_scores[1],
                             identity_scores[2], pair[1], pair[2])
    }
  }

  # Convert results_list to a data frame with stringsAsFactors = FALSE
  results_df <- do.call(rbind, results_list)
  colnames(results_df) <- c("Query", "Target", "Identity", "Similarity", "Cluster1", "Cluster2")
  return(data.frame(results_df, stringsAsFactors = FALSE))
}

# Detect the number of cores
no_cores <- detectCores() - 1

# Use parallel processing
results <- mclapply(cluster_combinations, process_pair, data, mc.cores = no_cores)

# Combine the results into a data frame
final_results <- do.call(rbind, results)
final_results <- data.frame(final_results, stringsAsFactors = FALSE)
colnames(final_results) <- c("Query", "Target", "Identity", "Similarity", "Cluster1", "Cluster2")

# View the first few rows
head(final_results)
