library(dplyr)
library(Biostrings)

file_path <- "~/Documents/2023 github/geneviewer/genbank_files/"

gbk <- geneviewer::read_gbk(file_path)


gbk_df <- geneviewer::gbk_features_to_df(gbk, feature = "CDS", keys = c("protein_id", "region", "translation"))

data <- gbk_df[!is.na(gbk_df$protein_id), ]
data <- data %>%
  group_by(cluster) %>%
  arrange(cluster, min(start, end)) %>%
  mutate(gene_nr = row_number()) %>%
  ungroup()

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

# Function to get protein combinations with translations for a pair of clusters
get_protein_combinations <- function(cluster_pair) {
  cluster1_data <- subset(data, cluster == cluster_pair[1])
  cluster2_data <- subset(data, cluster == cluster_pair[2])

  protein_combinations <- expand.grid(protein_id.x= cluster1_data$protein_id,
                                      protein_id.y = cluster2_data$protein_id,
                                      stringsAsFactors = FALSE)

  # Merge translations for Query
  protein_combinations <- merge(protein_combinations, cluster1_data[, c("protein_id", "translation", "gene_nr", "strand", "start", "end")],
                                by.x = "protein_id.x", by.y = "protein_id", all.x = TRUE, suffixes = c("1","2"))

  # Merge translations for Target
  protein_combinations <- merge(protein_combinations, cluster2_data[, c("protein_id", "translation", "gene_nr", "strand", "start", "end")],
                                by.x = "protein_id.y", by.y = "protein_id", all.x = TRUE, suffixes = c("1","2"))

  # Add cluster names
  protein_combinations$cluster1 <- cluster_pair[1]
  protein_combinations$cluster2 <- cluster_pair[2]

  return(protein_combinations)
}

# Apply the function to each cluster pair and combine results
protein_combinations <- do.call(rbind, lapply(cluster_combinations, get_protein_combinations))

alignment <- Biostrings::pairwiseAlignment(pattern = protein_combinations$translation1,
                                           subject = protein_combinations$translation2,
                                           scoreOnly = FALSE)

pattern <- as.character(Biostrings::pattern(alignment))
subject <- as.character(Biostrings::subject(alignment))

identity_df <- compute_identity(pattern, subject)

protein_combinations$identity <- identity_df$identity
protein_combinations$similarity <- identity_df$similarity

protein_combinations <- protein_combinations %>% select(-translation1, -translation2)
protein_combinations <- protein_combinations[protein_combinations$identity > 0.3, ]

GC_chart(data, cluster = "cluster")


###########################################################################################

cluster1 <- "V792X672X442M18_1"
cluster2 <- "V792X672X442M18_2"

cluster1_data <- data[data$cluster == cluster1, ]
cluster2_data <- data[data$cluster == cluster2, ]

protein_combinations <- protein_combinations[protein_combinations$identity > 0.0, ]

selected_cluster_combinations <- protein_combinations[
  protein_combinations$cluster1 == cluster1 &
    protein_combinations$cluster2 == cluster2, ]
selected_cluster_combinations

get_pairs <- function(cluster) {
  cluster <- sort(cluster)
  pairs <- list()
  pair_index <- 1

  for (i in 1:(length(cluster) - 1)) {
    if (cluster[i + 1] == cluster[i] + 1) {
      pairs[[pair_index]] <- c(cluster[i], cluster[i + 1])
      pair_index <- pair_index + 1
    }
  }

  return(pairs)
}


pairs_cluster1 <- get_pairs(selected_cluster_combinations$gene_nr.x)
pairs_cluster1
pairs_cluster2 <- get_pairs(selected_cluster_combinations$gene_nr.y)
pairs_cluster2

homologous_groups <- assign_groups(selected_cluster_combinations)

compare_pairs(pairs_cluster1, pairs_cluster2)


assign_groups <- function(links, threshold = 0.3) {
  groups <- list()
  group_index <- 1

  for (link in 1:nrow(links)) {
    if (links[link, "identity"] < threshold) {
      next
    }

    found <- FALSE
    for (i in seq_along(groups)) {
      if (links[link, "Target"] %in% groups[[i]] || links[link, "Query"] %in% groups[[i]]) {
        found <- TRUE
        if (!(links[link, "Target"] %in% groups[[i]])) {
          groups[[i]] <- c(groups[[i]], links[link, "Target"])
        }
        if (!(links[link, "Query"] %in% groups[[i]])) {
          groups[[i]] <- c(groups[[i]], links[link, "Query"])
        }
        break
      }
    }

    if (!found) {
      groups[[group_index]] <- c(links[link, "Target"], links[link, "Query"])
      group_index <- group_index + 1
    }
  }

  return(groups)
}




homologous_groups <- assign_groups(selected_cluster_combinations)

compare_pairs(pairs_cluster1, pairs_cluster2)

assign_groups <- function(homologous_pairs) {
  groups <- list()
  group_id <- 1

  for (row in 1:nrow(homologous_pairs)) {
    pair <- c(homologous_pairs[row, "gene_nr.x"], homologous_pairs[row, "gene_nr.y"])
    assigned <- FALSE
    for (i in seq_along(groups)) {
      if (pair[[1]] %in% groups[[i]] || pair[[2]] %in% groups[[i]]) {
        groups[[i]] <- unique(c(groups[[i]], pair))
        assigned <- TRUE
        break
      }
    }
    if (!assigned) {
      groups[[group_id]] <- pair
      group_id <- group_id + 1
    }
  }
  return(groups)
}



get_pairs <- function(cluster) {
  cluster <- sort(cluster)
  pairs <- list()
  for (i in 1:(length(cluster) - 1)) {
    pairs[[i]] <- c(cluster[i], cluster[i + 1])
  }
  return(pairs)
}

assign_groups <- function(homologous_pairs) {
  groups <- list()
  group_id <- 1
  for (pair in homologous_pairs) {
    assigned <- FALSE
    for (group in groups) {
      if (pair[[1]] %in% group || pair[[2]] %in% group) {
        group <- c(group, pair[[1]], pair[[2]])
        assigned <- TRUE
        break
      }
    }
    if (!assigned) {
      groups[[group_id]] <- pair
      group_id <- group_id + 1
    }
  }
  return(groups)
}

# Function to compare pairs between clusters
compare_pairs <- function(pairs_one, pairs_two) {
  common <- 0
  for (pair_one in pairs_one) {
    for (pair_two in pairs_two) {
      if (identical(pair_one, pair_two)) {
        common <- common + 1
      }
    }
  }
  return(common)
}

# Function to calculate synteny score
calculate_synteny <- function(cluster_one, cluster_two, homologous_pairs, i = 0.5) {
  groups_one <- assign_groups(cluster_one, homologous_pairs)
  groups_two <- assign_groups(cluster_two, homologous_pairs)

  pairs_one <- get_pairs(groups_one)
  pairs_two <- get_pairs(groups_two)

  contiguity <- compare_pairs(pairs_one, pairs_two)
  homology <- length(homologous_pairs) # Simplified homology calculation

  synteny_score <- homology + i * contiguity
  return(synteny_score)
}


cluster_combinations <- combn(unique(data$cluster), 2, simplify = FALSE)

for(i in 1:length(cluster_combinations)){
  cluster1 <- cluster_combinations[[i]][1]
  cluster2 <- cluster_combinations[[i]][2]

  cluster1_data <- data[data$cluster == cluster1, ]
  cluster2_data <- data[data$cluster == cluster2, ]

  #homologous_pairs <- protein_combinations[protein_combinations$cluster1 == cluster1 & cluster2 == cluster2, ]

  pairs1 <- get_pairs(cluster1_data)
  pairs2 <- get_pairs(cluster2_data)


}
cluster1 <- data[data$cluster == cluster]
cluster2 <-

chart <- GC_chart(data, cluster = "cluster")

links <- get_links(chart, group = "protein_id")



assign_groups_from_df <- function(homologous_pairs_df) {
  groups <- list()
  group_id <- 1

  for (row in 1:nrow(homologous_pairs_df)) {
    pair <- c(homologous_pairs_df[row, "cluster1"], homologous_pairs_df[row, "cluster2"])
    assigned <- FALSE
    for (i in seq_along(groups)) {
      if (pair[[1]] %in% groups[[i]] || pair[[2]] %in% groups[[i]]) {
        groups[[i]] <- unique(c(groups[[i]], pair))
        assigned <- TRUE
        break
      }
    }
    if (!assigned) {
      groups[[group_id]] <- pair
      group_id <- group_id + 1
    }
  }
  return(groups)
}



get_pairs(protein_combinations$Target[1:10])


# Define the gene clusters
cluster1 <- c("geneA", "geneB", "geneC", "geneD")
cluster2 <- c("geneW", "geneX", "geneY", "geneZ")

# Define homologous gene pairs
homologous_pairs <- list(c("geneA", "geneW"),  c("geneC", "geneY"), c("geneD", "geneZ"))

# Function to find contiguous homologous gene pairs
find_contiguous_homologues <- function(cluster1, cluster2, homologous_pairs) {
  contiguous_pairs <- list()

  for (i in 1:(length(cluster1) - 1)) {
    for (pair in homologous_pairs) {
      if (cluster1[i] == pair[1] && cluster1[i + 1] == homologous_pairs[[which(sapply(homologous_pairs, function(x) x[1] == cluster1[i + 1]))]][1]) {
        corresponding_index_2 <- which(cluster2 == pair[2])
        if (corresponding_index_2 < length(cluster2) && cluster2[corresponding_index_2 + 1] == homologous_pairs[[which(sapply(homologous_pairs, function(x) x[1] == cluster1[i + 1]))]][2]) {
          contiguous_pairs <- c(contiguous_pairs, list(c(pair, homologous_pairs[[which(sapply(homologous_pairs, function(x) x[1] == cluster1[i + 1]))]])))
        }
      }
    }
  }

  return(contiguous_pairs)
}

# Identify contiguous homologous gene pairs
contiguous_homologues <- find_contiguous_homologues(cluster1, cluster2, homologous_pairs)

# Print the results
print(contiguous_homologues)



# Define homologous pairs (P->Q mapping)
homologous_pairs <- list(P1 = "Q3", P2 = "Q4", P3 = "Q1", P4 = "Q2")

# Define a function to get contiguous pairs
get_contiguous_pairs <- function(cluster) {
  browser()
  pairs <- vector("list", length(cluster) - 1)
  for (i in 1:(length(cluster) - 1)) {
    pairs[[i]] <- paste(cluster[i], cluster[i+1], sep=", ")
  }
  return(pairs)
}

# Define the clusters
cluster1 <- c("P1", "P2", "P3", "P4")
cluster2 <- c("Q1", "Q2", "Q3", "Q4")

# Get contiguous pairs
contiguous_pairs_cluster1 <- get_contiguous_pairs(cluster1)
contiguous_pairs_cluster2 <- get_contiguous_pairs(cluster2)

# Map Cluster1 pairs to Cluster2 homologs
mapped_pairs_cluster1_to_2 <- sapply(contiguous_pairs_cluster1, function(pair) {
  p_split <- strsplit(pair, ", ")[[1]]
  paste(homologous_pairs[[p_split[1]]], homologous_pairs[[p_split[2]]], sep=", ")
})

# Determine contiguity count
contiguity_count <- sum(mapped_pairs_cluster1_to_2 %in% contiguous_pairs_cluster2)
print(paste("Contiguity count:", contiguity_count))



# Assuming your dataframe is named df and has the columns as described
df$group.x <- NA  # Initialize group column for protein.x (query)
df$group.y <- NA  # Initialize group column for protein.y (target)

# Function to assign groups based on identity threshold
assign_groups <- function(df, threshold = 0.0) {
  groups <- list()
  group_mapping <- list()

  for (i in 1:nrow(df)) {
    if (df$identity[i] < threshold) next

    query <- df$protein_id.x[i]
    target <- df$protein_id.y[i]

    query_in_group <- FALSE
    target_in_group <- FALSE
    group_index <- 0

    # Check if query or target is already in a group
    for (group_id in names(group_mapping)) {
      if (query %in% group_mapping[[group_id]] || target %in% group_mapping[[group_id]]) {
        group_index <- as.integer(group_id)
        query_in_group <- TRUE
        target_in_group <- TRUE
        break
      }
    }

    # Assign or create group as necessary
    if (!query_in_group || !target_in_group) {
      if (group_index == 0) {
        group_index <- length(groups) + 1
        groups[[group_index]] <- c(query, target)
        group_mapping[[as.character(group_index)]] <- c(query, target)
      } else {
        groups[[group_index]] <- unique(c(groups[[group_index]], query, target))
        group_mapping[[as.character(group_index)]] <- unique(c(group_mapping[[as.character(group_index)]], query, target))
      }
    }

    # Update the dataframe with the group index
    df$group.x[i] <- group_index
    df$group.y[i] <- group_index
  }

  return(df)
}

# Apply the function to your data frame
df <- assign_groups(selected_cluster_combinations) %>% select(protein_id.y, protein_id.x, group.x, group.y)

# Inspect the modified data frame
print(df)


