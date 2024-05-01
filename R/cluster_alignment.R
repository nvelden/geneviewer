GC_mummer <- function(
    file_path,
    cluster = c("MT939486", "MT939487","MT939488", "LT960552"),
    maptype = "many-to-many",
    seqtype = "protein",
    mummer_options = "",
    filter_options = "",
    remove_files = TRUE
    ){

  # Check if the file_path contains spaces
  if (grepl(" ", file_path)) {
    stop("Error: MUMmer requires a file path without spaces.")
  }

  all_files <- list.files(file_path, full.names = TRUE, pattern = "\\.gbk$|\\.gb$|\\.fasta$")

  cluster_pairs <-
    mapply(c, cluster[-length(cluster)], cluster[-1], SIMPLIFY = FALSE)

  links <- lapply(cluster_pairs, function(pair) {

    pattern_reference <- paste0("^", pair[1], "\\.")
    pattern_query <- paste0("^", pair[2], "\\.")
    reference_seq <- all_files[grep(pattern_reference, basename(all_files))]
    query_seq <- all_files[grep(pattern_query, basename(all_files))]

    # Error handling for reference and query file detection
    if (length(reference_seq) != 1 || length(query_seq) != 1) {
      return(sprintf("Error: Expected exactly one matching file for each of %s and %s, found %d and %d respectively.",
                     pair[1],
                     pair[2],
                     length(reference_seq),
                     length(query_seq)
                     )
             )
    }

    tryCatch({
      # Call the mummer_alignment function
      mummer_alignment(
        reference = reference_seq,
        query = query_seq,
        maptype = maptype,
        seqtype = seqtype,
        mummer_options = mummer_options,
        filter_options = filter_options
        )

      coords_file <- sprintf("%s/%s_%s_%s_%s.coords",
                             file_path,
                             pair[1],
                             pair[2],
                             seqtype,
                             gsub("-","_", maptype)
      )

      if(seqtype == "nucleotide"){
       coords <- parse_nucmer(coords_file, pair[1], pair[2])
      } else if(seqtype =="protein"){
        coords <- parse_procmer(coords_file, pair[1], pair[2])
      }

      if (remove_files) {
        file.remove(list.files(path=file_path, pattern=sprintf("%s_%s", pair[1], pair[2]), full.names=TRUE))
      }

      return(coords)

    }, error = function(e) {
      cat(sprintf("Error in aligning %s vs %s: %s\n", pair[1], pair[2], e$message))
    })
  })

  links <- Filter(Negate(is.null), links)
  links <- do.call(rbind, links)
  return(links)
}

mummer_alignment <- function(reference, query, maptype = "many-to-many", seqtype = "protein", mummer_options = "", filter_options = "") {

  # Validate the sequence type
  if (!seqtype %in% c("protein", "nucleotide")) {
    stop("Invalid sequence type. Please choose either 'protein' or 'nucleotide'.")
  }

  # Determine the appropriate command based on the sequence type
  command_type <- ifelse(seqtype == "protein", "promer", "nucmer")

  # Check if MUMmer (either nucmer or promer) is installed
  if (system(paste("command -v", command_type), ignore.stdout = TRUE, ignore.stderr = TRUE) != 0) {
    stop(paste(command_type, "is not installed or not in the PATH."))
  }

  # Validate mapping type
  if (!maptype %in% c("many-to-many", "one-to-one")) {
    stop("Invalid mapping type. Please choose either 'many-to-many' or 'one-to-one'.")
  }
  maptype_flag <- ifelse(maptype == "one-to-one", "-1", "-m")

  # Check if files exist
  if (!file.exists(query)) {
    stop("Query file does not exist: ", query)
  }
  if (!file.exists(reference)) {
    stop("Reference file does not exist: ", reference)
  }

  # Get the directory of the query file
  query_dir <- dirname(query)

  # Set prefix for output files to be in the same directory as the query file
  prefix <- file.path(query_dir, paste0(
    tools::file_path_sans_ext(basename(reference)),
    "_",
    tools::file_path_sans_ext(basename(query))
  ))

  # Construct and run the MUMmer command
  mummer_cmd <- sprintf(
    "%s --prefix=%s_%s %s %s %s",
    command_type,
    prefix,
    seqtype,
    mummer_options,
    reference,
    query)

  if (system(mummer_cmd) != 0) {
    stop(sprintf("Failed to run %s.", command_type))
  }

  # Run delta-filter with the specified alignment type
  delta_filter_cmd <- sprintf(
    "delta-filter %s %s %s_%s.delta > %s_%s_%s.delta",
    maptype_flag,
    filter_options,
    prefix,
    seqtype,
    prefix,
    seqtype,
    gsub("-", "_", maptype)
  )

  if (system(delta_filter_cmd) != 0) {
    stop("Failed to run delta-filter.")
  }

  # Extract and format alignment coordinates
  # Add -k flag if sequence type is 'protein'
  show_coords_options <- if (seqtype == "protein") "-H -T -r -k" else "-H -T -r"
  show_coords_cmd <- sprintf(
    "show-coords %s %s_%s_%s.delta > %s_%s_%s.coords",
    show_coords_options,
    prefix,
    seqtype,
    gsub("-", "_", maptype),
    prefix,
    seqtype,
    gsub("-", "_", maptype)
  )

  if (system(show_coords_cmd) != 0) {
    stop("Failed to run show-coords.")
  }
  cat("Alignment process completed successfully.\n")
}

parse_nucmer <- function(path, reference, query){

  col_names <- c("start1", "end1", "start2", "end2", "length1", "length2", "identity", "tag1", "tag2")

  if(file.exists(path)){
    lines <- readLines(path)
    data <- read.table(
      text = lines,
      header = FALSE,
      sep = "\t",
      fill = TRUE,
      col.names = col_names
    )

    data$cluster1 <- reference
    data$cluster2 <- query

    } else {
      stop("The specified path does not exist.")
    }
  return(data)
}

parse_procmer <- function(path, reference, query){

  col_names <- c("start1", "end1", "start2", "end2", "length1", "length2", "identity",
                 "similarity", "stop", "frame1", "frame2", "tag1", "tag2")

  if(file.exists(path)){
    lines <- readLines(path)
    data <- read.table(
      text = lines,
      header = FALSE,
      sep = "\t",
      fill = TRUE,
      col.names = col_names
    )

    data$cluster1 <- reference
    data$cluster2 <- query

  } else {
    stop("The specified path does not exist.")
  }
  return(data)
}
