#' Perform Sequence Alignment Using MUMmer
#'
#' This function orchestrates the alignment of sequences in a specified
#' directory using MUMmer, a tool for aligning large DNA or protein sequences.
#' It can handle GenBank and FASTA file formats. and performs checks to ensure
#' necessary files are present.
#'
#' @param path The directory containing the sequence files.
#' @param cluster Optional vector of cluster names to consider for alignment. If
#'   NULL, clusters are inferred from file names. The order of names determines
#'   the alignment sequence.
#' @param maptype The type of mapping to perform; "many-to-many" or
#'   "one-to-one". "many-to-many" allows for multiple matches between clusters,
#'   "one-to-one" restricts alignments to unique matches between a pair.
#' @param seqtype The type of sequences, either "protein" or "nucleotide".
#' @param mummer_options Additional command line options for MUMmer. To see all
#'   available options, you can run `nucmer --help` or `promer --help` in the
#'   terminal depending on whether you are aligning nucleotide or protein
#'   sequences.
#' @param filter_options Additional options for filtering MUMmer results. To
#'   view all filtering options, run `delta-filter --help` in the terminal.
#' @param remove_files Logical indicating whether to remove intermediate files
#'   generated during the process, defaults to TRUE.
#' @param output_dir Optional directory to save output files; defaults to NULL,
#'                   which uses the input file directory for outputs.
#'
#' @return A data frame combining all alignment results, or NULL if errors occur
#'   during processing.
#'
#' @examples
#' \dontrun{
#' # Basic alignment with default options
#' mummer_alignment(
#'   path = "/path/to/sequences",
#'   maptype = "many-to-many",
#'   seqtype = "protein"
#' )
#'
#' # Alignment with specific MUMmer options
#' mummer_alignment(
#'   path = "/path/to/sequences",
#'   maptype = "one-to-one",
#'   seqtype = "protein",
#'   mummer_options = "--maxgap=500 --mincluster=100",
#'   filter_options = "-i 90"
#' )
#' }
#' @references Kurtz S, Phillippy A, Delcher AL, Smoot M, Shumway M, Antonescu
#' C, Salzberg SL (2004). Versatile and open software for comparing large
#' genomes. Genome Biology, 5(R12).
#'
#'
#' @export
mummer_alignment <- function(
    path,
    cluster = NULL,
    maptype = "many-to-many",
    seqtype = "protein",
    mummer_options = "",
    filter_options = "",
    remove_files = TRUE,
    output_dir = NULL
    ){

  if (!dir.exists(path)) {
    stop("The specified directory does not exist. Please check the file path.")
  }

  # Check if the file_path contains spaces
  if (grepl(" ", path)) {
    stop("MUMmer requires a directory path without spaces.")
  }

  if (!is.null(output_dir) && !dir.exists(output_dir)) {
    stop("The specified output directory does not exist. Please check the file path.")
  }

  # Check if the file_path contains spaces
  if (!is.null(output_dir) &&  grepl(" ", output_dir)) {
    stop("MUMmer requires a output directory path without spaces.")
  }

  all_files <- list.files(path, full.names = TRUE, pattern = "\\.gbk$|\\.gb$|\\.fasta$")

  # Move all files to the output dir if specified
  if(!is.null(output_dir)){
    sapply(all_files, function(x) file.copy(x, output_dir, overwrite = TRUE))
    path <- output_dir
    all_files <- list.files(path, full.names = TRUE, pattern = "\\.gbk$|\\.gb$|\\.fasta$")
  }

  if (length(all_files) == 0) {
    stop("No files found in the specified directory. Check the path or file types.")
  }

  if(is.null(cluster)){
    cluster <- unique(gsub("\\.gbk$|\\.gb$|\\.fasta$", "", basename(all_files)))
  }

  if (length(cluster) < 2) {
    stop("At least two clusters must be provided.")
  }

  # Check if FASTA files and matching cluster names
  cluster_pattern <- paste0("^(", paste(cluster, collapse = "|"), ")\\.fasta$")
  fasta_files <- all_files[grep(cluster_pattern, basename(all_files))]

  found_fasta_names <- gsub("\\.fasta$", "", basename(fasta_files))
  missing_files <- cluster[!cluster %in% found_fasta_names]

  gbk_files <- NULL
  # Check for GenBank and convert to FASTA if missing
  if(length(missing_files) > 0){
    cluster_pattern <- paste0("^(", paste(missing_files, collapse = "|"), ")(\\.gbk|\\.gb)$")
    gbk_files <- all_files[grep(cluster_pattern, basename(all_files))]

    if (length(gbk_files) > 0) {
      sapply(gbk_files, function(file) {
        genbank_to_fasta(file)
      })
      fasta_files <- all_files[grep(cluster_pattern, basename(all_files))]
    }

    pattern <- paste0("^(", paste(cluster, collapse = "|"), ")\\.fasta$")
    fasta_files <- list.files(path, pattern = pattern, full.names = TRUE)
    found_clusters <- gsub("\\.fasta$", "", basename(fasta_files))
    missing_files <- setdiff(cluster, found_clusters)

    if(length(missing_files) > 0){
      stop(paste("Sequence file(s) for:", paste(missing_files, collapse=", "), "missing."))
    }
  }

  cluster_pairs <-
    mapply(c, cluster[-length(cluster)], cluster[-1], SIMPLIFY = FALSE)

  links <- lapply(cluster_pairs, function(pair) {

    pattern_reference <- paste0(pair[1], "\\.fasta$")
    pattern_query <- paste0(pair[2], "\\.fasta$")
    reference_seq <- fasta_files[grep(pattern_reference, fasta_files)]
    query_seq <- fasta_files[grep(pattern_query, fasta_files)]

    tryCatch({
      # Call the mummer_alignment function
      mummer(
        reference = reference_seq,
        query = query_seq,
        maptype = maptype,
        seqtype = seqtype,
        mummer_options = mummer_options,
        filter_options = filter_options
        )

      coords_file <- sprintf("%s/%s_%s_%s_%s.coords",
                             path,
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
        file.remove(list.files(path=path, pattern=sprintf("%s_%s", pair[1], pair[2]), full.names=TRUE))
      }

      return(coords)

    }, error = function(e) {
      warning(sprintf("Error in aligning %s vs %s: %s", pair[1], pair[2], e$message))
      return(NULL)
    })
  })

  links <- Filter(Negate(is.null), links)
  links <- do.call(rbind, links)

  if (!is.null(gbk_files) && length(gbk_files) > 0 && remove_files) {
      remove_files <- sub("\\.gbk$|\\.gb$", ".fasta", gbk_files)
      file.remove(remove_files)
  }

  return(links)
}

#' @noRd
mummer <- function(reference, query, maptype = "many-to-many", seqtype = "protein", mummer_options = "", filter_options = "", output_dir = NULL) {

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
  if(!is.null(output_dir)){
    query_dir <- output_dir
  } else {
    query_dir <- dirname(query)
  }

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

#' @noRd
parse_nucmer <- function(path, reference, query){

  col_names <- c("start1", "end1", "start2", "end2", "length1", "length2", "identity", "tag1", "tag2")

  if(file.exists(path)){

    lines <- readLines(path)

    # Check if lines is empty
    if(length(lines) == 0){
      # Return an empty data frame with the specified column names
      data <- data.frame(matrix(ncol = length(col_names), nrow = 0))
      colnames(data) <- col_names
    } else {
      # Proceed with reading the table if lines is not empty
      data <- read.table(
        text = lines,
        header = FALSE,
        sep = "\t",
        fill = TRUE,
        col.names = col_names
      )

      data$cluster1 <- reference
      data$cluster2 <- query
    }

    } else {
      stop("The specified path does not exist.")
    }
  return(data)
}

#' @noRd
parse_procmer <- function(path, reference, query){

  col_names <- c("start1", "end1", "start2", "end2", "length1", "length2", "identity",
                 "similarity", "stop", "frame1", "frame2", "tag1", "tag2")

  if(file.exists(path)){

    lines <- readLines(path)
    # Check if lines is empty
    if(length(lines) == 0){
      # Return an empty data frame with the specified column names
      data <- data.frame(matrix(ncol = length(col_names), nrow = 0))
      colnames(data) <- col_names
    } else {
      # Proceed with reading the table if lines is not empty
      data <- read.table(
        text = lines,
        header = FALSE,
        sep = "\t",
        fill = TRUE,
        col.names = col_names
      )

      data$cluster1 <- reference
      data$cluster2 <- query
    }

  } else {
    stop("The specified path does not exist.")
  }
  return(data)
}

#' Convert GenBank to FASTA Format
#'
#' This function reads a GenBank file, extracts the sequence and
#' writes it to a new file in FASTA format. It parses the DEFINITION and VERSION
#' for the header and sequences from the ORIGIN section.
#'
#' @param path Path to the GenBank file.
#' @param output_dir Optional path for the output FASTA file. If NULL, the
#'   output is saved in the same directory as the input file with the same base
#'   name but with a .fasta extension.
#'
#' @return None explicitly, but writes the FASTA formatted data to a file.
#'
#' @examples
#' \donttest{
#' # Path to the example GenBank file in the package
#' gbk_file <- system.file("extdata", "BGC0000001.gbk", package = "geneviewer")
#'
#' # Convert the GenBank file to FASTA format
#' genbank_to_fasta(gbk_file)
#' }
#'
#'
#' @importFrom tools file_path_sans_ext
#'
#' @export
genbank_to_fasta <- function(path, output_dir=NULL){

  # Check if the file exists
  if (!file.exists(path)) {
    stop("File does not exist")
  }

  # Read all lines from the file
  lines <- readLines(path)

  # Extract the definition and version for the FASTA header
  definition <- lines[grep("^DEFINITION", lines)]
  definition <- sub("^DEFINITION\\s+", "", definition)
  definition <- sub("[^a-zA-Z0-9]+$", "", definition)
  version <- lines[grep("^VERSION", lines)]
  version <- sub("^VERSION\\s+", "", version)
  fasta_header <- sprintf(">%s %s", version, definition)

  # Identify the start of the sequence block
  origin_index <- grep("^ORIGIN", lines)
  if (length(origin_index) == 0) {
    stop("No sequence detected in GenBank file.")
  }
  end_index <- grep("^//", lines)

  # Extract and clean all lines after "ORIGIN"
  if (length(origin_index) > 0 && length(end_index) > 0 && origin_index < end_index) {
    sequence_lines <- lines[(origin_index + 1):(end_index - 1)]
    sequence <- paste(sequence_lines, collapse = "")
    sequence <- gsub("\\s+|[0-9]+", "", sequence)
    sequence <- toupper(sequence)
  } else {
    stop("The sequence block is not properly formatted or missing.")
  }

  # Combine the header and sequence into one FASTA format string
  fasta_content <- sprintf("%s\n%s", fasta_header, sequence)
  gbk_names <- tools::file_path_sans_ext(basename(path))

  if (is.null(output_dir)) {
    file_dir <- dirname(path)
    fasta_file_path <- file.path(file_dir, sprintf("%s.fasta", gbk_names))
  } else {
    fasta_file_path <- file.path(output_dir, sprintf("%s.fasta", gbk_names))
  }

  # Write the FASTA content to the specified file path
  writeLines(fasta_content, fasta_file_path)

}
