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
#' @param output_dir Parent directory for a unique working subdirectory;
#'   defaults to \code{tempdir()}. With \code{remove_files = FALSE}, copied
#'   inputs and alignment files are retained in that subdirectory.
#'
#' @return A data frame combining all alignment results, or NULL if errors occur
#'   during processing.
#'
#' @examples
#' \dontrun{
#' # Requires the external MUMmer programs (nucmer or promer, delta-filter and
#' # show-coords) on the PATH.
#'
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
    output_dir = tempdir()
    ){

  if (!dir.exists(path)) {
    stop("The specified directory does not exist. Please check the file path.")
  }

  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  if (grepl(" ", path)) {
    stop("MUMmer requires a directory path without spaces.")
  }

  if (grepl(" ", output_dir)) {
    stop("MUMmer requires an output directory path without spaces.")
  }

  all_files <- list.files(path, full.names = TRUE, pattern = "\\.gbk$|\\.gb$|\\.fasta$")

  if (length(all_files) == 0) {
    stop("No files found in the specified directory. Check the path or file types.")
  }

  if(is.null(cluster)){
    cluster <- unique(gsub("\\.gbk$|\\.gb$|\\.fasta$", "", basename(all_files)))
  }

  if (length(cluster) < 2) {
    stop("At least two clusters must be provided.")
  }

  if (any(grepl(" ", cluster))) {
    stop("MUMmer requires file names without spaces: ", paste(cluster[grepl(" ", cluster)], collapse = ", "))
  }

  # Every call owns its working directory, including when inputs and outputs
  # share a directory. Never overwrite or clean up pre-existing user files.
  work_dir <- tempfile("geneviewer-mummer-", tmpdir = output_dir)
  if (!dir.create(work_dir)) {
    stop("Could not create the MUMmer working directory.")
  }
  if (remove_files) {
    on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  }
  input_names <- sub("\\.gbk$|\\.gb$|\\.fasta$", "", basename(all_files))
  all_files <- all_files[input_names %in% cluster]
  if (!all(file.copy(all_files, work_dir))) {
    stop("Could not copy sequence files to the MUMmer working directory.")
  }
  all_files <- file.path(work_dir, basename(all_files))
  path <- work_dir

  # Check if FASTA files and matching cluster names (exact file names)
  fasta_files <- all_files[basename(all_files) %in% paste0(cluster, ".fasta")]

  found_fasta_names <- gsub("\\.fasta$", "", basename(fasta_files))
  missing_files <- cluster[!cluster %in% found_fasta_names]

  gbk_files <- NULL
  # Check for GenBank and convert to FASTA if missing
  if(length(missing_files) > 0){
    is_gbk <- grepl("\\.gbk$|\\.gb$", all_files)
    gbk_files <- all_files[is_gbk & sub("\\.gbk$|\\.gb$", "", basename(all_files)) %in% missing_files]

    if (length(gbk_files) > 0) {
      sapply(gbk_files, function(file) {
        genbank_to_fasta(file)
      })
    }

    fasta_files <- file.path(path, paste0(cluster, ".fasta"))
    fasta_files <- fasta_files[file.exists(fasta_files)]
    found_clusters <- gsub("\\.fasta$", "", basename(fasta_files))
    missing_files <- setdiff(cluster, found_clusters)

    if(length(missing_files) > 0){
      stop(paste("Sequence file(s) for:", paste(missing_files, collapse=", "), "missing."))
    }
  }

  cluster_pairs <-
    mapply(c, cluster[-length(cluster)], cluster[-1], SIMPLIFY = FALSE)

  links <- lapply(cluster_pairs, function(pair) {

    reference_seq <- fasta_files[basename(fasta_files) == paste0(pair[1], ".fasta")]
    query_seq <- fasta_files[basename(fasta_files) == paste0(pair[2], ".fasta")]

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

      return(coords)

    }, error = function(e) {
      warning(sprintf("Error in aligning %s vs %s: %s", pair[1], pair[2], e$message))
      return(NULL)
    })
  })

  links <- Filter(Negate(is.null), links)
  links <- do.call(rbind, links)

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
  if (!nzchar(Sys.which(command_type))) {
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
    "%s --prefix=%s %s %s %s",
    command_type,
    shQuote(paste0(prefix, "_", seqtype)),
    mummer_options,
    shQuote(reference),
    shQuote(query))

  if (system(mummer_cmd) != 0) {
    stop(sprintf("Failed to run %s.", command_type))
  }

  # Run delta-filter with the specified alignment type
  delta_file <- sprintf("%s_%s.delta", prefix, seqtype)
  filtered_delta_file <- sprintf("%s_%s_%s.delta", prefix, seqtype, gsub("-", "_", maptype))
  coords_file <- sprintf("%s_%s_%s.coords", prefix, seqtype, gsub("-", "_", maptype))

  delta_filter_cmd <- sprintf(
    "delta-filter %s %s %s > %s",
    maptype_flag,
    filter_options,
    shQuote(delta_file),
    shQuote(filtered_delta_file)
  )

  if (system(delta_filter_cmd) != 0) {
    stop("Failed to run delta-filter.")
  }

  # Extract and format alignment coordinates
  # Add -k flag if sequence type is 'protein'
  show_coords_options <- if (seqtype == "protein") "-H -T -r -k" else "-H -T -r"
  show_coords_cmd <- sprintf(
    "show-coords %s %s > %s",
    show_coords_options,
    shQuote(filtered_delta_file),
    shQuote(coords_file)
  )

  if (system(show_coords_cmd) != 0) {
    stop("Failed to run show-coords.")
  }
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
#' # Convert the GenBank file to FASTA format in a temporary folder
#' genbank_to_fasta(gbk_file, output_dir = tempdir())
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

  # Split the file into records (each record ends with "//")
  record_ends <- grep("^//", lines)
  if (length(grep("^ORIGIN", lines)) == 0) {
    stop("No sequence detected in GenBank file.")
  }
  if (length(record_ends) == 0) {
    stop("The sequence block is not properly formatted or missing.")
  }
  record_starts <- c(1, utils::head(record_ends, -1) + 1)

  fasta_records <- character()
  for (r in seq_along(record_ends)) {
    record <- lines[record_starts[r]:record_ends[r]]

    # Extract the definition and version for the FASTA header
    definition <- record[grep("^DEFINITION", record)]
    definition <- sub("^DEFINITION\\s+", "", definition)
    definition <- sub("[^a-zA-Z0-9]+$", "", definition)
    version <- record[grep("^VERSION", record)]
    version <- sub("^VERSION\\s+", "", version)
    fasta_header <- sprintf(">%s %s", version, definition)

    # Identify the start of the sequence block
    origin_index <- grep("^ORIGIN", record)
    if (length(origin_index) == 0) {
      warning(sprintf("Record %d has no sequence and is skipped.", r))
      next
    }

    # Extract and clean all lines after "ORIGIN"
    sequence_lines <- record[seq_len(length(record) - 1)][-seq_len(origin_index[1])]
    sequence <- paste(sequence_lines, collapse = "")
    sequence <- gsub("\\s+|[0-9]+", "", sequence)
    sequence <- toupper(sequence)

    fasta_records <- c(fasta_records, sprintf("%s\n%s", fasta_header, sequence))
  }

  # Combine the headers and sequences into one FASTA format string
  fasta_content <- paste(fasta_records, collapse = "\n")
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
