#' Read clinker results
#'
#' Reads the results of [clinker](https://github.com/gamcil/clinker) into a
#' data frame of genes and a data frame of links, ready for [GC_chart()] and
#' [GC_links()]. Two files are read: the session file written by
#' \code{clinker -s session.json}, and the file saved with the "Save data"
#' button of the clinker plot in the browser. The second keeps the changes
#' made in the browser: the order of the clusters, flipped loci, trimmed loci,
#' renamed, recoloured and hidden groups.
#'
#' Clusters can consist of several loci, e.g. when a GenBank file holds the
#' cluster on more than one contig. The loci of a cluster are placed one after
#' the other, in their order in the file, with \code{locus_gap} base pairs
#' between them, and \code{start} and \code{end} are positions on this track.
#' The positions on the original record are kept in \code{record_start},
#' \code{record_end} and \code{record_strand}.
#'
#' Clinker draws links only between clusters next to each other, only from
#' an identity of \code{threshold} on and not to hidden genes. The links are
#' returned the same way, unless \code{all_links = TRUE}. The horizontal
#' offsets of clusters after anchoring on a gene in the browser are not saved
#' by clinker; use [GC_align()] to align the clusters on a gene again.
#'
#' @param path Character, the path to a clinker JSON file: a session file
#'   (\code{-s}) or a file saved with "Save data" in the clinker plot.
#' @param trim Logical, whether to show each locus only from its first to its
#'   last gene in a group (a gene with homologues in other clusters), as when
#'   trimming loci in the clinker plot. Loci without such genes are left out.
#'   The genes outside are hidden. Useful when a cluster is split over several
#'   contigs. Default is FALSE.
#' @param keep_hidden Logical, whether to keep genes that are hidden in the
#'   clinker plot (in a hidden group, or outside a trimmed locus). They are
#'   marked in the \code{hidden} column. Default is FALSE.
#' @param all_links Logical, whether to return the links between all pairs of
#'   clusters instead of only between clusters next to each other. Default is
#'   FALSE.
#' @param locus_gap Numeric or NULL, the base pairs between the loci of a
#'   cluster. NULL (default) uses the spacing of the clinker plot.
#' @param threshold Numeric or NULL, the lowest identity (percentage) of the
#'   links to keep. NULL (default) uses the threshold of the clinker plot, or 0.
#' @param sequences Logical, whether to add the nucleotide and protein
#'   sequences of the genes (columns \code{sequence} and \code{translation}).
#'   Default is FALSE.
#'
#' @return A list with three data frames:
#' \describe{
#'   \item{genes}{One row per gene: \code{cluster}, \code{locus},
#'     \code{start}, \code{end}, \code{strand}, \code{label} (the name shown
#'     by clinker), \code{group} (the clinker group), \code{colour} (the colour
#'     of the gene in clinker), \code{hidden}, \code{flipped},
#'     \code{record_start}, \code{record_end}, \code{record_strand},
#'     \code{uid}, followed by the qualifiers of the gene (e.g.
#'     \code{locus_tag}, \code{protein_id}, \code{product}).}
#'   \item{links}{One row per link: \code{cluster1}, \code{start1},
#'     \code{end1}, \code{cluster2}, \code{start2}, \code{end2},
#'     \code{identity} and \code{similarity} (percentages), \code{label1},
#'     \code{label2}, \code{group}, \code{uid1} and \code{uid2}.}
#'   \item{groups}{One row per group: \code{group}, \code{colour},
#'     \code{hidden} and \code{genes} (the number of genes).}
#' }
#'
#' @examples
#' session <- tempfile(fileext = ".json")
#' gene <- function(uid, label, start, end, strand) {
#'   list(uid = uid, label = label, names = list(locus_tag = label),
#'        start = start, end = end, strand = strand)
#' }
#' jsonlite::write_json(list(
#'   order = list("c1", "c2"),
#'   clusters = list(
#'     c1 = list(uid = "c1", name = "Cluster 1", loci = list("l1")),
#'     c2 = list(uid = "c2", name = "Cluster 2", loci = list("l2"))
#'   ),
#'   loci = list(
#'     l1 = list(uid = "l1", name = "contig_1", start = 0, end = 3000,
#'               genes = list("g1", "g2")),
#'     l2 = list(uid = "l2", name = "contig_2", start = 0, end = 3200,
#'               genes = list("g3", "g4"))
#'   ),
#'   genes = list(
#'     g1 = gene("g1", "A1", 100, 1200, 1), g2 = gene("g2", "A2", 1500, 2800, -1),
#'     g3 = gene("g3", "B1", 200, 1400, 1), g4 = gene("g4", "B2", 1700, 3100, -1)
#'   ),
#'   groups = list(
#'     list(uid = "grp1", label = "Synthase", genes = list("g1", "g3")),
#'     list(uid = "grp2", label = "Oxidase", genes = list("g2", "g4"))
#'   ),
#'   links = list(
#'     k1 = list(uid = "k1", query = list(uid = "g1"), target = list(uid = "g3"),
#'               identity = 0.82, similarity = 0.9),
#'     k2 = list(uid = "k2", query = list(uid = "g2"), target = list(uid = "g4"),
#'               identity = 0.64, similarity = 0.75)
#'   )
#' ), session, auto_unbox = TRUE)
#'
#' clinker <- read_clinker(session)
#' clinker$genes
#' clinker$links
#'
#' GC_chart(clinker$genes, cluster = "cluster", group = "group",
#'          strand = "strand", height = "200px") %>%
#'   GC_color(customColors = as.list(stats::setNames(clinker$groups$colour,
#'                                                   clinker$groups$group))) %>%
#'   GC_links(data = clinker$links) %>%
#'   GC_clusterLabel()
#'
#' unlink(session)
#' @importFrom dplyr bind_rows
#' @export
read_clinker <- function(path, trim = FALSE, keep_hidden = FALSE,
                         all_links = FALSE, locus_gap = NULL, threshold = NULL,
                         sequences = FALSE) {
  if (!is.character(path) || length(path) != 1 || !file.exists(path)) {
    stop("'path' must be the path to a clinker JSON file.", call. = FALSE)
  }
  json <- jsonlite::fromJSON(path, simplifyVector = FALSE)

  clinker <- if (!is.null(json$data) && is.list(json$data$clusters)) {
    clinker_from_plot(json)
  } else if (is.list(json$clusters) && is.list(json$genes)) {
    clinker_from_session(json)
  } else {
    stop("'", basename(path), "' is not a clinker session file or a file ",
         "saved with \"Save data\" in a clinker plot.", call. = FALSE)
  }

  if (is.null(locus_gap)) locus_gap <- clinker$locus_gap
  if (is.null(threshold)) threshold <- clinker$threshold

  groups <- clinker_groups(clinker$groups)
  genes <- clinker_genes(clinker$clusters, groups, locus_gap, sequences, trim)
  links <- clinker_links(clinker$links, genes, all_links, threshold)

  if (!keep_hidden) {
    hidden <- genes$uid[genes$hidden]
    genes <- genes[!genes$hidden, , drop = FALSE]
    links <- links[!(links$uid1 %in% hidden | links$uid2 %in% hidden), , drop = FALSE]
  }
  rownames(genes) <- NULL
  rownames(links) <- NULL
  attr(groups, "gene_uids") <- NULL

  list(genes = genes, links = links, groups = groups)
}

# Value of x, or y when x is NULL
null_default <- function(x, y) if (is.null(x)) y else x

# Clusters, groups and links of a file saved with "Save data" in the clinker
# plot. Gene and locus positions are relative to the locus: _start, _end and
# _strand of a gene are its position after flipping, start and end where it
# is drawn (the same unless genes are drawn unscaled), and _start and _end of
# a locus are the part shown after trimming.
clinker_from_plot <- function(json) {
  config <- null_default(json$config, list())
  clusters <- lapply(json$data$clusters, function(cl) {
    list(name = cl$name, loci = lapply(cl$loci, function(lc) {
      length <- lc$end - lc$start
      list(
        name = lc$name,
        length = length,
        record_offset = null_default(lc$`_bio_start`, lc$start),
        flipped = isTRUE(lc$`_flipped`),
        from = null_default(lc$`_start`, 0),
        to = null_default(lc$`_end`, length),
        genes = lapply(lc$genes, function(g) {
          list(
            uid = g$uid, label = g$label, names = g$names,
            colour = g$colour,
            start = g$start, end = g$end, strand = g$strand,
            real_start = null_default(g$`_start`, g$start),
            real_end = null_default(g$`_end`, g$end),
            real_strand = null_default(g$`_strand`, g$strand),
            sequence = g$sequence, translation = g$translation
          )
        })
      )
    }))
  })
  spacing <- null_default(config$locus$spacing, 50)
  scale <- null_default(config$plot$scaleFactor, 30)
  list(
    clusters = clusters,
    groups = json$data$groups,
    links = json$data$links,
    # The plot draws 1000 bp in scaleFactor pixels
    locus_gap = round(spacing * 1000 / scale),
    threshold = 100 * null_default(config$link$threshold, 0)
  )
}

# Clusters, groups and links of a clinker session file (-s). Genes and loci
# have their positions on the record.
clinker_from_session <- function(json) {
  order <- unlist(null_default(json$order, names(json$clusters)))
  clusters <- lapply(json$clusters[order], function(cl) {
    list(name = cl$name, loci = lapply(unlist(cl$loci), function(lc_uid) {
      lc <- json$loci[[lc_uid]]
      length <- lc$end - lc$start
      list(
        name = lc$name,
        length = length,
        record_offset = lc$start,
        flipped = FALSE,
        from = 0,
        to = length,
        genes = lapply(unlist(lc$genes), function(g_uid) {
          g <- json$genes[[g_uid]]
          list(
            uid = g$uid, label = g$label, names = g$names, colour = NULL,
            start = g$start - lc$start, end = g$end - lc$start, strand = g$strand,
            real_start = g$start - lc$start, real_end = g$end - lc$start,
            real_strand = g$strand,
            sequence = g$sequence, translation = g$translation
          )
        })
      )
    }))
  })
  list(
    clusters = clusters,
    groups = json$groups,
    links = unname(json$links),
    # The defaults of the clinker plot: 50 px between loci, 30 px per 1000 bp
    locus_gap = round(50 * 1000 / 30),
    threshold = 0
  )
}

# Data frame of the groups. Groups without a saved colour get the colours
# clinker gives them: d3.interpolateRainbow in equal steps.
clinker_groups <- function(groups) {
  if (length(groups) == 0) {
    return(data.frame(group = character(), colour = character(),
                      hidden = logical(), genes = integer(),
                      stringsAsFactors = FALSE))
  }
  rainbow <- clinker_rainbow(length(groups))
  out <- data.frame(
    group = vapply(groups, function(g) null_default(g$label, g$uid), ""),
    colour = vapply(seq_along(groups), function(i) {
      colour <- groups[[i]]$colour
      if (is.null(colour)) rainbow[i] else css_to_hex(colour)
    }, ""),
    hidden = vapply(groups, function(g) isTRUE(g$hidden), TRUE),
    genes = vapply(groups, function(g) length(g$genes), 1L),
    stringsAsFactors = FALSE
  )
  # The genes of each group, to look up the group of a gene
  attr(out, "gene_uids") <- lapply(groups, function(g) unlist(g$genes))
  out
}

# The colours d3.quantize(d3.interpolateRainbow, n + 1) gives the first n
# groups: a cubehelix rainbow.
clinker_rainbow <- function(n) {
  t <- (seq_len(n) - 1) / n
  ts <- abs(t - 0.5)
  h <- (360 * t - 100 + 120) * pi / 180
  s <- 1.5 - 1.5 * ts
  l <- 0.8 - 0.9 * ts
  a <- s * l * (1 - l)
  rgb <- cbind(
    l + a * (-0.14861 * cos(h) + 1.78277 * sin(h)),
    l + a * (-0.29227 * cos(h) - 0.90649 * sin(h)),
    l + a * (1.97294 * cos(h))
  )
  rgb <- pmin(pmax(round(rgb * 255), 0), 255)
  sprintf("#%02x%02x%02x", rgb[, 1], rgb[, 2], rgb[, 3])
}

# "rgb(110, 64, 170)" as "#6e40aa"; other colours are returned unchanged
css_to_hex <- function(colour) {
  if (!grepl("^rgba?\\(", colour)) return(colour)
  values <- as.numeric(regmatches(colour, gregexpr("[0-9.]+", colour))[[1]])
  sprintf("#%02x%02x%02x", values[1], values[2], values[3])
}

# Data frame of the genes, with the loci of each cluster one after the other
clinker_genes <- function(clusters, groups, locus_gap, sequences, trim) {
  # Genes in a group that is shown
  grouped <- unlist(attr(groups, "gene_uids")[!groups$hidden])
  rows <- list()
  for (cl in clusters) {
    offset <- 0
    for (lc in cl$loci) {
      if (trim) {
        # Show the locus from its first to its last grouped gene
        in_group <- Filter(function(g) g$uid %in% grouped &&
                             g$start >= lc$from && g$end <= lc$to + 1, lc$genes)
        if (length(in_group) == 0) {
          lc$to <- lc$from - 1
        } else {
          lc$from <- min(vapply(in_group, function(g) g$start, 1))
          lc$to <- max(vapply(in_group, function(g) g$end, 1))
        }
      }
      for (g in lc$genes) {
        # Position on the record, undoing a flip
        if (lc$flipped) {
          record_start <- lc$length - g$real_end
          record_end <- lc$length - g$real_start
          record_strand <- -g$real_strand
        } else {
          record_start <- g$real_start
          record_end <- g$real_end
          record_strand <- g$real_strand
        }
        # Qualifiers, those named like a column (e.g. /label) with a suffix
        names <- g$names[!vapply(g$names, is.null, TRUE)]
        clash <- names(names) %in% clinker_gene_columns
        names(names)[clash] <- paste0(names(names)[clash], "_qualifier")
        row <- c(
          list(
            cluster = cl$name,
            locus = lc$name,
            start = offset + g$start - lc$from + 1,
            end = offset + g$end - lc$from,
            strand = ifelse(g$strand == 1, "forward", "reverse"),
            label = null_default(g$label, NA_character_),
            colour = if (is.null(g$colour)) NA_character_ else css_to_hex(g$colour),
            trimmed = g$start < lc$from || g$end > lc$to + 1,
            flipped = lc$flipped,
            record_start = lc$record_offset + record_start + 1,
            record_end = lc$record_offset + record_end,
            record_strand = ifelse(record_strand == 1, "forward", "reverse"),
            uid = g$uid
          ),
          lapply(names, as.character)
        )
        if (sequences) {
          row$sequence <- null_default(g$sequence, NA_character_)
          row$translation <- null_default(g$translation, NA_character_)
        }
        rows[[length(rows) + 1]] <- row
      }
      # A locus without genes to show takes no place
      if (lc$to >= lc$from) offset <- offset + (lc$to - lc$from) + locus_gap
    }
  }
  genes <- as.data.frame(dplyr::bind_rows(rows), stringsAsFactors = FALSE)
  if (nrow(genes) == 0) stop("The clinker file has no genes.", call. = FALSE)

  # Groups: label, colour (unless the gene has its own) and hidden
  genes$group <- NA_character_
  hidden_group <- rep(FALSE, nrow(genes))
  group_uids <- attr(groups, "gene_uids")
  for (i in seq_len(nrow(groups))) {
    in_group <- genes$uid %in% group_uids[[i]]
    genes$group[in_group] <- groups$group[i]
    genes$colour[in_group & is.na(genes$colour)] <- groups$colour[i]
    hidden_group[in_group] <- groups$hidden[i]
  }
  # Genes without a group are grey in clinker
  genes$colour[is.na(genes$colour)] <- "#bbbbbb"
  genes$hidden <- hidden_group | genes$trimmed
  genes$trimmed <- NULL

  genes[, c(clinker_gene_columns, setdiff(names(genes), clinker_gene_columns))]
}

# The columns read_clinker() gives every gene, before its qualifiers
clinker_gene_columns <- c(
  "cluster", "locus", "start", "end", "strand", "label", "group", "colour",
  "hidden", "flipped", "record_start", "record_end", "record_strand", "uid"
)

# Data frame of the links, oriented from the upper to the lower cluster
clinker_links <- function(links, genes, all_links, threshold) {
  empty <- data.frame(
    cluster1 = character(), start1 = numeric(), end1 = numeric(),
    cluster2 = character(), start2 = numeric(), end2 = numeric(),
    identity = numeric(), similarity = numeric(), label1 = character(),
    label2 = character(), group = character(), uid1 = character(),
    uid2 = character(), stringsAsFactors = FALSE
  )
  if (length(links) == 0) return(empty)

  uid1 <- vapply(links, function(l) l$query$uid, "")
  uid2 <- vapply(links, function(l) l$target$uid, "")
  identity <- vapply(links, function(l) 100 * null_default(l$identity, NA_real_), 1)
  similarity <- vapply(links, function(l) 100 * null_default(l$similarity, NA_real_), 1)

  g1 <- match(uid1, genes$uid)
  g2 <- match(uid2, genes$uid)
  keep <- !is.na(g1) & !is.na(g2) & !is.na(identity) & identity >= threshold
  g1 <- g1[keep]
  g2 <- g2[keep]
  identity <- identity[keep]
  similarity <- similarity[keep]

  # Gene of the upper cluster first
  rank <- match(genes$cluster, unique(genes$cluster))
  swap <- rank[g1] > rank[g2]
  upper <- ifelse(swap, g2, g1)
  g2 <- ifelse(swap, g1, g2)
  g1 <- upper
  same_group <- !is.na(genes$group[g1]) & genes$group[g1] %in% genes$group[g2] &
    genes$group[g1] == genes$group[g2]

  links <- data.frame(
    cluster1 = genes$cluster[g1], start1 = genes$start[g1], end1 = genes$end[g1],
    cluster2 = genes$cluster[g2], start2 = genes$start[g2], end2 = genes$end[g2],
    identity = identity, similarity = similarity,
    label1 = genes$label[g1], label2 = genes$label[g2],
    group = ifelse(same_group, genes$group[g1], NA_character_),
    uid1 = genes$uid[g1], uid2 = genes$uid[g2],
    stringsAsFactors = FALSE
  )
  if (!all_links) {
    links <- links[rank[g2] - rank[g1] == 1, , drop = FALSE]
  }
  links[order(match(links$cluster1, unique(genes$cluster)), links$start1), , drop = FALSE]
}
