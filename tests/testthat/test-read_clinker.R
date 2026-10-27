clinker_gene <- function(uid, label, start, end, strand, qualifiers = list()) {
  list(uid = uid, label = label,
       names = c(list(locus_tag = label, product = "protein"), qualifiers),
       start = start, end = end, strand = strand)
}

clinker_link <- function(uid, query, target, identity) {
  list(uid = uid, query = list(uid = query), target = list(uid = target),
       identity = identity, similarity = identity + 0.05)
}

write_clinker <- function(x) {
  path <- tempfile(fileext = ".json")
  jsonlite::write_json(x, path, auto_unbox = TRUE, null = "null")
  path
}

# Three clusters; the second has two loci. Session coordinates are on the
# record: the second locus of cluster 2 starts at 500.
clinker_session <- function() {
  list(
    order = list("c2", "c1", "c3"),
    clusters = list(
      c1 = list(uid = "c1", name = "One", loci = list("l1")),
      c2 = list(uid = "c2", name = "Two", loci = list("l2", "l3")),
      c3 = list(uid = "c3", name = "Three", loci = list("l4"))
    ),
    loci = list(
      l1 = list(uid = "l1", name = "contig_1", start = 0, end = 3000, genes = list("g1", "g2")),
      l2 = list(uid = "l2", name = "contig_2", start = 0, end = 2000, genes = list("g3")),
      l3 = list(uid = "l3", name = "contig_3", start = 500, end = 2500, genes = list("g4")),
      l4 = list(uid = "l4", name = "contig_4", start = 0, end = 3000, genes = list("g5", "g6"))
    ),
    genes = list(
      g1 = clinker_gene("g1", "A1", 100, 1200, 1, list(label = "A1 CDS")),
      g2 = clinker_gene("g2", "A2", 1500, 2800, -1),
      g3 = clinker_gene("g3", "B1", 200, 1400, 1),
      g4 = clinker_gene("g4", "B2", 700, 2000, -1),
      g5 = clinker_gene("g5", "C1", 100, 1300, 1),
      g6 = clinker_gene("g6", "C2", 1600, 2900, -1)
    ),
    groups = list(
      list(uid = "grp1", label = "Group 0", genes = list("g1", "g3", "g5")),
      list(uid = "grp2", label = "Group 1", genes = list("g2", "g4", "g6"))
    ),
    links = list(
      k1 = clinker_link("k1", "g1", "g3", 0.8),
      k2 = clinker_link("k2", "g2", "g4", 0.6),
      k3 = clinker_link("k3", "g1", "g5", 0.5),
      k4 = clinker_link("k4", "g3", "g5", 0.4)
    )
  )
}

test_that("read_clinker reads a clinker session file", {
  path <- write_clinker(clinker_session())
  on.exit(unlink(path))
  x <- read_clinker(path)

  # Clusters in the order of the session, loci after each other
  expect_equal(unique(x$genes$cluster), c("Two", "One", "Three"))
  two <- x$genes[x$genes$cluster == "Two", ]
  gap <- round(50 * 1000 / 30)
  expect_equal(two$start, c(201, 2000 + gap + 201))
  expect_equal(two$end, c(1400, 2000 + gap + 1500))
  expect_equal(two$record_start, c(201, 701))
  expect_equal(two$record_end, c(1400, 2000))
  expect_equal(two$strand, c("forward", "reverse"))

  # Qualifiers, with /label renamed, and clinker's group colours
  expect_equal(x$genes$locus_tag[x$genes$cluster == "One"], c("A1", "A2"))
  expect_equal(x$genes$label_qualifier[x$genes$uid == "g1"], "A1 CDS")
  expect_equal(x$groups$group, c("Group 0", "Group 1"))
  expect_equal(x$groups$colour, c("#6e40aa", "#aff05b"))
  expect_equal(x$genes$colour[x$genes$uid == "g2"], "#aff05b")

  # Links between neighbouring clusters, upper cluster first, in percent
  expect_equal(nrow(x$links), 3)
  expect_true(all(paste(x$links$cluster1, x$links$cluster2) %in%
                    c("Two One", "One Three")))
  k1 <- x$links[x$links$uid1 == "g3" & x$links$uid2 == "g1", ]
  expect_equal(k1$identity, 80)
  expect_equal(k1$similarity, 85)
  expect_equal(k1$group, "Group 0")
  expect_equal(c(k1$start1, k1$end1, k1$start2, k1$end2), c(201, 1400, 101, 1200))

  # All pairs, and an identity threshold
  expect_equal(nrow(read_clinker(path, all_links = TRUE)$links), 4)
  expect_equal(nrow(read_clinker(path, threshold = 55)$links), 2)
  expect_equal(read_clinker(path, locus_gap = 100)$genes$start[2], 2000 + 100 + 201)
})

test_that("read_clinker keeps the changes saved in a clinker plot", {
  plot_gene <- function(uid, label, start, end, strand, real = c(start, end, strand)) {
    g <- clinker_gene(uid, label, start, end, strand)
    g[c("_start", "_end", "_strand")] <- as.list(real)
    g
  }
  data <- list(
    config = list(plot = list(scaleFactor = 20), locus = list(spacing = 40),
                  link = list(threshold = 0.3)),
    data = list(
      clusters = list(
        list(uid = "c1", name = "One", loci = list(list(
          uid = "l1", name = "contig_1", start = 0, end = 3000,
          `_start` = 0, `_end` = 3000, `_flipped` = TRUE,
          # Flipped: drawn positions are mirrored on the locus
          genes = list(plot_gene("g2", "A2", 200, 1500, 1),
                       plot_gene("g1", "A1", 1800, 2900, -1))
        ))),
        list(uid = "c2", name = "Two", loci = list(list(
          uid = "l2", name = "contig_2", start = 0, end = 4000,
          `_start` = 0, `_end` = 2500, `_flipped` = FALSE,
          # Trimmed after 2500: the third gene is hidden
          genes = list(plot_gene("g3", "B1", 200, 1400, 1),
                       plot_gene("g4", "B2", 1500, 2500, -1),
                       plot_gene("g5", "B3", 3000, 3900, 1))
        )))
      ),
      groups = list(
        list(uid = "grp1", label = "Synthase", genes = list("g1", "g3"),
             hidden = FALSE, colour = "rgb(110, 64, 170)"),
        list(uid = "grp2", label = "Group 1", genes = list("g2", "g4"),
             hidden = TRUE, colour = "#123456"),
        list(uid = "grp3", label = "Group 2", genes = list("g5"),
             hidden = FALSE, colour = "rgb(1, 2, 3)")
      ),
      links = list(
        clinker_link("k1", "g3", "g1", 0.9),
        clinker_link("k2", "g4", "g2", 0.7),
        clinker_link("k3", "g5", "g1", 0.2)
      )
    )
  )
  path <- write_clinker(data)
  on.exit(unlink(path))

  x <- read_clinker(path)
  # The hidden group and the trimmed gene are left out, with their links
  expect_equal(x$genes$uid, c("g1", "g3"))
  expect_equal(x$links$uid1, "g1")
  expect_equal(x$links$identity, 90)

  # Flipped genes keep their place on the record
  g1 <- x$genes[x$genes$uid == "g1", ]
  expect_true(g1$flipped)
  expect_equal(c(g1$start, g1$end, g1$strand), c("1801", "2900", "reverse"))
  expect_equal(c(g1$record_start, g1$record_end), c(101, 1200))
  expect_equal(g1$record_strand, "forward")
  expect_equal(g1$group, "Synthase")
  expect_equal(g1$colour, "#6e40aa")

  all <- read_clinker(path, keep_hidden = TRUE)
  expect_equal(all$genes$hidden, c(TRUE, FALSE, FALSE, TRUE, TRUE))
  # The plot's spacing between loci: 40 px at 20 px per 1000 bp
  expect_equal(clinker_from_plot(jsonlite::fromJSON(path, simplifyVector = FALSE))$locus_gap, 2000)
  # The plot's link threshold of 0.3 identity
  expect_equal(nrow(all$links), 2)
  expect_equal(nrow(read_clinker(path, keep_hidden = TRUE, threshold = 0)$links), 3)
})

test_that("read_clinker trims loci to their grouped genes", {
  session <- clinker_session()
  # Ungrouped genes around the grouped gene of contig_3, a locus with no
  # grouped genes, and an ungrouped gene between grouped genes of contig_1
  session$loci$l3$genes <- list("u1", "g4", "u2")
  session$loci$l5 <- list(uid = "l5", name = "contig_5", start = 0, end = 900,
                          genes = list("u3"))
  session$clusters$c2$loci <- list("l2", "l5", "l3")
  session$loci$l1$genes <- list("g1", "u4", "g2")
  session$genes$u1 <- clinker_gene("u1", "U1", 520, 650, 1)
  session$genes$u2 <- clinker_gene("u2", "U2", 2100, 2400, 1)
  session$genes$u3 <- clinker_gene("u3", "U3", 100, 800, 1)
  session$genes$u4 <- clinker_gene("u4", "U4", 1250, 1450, 1)
  path <- write_clinker(session)
  on.exit(unlink(path))

  gap <- round(50 * 1000 / 30)
  x <- read_clinker(path, trim = TRUE)
  two <- x$genes[x$genes$cluster == "Two", ]
  expect_equal(two$label, c("B1", "B2"))
  # contig_2 is shown from B1 (201-1400), contig_5 is left out, contig_3
  # starts at B2
  expect_equal(two$start, c(1, 1200 + gap + 1))
  expect_equal(two$end, c(1200, 1200 + gap + 1300))
  # Ungrouped genes between grouped genes stay
  expect_equal(x$genes$label[x$genes$cluster == "One"], c("A1", "U4", "A2"))

  all <- read_clinker(path, trim = TRUE, keep_hidden = TRUE)
  expect_equal(all$genes$label[all$genes$hidden], c("U3", "U1", "U2"))
  expect_equal(nrow(read_clinker(path)$genes), 10)
})

test_that("read_clinker gives clear errors and reads files without groups", {
  expect_error(read_clinker("no_such_file.json"), "clinker JSON file")

  other <- write_clinker(list(a = 1))
  on.exit(unlink(other), add = TRUE)
  expect_error(read_clinker(other), "not a clinker session file")

  # Older sessions have no order and no groups
  session <- clinker_session()
  session$order <- NULL
  session$groups <- NULL
  path <- write_clinker(session)
  on.exit(unlink(path), add = TRUE)
  x <- read_clinker(path)
  expect_equal(unique(x$genes$cluster), c("One", "Two", "Three"))
  expect_true(all(is.na(x$genes$group)))
  expect_true(all(x$genes$colour == "#bbbbbb"))
  expect_equal(nrow(x$groups), 0)
})

test_that("read_clinker output draws with GC_chart and GC_links", {
  path <- write_clinker(clinker_session())
  on.exit(unlink(path))
  x <- read_clinker(path)
  chart <- GC_chart(x$genes, cluster = "cluster", group = "group", strand = "strand") %>%
    GC_links(data = x$links)
  expect_s3_class(chart, "htmlwidget")
  expect_equal(nrow(chart$x$links[[1]]$data), 3)
})
