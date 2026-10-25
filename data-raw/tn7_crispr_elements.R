# Creates the tn7_crispr_elements dataset: the genes of Tn7, of Tn6022 and of
# fourteen Tn7-like transposons that carry a minimal CRISPR-Cas system, the
# sixteen rows of Fig. 3 of the reference below. The elements and
# the genomes they sit in are those of Table S1 and Fig. 3 of the reference
# below, and the regions are drawn in the same orientation as that figure, with
# tnsA at the left. Needs an internet connection.
#
# Peters JE, Makarova KS, Shmakov S, Koonin EV. Recruitment of CRISPR-Cas
# systems by Tn7-like transposons. Proc Natl Acad Sci USA. 2017;114(35):E7358-
# E7366. doi: 10.1073/pnas.1709035114

library(geneviewer)
library(dplyr)

# Elements in plotting order. `element` is the TnsA tree node that Fig. 3
# labels the row with, `site` the gene the element has inserted next to and
# `strand` the orientation the region is fetched in (2 reverse complements it).
elements <- data.frame(
  element = c("Tn7", "Tn6022", "#2", "#141", "#199", "#262", "#298", "#330",
              "#340", "#375", "#409", "#424", "#438", "#446", "#2757", "#5291"),
  strain = c(
    "Acinetobacter baumannii UC20804",
    "Acinetobacter baumannii NIPH 1734",
    "Vibrio parahaemolyticus RIMD 2210633",
    "Vibrio campbellii ATCC BAA-1116",
    "Vibrio parahaemolyticus O1:Kuk FDA_R31",
    "Vibrio fluvialis ATCC 33809",
    "Vibrio cholerae VC35",
    "Vibrio natriegens NBRC 15636",
    "Vibrio crassostreae J5-20",
    "Pseudoalteromonas translucida KMM 520",
    "Vibrio cholerae YB2A06",
    "Vibrio hyugaensis 151112A",
    "Vibrio sp. EJY3",
    "Shewanella sp. ANA-3",
    "Rippkaea orientalis PCC 8801",
    "Trichormus variabilis ATCC 29413"
  ),
  accession = c("CP076807.1", "NZ_KB849325.1",
                "NC_004605.1", "NC_009784.1", "NC_021821.1",
                "NZ_CP014034.2", "NZ_AMBR01000012.1", "NZ_ATFJ01000001.1",
                "NZ_CCKB01000071.1", "NZ_CP011034.1", "NZ_LBFX01000002.1",
                "NZ_BBLF01000006.1", "NC_016614.1", "NC_008577.1",
                "NC_011726.1", "NC_007413.1"),
  start = c(673100, 2813500, 1456300, 1392800, 28900, 1321900, 165900, 3400, 77600,
            2293200, 289800, 142100, 1614100, 4617000, 1727300, 4321900),
  end = c(689900, 2826200, 1468400, 1407000, 42100, 1340500, 187700, 27350,
          104800, 2315700, 307900, 166600, 1626600, 4641700, 1745900, 4337600),
  strand = c(2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1),
  site = c("attTn7 (glmS)", "yifB (comM)",
           "yciA", "IMPDH", "yciA", "IMPDH", "SRP-RNA",
           "SRP-RNA", "SRP-RNA", "SRP-RNA", "SRP-RNA", "SRP-RNA", "AraC type",
           "conserved gene", "DNA methyltransferase", "glmS"),
  # the product the insertion site carries in the annotation
  site_product = c(
    "glutamine--fructose-6-phosphate",
    # the yifB (comM) that Tn6022 sits in is split in two, and the fragment at
    # the R end is annotated only as an ATP-binding protein
    "^ATP-binding protein$",
    "acyl-CoA thioesterase",
    "GMP reductase", "acyl-CoA thioesterase", "GMP reductase",
    "signal recognition particle", "signal recognition particle",
    "signal recognition particle", "signal recognition particle",
    "signal recognition particle", "signal recognition particle",
    "AraC family transcriptional regulator", "hypothetical protein",
    "restriction/modification enzyme",
    "glutamine--fructose-6-phosphate transaminase"
  ),
  stringsAsFactors = FALSE
)

# Ends of the element itself, as positions on `accession`. The transposon runs
# from its right end, next to the insertion site, to its left end, so the right
# end is the terminus nearest tnsA. Tn7's ends are where the Tn7 of GenBank
# DQ176450.1 (the transposon alone) aligns to the chromosome, with blastn, at
# 99.9% identity over its whole length. The others are the intervals of Table S1,
# which the authors place by the terminal repeats and the target site
# duplication; Tn6022's are the window between the two halves of the yifB it
# split, which the authors do not resolve to the base. The right end always
# falls inside the drawn window; a left end beyond it is kept at its true
# position, past the last gene.
elements$element_start <- c(
  673227, 2813537, 1387705, 1378672, -65552, 1283240, 158580, 4012, 77814,
  2293394, 272380, 135220, 1508457, 4617245, 1727676, 4324033
)
elements$element_end <- c(
  688078, 2825485, 1467746, 1405821, 41472, 1339305, 187534, 27158, 104203,
  2315161, 307332, 166070, 1625643, 4641075, 1743903, 4355401
)

# read_gbk names each cluster after its file, so "#" cannot be part of it
elements$file <- sub("#", "n", elements$element)

# Download each region as its own GenBank file, named after the element
dir <- file.path(tempdir(), "tn7")
dir.create(dir, showWarnings = FALSE)
for (i in seq_len(nrow(elements))) {
  download.file(
    paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
           "?db=nuccore&rettype=gbwithparts&retmode=text",
           "&id=", elements$accession[i],
           "&seq_start=", elements$start[i],
           "&seq_stop=", elements$end[i],
           "&strand=", elements$strand[i]),
    file.path(dir, paste0(elements$file[i], ".gbk")),
    quiet = TRUE
  )
  Sys.sleep(0.4)
}

gbk <- read_gbk(dir, origin = TRUE)
names(gbk) <- elements$element[match(names(gbk), elements$file)]

# Genes, grouped by the role Fig. 3 colours them by. tnsC is a small NTPase, so
# a much larger "ATP-binding protein" is cargo rather than tnsC.
keys <- c("region", "locus_tag", "gene", "product")

# six of the elements sit next to an SRP RNA, which is annotated as an ncRNA
# rather than a coding sequence
rna <- suppressWarnings(gbk_features_to_df(gbk, feature = "ncRNA", keys = keys))
rna <- rna[grepl("signal recognition particle", rna$product), ]

genes <- bind_rows(gbk_features_to_df(gbk, feature = "CDS", keys = keys), rna) %>%
  mutate(
    product = ifelse(is.na(product), "", product),
    gene = ifelse(is.na(gene), "", gene),
    length = abs(end - start) + 1,
    category = case_when(
      # the Tn7 of the chromosome is annotated by protein family
      cluster == "Tn7" & product == "transposase family protein" ~ "tnsB",
      cluster == "Tn7" & product == "AAA family ATPase" ~ "tnsC",
      cluster == "Tn7" & product == "transcriptional antiterminator" ~ "tnsE",
      grepl("TnsA endonuclease|subunit TnsA|^transposase A$", product) ~ "tnsA",
      grepl(paste("TnsB C-terminal", "Mu transposase",
                  "integrase catalytic domain", "^transposase B$",
                  sep = "|"), product) ~ "tnsB",
      grepl("TniB family|transposition regulator", product) |
        (grepl("ATP-binding protein", product) & length <= 2200) ~ "tnsC",
      grepl("TniQ family|TnsD family|target selection protein D",
            product) ~ "tniQ / tnsD",
      grepl("target selection protein E", product) ~ "tnsE",
      grepl("Csy2|Cas8f|Cas8a1/Cmx1", product) ~ "cas8f / cas5f",
      grepl("Csy3|Cas7f", product) ~ "cas7f",
      grepl("Cas6/Csy4|Cas6f|Cas6/Cmx6", product) ~ "cas6f",
      grepl("Cas5/Cmx5|DevS", product) ~ "cas5",
      grepl("autoregulator", product) ~ "cas, other",
      TRUE ~ "other"
    ),
    # the short name Fig. 3 writes inside the arrow
    gene = case_when(
      category == "tnsA" ~ "tnsA",
      category == "tnsB" ~ "tnsB",
      category == "tnsC" ~ "tnsC",
      category == "tniQ / tnsD" ~ "tniQ",
      category == "tnsE" ~ "tnsE",
      category == "cas8f / cas5f" ~ "cas8f",
      category == "cas7f" ~ "cas7f",
      category == "cas6f" ~ "cas6f",
      category == "cas5" ~ "cas5",
      category == "cas, other" ~ "devR",
      TRUE ~ ifelse(is.na(gene), "", gene)
    )
  )

# Element #2757 has no tnsA of its own: its tnsB is longer than that of Tn7 by
# about the size of a TnsA, so it is taken as a TnsA-TnsB fusion. The gene is
# split in two, with the extra length at the N-terminal end as tnsA, so the
# chart can draw both parts. The boundary is an estimate.
tn7_tnsB <- with(filter(genes, cluster == "Tn7", category == "tnsB"), length)
fused <- which(genes$cluster == "#2757" & genes$category == "tnsB")
tnsA_length <- genes$length[fused] - tn7_tnsB
fwd <- genes$strand[fused] == "forward"
tnsA <- genes[fused, ]
tnsA$category <- "tnsA"
# its own gene name, so a chart can draw it differently; its label is "tnsA"
tnsA$gene <- "tnsA (fused)"
tnsA$product <- "TnsA part of a TnsA-TnsB fusion (estimated boundary)"
if (fwd) {
  tnsA$end <- tnsA$start + tnsA_length - 1
  genes$start[fused] <- tnsA$end + 1
} else {
  tnsA$start <- tnsA$end - tnsA_length + 1
  genes$end[fused] <- tnsA$start - 1
}
genes$product[fused] <- paste(genes$product[fused],
                              "(TnsB part of a TnsA-TnsB fusion)")
genes <- bind_rows(genes, tnsA)

# Mark the gene the element inserted next to
genes <- genes %>%
  left_join(select(elements, cluster = element, site, site_product),
            by = c("cluster")) %>%
  group_by(cluster) %>%
  mutate(
    is_site = grepl(first(site_product), product, ignore.case = TRUE),
    is_site = is_site & start == suppressWarnings(min(start[is_site])),
    is_site = !is.na(is_site) & is_site,
    category = ifelse(is_site, "insertion site", category),
    gene = ifelse(is_site, first(site), gene)
  ) %>%
  ungroup()

# CRISPR arrays. RefSeq annotates a few of them as repeat regions; the rest are
# found in the sequence, as copies of the repeat consensus of the type I-F
# arrays these elements carry, spaced a repeat plus a spacer apart. A lone copy
# only counts when it lies next to cas6.
CONSENSUS <- "GTTCACTGCCGTACAGGCAGCTTAGAAA"
ANCHOR_DIFF <- 10   # mismatches allowed against the consensus
LONE_DIFF <- 8      # a lone copy has to be at least this close
CORE <- 20          # how much of a copy is compared with the next one
MAX_PAIR <- 4       # mismatches allowed between the cores of two copies
PERIOD <- c(45, 95) # a repeat plus its spacer
LONE_REACH <- 1500  # how close to cas6 a lone copy has to lie

revcomp <- function(x) {
  chartr("ACGT", "TGCA", paste(rev(strsplit(x, "")[[1]]), collapse = ""))
}

# mismatches between the consensus and the window starting at each position
mismatches <- function(seq, probe) {
  s <- utf8ToInt(seq)
  p <- utf8ToInt(probe)
  n <- length(s) - length(p) + 1
  if (n < 1) return(integer(0))
  out <- integer(n)
  for (k in seq_along(p)) out <- out + (s[k:(n + k - 1)] != p[k])
  out
}

# The arrays of the two cyanobacterial elements belong to another repeat
# family, so they are found as any seed that recurs at a fixed period.
SEED <- 20

find_tandem <- function(seq) {
  if (nchar(seq) < SEED + PERIOD[1]) return(NULL)
  at <- seq_len(nchar(seq) - SEED + 1)
  by_seed <- split(at, substring(seq, at, at + SEED - 1))
  hits <- list()
  for (ps in by_seed[lengths(by_seed) >= 2]) {
    run <- ps[1]
    for (p in c(ps[-1], .Machine$integer.max)) {
      step <- p - max(run)
      if (step < PERIOD[1] || step > PERIOD[2]) {
        if (length(run) >= 2) {
          period <- round((max(run) - min(run)) / (length(run) - 1))
          hits[[length(hits) + 1]] <- data.frame(
            start = min(run), end = max(run) + period - 1, repeats = length(run)
          )
        }
        run <- integer(0)
      }
      run <- c(run, p)
    }
  }
  if (length(hits)) bind_rows(hits) else NULL
}

find_arrays <- function(seq, cas6) {
  width <- nchar(CONSENSUS)
  hits <- list()
  for (probe in c(CONSENSUS, revcomp(CONSENSUS))) {
    diff <- mismatches(seq, probe)
    anchors <- order(diff)[diff[order(diff)] <= ANCHOR_DIFF]
    taken <- integer(0)
    for (i in anchors) {
      if (any(abs(i - taken) < width)) next
      unit <- substring(seq, i, i + CORE - 1)
      run <- i
      for (step in c(1, -1)) {
        repeat {
          here <- if (step > 0) max(run) else min(run)
          cand <- here + step * seq(PERIOD[1], PERIOD[2])
          cand <- cand[cand >= 1 & cand + width - 1 <= nchar(seq)]
          keep <- cand[vapply(cand, function(j)
            sum(strsplit(substring(seq, j, j + CORE - 1), "")[[1]] !=
                  strsplit(unit, "")[[1]]) <= MAX_PAIR, logical(1))]
          if (!length(keep)) break
          run <- c(run, keep[1])
        }
      }
      taken <- c(taken, run)
      lone_ok <- diff[i] <= LONE_DIFF &&
        length(cas6) && min(abs(min(run) - cas6)) <= LONE_REACH
      if (length(run) >= 2 || lone_ok) {
        hits[[length(hits) + 1]] <- data.frame(
          start = min(run), end = max(run) + width - 1, repeats = length(run)
        )
      }
    }
  }
  hits <- bind_rows(c(hits, list(find_tandem(seq))))
  if (!nrow(hits)) return(NULL)
  hits <- hits %>% arrange(desc(repeats), start)
  keep <- hits[0, ]
  for (r in seq_len(nrow(hits))) {
    if (any(hits$start[r] < keep$end & hits$end[r] > keep$start)) next
    keep <- rbind(keep, hits[r, ])
  }
  keep
}

MIN_GAP <- 150    # the shortest intergenic stretch worth searching

arrays <- lapply(names(gbk), function(cl) {
  seq <- toupper(paste(gbk[[cl]]$ORIGIN, collapse = ""))
  here <- filter(genes, cluster == cl)
  cas6 <- with(filter(here, category == "cas6f"), c(start, end))

  # an array lies between genes, so only the intergenic stretches are searched
  coding <- arrange(here, start)
  edge <- cummax(c(1, coding$end))
  gaps <- data.frame(from = head(edge, -1), to = coding$start)
  gaps <- rbind(gaps, data.frame(from = max(edge), to = nchar(seq)))
  gaps <- filter(gaps, to - from >= MIN_GAP)

  found <- bind_rows(lapply(seq_len(nrow(gaps)), function(k) {
    hit <- find_arrays(substring(seq, gaps$from[k], gaps$to[k]),
                       cas6 - gaps$from[k] + 1)
    if (!is.null(hit)) mutate(hit, start = start + gaps$from[k] - 1,
                              end = end + gaps$from[k] - 1)
  }))
  if (is.null(found) || !nrow(found)) {
    warning("no CRISPR array found in ", cl)
    return(NULL)
  }
  data.frame(
    cluster = cl, locus_tag = NA_character_, gene = "CRISPR",
    product = paste0("CRISPR array (", found$repeats, " repeats)"),
    category = "CRISPR array",
    start = found$start, end = found$end, strand = "forward"
  )
})

ends <- lapply(seq_len(nrow(elements)), function(k) {
  w <- elements[k, ]
  # into the coordinates of the drawn window, which is reverse complemented
  # when the element runs leftwards on the record
  local <- if (w$strand == 2) {
    c(R = w$end - w$element_end + 1, L = w$end - w$element_start + 1)
  } else {
    c(R = w$element_start - w$start + 1, L = w$element_end - w$start + 1)
  }
  # R has to lie in the window; L is kept when it lies beyond it, at its true
  # position, so a chart can show where the element ends
  local <- local[local >= 1 & (names(local) == "L" | local <= w$end - w$start + 1)]
  if (!length(local)) return(NULL)
  data.frame(
    cluster = w$element, locus_tag = NA_character_, gene = names(local),
    product = paste(ifelse(names(local) == "R", "right", "left"),
                    "end of the element"),
    category = ifelse(names(local) == "R", "right end", "left end"),
    start = unname(local), end = unname(local), strand = "forward"
  )
})

tn7_crispr_elements <- bind_rows(
  genes %>%
    # a neighbouring gene can be clipped to a sliver by the edge of the window
    filter(!(category == "other" & abs(end - start) + 1 < 50)) %>%
    select(cluster, locus_tag, gene, product, category, start, end, strand),
  bind_rows(arrays),
  bind_rows(ends)
) %>%
  left_join(select(elements, cluster = element, strain, accession, site),
            by = "cluster") %>%
  # the strain with its genus as an initial, e.g. "V. cholerae VC35", for short
  # labels; "sp." keeps its genus, e.g. "Vibrio sp. EJY3"
  mutate(strain_short = sub("^([A-Z])[a-z]+ (?!sp\\.)", "\\1. ", strain, perl = TRUE)) %>%
  # each row runs from the insertion site to the left end, as in Fig. 3: the
  # genes beyond those belong to the host, not to the element, and a gene that
  # runs on past the left end (intI2 of Tn7) is cut off there
  group_by(cluster) %>%
  mutate(left_end = start[category == "left end"]) %>%
  filter(end >= min(start[category == "insertion site"]), start <= left_end) %>%
  mutate(end = pmin(end, left_end)) %>%
  ungroup() %>%
  arrange(factor(cluster, levels = elements$element), start) %>%
  # The names Fig. 3 writes on the genes: the transposition and cas genes, the
  # insertion sites in short (not those of #438, #446 and #2757) and, of the
  # cargo, only the resistance genes and integrase of Tn7
  group_by(cluster) %>%
  mutate(
    tn7_named = cluster == "Tn7" &
      (gene %in% c("aadA1", "dfrA1", "intI2") |
         (gene == "sat2" & start == min(start[gene == "sat2"]))),
    label = case_when(
      category %in% c("CRISPR array", "right end", "left end") ~ "",
      gene == "tnsA (fused)" ~ "tnsA",
      category == "insertion site" ~ recode(
        gene, "attTn7 (glmS)" = "glmS", "yifB (comM)" = "yifB",
        "AraC type" = "", "conserved gene" = "", "DNA methyltransferase" = ""),
      category == "other" & !tn7_named ~ "",
      TRUE ~ gene
    )
  ) %>%
  ungroup() %>%
  select(cluster, strain, strain_short, accession, site, locus_tag, gene, label,
         product, category, start, end, strand) %>%
  as.data.frame()

usethis::use_data(tn7_crispr_elements, overwrite = TRUE)
