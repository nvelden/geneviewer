# Creates the coronavirus_genomes dataset: the genes of eleven coronaviruses
# from the four genera, the genomes of Fig. 1 of the reference below, with the
# homologous genes of closely related neighbouring genomes. The genes are read
# from the GenBank records; the accessory ORFs that the records do not annotate
# are added from their positions in the genome. Needs an internet connection
# and the Bioconductor packages Biostrings and pwalign.
#
# Chazal N. Coronavirus, the King Who Wanted More Than a Crown: From Common to
# the Highly Pathogenic SARS-CoV-2, Is the Key in the Accessory Genes? Front
# Microbiol. 2021;12:682603. doi: 10.3389/fmicb.2021.682603

library(geneviewer)
library(dplyr)
library(Biostrings)
library(pwalign)

# Genomes in plotting order: by genus and, within a genus, by subgenus, so
# that each genome sits next to its closest relatives here. The accessions are
# those of the reference where it gives them; MHV is strain JHM, as the A59
# records leave out HE, 4 and 5a.
genomes <- data.frame(
  cluster = c("HCoV-229E", "HCoV-NL63", "MHV", "HCoV-OC43", "HCoV-HKU1",
              "MERS-CoV", "SARS-CoV-1", "SARS-CoV-2", "RaTG13", "IBV", "PDCoV"),
  genus = c("Alphacoronavirus", "Alphacoronavirus", rep("Betacoronavirus", 7),
            "Gammacoronavirus", "Deltacoronavirus"),
  subgenus = c("Duvinacovirus", "Setracovirus", "Embecovirus", "Embecovirus",
               "Embecovirus", "Merbecovirus", "Sarbecovirus", "Sarbecovirus",
               "Sarbecovirus", "Igacovirus", "Buldecovirus"),
  virus = c("Human coronavirus 229E", "Human coronavirus NL63",
            "Murine hepatitis virus", "Human coronavirus OC43",
            "Human coronavirus HKU1", "MERS coronavirus", "SARS coronavirus",
            "SARS coronavirus 2", "Bat coronavirus RaTG13",
            "Infectious bronchitis virus", "Porcine deltacoronavirus"),
  # strain or isolate as in the record
  strain = c("229E", "Amsterdam I", "JHM", "19572 Belgium 2004", "HKU1",
             "HCoV-EMC/2012", "Tor2", "Wuhan-Hu-1", "RaTG13", "Beaudette",
             "HKU15-155"),
  host = c("human", "human", "mouse", "human", "human", "human", "human",
           "human", "bat", "chicken", "pig"),
  accession = c("AF304460.1", "AY567487.2", "AC_000192.1", "AY903460.1",
                "AY597011.2", "NC_019843.3", "AY274119.3", "NC_045512.2",
                "MN996532.2", "NC_001451.1", "NC_039208.1")
)

# Download the GenBank files, named after the genomes
dir <- file.path(tempdir(), "coronavirus_genomes")
dir.create(dir, showWarnings = FALSE)
for (i in seq_len(nrow(genomes))) {
  download.file(
    paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nuccore&id=",
           genomes$accession[i], "&rettype=gbwithparts&retmode=text"),
    file.path(dir, paste0(genomes$cluster[i], ".gbk"))
  )
  Sys.sleep(1) # NCBI allows three requests a second
}
gbk <- read_gbk(dir)

# The records name the genes in many ways; give each the short name used in
# the literature, by its product in the record
names_by_product <- list(
  "HCoV-229E" = c("surface glycoprotein" = "S", "4a protein" = "4a",
                  "4b protein" = "4b", "envelope protein" = "E",
                  "membrane protein" = "M", "nucleocapsid protein" = "N"),
  "HCoV-NL63" = c("spike protein" = "S", "protein 3" = "3",
                  "envelope protein" = "E", "membrane protein" = "M",
                  "nucleocapsid protein" = "N"),
  "MHV" = c("p30 accessory protein" = "2a",
            "hemagglutinin-esterase glycoprotein" = "HE",
            "spike glycoprotein" = "S", "accessory protein 4" = "4",
            "accessory protein 5a" = "5a", "envelope protein" = "E",
            "membrane protein" = "M", "nucleocapsid protein" = "N",
            "putative internal (I) ORF" = "I"),
  "HCoV-OC43" = c("ns2 protein" = "2a", "HE protein" = "HE",
                  "S glycoprotein" = "S", "ns12.9" = "ns12.9",
                  "E protein" = "E", "M protein" = "M", "N protein" = "N",
                  "unknown" = "I"),
  "HCoV-HKU1" = c("hemagglutinin-esterase glycoprotein" = "HE",
                  "spike glycoprotein" = "S", "non-structural protein" = "4",
                  "small membrane protein" = "E",
                  "membrane glycoprotein" = "M",
                  "nucleocapsid phosphoprotein" = "N",
                  "nucleocapsid phosphoprotein 2" = "I"),
  "RaTG13" = c("spike glycoprotein" = "S",
               "nonstructural protein NS3" = "3a", "envelope protein" = "E",
               "membrane protein" = "M", "nonstructural protein NS6" = "6",
               "nonstructural protein NS7a" = "7a",
               "nonstructural protein NS7b" = "7b",
               "nonstructural protein NS8" = "8",
               "nucleocapsid protein" = "N"),
  "SARS-CoV-1" = c("spike glycoprotein" = "S", "Orf3" = "3a", "Orf4" = "3b",
                   "small envelope E protein" = "E",
                   "membrane glycoprotein M" = "M", "Orf7" = "6",
                   "Orf8" = "7a", "Orf9" = "7b", "Orf10" = "8a",
                   "Orf11" = "8b", "nucleocapsid protein" = "N",
                   "Orf13" = "9b", "Orf14" = "9c"),
  "MERS-CoV" = c("spike protein" = "S", "NS3 protein" = "3",
                 "NS4A protein" = "4a", "NS4B protein" = "4b",
                 "NS5 protein" = "5", "envelope protein" = "E",
                 "membrane protein" = "M", "nucleocapsid protein" = "N",
                 "ORF8b protein" = "8b"),
  "SARS-CoV-2" = c("surface glycoprotein" = "S", "ORF3a protein" = "3a",
                   "envelope protein" = "E", "membrane glycoprotein" = "M",
                   "ORF6 protein" = "6", "ORF7a protein" = "7a",
                   "ORF7b" = "7b", "ORF8 protein" = "8",
                   "nucleocapsid phosphoprotein" = "N", "ORF10 protein" = "10"),
  "IBV" = c("spike protein" = "S", "3a protein" = "3a", "3b protein" = "3b",
            "small virion-associated protein" = "E",
            "membrane protein" = "M", "5a protein" = "5a",
            "5b protein" = "5b", "nucleocapsid protein" = "N"),
  "PDCoV" = c("spike glycoprotein" = "S", "envelope protein" = "E",
              "membrane protein" = "M", "NS6 protein" = "6",
              "nucleocapsid protein" = "N", "NS7 protein" = "7")
)
products <- bind_rows(lapply(names(names_by_product), function(cl) {
  data.frame(cluster = cl, product = names(names_by_product[[cl]]),
             gene = unname(names_by_product[[cl]]))
}))

cds <- gbk_features_to_df(gbk, keys = c("region", "product"))

# ORF1ab is read as ORF1a and, after the -1 ribosomal frameshift, ORF1b: split
# it at the frameshift. The separate ORF1a records are left out.
orf1ab <- cds %>%
  filter(grepl("^join", region)) %>%
  mutate(shift = as.numeric(sub("^join\\([0-9]+\\.\\.([0-9]+),.*", "\\1", region)))
orf1 <- bind_rows(
  transmute(orf1ab, cluster, gene = "ORF1a", product = "replicase polyprotein 1a",
            start, end = shift, strand),
  transmute(orf1ab, cluster, gene = "ORF1b",
            product = "replicase polyprotein 1b (after the frameshift)",
            start = shift, end, strand)
)
annotated <- cds %>%
  filter(!grepl("^join", region)) %>%
  inner_join(products, by = c("cluster", "product")) %>%
  select(cluster, gene, product, start, end, strand)
stopifnot(nrow(annotated) == nrow(products))

# Accessory ORFs the records do not annotate, by their position in the genome:
# the ORFs that start with ATG inside N (and in SARS-CoV-2 and RaTG13 inside
# ORF3a) in another reading frame, and ORF10 of RaTG13, which matches ORF10 of
# SARS-CoV-2. The SARS-CoV-2 ORFs are described by Jungreis et al. (2021), Nat
# Commun 12:2642.
extra <- data.frame(
  cluster = c("HCoV-229E", "HCoV-229E", "HCoV-NL63", "HCoV-NL63",
              "RaTG13", "RaTG13", "RaTG13", "RaTG13", "MERS-CoV",
              "SARS-CoV-2", "SARS-CoV-2", "SARS-CoV-2", "SARS-CoV-2",
              "SARS-CoV-2"),
  gene = c("Ia", "Ib", "Ia", "Ib", "3b", "9b", "9c", "10", "8c",
           "3c", "3d", "3b", "9b", "9c"),
  start = c(25861, 26464, 26155, 26419, 25799, 28265, 28715, 29539, 29290,
            25457, 25524, 25814, 28284, 28734),
  end = c(26235, 26643, 26376, 26712, 25867, 28558, 28936, 29655, 29415,
          25582, 25697, 25882, 28577, 28955)
) %>%
  mutate(product = paste("ORF", gene, "(not annotated in the record)"),
         strand = "forward")

# Each added ORF must run from ATG to its first stop codon
for (i in seq_len(nrow(extra))) {
  sequence <- tolower(gbk[[extra$cluster[i]]][["ORIGIN"]])
  orf <- substring(sequence, extra$start[i], extra$end[i])
  codons <- substring(orf, seq(1, nchar(orf), 3), seq(3, nchar(orf), 3))
  stopifnot(nchar(orf) %% 3 == 0, codons[1] == "atg",
            which(codons %in% c("taa", "tag", "tga"))[1] == length(codons))
}

# The untranslated ends: the 5' UTR before ORF1a, which starts with the leader
# (L), and the 3' UTR after the last gene, up to the end of the genome
genome_length <- vapply(gbk, function(x) {
  as.numeric(sub("^\\S+\\s+([0-9]+) bp.*", "\\1", x[["LOCUS"]]))
}, numeric(1))
genes <- bind_rows(orf1, annotated, extra)
ends <- genes %>%
  group_by(cluster) %>%
  summarise(first = min(start), last = max(end))
utrs <- bind_rows(
  transmute(ends, cluster, gene = "L",
            product = "5' untranslated region, starting with the leader",
            start = 1, end = first - 1, strand = "forward"),
  transmute(ends, cluster, gene = "3'UTR", product = "3' untranslated region",
            start = last + 1, end = genome_length[cluster], strand = "forward")
)

categories <- c(L = "5' UTR", ORF1a = "ORF1a", ORF1b = "ORF1b", S = "spike",
                HE = "hemagglutinin-esterase", E = "envelope", M = "membrane",
                N = "nucleocapsid", "3'UTR" = "3' UTR")
coronavirus_genomes <- bind_rows(genes, utrs) %>%
  mutate(category = ifelse(gene %in% names(categories),
                           categories[gene], "accessory")) %>%
  left_join(genomes, by = "cluster") %>%
  arrange(factor(cluster, levels = genomes$cluster), start, end) %>%
  select(cluster, genus, subgenus, virus, strain, host, accession, gene,
         product, category, start, end, strand) %>%
  as.data.frame()
rownames(coronavirus_genomes) <- NULL

# Homologous genes of closely related neighbouring genomes, from the protein identity of a global alignment (protein_blast()). Every
# gene is translated from the genome, with X for codons with an ambiguous base;
# ORF1a ends at the frameshift, so it has no stop codon.
proteins <- coronavirus_genomes %>%
  filter(!gene %in% c("L", "3'UTR")) %>%
  mutate(protein_id = paste(cluster, gene, sep = "_"))
proteins$translation <- mapply(function(cluster, start, end) {
  dna <- DNAString(toupper(substring(gbk[[cluster]][["ORIGIN"]], start, end)))
  dna <- subseq(dna, 1, length(dna) - length(dna) %% 3)
  sub("\\*$", "", as.character(translate(dna, if.fuzzy.codon = "X")))
}, proteins$cluster, proteins$start, proteins$end)
stopifnot(!grepl("\\*", proteins$translation))

# The best hit in genome b of every protein of genome a, with its identity
best_hits <- function(a, b) {
  protein_blast(filter(proteins, cluster %in% c(a, b)), query = a,
                id = "protein_id", identity = 0) %>%
    filter(cluster == b, BlastP != "No Hit") %>%
    transmute(id1 = BlastP, id2 = protein_id, identity)
}

# Only close relatives are linked: neighbours whose ORF1b, which holds the
# polymerase and is the most conserved protein, is at least 70% identical.
# This keeps the genomes of a subgenus and the two alphacoronaviruses (82%)
# together and parts the subgenera of the betacoronaviruses (62-66%) and the
# genera (50-56%), whose proteins are so far apart that their identity says
# little. The cut-off is chosen for these genomes, not an official one.
# Between close relatives, the core genes are homologous and are linked to the
# same gene of the next genome, however low their identity. Accessory genes are
# linked when each is the other's best hit, with at least 30% identity, or 50%
# when either protein is shorter than 50 amino acids: short alignments reach a
# third identity by chance.
core <- c("ORF1a", "ORF1b", "S", "HE", "E", "M", "N")
length_aa <- setNames(nchar(proteins$translation), proteins$protein_id)
links <- list()
for (i in seq_len(nrow(genomes) - 1)) {
  a <- genomes$cluster[i]
  b <- genomes$cluster[i + 1]
  polymerase <- filter(proteins, gene == "ORF1b", cluster %in% c(a, b))
  polymerase_identity <- protein_blast(polymerase, query = a, id = "protein_id",
                                       identity = 0) %>%
    filter(cluster == b) %>%
    pull(identity)
  if (polymerase_identity < 70) next
  core_links <- lapply(intersect(core, intersect(proteins$gene[proteins$cluster == a],
                                                 proteins$gene[proteins$cluster == b])),
                       function(g) {
    pair <- filter(proteins, gene == g, cluster %in% c(a, b))
    protein_blast(pair, query = a, id = "protein_id", identity = 0) %>%
      filter(cluster == b) %>%
      transmute(id1 = paste(a, g, sep = "_"), id2 = protein_id, identity)
  })
  accessory_links <- inner_join(best_hits(a, b), best_hits(b, a),
                                by = c(id1 = "id2", id2 = "id1"),
                                suffix = c("", ".reverse")) %>%
    filter(!sub(".*_", "", id1) %in% core, !sub(".*_", "", id2) %in% core,
           identity >= ifelse(pmin(length_aa[id1], length_aa[id2]) < 50, 50, 30)) %>%
    select(id1, id2, identity)
  links[[i]] <- bind_rows(core_links, accessory_links)
}

# Genes joined by links form a chain of homologs across neighbouring genomes;
# give each chain the name of its first gene ("MHV 5a" for 5a of MHV, ns12.9
# of OC43 and 4 of HKU1), for GC_links(group = "homolog"). The identity of a
# gene is that to its homolog in the genome above, as protein_blast() gives.
links <- bind_rows(links)
homolog <- setNames(rep(NA_character_, nrow(proteins)), proteins$protein_id)
for (i in seq_len(nrow(links))) {
  if (is.na(homolog[links$id1[i]])) {
    homolog[links$id1[i]] <- sub("_", " ", links$id1[i])
  }
  homolog[links$id2[i]] <- homolog[links$id1[i]]
}
identity <- setNames(links$identity, links$id2)
coronavirus_genomes <- coronavirus_genomes %>%
  mutate(protein_id = paste(cluster, gene, sep = "_"),
         homolog = unname(homolog[protein_id]),
         identity = unname(identity[protein_id])) %>%
  select(-protein_id)

usethis::use_data(coronavirus_genomes, overwrite = TRUE)
