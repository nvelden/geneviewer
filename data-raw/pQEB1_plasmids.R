# Creates the pQEB1_plasmids and pQEB1_alignment datasets: the genes of five
# blaKPC-2 plasmids of the pQEB1 lineage and a nucleotide alignment between
# neighbouring plasmids. Needs an internet connection and MUMmer (nucmer).
#
# Moran RA et al. pQEB1: a hospital outbreak plasmid lineage carrying blaKPC-2.
# Microb Genom. 2024;10(9). doi: 10.1099/mgen.0.001294

library(geneviewer)
library(dplyr)

# Plasmids in plotting order: each is aligned to the next one
plasmids <- data.frame(
  accession = c("NZ_PQ162293", "NZ_PQ162285", "NZ_PQ162289", "NZ_PQ162292", "NZ_PQ162291"),
  cluster = c("pQEB1_inv3_Tn5403", "pQEB1", "pQEB1_inv1", "pQEB1_inv1_inv2", "pQEB1_inv1_arsTn"),
  host = c("E. cloacae ECL-21", "E. cloacae", "E. cloacae ECL-26", "E. cloacae ECL-22",
           "K. oxytoca KOX-3")
)

# Download the GenBank files, named after the plasmids. MUMmer needs a path
# without spaces
dir <- file.path(tempdir(), "pQEB1")
dir.create(dir, showWarnings = FALSE)
for (i in seq_len(nrow(plasmids))) {
  download.file(
    paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nuccore&id=",
           plasmids$accession[i], "&rettype=gbwithparts&retmode=text"),
    file.path(dir, paste0(plasmids$cluster[i], ".gbk"))
  )
}

# Genes, grouped by function from their gene name and product
pQEB1_plasmids <- read_gbk(dir) %>%
  gbk_features_to_df(keys = c("region", "locus_tag", "gene", "product")) %>%
  mutate(
    gene = ifelse(is.na(gene), "", gene),
    product = ifelse(is.na(product), "", product),
    text = paste(gene, product),
    category = case_when(
      grepl("KPC", product) ~ "Carbapenemase (KPC)",
      grepl("lactamase|aminoglycoside|resistan|dihydrofolate|dihydropteroate|Qnr", product) ~ "Other resistance",
      grepl("transposase|resolvase|recombinase|integrase|\\bIS[0-9]|\\bTn[0-9]|ISCR|intI", text) ~ "Mobile element",
      grepl("conjuga|type IV|\\bVir[B-D]|\\bTrb|\\bTra[A-Z]\\b|relaxase|\\bmob|entry exclusion", text) ~ "Conjugation",
      grepl("replication|partition|\\bPar[AB]\\b|\\bstb|antirestriction|\\bard|toxin|antitoxin", text) ~ "Replication & maintenance",
      grepl("arsenate|\\bArs[A-Z]|\\bars", text) ~ "Arsenic resistance",
      grepl("hypothetical", product) ~ "Hypothetical",
      TRUE ~ "Other"
    )
  ) %>%
  left_join(plasmids, by = "cluster") %>%
  arrange(factor(cluster, levels = plasmids$cluster), start) %>%
  select(cluster, host, accession, locus_tag, gene, product, category, start, end, strand)

# Nucleotide alignment between neighbouring plasmids; short repeats are left out
pQEB1_alignment <- mummer_alignment(
  path = dir,
  cluster = plasmids$cluster,
  seqtype = "nucleotide",
  maptype = "one-to-one"
) %>%
  filter(length1 >= 500) %>%
  select(cluster1, start1, end1, cluster2, start2, end2, length1, length2, identity)
rownames(pQEB1_alignment) <- NULL

usethis::use_data(pQEB1_plasmids, pQEB1_alignment, overwrite = TRUE)
