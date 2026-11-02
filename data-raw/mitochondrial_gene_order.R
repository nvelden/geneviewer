# Rebuild the mitochondrial_gene_order dataset from public RefSeq records.
# Jameson D et al. OGRe: a relational database for comparative analysis of
# mitochondrial genomes. Nucleic Acids Res. 2003;31:202-206, Fig. 3.
# doi: 10.1093/nar/gkg077
library(geneviewer)
library(dplyr)

genomes <- data.frame(
  cluster = c("Locusta migratoria", "Homo sapiens"),
  accession = c("NC_001712.1", "NC_012920.1"),
  genome_length = c(15722L, 16569L)
)
record_dir <- file.path(tempdir(), "mitochondrial_gene_order")
dir.create(record_dir, showWarnings = FALSE)
for (i in seq_len(nrow(genomes))) {
  download.file(
    paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nuccore&id=",
           genomes$accession[i], "&rettype=gbwithparts&retmode=text"),
    file.path(record_dir, paste0(genomes$cluster[i], ".gbk"))
  )
}
gbk <- read_gbk(record_dir, origin = FALSE)
mitochondrial_gene_order <- bind_rows(
  gbk_features_to_df(gbk, feature = "CDS", keys = c("region", "gene", "product")),
  gbk_features_to_df(gbk, feature = "rRNA", keys = c("region", "gene", "product"))
) %>%
  mutate(
    gene = case_when(product == "s-rRNA" ~ "RNS",
                     product == "l-rRNA" ~ "RNL", TRUE ~ gene),
    block = case_when(
      gene %in% c("ND4L", "ND4", "ND5") ~ "ND4L / ND4 / ND5",
      gene == "ND6" ~ "ND6",
      gene == "CYTB" ~ "CYTB",
      gene %in% c("RNS", "RNL", "ND1") ~ "RNS / RNL / ND1",
      TRUE ~ "Conserved flanks"
    )
  ) %>%
  left_join(genomes, by = "cluster") %>%
  group_by(cluster) %>%
  # Rotate the circular order to start at COX1; keep original coordinates.
  mutate(circular_start = (start - start[gene == "COX1"]) %% first(genome_length)) %>%
  arrange(circular_start, .by_group = TRUE) %>%
  ungroup() %>%
  arrange(factor(cluster, levels = genomes$cluster)) %>%
  select(cluster, accession, genome_length, gene, product, block, start, end, strand)
rownames(mitochondrial_gene_order) <- NULL
stopifnot(nrow(mitochondrial_gene_order) == 30L,
          all(table(mitochondrial_gene_order$cluster) == 15L))
save(mitochondrial_gene_order, file = "data/mitochondrial_gene_order.rda", compress = "xz")
