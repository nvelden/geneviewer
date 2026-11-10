# The package's old website as redirects to the new one
#
# The documentation moved from https://nvelden.github.io/geneviewer (a pkgdown
# site in docs/, served by GitHub Pages from main) to
# https://nvelden.github.io/geneviewer-docs/. GitHub Pages cannot redirect on
# the server, so every address of the old site becomes a small page that
# sends the reader on: it goes to the new page at once, keeps the #section of
# the link, and names the new page as canonical for search engines.
#
# Usage, from the package root:
#
#   Rscript dev/make_site_redirects.R
#
# Replaces docs/ with the redirect pages, and writes docs/redirects.json, the
# list of old paths with their new addresses. The docs site checks that list
# against its pages (geneviewer-docs, test/site.cjs), so a page renamed there
# cannot break an old link unnoticed.
#
# The old addresses are listed here, not read from docs/, so the script gives
# the same result when run again.

new_site <- "https://nvelden.github.io/geneviewer-docs"

articles <- c(
  index = "/getting-started?lang=r",
  geneviewer = "/getting-started?lang=r",
  Examples = "/examples?lang=r",
  LoadGenBankFiles = "/r/articles/genbank",
  LoadGFF = "/r/articles/gff",
  LoadFastaFiles = "/r/articles/fasta",
  BLASTP = "/r/articles/protein-alignment",
  MUMMER = "/r/articles/mummer",
  Transcripts = "/r/articles/transcripts",
  Tn7_CRISPR = "/r/articles/published-figure"
)

# The reference pages of the old site: one per help page, under the same name
# on the new site
reference <- c(
  "BRCA1_splice_variants", "GC_align", "GC_annotation", "GC_chart", "GC_cluster",
  "GC_clusterFooter", "GC_clusterLabel", "GC_clusterTitle", "GC_color",
  "GC_coordinates", "GC_element_style", "GC_events", "GC_genes", "GC_grid",
  "GC_labels", "GC_legend", "GC_links", "GC_normalize", "GC_pattern", "GC_scale",
  "GC_scaleBar", "GC_sequence", "GC_theme", "GC_title", "GC_toolbar", "GC_tooltip",
  "GC_trackMouse", "GC_transcript", "GC_zoom", "GC_zoom_to", "coronavirus_genomes",
  "erythromycin_BlastP", "erythromycin_cluster", "gbk_features_to_df",
  "genbank_to_fasta", "geneviewer-shiny", "geneviewer_palette", "geneviewer_theme",
  "get_introns", "hs_dystrophin_transcripts", "human_hox_genes", "mummer_alignment",
  "ophA_clusters", "pQEB1_alignment", "pQEB1_plasmids", "protein_blast", "read_bed",
  "read_fasta", "read_gbk", "read_gff", "reexports", "tn7_crispr_elements"
)
missing <- setdiff(reference, sub("\\.Rd$", "", list.files("man", "\\.Rd$")))
if (length(missing)) {
  stop("No help page any more for: ", paste(missing, collapse = ", "),
       ". Give these old pages a new address by hand.", call. = FALSE)
}

redirects <- c(
  "index.html" = "/",
  "404.html" = "/",
  "authors.html" = "/",
  "LICENSE.html" = "/",
  "LICENSE-text.html" = "/",
  "news/index.html" = "/r/news",
  "reference/index.html" = "/r/reference/",
  stats::setNames(unname(articles), paste0("articles/", names(articles), ".html")),
  stats::setNames(paste0("/r/reference/", reference), paste0("reference/", reference, ".html"))
)

page <- function(target) {
  url <- paste0(new_site, target)
  c(
    "<!DOCTYPE html>",
    "<html lang=\"en\">",
    "<head>",
    "<meta charset=\"utf-8\">",
    "<title>The geneviewer documentation has moved</title>",
    # Keeps this address verified in Google Search Console, as the pkgdown
    # site was; the new site has the same tag
    "<meta name=\"google-site-verification\" content=\"yTGAUIDwbj1rCz39OTZSzq5wZAPsbzfL9O_Gc7u6w14\">",
    sprintf("<link rel=\"canonical\" href=\"%s\">", url),
    sprintf("<meta http-equiv=\"refresh\" content=\"0; url=%s\">", url),
    # With the #section of the link, which the line above cannot keep
    sprintf("<script>location.replace(%s + location.hash);</script>", jsonlite::toJSON(url, auto_unbox = TRUE)),
    "</head>",
    "<body>",
    sprintf("<p>The geneviewer documentation has moved to <a href=\"%s\">%s</a>.</p>", url, url),
    "</body>",
    "</html>"
  )
}

favicon <- if (file.exists("docs/favicon.ico")) readBin("docs/favicon.ico", "raw", file.size("docs/favicon.ico"))
unlink("docs", recursive = TRUE)
for (path in names(redirects)) {
  file <- file.path("docs", path)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines(page(redirects[[path]]), file)
}
if (!is.null(favicon)) writeBin(favicon, "docs/favicon.ico")
# Served as they are, without Jekyll
invisible(file.create("docs/.nojekyll"))
writeLines(
  jsonlite::toJSON(list(site = new_site, redirects = as.list(redirects)), auto_unbox = TRUE, pretty = TRUE),
  "docs/redirects.json"
)
cat(length(redirects), "redirect pages written to docs/\n")
