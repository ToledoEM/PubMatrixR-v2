## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 14,
  fig.height = 12,
  fig.align = "center",
  warning = FALSE,
  message = FALSE,
  out.width = "100%",
  dpi = 150
)

## ----setup, message=FALSE-----------------------------------------------------
library(PubMatrixR)
library(knitr)
library(kableExtra)
# library(msigdf)
library(dplyr)
library(pheatmap)
library(ggplot2)

## ----gene_extraction, eval = FALSE--------------------------------------------
# # Extract WNT-related genes
# A <- msigdf::msigdf.human %>%
#   dplyr::filter(grepl(geneset, pattern = "wnt", ignore.case = TRUE)) %>%
#   dplyr::pull(symbol) %>%
#   unique()
#
# # Extract obesity-related genes
# B <- msigdf::msigdf.human %>%
#   dplyr::filter(grepl(geneset, pattern = "obesity", ignore.case = TRUE)) %>%
#   dplyr::pull(symbol) %>%
#   unique()
#
# # Sample genes for demonstration (making them equal in length)
# A <- sample(A, 10, replace = FALSE)
# B <- sample(B, 10, replace = FALSE)

## ----example_genes------------------------------------------------------------
# WNT signaling pathway genes
A <- c("WNT1", "WNT2", "WNT3A", "WNT5A", "WNT7B", "CTNNB1", "DVL1")

# Obesity-related genes
B <- c("LEPR", "ADIPOQ", "PPARG", "TNF", "IL6", "ADRB2", "INSR")


## ----pubmatrix_analysis-------------------------------------------------------
# Run actual PubMatrix analysis
current_year <- format(Sys.Date(), "%Y")
result <- PubMatrix(
  A = A,
  B = B,
  Database = "pubmed",
  daterange = c(2020, current_year),
  outfile = "pubmatrix_result"
)

## ----results_table------------------------------------------------------------
kable(result,
  caption = "Co-occurrence Matrix: WNT Genes vs Obesity Genes (Publication Counts)",
  align = "c",
  format = "html"
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) %>%
  kableExtra::add_header_above(c(" " = 1, "Obesity Genes" = length(A)))

## ----bar_plots, fig.width=14, fig.height=10, out.width="100%", dpi=150--------
# Create data frame for List A genes (rows) colored by List B genes (columns)
a_genes_data <- data.frame(
  gene = rownames(result),
  total_pubs = rowSums(result),
  stringsAsFactors = FALSE
)

# Add color coding based on max overlap with B genes
a_genes_data$max_b_gene <- apply(result, 1, function(x) colnames(result)[which.max(x)])
a_genes_data$max_overlap <- apply(result, 1, max)

# Create data frame for List B genes (columns) colored by List A genes (rows)
b_genes_data <- data.frame(
  gene = colnames(result),
  total_pubs = colSums(result),
  stringsAsFactors = FALSE
)

# Add color coding based on max overlap with A genes
b_genes_data$max_a_gene <- apply(result, 2, function(x) rownames(result)[which.max(x)])
b_genes_data$max_overlap <- apply(result, 2, max)

# Plot A genes colored by their strongest B gene partner
p1 <- ggplot(a_genes_data, aes(x = reorder(gene, total_pubs), y = total_pubs, fill = max_b_gene)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "List A Genes by Publication Count",
    subtitle = "Colored by strongest List B gene partner",
    x = "Genes (List A)",
    y = "Total Publications",
    fill = "Strongest B Partner"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set1")


# Plot B genes colored by their strongest A gene partner
p2 <- ggplot(b_genes_data, aes(x = reorder(gene, total_pubs), y = total_pubs, fill = max_a_gene)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "List B Genes by Publication Count",
    subtitle = "Colored by strongest List A gene partner",
    x = "Genes (List B)",
    y = "Total Publications",
    fill = "Strongest A Partner"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set1")


print(p1)
print(p2)

## ----heatmap_with_numbers_asymmetric, fig.width=16, fig.height=14, out.width="100%", dpi=150, fig.cap="Overlap percentage heatmap with values displayed in each cell"----
plot_pubmatrix_heatmap(result,
  title = "WNT-Obesity Gene Overlap Percentages",
  show_numbers = TRUE,
  width = 16,
  height = 14
)

## ----heatmap_clean_asymmetric, fig.width=16, fig.height=14, out.width="100%", dpi=150, fig.cap="Co-occurrence heatmap without numbers for better visual clarity"----
plot_pubmatrix_heatmap(result,
  title = "WNT-Obesity Gene Co-occurrence (Clean)",
  show_numbers = FALSE,
  width = 16,
  height = 14
)

## ----asymmetric_example-------------------------------------------------------
A <- c("NCOR2", "NCSTN", "NKD1", "NOTCH1", "NOTCH4", "NUMB", "PPARD", "PSEN2", "PTCH1", "RBPJ", "SKP2", "TCF7", "TP53")


B <- c("EIF1", "EIF1AX", "EIF2B1", "EIF2B2", "EIF2B3", "EIF2B4", "EIF2B5", "EIF2S1", "EIF2S2", "EIF2S3", "ELAVL1")


# Run actual PubMatrix analysis
current_year <- format(Sys.Date(), "%Y")
result <- PubMatrix(
  A = A,
  B = B,
  Database = "pubmed",
  daterange = c(2020, current_year),
  outfile = "pubmatrix_result"
)

## ----results_table_asymmetric-------------------------------------------------
kable(result,
  caption = "Co-occurrence Matrix: Longer Lists (Publication Counts)",
  align = "c",
  format = "html"
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) %>%
  kableExtra::add_header_above(c(" " = 1, "Genes" = length(A)))

## ----bar_plots_asymmetric, fig.width=14, fig.height=10, out.width="100%", dpi=150----
# Create data frame for List A genes (rows) colored by List B genes (columns)
a_genes_data2 <- data.frame(
  gene = rownames(result),
  total_pubs = rowSums(result),
  stringsAsFactors = FALSE
)

# Add color coding based on max overlap with B genes
a_genes_data2$max_b_gene <- apply(result, 1, function(x) colnames(result)[which.max(x)])
a_genes_data2$max_overlap <- apply(result, 1, max)

# Create data frame for List B genes (columns) colored by List A genes (rows)
b_genes_data2 <- data.frame(
  gene = colnames(result),
  total_pubs = colSums(result),
  stringsAsFactors = FALSE
)

# Add color coding based on max overlap with A genes
b_genes_data2$max_a_gene <- apply(result, 2, function(x) rownames(result)[which.max(x)])
b_genes_data2$max_overlap <- apply(result, 2, max)

# Plot A genes colored by their strongest B gene partner
p3 <- ggplot(a_genes_data2, aes(x = reorder(gene, total_pubs), y = total_pubs, fill = max_b_gene)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "List A Genes by Publication Count (Asymmetric)",
    subtitle = "Colored by strongest List B gene partner",
    x = "Genes (List A)",
    y = "Total Publications",
    fill = "Strongest B Partner"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set1")


# Plot B genes colored by their strongest A gene partner
p4 <- ggplot(b_genes_data2, aes(x = reorder(gene, total_pubs), y = total_pubs, fill = max_a_gene)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "List B Genes by Publication Count (Asymmetric)",
    subtitle = "Colored by strongest List A gene partner",
    x = "Genes (List B)",
    y = "Total Publications",
    fill = "Strongest A Partner"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set1")


print(p3)
print(p4)

## ----heatmap_with_numbers_asymmetric2, fig.width=16, fig.height=14, out.width="100%", dpi=150, fig.cap="Overlap percentage heatmap with values displayed in each cell"----
plot_pubmatrix_heatmap(result,
  title = "Asymmetric Gene Lists Overlap Percentages",
  show_numbers = TRUE,
  width = 16,
  height = 14
)

## ----heatmap_clean_asymmetric2, fig.width=16, fig.height=14, out.width="100%", dpi=150, fig.cap="Co-occurrence heatmap without numbers for better visual clarity"----
plot_pubmatrix_heatmap(result,
  title = "Asymmetric Gene Lists Co-occurrence (Clean)",
  show_numbers = FALSE,
  width = 16,
  height = 14
)

## ----system_info--------------------------------------------------------------
sessionInfo()

## ----system_details-----------------------------------------------------------
# Additional system details
cat("Date generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n")
cat("R version:", R.version.string, "\n")
cat("Platform:", R.version$platform, "\n")
cat("Operating System:", Sys.info()["sysname"], Sys.info()["release"], "\n")
cat("User:", Sys.info()["user"], "\n")
