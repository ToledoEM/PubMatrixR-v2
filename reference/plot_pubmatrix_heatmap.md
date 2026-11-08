# Create a formatted heatmap from PubMatrix results

This function creates a heatmap displaying overlap percentages derived
from a PubMatrix result matrix, with Euclidean distance clustering for
rows and columns.

## Usage

``` r
plot_pubmatrix_heatmap(
  matrix,
  values = c("raw", "row_pct", "relative"),
  title = "PubMatrix Co-occurrence Heatmap",
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  show_numbers = TRUE,
  color_palette = NULL,
  filename = NULL,
  width = 10,
  height = 8,
  cellwidth = NA,
  cellheight = NA,
  scale_font = TRUE
)
```

## Arguments

- matrix:

  A data frame or matrix from PubMatrix results containing publication
  co-occurrence counts

- values:

  Character scalar selecting what is plotted in each cell. One of
  \`"raw"\` (default, the co-occurrence counts as returned by
  \[PubMatrix()\]), \`"row_pct"\` (each cell as a percentage of its row
  total), or \`"relative"\` (see Details).

- title:

  Character string for the heatmap title. Default is "PubMatrix
  Co-occurrence Heatmap"

- cluster_rows:

  Logical value determining if rows should be clustered using Euclidean
  distance. Default is TRUE

- cluster_cols:

  Logical value determining if columns should be clustered using
  Euclidean distance. Default is TRUE

- show_numbers:

  Logical value determining if overlap percentage values should be
  displayed in cells. Default is TRUE

- color_palette:

  Color palette for the heatmap. Default uses a red gradient color scale

- filename:

  Optional filename to save the heatmap. If NULL, displays the plot

- width:

  Width of saved plot in inches. Default is 10

- height:

  Height of saved plot in inches. Default is 8

- cellwidth:

  Optional numeric cell width for pheatmap (in pixels). Default \`NA\`
  lets pheatmap auto-size.

- cellheight:

  Optional numeric cell height for pheatmap (in pixels). Default \`NA\`
  lets pheatmap auto-size.

- scale_font:

  Logical value determining if font size should scale with cell size.
  Default is TRUE

## Value

A pheatmap object (invisible). The matrix actually plotted is attached
as the \`"plotted_values"\` attribute.

## Details

Rows and columns are clustered with Euclidean distance. NA values in the
input matrix are converted to 0 before any calculation.

The \`values\` argument controls what each cell shows:

- \`"raw"\` (default) - the co-occurrence counts themselves.

- \`"row_pct"\` - each count as a percentage of its row total, useful
  for comparing how one row's attention is distributed across columns.

- \`"relative"\` - \`count / (row_total + col_total - count) \* 100\`,
  where the totals are sums over the supplied matrix.

Note that \`"relative"\` is \*\*not\*\* a Jaccard index and is \*\*not
comparable across runs\*\*. Its totals are sums over whichever partner
terms happen to be in the matrix, not the marginal publication counts
for each term, so adding an unrelated term changes the value reported
for every existing cell. A true Jaccard index would require the
single-term counts, which \[PubMatrix()\] does not fetch. Use it only to
compare cells within one fixed matrix.

## Examples

``` r
# Create a small test matrix
test_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
rownames(test_matrix) <- c("Gene1", "Gene2")
colnames(test_matrix) <- c("GeneA", "GeneB")

# Create heatmap using the helper (plots the raw counts by default)
plot_pubmatrix_heatmap(test_matrix, title = "Test Heatmap")


# Percentage views are available too:
plot_pubmatrix_heatmap(test_matrix, values = "row_pct", title = "Row %")


# Equivalent using pheatmap directly:
pheatmap::pheatmap(
  test_matrix,
  main = "Test Heatmap (pheatmap)",
  color = colorRampPalette(c("#fee5d9", "#cb181d"))(100),
  display_numbers = TRUE,
  fontsize = 16,
  fontsize_number = 14,
  border_color = "lightgray",
  show_rownames = TRUE,
  show_colnames = TRUE
)
```
