# Create a simple heatmap from PubMatrix results

A simplified version of plot_pubmatrix_heatmap for quick visualization

## Usage

``` r
pubmatrix_heatmap(
  matrix,
  title = "PubMatrix Results",
  values = c("raw", "row_pct", "relative")
)
```

## Arguments

- matrix:

  A numeric matrix from PubMatrix results

- title:

  Character string for the heatmap title

- values:

  Character scalar passed to \[plot_pubmatrix_heatmap()\]. Defaults to
  \`"raw"\` (co-occurrence counts).

## Value

A pheatmap object (invisible)

## Examples

``` r
# Create a small test matrix
test_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
rownames(test_matrix) <- c("Gene1", "Gene2")
colnames(test_matrix) <- c("GeneA", "GeneB")

# Create simple heatmap (wrapper)
pubmatrix_heatmap(test_matrix, title = "Simple Test Heatmap")


# Equivalent pheatmap call
pheatmap::pheatmap(
  test_matrix,
  main = "Simple Test Heatmap (pheatmap)",
  color = colorRampPalette(c("#fee5d9", "#cb181d"))(100),
  display_numbers = TRUE,
  fontsize = 16,
  fontsize_number = 14
)
```
