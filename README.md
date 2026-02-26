# PubMatrixR

<img src="https://toledoem.github.io/img/LogoPubmatrix.png" align="right" width=150/>

- Repository: [https://github.com/ToledoEM/PubMatrixR-v2](https://github.com/ToledoEM/PubMatrixR-v2)
- Original code from: [https://github.com/tslaird/PubMatrixR](https://github.com/tslaird/PubMatrixR)
- Based on paper : [PubMatrix: a tool for multiplex literature mining](https://pmc.ncbi.nlm.nih.gov/articles/PMC317283/) of **Becker KG et al. BMC Bioinformatics. 2003 Dec 10;4:61. doi: 10.1186/1471-2105-4-61**

## Overview

**PubMatrixR** is an R package that performs systematic literature searches on PubMed and PMC databases using pairwise combinations of search terms. It creates co-occurrence matrices showing the number of publications that mention both terms from two different sets, enabling researchers to explore relationships between genes, diseases, pathways, or any other biomedical concepts.

This repository maintains and extends the original `PubMatrixR` package with
improved validation, offline-safe tests/vignettes, and heatmap helpers.

## Branch Note (`dev_not_cran`)

This branch restores the full vignette workflow for development and demonstration use, and is intentionally **not for CRAN submission**.

Reasons this branch is not CRAN-safe:

- The vignette loads and demonstrates `msigdf`, which is not available from CRAN mainstream repositories.
- The vignette executes live NCBI PubMed/PMC queries during rendering (`PubMatrix()`), which requires internet access.
- Vignette output depends on external services and current data/date, so renders are not fully deterministic/reproducible for checks.
- The vignette includes an external image URL, which can trigger network fetches during rendering.
- Live API calls can be slow or rate-limited, making automated checks more fragile.

### Key Features

- **Pairwise Literature Search**: Automatically searches all combinations of terms from two vectors
- **Multiple Database Support**: Search PubMed or PMC databases via NCBI E-utilities
- **Static Visualizations**: Generate heatmaps using `pheatmap`
- **Export Capabilities**: Save results as CSV files with clickable hyperlinks to PubMed
- **Date Filtering**: Restrict searches to specific publication date ranges
- **Flexible Input**: Use vectors directly or read terms from a file
- **Progress Tracking**: Built-in progress bars for long searches

## Try it Online

**Interactive Shiny App**: [https://toledoem.shinyapps.io/pubmatrix-app/](https://toledoem.shinyapps.io/pubmatrix-app/)

No installation required - just open the link and start analyzing!

## Installation

### CRAN (after acceptance)

```r
install.packages("PubMatrixR")
```

### GitHub (development version)

```r
# Install remotes if you haven't already
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

# Install PubMatrixR
remotes::install_github("ToledoEM/PubMatrixR-v2")
```

### Dependencies

PubMatrixR requires the following R packages:

- `pbapply` - Progress bars for apply functions
- `pheatmap` - Static heatmap generation
- `xml2` - XML parsing for API responses
- `readODS` - To export results in OpenDocument Spreadsheet - Excel compatible - for hyperlink export.

## Quick Start

```r
library(PubMatrixR)

# Define two sets of search terms
genes_set1 <- c("SREBP1", "SOX4", "GLP1R")
genes_set2 <- c("NR1H4", "liver", "obesity")

# Perform the search and create a matrix
result <- PubMatrix(
  A = genes_set1,
  B = genes_set2,
  Database = "pubmed",
  daterange = c(2010, 2024),
  outfile = "my_results",
  export_format = "csv"  # Options: NULL (no export), "csv", or "ods"
)

# Create a heatmap with overlap percentages and Euclidean clustering
plot_pubmatrix_heatmap(result)
```

## Function Documentation

### PubMatrix()

The main function that performs pairwise literature searches and generates co-occurrence matrices.

#### Parameters

| Parameter | Type | Default | Description |
| --------- | ---- | ------- | ----------- |
| `file` | character | - | Path to file containing search terms (alternative to A/B vectors) |
| `A` | character vector | NULL | First set of search terms |
| `B` | character vector | NULL | Second set of search terms |
| `API.key` | character | NULL | NCBI E-utilities API key (optional, increases rate limits) |
| `Database` | character | "pubmed" | Database to search: "pubmed" or "pmc" |
| `daterange` | numeric vector | NULL | Date range as c(start_year, end_year) |
| `outfile` | character | NULL | Base filename for outputs (without extension). Required if export_format is specified. |
| `export_format` | character | NULL | Export format for the hyperlinked results matrix. Options: NULL (default, no file export), 'csv' (Excel-compatible with HYPERLINK formulas), or 'ods' (LibreOffice/OpenOffice format). |

#### Return Value

Returns a matrix-like data frame where:

- Rows correspond to terms from vector `B`
- Columns correspond to terms from vector `A`
- Each cell contains the number of publications mentioning both terms

#### File Input Format

When using the `file` parameter, the input file should contain:

```text
term1_from_A
term2_from_A
term3_from_A
#
term1_from_B
term2_from_B
term3_from_B
```

The `#` character separates the two sets of search terms.

### Heatmap Functions

PubMatrixR provides dedicated functions for creating heatmaps from PubMatrix results.

#### plot_pubmatrix_heatmap()

Creates a formatted heatmap displaying **overlap percentages** in cells, with
**Euclidean distance clustering** for row/column ordering.

**Cell Values**: Overlap percentages derived from co-occurrence counts
**Clustering Method**: Euclidean distance on the overlap percentage matrix

##### Heatmap Parameters

| Parameter | Type | Default | Description |
| --------- | ---- | ------- | ----------- |
| `matrix` | numeric matrix | - | A PubMatrix result matrix containing publication co-occurrence counts |
| `title` | character | "PubMatrix Co-occurrence Heatmap" | Heatmap title |
| `cluster_rows` | logical | TRUE | Whether to cluster rows using Euclidean distance |
| `cluster_cols` | logical | TRUE | Whether to cluster columns using Euclidean distance |
| `show_numbers` | logical | TRUE | Display overlap percentages in cells |
| `filename` | character | NULL | Optional filename to save plot |

##### Example

```r
# First generate a matrix
result <- PubMatrix(A = c("gene1", "gene2"), B = c("disease1", "disease2"))

# Create heatmap with overlap percentages and Euclidean clustering
plot_pubmatrix_heatmap(result)

# Save to file
plot_pubmatrix_heatmap(result, filename = "my_heatmap.png")
```

#### pubmatrix_heatmap()

Thin wrapper around `plot_pubmatrix_heatmap()` for quick visualization.

```r
# Quick heatmap using defaults
pubmatrix_heatmap(result, title = "Quick PubMatrix Heatmap")
```

## Examples

### Basic Usage with Gene Symbols

```r
library(PubMatrixR)

# Define gene sets
genes_of_interest <- c("TP53", "BRCA1", "EGFR", "MYC")
pathways <- c("apoptosis", "DNA repair", "cell cycle", "oncogene")

# Perform search
results <- PubMatrix(
  A = genes_of_interest,
  B = pathways,
  Database = "pubmed",
  daterange = c(2015, 2024),
  outfile = "gene_pathway_matrix"
)

# View results
print(results)
#            TP53 BRCA1 EGFR  MYC
# apoptosis  1456   234  567  890
# DNA repair  789  1456  123  234
# cell cycle 1234   456  890  567
# oncogene    567   123  789 1456
```

### Using MSigDB Gene Sets

```r
library(PubMatrixR)
library(msigdf)
library(dplyr)

# Extract gene symbols from MSigDB pathways
wnt_genes <- msigdf::msigdf.human %>%
  filter(grepl("wnt", geneset, ignore.case = TRUE)) %>%
  pull(symbol) %>%
  unique() %>%
  sample(10)  # Sample 10 genes for demonstration

obesity_genes <- msigdf::msigdf.human %>%
  filter(grepl("obesity", geneset, ignore.case = TRUE)) %>%
  pull(symbol) %>%
  unique() %>%
  sample(10)  # Sample 10 genes for demonstration

# Search for co-occurrences
wnt_obesity_matrix <- PubMatrix(
  A = wnt_genes,
  B = obesity_genes,
  Database = "pubmed",
  outfile = "wnt_obesity_cooccurrence"
)

# Create heatmap with overlap percentages and Euclidean clustering
plot_pubmatrix_heatmap(wnt_obesity_matrix)
```

### Using File Input

Create a file called `search_terms.txt`:

```text
insulin
glucose
diabetes
metabolic syndrome
#
liver
pancreas
adipose tissue
muscle
```

Then run:

```r
results <- PubMatrix(
  file = "search_terms.txt",
  Database = "pubmed",
  daterange = c(2020, 2024),
  outfile = "metabolic_tissue_matrix"
)

# Create heatmap visualization
plot_pubmatrix_heatmap(results)
```

### Advanced Example with API Key

```r
# Get better rate limits with an API key
results <- PubMatrix(
  A = c("CRISPR", "base editing", "prime editing"),
  B = c("therapeutic", "clinical trial", "safety"),
  API.key = "your_ncbi_api_key_here",
  Database = "pubmed",
  daterange = c(2020, 2024),
  outfile = "gene_editing_therapeutics"
)
```

## Output Files

When `outfile` and `export_format` parameters are specified, PubMatrixR generates a results file with clickable hyperlinks:

### Export Format Options

| Format | Parameter Value | File Extension | Use Case |
| ------ | --------------- | -------------- | -------- |
| **No Export** | `export_format = NULL` (default) | - | Results returned only to R environment, no file saved |
| **CSV** | `export_format = "csv"` | `.csv` | Excel-compatible format with HYPERLINK formulas for direct linking to PubMed searches |
| **ODS** | `export_format = "ods"` | `.ods` | LibreOffice/OpenOffice format with embedded hyperlinks, better for cross-platform compatibility |

### Output File Format

The output filename follows the pattern: `{outfile}_result.{extension}`

All formats include:

- **Row names**: Terms from vector B
- **Column names**: Terms from vector A
- **Cell values**: Publication co-occurrence counts with clickable hyperlinks to the corresponding PubMed search

### Output Examples

```r
# No file export - results only in R
result <- PubMatrix(A = genes, B = diseases, Database = "pubmed")

# Export as CSV with hyperlinks
result <- PubMatrix(
  A = genes, 
  B = diseases, 
  Database = "pubmed",
  outfile = "my_results",
  export_format = "csv"
)
# Creates: my_results_result.csv

# Export as ODS (LibreOffice format)
result <- PubMatrix(
  A = genes, 
  B = diseases, 
  Database = "pubmed",
  outfile = "my_results",
  export_format = "ods"
)
# Creates: my_results_result.ods
```

## Visualization

Create heatmaps using the dedicated heatmap functions:

```r
# Basic heatmap with overlap percentages and Euclidean clustering
plot_pubmatrix_heatmap(your_matrix)

# Save heatmap to file
plot_pubmatrix_heatmap(your_matrix,
                       filename = "my_heatmap.png",
                       title = "Custom Title")
```

**Features of the visualization:**

- **Cell Values**: Publication co-occurrence counts between gene pairs
- **Clustering Method**: Euclidean distance on overlap percentages
- **Color Scale**: Custom red gradient from light pink (`#fee5d9`) to dark red (`#99000d`) representing publication counts
- **Legend**: Shows "Publication Count" scale for interpreting cell values
- **Publication Quality**: High-resolution output suitable for manuscripts and presentations

## Performance Notes

- **Rate Limiting**: NCBI allows 3 requests per second without an API key, 10 requests per second with a key
- **Search Time**: Depends on matrix size (A × B combinations) and network speed
- **Progress Tracking**: Built-in progress bars show search completion status
- **Memory Usage**: Results are stored in memory; very large matrices may require substantial RAM

## API Key Setup

To improve search speed and avoid rate limiting:

1. Create a free NCBI account at <https://account.ncbi.nlm.nih.gov/>
2. Go to Account Settings → API Key Management
3. Generate a new API key
4. Use the key in the `API.key` parameter

*Reference: [NCBI E-utilities documentation](https://www.ncbi.nlm.nih.gov/books/NBK25497/)*

## Use Cases

PubMatrixR is particularly useful for:

- **Gene-Disease Association Studies**: Explore literature connections between genes and diseases
- **Pathway Analysis**: Investigate co-occurrence of genes within or across biological pathways
- **Drug-Target Research**: Analyze relationships between compounds and potential targets
- **Systematic Literature Reviews**: Quantify research coverage across multiple topics
- **Knowledge Gap Identification**: Find under-researched combinations of terms
- **Bibliometric Analysis**: Measure research activity in specific domains

## Troubleshooting

### Common Issues

**Empty Results**: If many searches return 0 results, try:

- Using broader search terms
- Expanding the date range
- Checking spelling of scientific terms
- Using alternative gene names or synonyms

**Rate Limiting Errors**: If you encounter HTTP 429 errors:

- Obtain and use an NCBI API key
- Reduce the size of your search matrix
- Add delays between searches

**Long Search Times**: For large matrices:

- Consider breaking into smaller sub-searches
- Use more specific date ranges
- Filter gene lists to most relevant terms

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
