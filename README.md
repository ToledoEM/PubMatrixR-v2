# PubMatrixR v2

[![R](https://img.shields.io/badge/R-%3E%3D3.1.0-blue.svg)](https://www.r-project.org/)
[![License: GPL-2](https://img.shields.io/badge/License-GPL--2-green.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)

- Forked from: [https://github.com/tslaird/PubMatrixR](https://github.com/tslaird/PubMatrixR)
- Based on paper : [PubMatrix: a tool for multiplex literature mining](https://pmc.ncbi.nlm.nih.gov/articles/PMC317283/) of **Becker KG et al. BMC Bioinformatics. 2003 Dec 10;4:61. doi: 10.1186/1471-2105-4-61**



## Overview

**PubMatrixR** is an R package that performs systematic literature searches on PubMed and PMC databases using pairwise combinations of search terms. It creates co-occurrence matrices showing the number of publications that mention both terms from two different sets, enabling researchers to explore relationships between genes, diseases, pathways, or any other biomedical concepts.

### Key Features

- **Pairwise Literature Search**: Automatically searches all combinations of terms from two vectors
- **Multiple Database Support**: Search PubMed or PMC databases via NCBI E-utilities
- **Static Visualizations**: Generate heatmaps using `pheatmap`
- **Export Capabilities**: Save results as CSV files with clickable hyperlinks to PubMed
- **Date Filtering**: Restrict searches to specific publication date ranges
- **Flexible Input**: Use vectors directly or read terms from a file
- **Progress Tracking**: Built-in progress bars for long searches

## Installation

You can install PubMatrixR from GitHub using:

```r
# Install devtools if you haven't already
if (!require(devtools)) install.packages("devtools")

# Install PubMatrixR
devtools::install_github("ToledoEM/PubMatrixR")
```

### Dependencies

PubMatrixR requires the following R packages:

- `pbapply` - Progress bars for apply functions
- `stringr` - String manipulation
- `pheatmap` - Static heatmap generation
- `xml2` - XML parsing for API responses

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
  outfile = "my_results"
)

# Create a heatmap with Jaccard distance clustering
plot_pubmatrix_heatmap(result)
```

## Function Documentation

### PubMatrix()

The main function that performs pairwise literature searches and generates co-occurrence matrices.

#### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `file` | character | - | Path to file containing search terms (alternative to A/B vectors) |
| `A` | character vector | NULL | First set of search terms |
| `B` | character vector | NULL | Second set of search terms |
| `API.key` | character | NULL | NCBI E-utilities API key (optional, increases rate limits) |
| `Database` | character | "pubmed" | Database to search: "pubmed" or "pmc" |
| `daterange` | numeric vector | NULL | Date range as c(start_year, end_year) |
| `outfile` | character | NULL | Base filename for outputs (without extension) |

#### Return Value

Returns a numeric matrix where:

- Rows correspond to terms from vector `A`
- Columns correspond to terms from vector `B`
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

Creates a formatted heatmap displaying **publication co-occurrence counts** in cells, with **Jaccard distance clustering** for row/column ordering. Jaccard distance is the clustering method used to group genes with similar co-occurrence patterns.

**Cell Values**: Publication co-occurrence counts
**Clustering Method**: Jaccard distance calculated as 1 - (intersection/union) based on presence/absence patterns

##### Heatmap Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `matrix` | numeric matrix | - | A PubMatrix result matrix containing publication co-occurrence counts |
| `title` | character | "PubMatrix Co-occurrence Heatmap" | Heatmap title |
| `cluster_rows` | logical | TRUE | Whether to cluster rows using Jaccard distance |
| `cluster_cols` | logical | TRUE | Whether to cluster columns using Jaccard distance |
| `show_numbers` | logical | TRUE | Display publication counts in cells |
| `filename` | character | NULL | Optional filename to save plot |

##### Example

```r
# First generate a matrix
result <- PubMatrix(A = c("gene1", "gene2"), B = c("disease1", "disease2"))

# Create heatmap with Jaccard clustering
plot_pubmatrix_heatmap(result)

# Save to file
plot_pubmatrix_heatmap(result, filename = "my_heatmap.png")
```

#### pubmatrix_heatmap()

Alternative heatmap function with additional customization options.

```r
# Create customized heatmap
pubmatrix_heatmap(result,
                  color_scheme = "viridis",
                  cluster_method = "complete")
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
#      apoptosis DNA repair cell cycle oncogene
# TP53       1456        789       1234      567
# BRCA1       234        1456        456      123
# EGFR        567         123        890      789
# MYC         890         234        567     1456
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

# Create heatmap with Jaccard distance clustering
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

When `outfile` is specified, PubMatrixR generates:

1. **CSV Matrix** (`{outfile}_result.csv`): Contains the co-occurrence counts with clickable hyperlinks to PubMed searches

The CSV file includes Excel-compatible hyperlink formulas that link directly to the corresponding PubMed search results.

## Visualization

Create heatmaps using the dedicated heatmap functions:

```r
# Basic heatmap with Jaccard distance clustering and red gradient colors
plot_pubmatrix_heatmap(your_matrix)

# Save heatmap to file
plot_pubmatrix_heatmap(your_matrix,
                       filename = "my_heatmap.png",
                       title = "Custom Title")
```

**Features of the visualization:**

- **Cell Values**: Publication co-occurrence counts between gene pairs
- **Clustering Method**: Jaccard distance based on presence/absence patterns (1 - intersection/union)
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

1. Create a free NCBI account at <https://www.ncbi.nlm.nih.gov/account/>
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
