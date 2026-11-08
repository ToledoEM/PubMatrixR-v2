# Query 'PubMed' or 'PMC' and Build a Pairwise Co-occurrence Matrix

\`PubMatrix()\` counts publications for all pairwise combinations of two
term sets using the 'NCBI' Entrez 'E-utilities' API. It returns a
matrix-like data frame with rows corresponding to terms in \`B\` and
columns corresponding to terms in \`A\`.

## Usage

``` r
PubMatrix(
  file = NULL,
  A = NULL,
  B = NULL,
  API.key = NULL,
  Database = "pubmed",
  daterange = NULL,
  outfile = NULL,
  export_format = NULL
)
```

## Arguments

- file:

  Optional path to a text file containing search terms. The file must
  contain a \`#\` separator line between the \`A\` and \`B\` term lists.
  Used only when \`A\` and \`B\` are both \`NULL\`.

- A:

  Character vector of search terms for matrix columns.

- B:

  Character vector of search terms for matrix rows.

- API.key:

  Optional 'NCBI' API key.

- Database:

  Character scalar. One of \`"pubmed"\` or \`"pmc"\`.

- daterange:

  Optional numeric vector of length 2 giving \`c(start_year,
  end_year)\`.

- outfile:

  Optional output file stem used when \`export_format\` is set.

- export_format:

  Optional export format: \`"csv"\` or \`"ods"\`.

## Value

A data frame of publication counts with rows named by \`B\` and columns
named by \`A\`.

## Details

Examples and vignettes should avoid live web queries during package
checks. This function performs live requests to 'NCBI' and may fail when
there is no internet connectivity or when the service is unavailable.

Terms are trimmed of surrounding whitespace, and duplicate terms within
\`A\` or within \`B\` are rejected. When reading from \`file\`, blank
lines on either side of the \`#\` separator are ignored.

Requests are paced to respect the 'NCBI' rate limits (3 requests per
second without an API key, 10 with one). Two options tune the request
behaviour:

- \`PubMatrixR.n_tries\` - number of attempts per query before giving up
  (default \`3\`). Failed attempts back off exponentially.

- \`PubMatrixR.min_interval\` - seconds to wait between requests
  (default \`0.34\` without an API key, \`0.11\` with one). Set to \`0\`
  to disable pacing.

## Examples

``` r
if (FALSE) { # \dontrun{
A <- c("WNT1", "WNT2")
B <- c("FZD1", "FZD2")
result <- PubMatrix(A = A, B = B, Database = "pubmed", daterange = c(2020, 2023))
print(result)
} # }

try(PubMatrix(A = NULL, B = NULL, file = NULL))
#> Error : Either provide vectors A and B, or specify a file containing search terms.
try(PubMatrix(A = "a", B = "b", Database = "invalid_db"))
#> Error : Database must be one of 'pubmed' or 'pmc'.
```
