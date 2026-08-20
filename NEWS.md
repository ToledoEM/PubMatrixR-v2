# PubMatrixR 1.0.1

### Breaking changes

- `plot_pubmatrix_heatmap()` and `pubmatrix_heatmap()` now plot raw co-occurrence counts by default, and take a new `values` argument to choose otherwise. Earlier versions always plotted a percentage the docs called `intersection / union * 100`, but the totals in that formula were row and column sums of whatever matrix you passed in, not the publication counts for each term on its own. Adding an unrelated term changed the number shown in every other cell. The old calculation is still there as `values = "relative"`, now documented with that caveat. `values = "row_pct"` divides each count by its row total.

### Bug fixes

- `PubMatrix()` no longer drops columns when `A` repeats a term, or rows when `B` does. Duplicates now raise an error, and the result matrix is filled positionally instead of by name.
- Character matrices passed to the heatmap helpers keep their row names. A single-row matrix no longer comes back transposed into a single column.
- Term file parsing handles blank lines, and a `#` separator on the first or last line now gives an error that names the file instead of complaining about missing terms.
- A matrix where every value is the same now warns and draws a single-colour heatmap. It used to stop with an error.
- Heatmap validation errors no longer print internal call context.

### Other improvements

- Requests are spaced out to stay under the NCBI rate limits: 3 per second without an API key, 10 with one. Set `PubMatrixR.min_interval` to override.
- Failed queries back off exponentially between retries, and the default number of attempts (`PubMatrixR.n_tries`) is now 3. Both options are documented.
- Queries drop `usehistory=y` and set `retmax=0`, so responses no longer carry PMIDs the package never looks at.
- Added coverage reporting through covr and Codecov, and set `Config/testthat/edition: 3`.
- The test suite went from 29 assertions to 73.

# PubMatrixR 1.0.0

### Core functionality

- Refactored `PubMatrix()` to use XML parsing via `xml2::read_xml()` with
  internal helpers for count extraction, retry handling, and date-range
  validation.
- Added stricter input validation and clearer error messages for missing terms,
  malformed input files, invalid database values, invalid date ranges, and
  export arguments.
- Standardized result assembly as a matrix-like data frame with rows from `B`
  and columns from `A`, with safer URL encoding for API queries and exported
  hyperlinks.

### Heatmap helpers

- Updated heatmap documentation and behavior to display overlap percentages and
  use Euclidean clustering terminology consistently.
- Improved heatmap robustness for character inputs, NA handling, and automatic
  font scaling when saving plots.

### Tests and vignettes

- Replaced network-dependent tests with deterministic mocked tests for
  `PubMatrix()` and offline fixture-based XML parser tests.
- Expanded heatmap tests to cover file output and avoid creating `Rplots.pdf`
  during checks.
- Simplified the main vignette to an offline, CRAN-safe workflow with
  non-evaluated live-query examples.
- Removed legacy vignette/test files that depended on live web access or large
  examples.

### Packaging and documentation

- Updated package metadata for a CRAN-oriented release (`Version: 1.0.0`,
  removed `biocViews`, cleaned imports/suggests, and refreshed `LICENSE`).
- Added package-level documentation and updated `README.md`, Rd files, and
  `inst/CITATION` to match current behavior and examples.
- Expanded `.Rbuildignore` to exclude local development and check artifacts.

### Repository cleanup

- Removed pkgdown configuration/generated `doc/` artifacts and the bundled
  `pubmatrix-app` Shiny subproject files from the package source tree.

# PubMatrixR 0.9.0

- Finalized the pre-CRAN/Bioconductor-oriented package line after iterative
  "BioC optimization" updates and namespace fixes.
- Added/updated the main `PubMatrix.R` implementation and related packaging
  files during refactoring.
- Added a bundled Shiny app subproject (`pubmatrix-app`) and deployment
  metadata.
- Added OpenDocument Spreadsheet (`.ods`) export support for result output.

Relevant commits include `5618668`, `3c10f5d`, `fbc4846`, `1c52679`,
`26e6597`, `591d458`, and `80699aa`.
