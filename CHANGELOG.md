# Changelog

All notable changes to this project are documented in this file.

Note: Versions `2.0`, `1.0`, and `1.0.0` are backfilled from git history
(`DESCRIPTION` version changes and commit messages). The current `0.1.0`
entry starts a CRAN-oriented versioning reset, so older legacy entries below
have higher version numbers and are listed in reverse chronological order.

## 0.1.0 (2026-02-23)

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

- Updated package metadata for a CRAN-oriented release (`Version: 0.1.0`,
  removed `biocViews`, cleaned imports/suggests, and refreshed `LICENSE`).
- Added package-level documentation and updated `README.md`, Rd files, and
  `inst/CITATION` to match current behavior and examples.
- Expanded `.Rbuildignore` to exclude local development and check artifacts.

### Repository cleanup

- Removed pkgdown configuration/generated `doc/` artifacts and the bundled
  `pubmatrix-app` Shiny subproject files from the package source tree.

## 1.0.0 (2025-12-05)

- Finalized the pre-CRAN/Bioconductor-oriented package line after iterative
  “BioC optimization” updates and namespace fixes.
- Added/updated the main `PubMatrix.R` implementation and related packaging
  files during refactoring.
- Added a bundled Shiny app subproject (`pubmatrix-app`) and deployment
  metadata.
- Added OpenDocument Spreadsheet (`.ods`) export support for result output.

Relevant commits include `5618668`, `3c10f5d`, `fbc4846`, `1c52679`,
`26e6597`, `591d458`, and `80699aa`.

## 1.0 (2025-11-22)

- Reworked package metadata toward Bioconductor submission conventions,
  including `biocViews` and related packaging changes.
- Included pkgdown/site configuration and general repository polishing updates
  before the Bioconductor-focused packaging pass.
- Changed package versioning from `2.0` to `1.0` in `DESCRIPTION`.

Relevant commits include `57fc306`, `8984cea`, `9656b10`, `92dde59`,
`c97536d`, `7a26d4a`, `b26670e`, and `8ee8cb9`.

## 2.0 (2025-10-24)

- Initial repository import/fork setup with the original package structure and
  `PubMatrixR` package metadata.

Relevant commit: `9137b46` (initial commit).
