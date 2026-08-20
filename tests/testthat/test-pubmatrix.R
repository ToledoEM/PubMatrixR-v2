library(testthat)
library(PubMatrixR)

mock_counts <- function() {
  c(
    "A1 AND B1" = 11,
    "A1 AND B2" = 12,
    "A2 AND B1" = 21,
    "A2 AND B2" = 22
  )
}

# Builds a fetch stub for callers to mock.
mock_fetch_counts <- function(mapping = mock_counts()) {
  function(base_url, encoded_term, n_tries = 2L) {
    decoded <- utils::URLdecode(encoded_term)
    if (!decoded %in% names(mapping)) {
      stop("Unexpected mocked term: ", decoded, call. = FALSE)
    }
    unname(mapping[[decoded]])
  }
}

test_that("PubMatrix validates inputs before attempting network access", {
  expect_error(
    PubMatrix(A = NULL, B = NULL, file = NULL),
    "Either provide vectors A and B, or specify a file containing search terms"
  )

  expect_error(
    PubMatrix(A = "a", B = "b", Database = "invalid_db"),
    "Database must be one of 'pubmed' or 'pmc'"
  )

  expect_error(
    PubMatrix(A = "a", B = "b", export_format = "xlsx", outfile = tempfile()),
    "export_format must be either 'csv' or 'ods'"
  )

  expect_error(
    PubMatrix(A = "a", B = "b", export_format = "csv", outfile = NULL),
    "outfile must be provided when export_format is specified"
  )

  expect_error(
    PubMatrix(A = "a", B = "b", daterange = c(2024, 2020)),
    "daterange start year must be less than or equal to end year"
  )
})

test_that("PubMatrix parses file input and rejects malformed files deterministically", {
  good_file <- tempfile(fileext = ".txt")
  writeLines(c("A1", "A2", "#", "B1", "B2"), good_file)
  bad_file <- tempfile(fileext = ".txt")
  writeLines(c("A1", "A2", "B1", "B2"), bad_file)

  local_mocked_bindings(
    .pubmatrix_fetch_count = mock_fetch_counts(),
    .package = "PubMatrixR"
  )

  result <- PubMatrix(file = good_file, Database = "pubmed")
  expect_equal(dim(result), c(2, 2))
  expect_identical(rownames(result), c("B1", "B2"))
  expect_identical(colnames(result), c("A1", "A2"))

  expect_error(
    PubMatrix(file = bad_file, Database = "pubmed"),
    "File must contain '#' separator"
  )

  unlink(c(good_file, bad_file))
})

test_that("PubMatrix assembles matrix with rows from B and columns from A", {
  local_mocked_bindings(
    .pubmatrix_fetch_count = mock_fetch_counts(),
    .package = "PubMatrixR"
  )

  result <- PubMatrix(
    A = c("A1", "A2"),
    B = c("B1", "B2"),
    Database = "pubmed",
    daterange = c(2020, 2021)
  )

  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), c(2, 2))
  expect_identical(rownames(result), c("B1", "B2"))
  expect_identical(colnames(result), c("A1", "A2"))
  expect_equal(unname(as.matrix(result)), matrix(c(11, 12, 21, 22), nrow = 2, ncol = 2))
})

test_that("PubMatrix exports CSV and ODS files using mocked counts", {
  local_mocked_bindings(
    .pubmatrix_fetch_count = mock_fetch_counts(),
    .package = "PubMatrixR"
  )

  out_stem_csv <- tempfile()
  result_csv <- PubMatrix(
    A = c("A1", "A2"),
    B = c("B1", "B2"),
    Database = "pubmed",
    outfile = out_stem_csv,
    export_format = "csv"
  )
  csv_file <- paste0(out_stem_csv, "_result.csv")
  expect_true(file.exists(csv_file))
  csv_data <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
  expect_true(any(grepl("HYPERLINK", unlist(csv_data), fixed = TRUE)))
  expect_equal(dim(result_csv), c(2, 2))

  out_stem_ods <- tempfile()
  result_ods <- PubMatrix(
    A = c("A1", "A2"),
    B = c("B1", "B2"),
    Database = "pubmed",
    outfile = out_stem_ods,
    export_format = "ods"
  )
  ods_file <- paste0(out_stem_ods, "_result.ods")
  expect_true(file.exists(ods_file))
  ods_data <- readODS::read_ods(ods_file)
  expect_true(nrow(ods_data) >= 2)
  expect_equal(dim(result_ods), c(2, 2))

  unlink(c(csv_file, ods_file))
})

test_that("PubMatrix surfaces a clear network error from the fetch helper", {
  local_mocked_bindings(
    .pubmatrix_fetch_count = function(base_url, encoded_term, n_tries = 2L) {
      stop("Failed to retrieve search count from NCBI after 2 attempt(s): offline", call. = FALSE)
    },
    .package = "PubMatrixR"
  )

  expect_error(
    PubMatrix(A = "a", B = "b", Database = "pubmed"),
    "Failed to retrieve search count from NCBI"
  )
})

test_that("PubMatrix rejects duplicate terms instead of collapsing columns", {
  # Duplicate terms must not silently collapse columns.
  expect_error(
    PubMatrix(A = c("TP53", "TP53"), B = c("b1", "b2"), Database = "pubmed"),
    "duplicate"
  )
  expect_error(
    PubMatrix(A = c("a1", "a2"), B = c("BRCA1", "BRCA1"), Database = "pubmed"),
    "duplicate"
  )
  # Whitespace-only differences still count as duplicates.
  expect_error(
    PubMatrix(A = c("TP53", " TP53 "), B = "b1", Database = "pubmed"),
    "duplicate"
  )
})

test_that("PubMatrix assembles non-square matrices positionally", {
  # Non-square shape catches transposition.
  local_mocked_bindings(
    .pubmatrix_fetch_count = mock_fetch_counts(c(
      "A1 AND B1" = 11, "A1 AND B2" = 12, "A1 AND B3" = 13,
      "A2 AND B1" = 21, "A2 AND B2" = 22, "A2 AND B3" = 23
    )),
    .package = "PubMatrixR"
  )

  result <- PubMatrix(A = c("A1", "A2"), B = c("B1", "B2", "B3"), Database = "pubmed")

  expect_equal(dim(result), c(3, 2))
  expect_identical(rownames(result), c("B1", "B2", "B3"))
  expect_identical(colnames(result), c("A1", "A2"))
  # Each cell matches its term pair.
  expect_equal(result["B3", "A1"], 13)
  expect_equal(result["B1", "A2"], 21)
  expect_equal(
    unname(as.matrix(result)),
    matrix(c(11, 12, 13, 21, 22, 23), nrow = 3, ncol = 2)
  )
})

test_that("PubMatrix handles term files with blank lines and a terminal separator", {
  local_mocked_bindings(
    .pubmatrix_fetch_count = mock_fetch_counts(),
    .package = "PubMatrixR"
  )

  # Trailing blank lines are ignored.
  blank_tail <- tempfile(fileext = ".txt")
  writeLines(c("A1", "A2", "#", "B1", "B2", ""), blank_tail)
  result <- PubMatrix(file = blank_tail, Database = "pubmed")
  expect_equal(dim(result), c(2, 2))
  expect_identical(rownames(result), c("B1", "B2"))

  # Terminal separator errors and names the file.
  no_b <- tempfile(fileext = ".txt")
  writeLines(c("A1", "A2", "#"), no_b)
  expect_error(PubMatrix(file = no_b, Database = "pubmed"), no_b, fixed = TRUE)

  # Leading separator means no A terms.
  no_a <- tempfile(fileext = ".txt")
  writeLines(c("#", "B1", "B2"), no_a)
  expect_error(PubMatrix(file = no_a, Database = "pubmed"), no_a, fixed = TRUE)

  unlink(c(blank_tail, no_b, no_a))
})

test_that("fetch helper retries with exponential backoff", {
  # Retry delay grows between attempts.
  slept <- numeric(0)
  attempts <- 0L

  local_mocked_bindings(
    Sys.sleep = function(time) {
      slept <<- c(slept, time)
      invisible(NULL)
    },
    .package = "base"
  )
  local_mocked_bindings(
    read_xml = function(x, ...) {
      attempts <<- attempts + 1L
      stop("simulated network failure", call. = FALSE)
    },
    .package = "xml2"
  )

  expect_error(
    PubMatrixR:::.pubmatrix_fetch_count("https://example.invalid?db=pubmed", "a", n_tries = 3L),
    "after 3 attempt"
  )

  expect_equal(attempts, 3L)
  expect_length(slept, 2L)
  expect_true(all(diff(slept) > 0))
})

test_that("PubMatrix paces requests to respect NCBI rate limits", {
  # Restore default pacing disabled by setup.R.
  withr::local_options(PubMatrixR.min_interval = NULL)
  slept <- numeric(0)
  local_mocked_bindings(
    Sys.sleep = function(time) {
      slept <<- c(slept, time)
      invisible(NULL)
    },
    .package = "base"
  )
  local_mocked_bindings(
    .pubmatrix_fetch_count = function(base_url, encoded_term, n_tries = 3L) 1,
    .package = "PubMatrixR"
  )

  PubMatrix(A = c("A1", "A2"), B = c("B1", "B2"), Database = "pubmed")
  expect_gt(length(slept), 0)
  # Without a key: 3 requests per second.
  expect_true(all(slept >= 0.3))

  slept <- numeric(0)
  PubMatrix(A = c("A1", "A2"), B = c("B1", "B2"),
            API.key = "dummy", Database = "pubmed")
  # With a key: interval shrinks.
  expect_true(all(slept < 0.3))
})

test_that("query URL omits usehistory and requests no PMIDs", {
  # Query omits usehistory and unused PMIDs.
  seen <- character(0)
  local_mocked_bindings(
    .pubmatrix_fetch_count = function(base_url, encoded_term, n_tries = 3L) {
      seen <<- c(seen, paste0(base_url, "&term=", encoded_term))
      1
    },
    .package = "PubMatrixR"
  )

  PubMatrix(A = "a", B = "b", Database = "pubmed")
  expect_false(any(grepl("usehistory", seen, fixed = TRUE)))

  # Fetch helper appends retmax to the URL.
  built <- character(0)
  real_read_xml <- xml2::read_xml
  local_mocked_bindings(
    read_xml = function(x, ...) {
      built <<- c(built, x)
      # Pristine reference avoids re-entering the mock.
      real_read_xml("<eSearchResult><Count>1</Count></eSearchResult>")
    },
    .package = "xml2"
  )
  PubMatrixR:::.pubmatrix_fetch_count("https://example.invalid?db=pubmed", "a", n_tries = 1L)
  expect_true(all(grepl("retmax=0", built, fixed = TRUE)))
  expect_false(any(grepl("usehistory", built, fixed = TRUE)))
})

test_that("API key is never echoed in error messages", {
  # API key must not leak into error messages.
  local_mocked_bindings(
    .pubmatrix_fetch_count = function(base_url, encoded_term, n_tries = 3L) {
      stop("Failed to retrieve search count from NCBI after 3 attempt(s): offline",
           call. = FALSE)
    },
    .package = "PubMatrixR"
  )

  err <- tryCatch(
    PubMatrix(A = "a", B = "b", API.key = "SECRETKEY123", Database = "pubmed"),
    error = function(e) conditionMessage(e)
  )
  expect_false(grepl("SECRETKEY123", err, fixed = TRUE))
})
