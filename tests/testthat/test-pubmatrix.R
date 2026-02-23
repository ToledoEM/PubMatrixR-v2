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

local_mock_fetch_counts <- function(mapping = mock_counts()) {
  local_mocked_bindings(
    .pubmatrix_fetch_count = function(base_url, encoded_term, n_tries = 2L) {
      decoded <- utils::URLdecode(encoded_term)
      if (!decoded %in% names(mapping)) {
        stop("Unexpected mocked term: ", decoded, call. = FALSE)
      }
      unname(mapping[[decoded]])
    },
    .env = asNamespace("PubMatrixR")
  )
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

  local_mock_fetch_counts()

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
  local_mock_fetch_counts()

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
  local_mock_fetch_counts()

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
    .env = asNamespace("PubMatrixR")
  )

  expect_error(
    PubMatrix(A = "a", B = "b", Database = "pubmed"),
    "Failed to retrieve search count from NCBI"
  )
})
