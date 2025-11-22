# Minimal Reproducible Examples for PubMatrixR
# Quick tests that can be run to verify package functionality

library(testthat)
library(PubMatrixR)

# Minimal example that should always work (if internet is available)
test_that("Minimal reproducible example works", {
  skip_on_cran()
  skip_if_offline()

  # Most basic possible search
  result <- PubMatrix(
    A = "test",
    B = "example",
    Database = "pubmed",
    daterange = c(2023, 2024),
    outfile = NULL,
  )

  expect_true(is.data.frame(result))
  expect_equal(dim(result), c(1, 1))
  expect_true(is.numeric(result[1, 1]))
  expect_true(result[1, 1] >= 0)
})

# Quick API connectivity test
test_that("NCBI API is reachable", {
  skip_on_cran()
  skip_if_offline()

  # Try to access NCBI API directly
  expect_no_error({
    url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=test&retmax=1"
    response <- xml2::read_html(url)

    # Should be able to parse the response
    expect_true(inherits(response, "xml_document"))
  })
})

# Test with common biomedical terms (high likelihood of results)
test_that("Common biomedical terms return results", {
  skip_on_cran()
  skip_if_offline()

  result <- PubMatrix(
    A = "insulin",
    B = "diabetes",
    Database = "pubmed",
    daterange = c(2020, 2024),
    outfile = NULL,
  )

  expect_true(is.data.frame(result))
  expect_true(result[1, 1] > 0) # Should definitely have publications
})

# Test file-based input with minimal example
test_that("File input minimal example works", {
  skip_on_cran()
  skip_if_offline()

  # Create minimal test file
  temp_file <- tempfile(fileext = ".txt")
  writeLines(c("gene", "#", "protein"), temp_file)

  result <- PubMatrix(
    file = temp_file,
    Database = "pubmed",
    daterange = c(2023, 2024),
    outfile = NULL,
  )

  unlink(temp_file)

  expect_true(is.data.frame(result))
  expect_equal(dim(result), c(1, 1))
})

# Quick performance check
test_that("Single search completes quickly", {
  skip_on_cran()
  skip_if_offline()

  start_time <- Sys.time()

  result <- PubMatrix(
    A = "quick",
    B = "test",
    Database = "pubmed",
    daterange = c(2024, 2024),
    outfile = NULL,
  )

  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  expect_true(duration < 30) # Should complete within 30 seconds
  expect_true(is.data.frame(result))
})

# Test both databases work
test_that("Both PubMed and PMC databases are accessible", {
  skip_on_cran()
  skip_if_offline()

  # Test PubMed
  result_pubmed <- PubMatrix(
    A = "database",
    B = "search",
    Database = "pubmed",
    daterange = c(2023, 2024),
    outfile = NULL,
  )

  # Test PMC
  result_pmc <- PubMatrix(
    A = "database",
    B = "search",
    Database = "pmc",
    daterange = c(2023, 2024),
    outfile = NULL,
  )

  expect_true(is.data.frame(result_pubmed))
  expect_true(is.data.frame(result_pmc))
  expect_equal(dim(result_pubmed), dim(result_pmc))
})
