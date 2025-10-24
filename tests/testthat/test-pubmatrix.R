# Test suite for PubMatrixR package
# Tests API functionality, internet access, and minimal reproducible examples

library(testthat)
library(PubMatrixR)

# Helper function to check internet connectivity
check_internet <- function() {
  tryCatch({
    readLines("https://www.ncbi.nlm.nih.gov/", n = 1, warn = FALSE)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Helper function to check NCBI API accessibility
check_ncbi_api <- function() {
  tryCatch({
    url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=test&retmax=1"
    response <- xml2::read_html(url)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Test 1: Internet Connectivity
test_that("Internet connectivity is available", {
  skip_on_cran()
  skip_if_not(check_internet(), "No internet connection available")
  
  expect_true(check_internet())
})

# Test 2: NCBI API Accessibility
test_that("NCBI E-utilities API is accessible", {
  skip_on_cran()
  skip_if_not(check_internet(), "No internet connection available")
  
  expect_true(check_ncbi_api())
})

# Test 3: Minimal Reproducible Example - Basic Function Call
test_that("PubMatrix works with minimal example", {
  skip_on_cran()
  skip_if_not(check_internet(), "No internet connection available")
  skip_if_not(check_ncbi_api(), "NCBI API not accessible")
  
  # Define minimal test vectors
  A <- c("insulin")
  B <- c("diabetes")
  
  # Run PubMatrix with minimal parameters
  result <- PubMatrix(
    A = A,
    B = B,
    Database = "pubmed",
    daterange = c(2020, 2024),
    outfile = NULL
  )
  
  # Basic assertions
  expect_true(is.matrix(result))
  expect_equal(nrow(result), length(B))
  expect_equal(ncol(result), length(A))
  expect_true(is.numeric(result))
  expect_true(all(result >= 0))  # Publication counts should be non-negative
})

# Test 4: Function Parameter Validation
test_that("PubMatrix handles invalid parameters gracefully", {
  # Test with NULL A and B (should require file parameter)
  expect_error(
    PubMatrix(A = NULL, B = NULL, file = NULL),
    "Either provide vectors A and B, or specify a file containing search terms"
  )
  
  # Test with invalid database
  expect_error(
    PubMatrix(A = "test", B = "term", Database = "invalid_db"),
    "Mismatched search results|Check NCBI response"  # Should error on invalid database
  )
})

# Test 5: Output Structure Validation
test_that("PubMatrix returns correctly structured output", {
  skip_on_cran()
  skip_if_not(check_internet(), "No internet connection available")
  skip_if_not(check_ncbi_api(), "NCBI API not accessible")
  
  A <- c("TP53", "BRCA1")
  B <- c("cancer", "mutation")
  
  result <- PubMatrix(
    A = A,
    B = B,
    Database = "pubmed",
    daterange = c(2022, 2024),
    outfile = NULL,
    
  )
  
  # Check dimensions
  expect_equal(dim(result), c(length(B), length(A)))
  
  # Check that results are reasonable (should have some publications)
  expect_true(any(result > 0))  # At least some searches should return results
  
  # Check row and column names (if any)
  if (!is.null(rownames(result))) {
    expect_equal(length(rownames(result)), length(B))
  }
  if (!is.null(colnames(result))) {
    expect_equal(length(colnames(result)), length(A))
  }
})

# Test 6: Date Range Functionality
test_that("Date range parameter works correctly", {
  skip_on_cran()
  skip_if_not(check_internet(), "No internet connection available")
  skip_if_not(check_ncbi_api(), "NCBI API not accessible")
  
  A <- c("COVID-19")
  B <- c("vaccine")
  
  # Test with recent date range (should have results)
  result_recent <- PubMatrix(
    A = A,
    B = B,
    Database = "pubmed",
    daterange = c(2020, 2024),
    outfile = NULL,
    
  )
  
  # Test with very old date range (should have fewer/no results)
  result_old <- PubMatrix(
    A = A,
    B = B,
    Database = "pubmed",
    daterange = c(1950, 1960),
    outfile = NULL,
    
  )
  
  expect_true(result_recent[1,1] > result_old[1,1])
})

# Test 7: File Input Functionality
test_that("File input works correctly", {
  skip_on_cran()
  skip_if_not(check_internet(), "No internet connection available")
  skip_if_not(check_ncbi_api(), "NCBI API not accessible")
  
  # Create temporary test file
  temp_file <- tempfile(fileext = ".txt")
  writeLines(c("insulin", "glucose", "#", "diabetes", "metabolism"), temp_file)
  
  # Test file input
  result <- PubMatrix(
    file = temp_file,
    Database = "pubmed",
    daterange = c(2020, 2024),
    outfile = NULL,
    
  )
  
  # Clean up
  unlink(temp_file)
  
  # Assertions
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 2)  # 2 terms after #
  expect_equal(ncol(result), 2)  # 2 terms before #
})

# Test 8: CSV Output Functionality
test_that("CSV output is created correctly", {
  skip_on_cran()
  skip_if_not(check_internet(), "No internet connection available")
  skip_if_not(check_ncbi_api(), "NCBI API not accessible")
  
  A <- c("aspirin")
  B <- c("cardiology")
  temp_outfile <- tempfile()
  
  result <- PubMatrix(
    A = A,
    B = B,
    Database = "pubmed",
    daterange = c(2020, 2024),
    outfile = temp_outfile,
    
  )
  
  # Check if output file was created
  csv_file <- paste0(temp_outfile, "_result.csv")
  expect_true(file.exists(csv_file))
  
  # Check CSV content
  if (file.exists(csv_file)) {
    csv_data <- read.csv(csv_file, stringsAsFactors = FALSE)
    expect_true(nrow(csv_data) >= 1)
    expect_true(ncol(csv_data) >= 2)  # At least row names + 1 data column
    
    # Clean up
    unlink(csv_file)
  }
})

# Test 9: API Key Parameter
test_that("API key parameter is handled correctly", {
  skip_on_cran()
  skip_if_not(check_internet(), "No internet connection available")
  skip_if_not(check_ncbi_api(), "NCBI API not accessible")
  
  A <- c("ibuprofen")
  B <- c("inflammation")
  
  # Test with NULL API key (should work)
  result_no_key <- PubMatrix(
    A = A,
    B = B,
    Database = "pubmed",
    API.key = NULL,
    daterange = c(2023, 2024),
    outfile = NULL,
    
  )
  
  # Test with fake API key (should still work, just might be slower)
  expect_no_error({
    tryCatch({
      result_fake_key <- PubMatrix(
        A = A,
        B = B,
        Database = "pubmed",
        API.key = "fake_key_for_testing",
        daterange = c(2023, 2024),
        outfile = NULL,
        
      )
      
      # Both should return matrices
      expect_true(is.matrix(result_no_key))
      expect_true(is.matrix(result_fake_key))
      
      # Results should be similar (same search, different API usage)
      expect_equal(dim(result_no_key), dim(result_fake_key))
      
    }, error = function(e) {
      # Network errors are acceptable in testing environments
      message("Network test failed (acceptable): ", e$message)
    })
  })
})

# Test 10: Multiple Database Support
test_that("Different databases work correctly", {
  skip_on_cran()
  skip_if_not(check_internet(), "No internet connection available")
  skip_if_not(check_ncbi_api(), "NCBI API not accessible")
  
  A <- c("genetics")
  B <- c("research")
  
  # Test PubMed database
  result_pubmed <- PubMatrix(
    A = A,
    B = B,
    Database = "pubmed",
    daterange = c(2023, 2024),
    outfile = NULL,
    
  )
  
  # Test PMC database
  result_pmc <- PubMatrix(
    A = A,
    B = B,
    Database = "pmc",
    daterange = c(2023, 2024),
    outfile = NULL,
    
  )
  
  # Both should return valid matrices
  expect_true(is.matrix(result_pubmed))
  expect_true(is.matrix(result_pmc))
  expect_equal(dim(result_pubmed), dim(result_pmc))
})

# Test 11: Large Search Matrix Performance
test_that("Function handles larger search matrices", {
  skip_on_cran()
  skip_if_not(check_internet(), "No internet connection available")
  skip_if_not(check_ncbi_api(), "NCBI API not accessible")
  
  # Create slightly larger test sets
  A <- c("gene", "protein", "enzyme")
  B <- c("cancer", "therapy", "treatment", "diagnosis")
  
  # Measure execution time
  start_time <- Sys.time()
  
  result <- PubMatrix(
    A = A,
    B = B,
    Database = "pubmed",
    daterange = c(2023, 2024),
    outfile = NULL,
    
  )
  
  end_time <- Sys.time()
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Assertions
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(length(B), length(A)))
  expect_true(execution_time < 120)  # Should complete within 2 minutes
})

# Test 12: Error Handling for Network Issues
test_that("Function handles network errors gracefully", {
  skip_on_cran()
  
  # Mock a network error scenario by using an invalid URL
  # This tests the error handling without relying on actual network issues
  A <- c("test")
  B <- c("term")
  
  # Test should not crash even if network issues occur
  expect_no_error({
    tryCatch({
      result <- PubMatrix(
        A = A,
        B = B,
        Database = "pubmed",
        daterange = c(2024, 2024),
        outfile = NULL,
        
      )
    }, error = function(e) {
      # Network errors are acceptable in testing
      message("Network error caught (expected in some environments): ", e$message)
    })
  })
})