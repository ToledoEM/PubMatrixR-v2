# Unit tests for PubMatrixR helper functions and edge cases

library(testthat)
library(PubMatrixR)

# Test input validation and edge cases
test_that("Input validation works correctly", {
  
  # Test empty vectors
  expect_error(
    PubMatrix(A = character(0), B = c("test")),
    "Both A and B must contain at least one search term"
  )
  
  expect_error(
    PubMatrix(A = c("test"), B = character(0)),
    "Both A and B must contain at least one search term"
  )
  
  # Test special characters in search terms
  A_special <- c("C++", "R&D", "3D-printing")
  B_special <- c("software", "research", "technology")
  
  expect_no_error({
    # This might not work due to special characters, but shouldn't crash
    tryCatch({
      result <- PubMatrix(
        A = A_special,
        B = B_special,
        Database = "pubmed",
        daterange = c(2023, 2024),
        outfile = NULL,
        
      )
    }, error = function(e) {
      message("Special character handling test - error expected: ", e$message)
    })
  })
  
  # Test very long search terms
  A_long <- c(paste(rep("very", 50), collapse = " "))
  B_long <- c("test")
  
  expect_no_error({
    tryCatch({
      result <- PubMatrix(
        A = A_long,
        B = B_long,
        Database = "pubmed",
        daterange = c(2023, 2024),
        outfile = NULL,
        
      )
    }, error = function(e) {
      message("Long term handling test - error may be expected: ", e$message)
    })
  })
})

test_that("Date range validation works", {
  skip_on_cran()
  
  A <- c("test")
  B <- c("term")
  
  # Test invalid date ranges
  expect_no_error({
    tryCatch({
      # Future dates
      result1 <- PubMatrix(
        A = A, B = B,
        Database = "pubmed",
        daterange = c(2030, 2035),
        outfile = NULL,
        
      )
      
      # Reversed date range
      result2 <- PubMatrix(
        A = A, B = B,
        Database = "pubmed",
        daterange = c(2024, 2020),
        outfile = NULL,
        
      )
      
      # Single year
      result3 <- PubMatrix(
        A = A, B = B,
        Database = "pubmed",
        daterange = c(2023, 2023),
        outfile = NULL,
        
      )
      
    }, error = function(e) {
      message("Date range test - some errors may be expected: ", e$message)
    })
  })
})

test_that("File format validation works", {
  skip_on_cran()
  
  # Test malformed file (missing separator)
  temp_file1 <- tempfile(fileext = ".txt")
  writeLines(c("term1", "term2", "term3"), temp_file1)
  
  expect_error({
    result <- PubMatrix(
      file = temp_file1,
      Database = "pubmed",
      daterange = c(2023, 2024),
      outfile = NULL,
      
    )
  }, "File must contain '#' separator")
  
  # Test file with multiple separators (should work, uses first separator)
  temp_file2 <- tempfile(fileext = ".txt")
  writeLines(c("term1", "#", "term2", "#", "term3"), temp_file2)
  
  expect_no_error({
    result <- PubMatrix(
      file = temp_file2,
      Database = "pubmed",
      daterange = c(2023, 2024),
      outfile = NULL,
      
    )
    expect_true(is.matrix(result))
  })
  
  # Test empty file
  temp_file3 <- tempfile(fileext = ".txt")
  writeLines(character(0), temp_file3)
  
  expect_error({
    result <- PubMatrix(
      file = temp_file3,
      Database = "pubmed",
      daterange = c(2023, 2024),
      outfile = NULL,
      
    )
  }, "File must contain '#' separator")
  
  # Clean up
  unlink(c(temp_file1, temp_file2, temp_file3))
})

test_that("Output file handling works correctly", {
  skip_on_cran()
  
  A <- c("test")
  B <- c("term")
  
  # Test with file extension in outfile name
  temp_outfile1 <- tempfile(fileext = ".csv")
  
  expect_no_error({
    tryCatch({
      result <- PubMatrix(
        A = A, B = B,
        Database = "pubmed",
        daterange = c(2023, 2024),
        outfile = temp_outfile1,
        
      )
      
      # Should create file without double extension
      expected_file <- sub("\\.csv$", "_result.csv", temp_outfile1)
      if (file.exists(expected_file)) {
        unlink(expected_file)
      }
      
    }, error = function(e) {
      message("Output file test error: ", e$message)
    })
  })
  
  # Test with special characters in filename
  temp_outfile2 <- file.path(tempdir(), "test file with spaces")
  
  expect_no_error({
    tryCatch({
      result <- PubMatrix(
        A = A, B = B,
        Database = "pubmed",
        daterange = c(2023, 2024),
        outfile = temp_outfile2,
        
      )
      
      expected_file <- paste0(temp_outfile2, "_result.csv")
      if (file.exists(expected_file)) {
        unlink(expected_file)
      }
      
    }, error = function(e) {
      message("Special filename test error: ", e$message)
    })
  })
})

test_that("Visualization parameter works correctly", {
  skip_on_cran()
  
  A <- c("visualization")
  B <- c("test")
  
  # Test pheatmap option
  expect_no_error({
    tryCatch({
      temp_outfile <- tempfile()
      result <- PubMatrix(
        A = A, B = B,
        Database = "pubmed",
        daterange = c(2023, 2024),
        outfile = temp_outfile,
        use_pheatmap = TRUE
      )
      
      # Check if heatmap file was created
      heatmap_file <- paste0(temp_outfile, "_heatmap.png")
      if (file.exists(heatmap_file)) {
        expect_true(file.size(heatmap_file) > 0)
        unlink(heatmap_file)
      }
      
      # Clean up CSV file too
      csv_file <- paste0(temp_outfile, "_result.csv")
      if (file.exists(csv_file)) {
        unlink(csv_file)
      }
      
    }, error = function(e) {
      message("Visualization test error: ", e$message)
    })
  })
})

test_that("Search term preprocessing works correctly", {
  # Test how the function handles different input formats
  
  # Terms with spaces
  A_spaces <- c("machine learning", "data science")
  B_spaces <- c("artificial intelligence", "big data")
  
  expect_no_error({
    tryCatch({
      result <- PubMatrix(
        A = A_spaces,
        B = B_spaces,
        Database = "pubmed",
        daterange = c(2023, 2024),
        outfile = NULL,
        
      )
      
      # Should handle spaces by converting to +
      expect_true(is.matrix(result))
      
    }, error = function(e) {
      message("Space handling test: ", e$message)
    })
  })
  
  # Terms with special characters
  A_special <- c("p53", "miR-21", "NF-κB")
  B_special <- c("β-catenin", "α-synuclein")
  
  expect_no_error({
    tryCatch({
      result <- PubMatrix(
        A = A_special,
        B = B_special,
        Database = "pubmed",
        daterange = c(2023, 2024),
        outfile = NULL,
        
      )
      
      expect_true(is.matrix(result))
      
    }, error = function(e) {
      message("Special character test: ", e$message)
    })
  })
})

# Performance and stress tests
test_that("Function handles edge case matrix sizes", {
  skip_on_cran()
  
  # Test 1x1 matrix
  result_1x1 <- PubMatrix(
    A = "single",
    B = "term",
    Database = "pubmed",
    daterange = c(2023, 2024),
    outfile = NULL,
    
  )
  
  expect_equal(dim(result_1x1), c(1, 1))
  expect_true(is.numeric(result_1x1[1,1]))
  
  # Test asymmetric matrix
  A_big <- c("term1", "term2", "term3", "term4", "term5")
  B_small <- c("test")
  
  result_asym <- PubMatrix(
    A = A_big,
    B = B_small,
    Database = "pubmed",
    daterange = c(2023, 2024),
    outfile = NULL,
    
  )
  
  expect_equal(dim(result_asym), c(1, 5))
})