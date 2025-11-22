test_that("pubmatrix_heatmap works with basic input", {
  # Create a simple test matrix
  test_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  rownames(test_matrix) <- c("Gene1", "Gene2")
  colnames(test_matrix) <- c("GeneA", "GeneB")

  # Test that it runs without error
  expect_no_error({
    result <- pubmatrix_heatmap(test_matrix, title = "Test Heatmap")
  })

  # Test that it returns a pheatmap object
  expect_s3_class(result, "pheatmap")
})