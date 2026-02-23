test_that("pubmatrix_heatmap works with basic input", {
  # Create a simple test matrix
  test_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  rownames(test_matrix) <- c("Gene1", "Gene2")
  colnames(test_matrix) <- c("GeneA", "GeneB")

  # Render into a temp graphics device to avoid creating Rplots.pdf in the repo.
  tmp_png <- tempfile(fileext = ".png")
  png(filename = tmp_png, width = 800, height = 600)
  on.exit({
    try(dev.off(), silent = TRUE)
    unlink(tmp_png)
  }, add = TRUE)

  result <- NULL
  expect_no_error({
    result <- pubmatrix_heatmap(test_matrix, title = "Test Heatmap")
  })

  expect_s3_class(result, "pheatmap")
})

test_that("plot_pubmatrix_heatmap saves to file and handles scale_font path", {
  test_matrix <- matrix(c(1, 2, 4, 8), nrow = 2, ncol = 2)
  rownames(test_matrix) <- c("R1", "R2")
  colnames(test_matrix) <- c("C1", "C2")

  out_file <- tempfile(fileext = ".png")
  expect_no_error({
    result <- plot_pubmatrix_heatmap(
      test_matrix,
      title = "Saved",
      filename = out_file,
      scale_font = TRUE,
      cellwidth = 40,
      cellheight = 30
    )
    expect_s3_class(result, "pheatmap")
  })
  expect_true(file.exists(out_file))
  expect_gt(file.info(out_file)$size, 0)
  unlink(out_file)
})
