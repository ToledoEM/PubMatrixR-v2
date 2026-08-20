test_that("pubmatrix_heatmap works with basic input", {
  # Simple square count matrix.
  test_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  rownames(test_matrix) <- c("Gene1", "Gene2")
  colnames(test_matrix) <- c("GeneA", "GeneB")

  # Render offscreen to avoid Rplots.pdf.
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

# Renders offscreen so tests never create Rplots.pdf.
local_null_device <- function(.env = parent.frame()) {
  tmp <- tempfile(fileext = ".png")
  png(filename = tmp, width = 800, height = 600)
  withr::defer({
    try(dev.off(), silent = TRUE)
    unlink(tmp)
  }, envir = .env)
}

test_that("character input coerces to numeric without losing dimnames", {
  # Character coercion keeps row and column names.
  local_null_device()
  m <- matrix(c("1", "2", "3", "4"), nrow = 2, ncol = 2,
              dimnames = list(c("r1", "r2"), c("c1", "c2")))

  expect_message(plot_pubmatrix_heatmap(m, title = "Coerced"), "coerced to numeric")

  res <- suppressMessages(plot_pubmatrix_heatmap(m, title = "Coerced"))
  expect_identical(rownames(attr(res, "plotted_values")), c("r1", "r2"))
  expect_identical(colnames(attr(res, "plotted_values")), c("c1", "c2"))
})

test_that("single-row character input is not transposed", {
  # Single-row input keeps its shape.
  local_null_device()
  m <- matrix(c("1", "2"), nrow = 1,
              dimnames = list("r1", c("c1", "c2")))

  res <- suppressMessages(plot_pubmatrix_heatmap(m, title = "Single row"))
  expect_equal(dim(attr(res, "plotted_values")), c(1L, 2L))
  expect_identical(rownames(attr(res, "plotted_values")), "r1")
})

test_that("NA report names the actual rows, not NA", {
  # NA report names real rows.
  local_null_device()
  m <- matrix(c("1", NA, "3", "4"), nrow = 2, ncol = 2,
              dimnames = list(c("r1", "r2"), c("c1", "c2")))

  msgs <- capture_messages(plot_pubmatrix_heatmap(m, title = "NA report"))
  expect_true(any(grepl("r2 vs c1", msgs, fixed = TRUE)))
  expect_false(any(grepl("NA vs", msgs, fixed = TRUE)))
})

test_that("values argument selects the plotted metric", {
  local_null_device()
  m <- matrix(c(10, 5, 5, 10), nrow = 2, ncol = 2,
              dimnames = list(c("r1", "r2"), c("c1", "c2")))

  # Default plots raw counts.
  expect_equal(attr(plot_pubmatrix_heatmap(m), "plotted_values")[["r1", "c1"]], 10)
  expect_equal(
    attr(plot_pubmatrix_heatmap(m, values = "raw"), "plotted_values")[["r1", "c1"]],
    10
  )
  # Row percentages sum to 100.
  row_pct <- attr(plot_pubmatrix_heatmap(m, values = "row_pct"), "plotted_values")
  expect_equal(unname(rowSums(row_pct)), c(100, 100))
  # Relative mode keeps the historical formula.
  rel <- attr(plot_pubmatrix_heatmap(m, values = "relative"), "plotted_values")
  expect_equal(rel[["r1", "c1"]], 50)

  expect_error(plot_pubmatrix_heatmap(m, values = "jaccard"))
})

test_that("raw values are unaffected by unrelated columns", {
  # Raw values ignore unrelated columns.
  local_null_device()
  base <- matrix(c(10, 5, 5, 10), nrow = 2, ncol = 2,
                 dimnames = list(c("r1", "r2"), c("c1", "c2")))
  wide <- cbind(base, c3 = c(1000, 1000))

  cell <- function(m) {
    attr(plot_pubmatrix_heatmap(m, values = "raw"), "plotted_values")[["r1", "c1"]]
  }
  expect_equal(cell(base), cell(wide))

  # Relative mode stays context-dependent.
  rel <- function(m) {
    attr(plot_pubmatrix_heatmap(m, values = "relative"), "plotted_values")[["r1", "c1"]]
  }
  expect_false(isTRUE(all.equal(rel(base), rel(wide))))
})

test_that("uniform matrix warns and still plots", {
  # Uniform data warns instead of stopping.
  local_null_device()
  m <- matrix(5, nrow = 2, ncol = 2,
              dimnames = list(c("r1", "r2"), c("c1", "c2")))

  expect_warning(
    plot_pubmatrix_heatmap(m, values = "relative", title = "Uniform"),
    "identical"
  )

  res <- suppressWarnings(
    plot_pubmatrix_heatmap(m, values = "relative", title = "Uniform")
  )
  expect_s3_class(res, "pheatmap")
})

test_that("heatmap input validation errors carry no call context", {
  # Validation errors carry no call context.
  err <- tryCatch(plot_pubmatrix_heatmap(list(1, 2)), error = function(e) e)
  expect_null(conditionCall(err))

  err2 <- tryCatch(
    plot_pubmatrix_heatmap(matrix(character(0), nrow = 0, ncol = 0)),
    error = function(e) e
  )
  expect_null(conditionCall(err2))
})

test_that("pubmatrix_heatmap forwards the values argument", {
  local_null_device()
  m <- matrix(c(10, 5, 5, 10), nrow = 2, ncol = 2,
              dimnames = list(c("r1", "r2"), c("c1", "c2")))

  expect_equal(attr(pubmatrix_heatmap(m), "plotted_values")[["r1", "c1"]], 10)
  expect_equal(
    attr(pubmatrix_heatmap(m, values = "relative"), "plotted_values")[["r1", "c1"]],
    50
  )
})
