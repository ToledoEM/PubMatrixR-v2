#' Create a formatted heatmap from PubMatrix results
#'
#' This function creates a heatmap displaying overlap percentages derived from a
#' PubMatrix result matrix, with Euclidean distance clustering for rows and columns.
#'
#' @param matrix A data frame or matrix from PubMatrix results containing publication co-occurrence counts
#' @param values Character scalar selecting what is plotted in each cell. One of
#'   `"raw"` (default, the co-occurrence counts as returned by [PubMatrix()]),
#'   `"row_pct"` (each cell as a percentage of its row total), or `"relative"`
#'   (see Details).
#' @param title Character string for the heatmap title. Default is "PubMatrix Co-occurrence Heatmap"
#' @param cluster_rows Logical value determining if rows should be clustered using Euclidean distance. Default is TRUE
#' @param cluster_cols Logical value determining if columns should be clustered using Euclidean distance. Default is TRUE
#' @param show_numbers Logical value determining if overlap percentage values should be displayed in cells. Default is TRUE
#' @param color_palette Color palette for the heatmap. Default uses a red gradient color scale
#' @param filename Optional filename to save the heatmap. If NULL, displays the plot
#' @param width Width of saved plot in inches. Default is 10
#' @param height Height of saved plot in inches. Default is 8
#' @param cellwidth Optional numeric cell width for pheatmap (in pixels). Default `NA` lets pheatmap auto-size.
#' @param cellheight Optional numeric cell height for pheatmap (in pixels). Default `NA` lets pheatmap auto-size.
#' @param scale_font Logical value determining if font size should scale with cell size. Default is TRUE
#' @details
#' Rows and columns are clustered with Euclidean distance. NA values in the
#' input matrix are converted to 0 before any calculation.
#'
#' The `values` argument controls what each cell shows:
#'
#' \itemize{
#'   \item `"raw"` (default) - the co-occurrence counts themselves.
#'   \item `"row_pct"` - each count as a percentage of its row total, useful
#'     for comparing how one row's attention is distributed across columns.
#'   \item `"relative"` - `count / (row_total + col_total - count) * 100`,
#'     where the totals are sums over the supplied matrix.
#' }
#'
#' Note that `"relative"` is **not** a Jaccard index and is **not comparable
#' across runs**. Its totals are sums over whichever partner terms happen to be
#' in the matrix, not the marginal publication counts for each term, so adding
#' an unrelated term changes the value reported for every existing cell. A true
#' Jaccard index would require the single-term counts, which [PubMatrix()] does
#' not fetch. Use it only to compare cells within one fixed matrix.
#' @return A pheatmap object (invisible). The matrix actually plotted is
#'   attached as the `"plotted_values"` attribute.
#' @importFrom grDevices png dev.off colorRampPalette
#' @importFrom utils head
#' @export
#' @examples
#' # Create a small test matrix
#' test_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
#' rownames(test_matrix) <- c("Gene1", "Gene2")
#' colnames(test_matrix) <- c("GeneA", "GeneB")
#'
#' # Create heatmap using the helper (plots the raw counts by default)
#' plot_pubmatrix_heatmap(test_matrix, title = "Test Heatmap")
#'
#' # Percentage views are available too:
#' plot_pubmatrix_heatmap(test_matrix, values = "row_pct", title = "Row %")
#'
#' # Equivalent using pheatmap directly:
#' pheatmap::pheatmap(
#'   test_matrix,
#'   main = "Test Heatmap (pheatmap)",
#'   color = colorRampPalette(c("#fee5d9", "#cb181d"))(100),
#'   display_numbers = TRUE,
#'   fontsize = 16,
#'   fontsize_number = 14,
#'   border_color = "lightgray",
#'   show_rownames = TRUE,
#'   show_colnames = TRUE
#' )
plot_pubmatrix_heatmap <- function(matrix,
                                   values = c("raw", "row_pct", "relative"),
                                   title = "PubMatrix Co-occurrence Heatmap",
                                   cluster_rows = TRUE,
                                   cluster_cols = TRUE, 
                                   show_numbers = TRUE,
                                   color_palette = NULL,
                                   filename = NULL,
                                   width = 10,
                                   height = 8,
                                   cellwidth = NA,
                                   cellheight = NA,
                                   scale_font = TRUE) {
  values <- match.arg(values)

  # --- Input validation and coercion ---
  if (is.data.frame(matrix)) {
    # Convert data frame to matrix
    matrix <- as.matrix(matrix)
  }

  if (!is.matrix(matrix)) {
    stop("Input must be a matrix (or a data frame coercible to a matrix).",
         call. = FALSE)
  }

  if (!is.numeric(matrix)) {
    if (is.character(matrix)) {
      # Coerce in place: apply() would drop row names, and would transpose a
      # single-row matrix into a single column.
      matrix_coerced <- matrix(
        suppressWarnings(as.numeric(matrix)),
        nrow = nrow(matrix),
        ncol = ncol(matrix),
        dimnames = dimnames(matrix)
      )

      # Check if coercion failed because of non-numeric strings (like formulas)
      if (all(is.na(matrix_coerced[!is.na(matrix)]))) {
        # The matrix likely contains non-numeric strings (like HTML/Excel formulas)
        stop(
          "Input matrix is character-based and contains non-numeric data (e.g., formulas/HTML links). \n",
          "Ensure you are passing the raw numeric count matrix (the direct output of PubMatrix) and not the CSV export data.",
          call. = FALSE
        )
      }

      # Use the coerced matrix
      matrix <- matrix_coerced
      message("Input matrix was character and has been coerced to numeric. Check for NA values if unexpected.")
    } else {
      # Stop if it's neither numeric nor character
      stop("Input must be a numeric matrix", call. = FALSE)
    }
  }
  # --- End Input validation and coercion ---

  if (nrow(matrix) == 0 || ncol(matrix) == 0) {
    stop("Matrix must have at least one row and one column", call. = FALSE)
  }

  # --- NA Handling: Convert NA to 0 and report the change ---
  na_count <- sum(is.na(matrix))
  if (na_count > 0) {
    # Get the names of NA positions
    na_indices <- which(is.na(matrix), arr.ind = TRUE)
    na_positions <- paste0(rownames(matrix)[na_indices[, 1]], " vs ", colnames(matrix)[na_indices[, 2]])

    # Replace NA with 0
    matrix[is.na(matrix)] <- 0

    # Report back to the user which values were converted
    message_output <- paste0("NA values found in the input matrix (", na_count, " total) and converted to 0 for overlap calculation.\n")
    message_output <- paste0(message_output, "Converted positions (Row vs Col): \n- ", paste(head(na_positions, 10), collapse = "\n- "))
    if (na_count > 10) {
      message_output <- paste0(message_output, "\n- ... (and ", na_count - 10, " more)")
    }
    message(message_output)
  }
  # --- End NA Handling ---

  # Build the matrix that will actually be plotted.
  if (identical(values, "raw")) {
    display_matrix <- matrix
  } else if (identical(values, "row_pct")) {
    row_totals <- rowSums(matrix)
    display_matrix <- matrix / ifelse(row_totals == 0, 1, row_totals) * 100
    display_matrix <- round(display_matrix, 1)
  } else {
    # "relative": totals are sums over this matrix only, so values shift when
    # unrelated terms are added. Documented as such; not a Jaccard index.
    row_totals <- rowSums(matrix)
    col_totals <- colSums(matrix)
    union <- outer(row_totals, col_totals, "+") - matrix
    display_matrix <- ifelse(union == 0, 0, round(matrix / union * 100, 1))
  }
  dimnames(display_matrix) <- dimnames(matrix)

  # Set default color palette if not provided
  if (is.null(color_palette)) {
    # Create a custom red gradient color palette for overlap percentages
    # Light colors for low overlap, dark colors for high overlap
    custom_colors <- c(
      "#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a",
      "#ef3b2c", "#cb181d", "#99000d"
    )
    color_palette <- colorRampPalette(custom_colors)(100)
  }

  # Prepare clustering distances (use Euclidean distance to avoid warnings)
  if (cluster_rows && nrow(matrix) > 1) {
    use_row_clustering <- TRUE
  } else {
    use_row_clustering <- FALSE
  }

  if (cluster_cols && ncol(matrix) > 1) {
    use_col_clustering <- TRUE
  } else {
    use_col_clustering <- FALSE
  }

  # Uniform data is valid input, so warn rather than abort. pheatmap cannot
  # cluster it, though, so clustering is disabled for this case.
  display_range <- range(display_matrix, na.rm = TRUE)
  uniform_values <- length(display_range) == 2 && isTRUE(diff(display_range) == 0)
  if (uniform_values) {
    warning(
      "All plotted values are identical (", display_range[1],
      "). Clustering disabled; the heatmap will be a single colour.",
      call. = FALSE
    )
    use_row_clustering <- FALSE
    use_col_clustering <- FALSE
    # pheatmap derives breaks from the data range; a zero-width range yields
    # non-unique breaks and errors out. Supply one colour and a bracketing
    # pair of breaks instead.
    color_palette <- color_palette[1]
    uniform_breaks <- c(display_range[1] - 0.5, display_range[1] + 0.5)
  }

  # Create the heatmap
  if (!is.null(filename)) {
    # Save to file
    png(filename = filename, width = width, height = height, units = "in", res = 300)
  }

  # Dynamically compute font sizes before plotting so scale_font affects pheatmap().
  nmax <- max(nrow(display_matrix), ncol(display_matrix))
  if (isTRUE(scale_font)) {
    if (!is.na(cellheight) || !is.na(cellwidth)) {
      ref_dim <- if (!is.na(cellheight)) cellheight else cellwidth
      calculated_fontsize <- min(20, max(5, round(ref_dim * 0.35)))
      calculated_fontsize_number <- max(4, round(calculated_fontsize * 0.9))
    } else {
      calculated_fontsize <- min(20, max(6, round(200 / nmax)))
      calculated_fontsize_number <- max(5, round(calculated_fontsize * 0.9))
    }
  } else {
    calculated_fontsize <- 16
    calculated_fontsize_number <- 14
  }

  # Counts are integers; percentages read better with one decimal.
  number_format <- if (identical(values, "raw")) "%.0f" else "%.1f"

  heatmap_plot <- pheatmap::pheatmap(
    display_matrix,
    main = title,
    color = color_palette,
    breaks = if (uniform_values) uniform_breaks else NA,
    cluster_rows = use_row_clustering,
    cluster_cols = use_col_clustering,
    clustering_distance_rows = "euclidean",  # Use Euclidean for clustering
    clustering_distance_cols = "euclidean",  # Use Euclidean for clustering
    clustering_method = "average",
    display_numbers = show_numbers,
    number_format = number_format,
    number_color = "black",
    fontsize = calculated_fontsize,
    fontsize_number = calculated_fontsize_number,
    cellwidth = cellwidth,
    cellheight = cellheight,
    border_color = "lightgray",
    show_rownames = TRUE,
    show_colnames = TRUE,
    angle_col = 45,
    legend = TRUE
  )

  if (!is.null(filename)) {
    dev.off()
    message("Heatmap saved to: ", filename)
  }

  # Expose what was plotted so callers (and tests) need not scrape grid grobs.
  attr(heatmap_plot, "plotted_values") <- display_matrix

  return(invisible(heatmap_plot))
}

#' Create a simple heatmap from PubMatrix results
#'
#' A simplified version of plot_pubmatrix_heatmap for quick visualization
#'
#' @param matrix A numeric matrix from PubMatrix results
#' @param title Character string for the heatmap title
#' @param values Character scalar passed to [plot_pubmatrix_heatmap()]. Defaults
#'   to `"raw"` (co-occurrence counts).
#' @return A pheatmap object (invisible)
#' @examples
#' # Create a small test matrix
#' test_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
#' rownames(test_matrix) <- c("Gene1", "Gene2")
#' colnames(test_matrix) <- c("GeneA", "GeneB")
#'
#' # Create simple heatmap (wrapper)
#' pubmatrix_heatmap(test_matrix, title = "Simple Test Heatmap")
#'
#' # Equivalent pheatmap call
#' pheatmap::pheatmap(
#'   test_matrix,
#'   main = "Simple Test Heatmap (pheatmap)",
#'   color = colorRampPalette(c("#fee5d9", "#cb181d"))(100),
#'   display_numbers = TRUE,
#'   fontsize = 16,
#'   fontsize_number = 14
#' )
#' @export
pubmatrix_heatmap <- function(matrix, title = "PubMatrix Results",
                              values = c("raw", "row_pct", "relative")) {
  plot_pubmatrix_heatmap(matrix, values = match.arg(values), title = title,
                         cluster_rows = TRUE, cluster_cols = TRUE)
}
