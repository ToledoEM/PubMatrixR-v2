#' Create a formatted heatmap from PubMatrix results
#'
#' This function creates a heatmap displaying Jaccard distance values calculated from
#' a PubMatrix result matrix, with Euclidean distance clustering for rows and columns.
#'
#' @param matrix A data frame or matrix from PubMatrix results containing publication co-occurrence counts
#' @param title Character string for the heatmap title. Default is "PubMatrix Co-occurrence Heatmap"
#' @param cluster_rows Logical value determining if rows should be clustered using Euclidean distance. Default is TRUE
#' @param cluster_cols Logical value determining if columns should be clustered using Euclidean distance. Default is TRUE
#' @param show_numbers Logical value determining if Jaccard distance values should be displayed in cells. Default is TRUE
#' @param color_palette Color palette for the heatmap. Default uses a red gradient color scale
#' @param filename Optional filename to save the heatmap. If NULL, displays the plot
#' @param width Width of saved plot in inches. Default is 10
#' @param height Height of saved plot in inches. Default is 8
#' @param cellwidth Optional numeric cell width for pheatmap (in pixels). Default `NA` lets pheatmap auto-size.
#' @param cellheight Optional numeric cell height for pheatmap (in pixels). Default `NA` lets pheatmap auto-size.
#' @details
#' The function displays Jaccard distance values in the heatmap cells (same as compute_jaccard_matrix)
#' and uses Euclidean distance for clustering rows and columns. Jaccard distance is calculated as
#' 1 - (intersection/union) where intersection is the number of common non-zero elements
#' and union is the total number of non-zero elements. NA values in the input matrix are converted to 0 before calculation to ensure stability.
#' @return A pheatmap object (invisible)
#' @importFrom grDevices png dev.off colorRampPalette
#' @importFrom stats dist
#' @export
#' @examples
#' # Create a small test matrix
#' test_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
#' rownames(test_matrix) <- c("Gene1", "Gene2")
#' colnames(test_matrix) <- c("GeneA", "GeneB")
#'
#' # Create heatmap using the helper
#' plot_pubmatrix_heatmap(test_matrix, title = "Test Heatmap")
#'
#' # Equivalent using pheatmap directly:
#' # Compute overlap matrix as the function does (here trivial because counts are raw)
#' overlap_matrix <- test_matrix
#' pheatmap::pheatmap(
#'   overlap_matrix,
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
  # --- Input validation and coercion ---
  if (is.data.frame(matrix)) {
    # Convert data frame to matrix
    matrix <- as.matrix(matrix)
  }

  if (!is.matrix(matrix)) {
    stop("Input must be a matrix (or a data frame coercible to a matrix).")
  }

  if (!is.numeric(matrix)) {
    if (is.character(matrix)) {
      # Attempt to coerce the character matrix to numeric.
      # apply is used for column-wise coercion, preserving column names.
      matrix_coerced <- as.matrix(apply(matrix, 2, as.numeric))

      # Check if coercion failed because of non-numeric strings (like formulas)
      if (all(is.na(matrix_coerced[!is.na(matrix)]))) {
        # The matrix likely contains non-numeric strings (like HTML/Excel formulas)
        stop(
          "Input matrix is character-based and contains non-numeric data (e.g., formulas/HTML links). \n",
          "Ensure you are passing the raw numeric count matrix (the direct output of PubMatrix) and not the CSV export data."
        )
      }

      # Use the coerced matrix
      matrix <- matrix_coerced
      message("Warning: Input matrix was character and has been coerced to numeric. Check for NA values if unexpected.")
    } else {
      # Stop if it's neither numeric nor character
      stop("Input must be a numeric matrix")
    }
  }
  # --- End Input validation and coercion ---

  if (nrow(matrix) == 0 || ncol(matrix) == 0) {
    stop("Matrix must have at least one row and one column")
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
    message_output <- paste0("NA values found in the input matrix (", na_count, " total) and converted to 0 for Jaccard calculation.\n")
    message_output <- paste0(message_output, "Converted positions (Row vs Col): \n- ", paste(head(na_positions, 10), collapse = "\n- "))
    if (na_count > 10) {
      message_output <- paste0(message_output, "\n- ... (and ", na_count - 10, " more)")
    }
    message(message_output)
  }
  # --- End NA Handling ---

  # Define Jaccard distance function
  jaccard_dist <- function(x) {
    # Convert to binary (presence/absence)
    x_binary <- as.matrix(x > 0)

    # Calculate Jaccard distance
    n <- nrow(x_binary)
    dist_matrix <- matrix(0, n, n)

    for (i in seq_len(n - 1)) {
      for (j in seq.int(i + 1, n)) {
        intersection <- sum(x_binary[i, ] & x_binary[j, ])
        union <- sum(x_binary[i, ] | x_binary[j, ])

        if (union == 0) {
          jaccard_dist_val <- 1 # Maximum distance if no union
        } else {
          jaccard_dist_val <- 1 - (intersection / union)
        }

        dist_matrix[i, j] <- jaccard_dist_val
        dist_matrix[j, i] <- jaccard_dist_val
      }
    }

    return(as.dist(dist_matrix))
  }

  # Calculate overlap percentage matrix for display
  # This shows the percentage of overlap versus total overlaps using actual counts
  overlap_matrix <- matrix(0, nrow = nrow(matrix), ncol = ncol(matrix))
  rownames(overlap_matrix) <- rownames(matrix)
  colnames(overlap_matrix) <- colnames(matrix)

  # Calculate overlap percentage for each cell using actual publication counts
  for (row_idx in seq_len(nrow(matrix))) {
    for (col_idx in seq_len(ncol(matrix))) {
      # Get the intersection (publications mentioning both terms)
      intersection <- matrix[row_idx, col_idx]
      # Get the union (total publications for row term + col term - intersection)
      row_total <- sum(matrix[row_idx, ])
      col_total <- sum(matrix[, col_idx])
      union <- row_total + col_total - intersection
      overlap_matrix[row_idx, col_idx] <- if (union == 0) 0 else round((intersection / union) * 100, 1)
    }
  }

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

  # Check for variation in the data - if all values are the same, throw an error
    overlap_range <- range(overlap_matrix, na.rm = TRUE)
    # Guard against unexpected empty ranges (avoid 'argument is of length zero')
    if (length(overlap_range) < 2 || (length(diff(overlap_range)) > 0 && diff(overlap_range) == 0)) {
      stop("Cannot create heatmap: All overlap percentages are identical (", 
           overlap_range[1], "%). No variation in data to visualize.")
    }

  # Create the heatmap
  if (!is.null(filename)) {
    # Save to file
    png(filename = filename, width = width, height = height, units = "in", res = 300)
  }

  # Dynamically compute font sizes based on matrix dimensions so labels scale
  nmax <- max(nrow(overlap_matrix), ncol(overlap_matrix))
  # heuristic: larger matrices -> smaller fonts; keep reasonable bounds
  calculated_fontsize <- min(20, max(6, round(150 / nmax)))
  calculated_fontsize_number <- max(5, round(calculated_fontsize * 0.9))

  heatmap_plot <- pheatmap::pheatmap(
    overlap_matrix,  # Use overlap percentage matrix for display
    main = title,
    color = color_palette,
    cluster_rows = use_row_clustering,
    cluster_cols = use_col_clustering,
    clustering_distance_rows = "euclidean",  # Use Euclidean for clustering
    clustering_distance_cols = "euclidean",  # Use Euclidean for clustering
    clustering_method = "average",
    display_numbers = show_numbers,
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
      if (isTRUE(scale_font)) {
        # If explicit cell dimensions provided, prefer them
        if (!is.na(cellheight) || !is.na(cellwidth)) {
          ref_dim <- if (!is.na(cellheight)) cellheight else cellwidth
          # heuristic: font size ~ 35% of cell height (or width), bounded
          calculated_fontsize <- min(20, max(5, round(ref_dim * 0.35)))
          calculated_fontsize_number <- max(4, round(calculated_fontsize * 0.9))
        } else {
          # fallback heuristic based on overall matrix size
          calculated_fontsize <- min(20, max(6, round(200 / nmax)))
          calculated_fontsize_number <- max(5, round(calculated_fontsize * 0.9))
        }
      } else {
        # user opted out of automatic scaling
        calculated_fontsize <- 16
        calculated_fontsize_number <- 14
      }

  if (!is.null(filename)) {
    dev.off()
    message("Heatmap saved to: ", filename)
  }

  return(invisible(heatmap_plot))
}

#' Create a simple heatmap from PubMatrix results
#'
#' A simplified version of plot_pubmatrix_heatmap for quick visualization
#'
#' @param matrix A numeric matrix from PubMatrix results
#' @param title Character string for the heatmap title
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
pubmatrix_heatmap <- function(matrix, title = "PubMatrix Results") {
  plot_pubmatrix_heatmap(matrix, title = title, cluster_rows = TRUE, cluster_cols = TRUE)
}
