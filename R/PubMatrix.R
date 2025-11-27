#' PubMatrix function
#'
#' This function takes two vectors A and B, an API key, a database name,
#' and performs a search operation on the specified database using the search terms from A and B.
#' It then generates a dataframe of search results and optionally exports it as a .csv file.
#' Use plot_pubmatrix_heatmap() to visualize the results as a heatmap.
#'
#' @param file A file containing search terms (optional). If NULL, search terms will be read from A and B vectors.
#' @param A A vector of search terms to be paired with B. If NULL, search terms will be read from the file.
#' @param B A vector of search terms to be paired with A. If NULL, search terms will be read from the file.
#' @param API.key An API key obtained from Entrez; not necessary.
#' @param Database Either 'pubmed' or 'pmc'. Determines the database to search.
#' @param daterange A range of dates to search if desired. Should be a vector of two elements: the start and end date.
#' @param outfile A file path to export the search dataframe as a .csv file. If NULL, no file will be exported.
#' @return A dataframe of search results. Each element of the dataframe is the number of search results for a pair of search terms from A and B.
#' @importFrom stringr str_extract_all
#' @importFrom grDevices dev.off png
#' @importFrom utils write.csv
#' @importFrom pbapply pblapply
#' @importFrom xml2 read_html
#' @examples
#' # Note: This example requires internet connection
#' A <- c("WNT1", "WNT2")
#' B <- c("FZD1", "FZD2")
#' # result <- PubMatrix(A = A, B = B, Database = "pubmed", daterange = c(2020, 2023))
#' # print(result)
#' message("Example commented out to avoid internet dependency in checks")
#' @export
PubMatrix <- function(file = NULL, A = NULL, B = NULL, API.key = NULL,
                      Database = "pubmed", daterange = NULL, outfile = NULL) {
  # Input validation
  if (is.null(A) & is.null(B)) {
    if (missing(file) || is.null(file)) {
      stop("Either provide vectors A and B, or specify a file containing search terms")
    }
    file_content <- readLines(file, warn = FALSE)
    if (!"#" %in% file_content) {
      stop("File must contain '#' separator between A and B term lists")
    }
    A <- file_content[seq_len(which(file_content == "#")[1] - 1)]
    B <- file_content[seq.int(which(file_content == "#")[1] + 1, length(file_content))]
  }

  if (length(A) == 0 || length(B) == 0) {
    stop("Both A and B must contain at least one search term")
  }

  # Create list of all search term combinations
  search_list_raw <- expand.grid(A = A, B = B, stringsAsFactors = FALSE)
  search_list_comb <- paste(search_list_raw$A, search_list_raw$B, sep = "+AND+")
  search_list <- gsub(" ", "+", search_list_comb)

  # Construct base URLs
  url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?", "db=", Database)
  result_url_base <- paste0('<b><a href="https://www.ncbi.nlm.nih.gov/', Database, "/?term=")

  # Add API key if provided
  if (!is.null(API.key)) {
    url <- paste0(url, "&api_key=", API.key)
  }

  # Add date range if provided
  if (!is.null(daterange)) {
    mindate <- daterange[1]
    maxdate <- daterange[2]
    url <- paste0(url, "&datetype=pdat", "&mindate=", mindate, "&maxdate=", maxdate)
    result_url_base <- paste0(result_url_base, mindate, ":", maxdate, "[DP]+AND+")
  }

  message(url)
  message(result_url_base)

  # Perform searches and extract counts
  z <- unlist(
    pbapply::pblapply(
      search_list,
      function(x) {
        as.numeric(stringr::str_extract_all(
          as.character(xml2::read_html(paste0(url, "&term=", x, "&usehistory=y"))),
          "(?<=<esearchresult><count>)\\d+"
        )[[1]])
      }
    )
  )

  expected_length <- length(A) * length(B)
  if (length(z) != expected_length) {
    stop(
      "Mismatched search results. Expected ", expected_length,
      " results, but received ", length(z),
      ". Check NCBI response or search terms."
    )
  }

  ### output: Create the dataframe

  search_results_df <- data.frame(
    A_term = search_list_raw$A,
    B_term = search_list_raw$B,
    Count = z,
    stringsAsFactors = FALSE
  )

  result_dataframe <- data.frame(B_term = B, stringsAsFactors = FALSE)

  for (i in seq_along(A)) {
    term_A <- A[i]
    start_index <- (i - 1) * length(B) + 1
    end_index <- i * length(B)

    column_data <- z[start_index:end_index]
    result_dataframe[[term_A]] <- column_data
  }

  rownames(result_dataframe) <- result_dataframe$B_term
  result_dataframe$B_term <- NULL


  ### export
  if (!is.null(outfile)) {
    if (grepl(".csv", outfile)) {
      outfile <- sub(".csv", "", outfile)
    }

    result_url_xlsx <- paste0("https://www.ncbi.nlm.nih.gov/", Database, "/?term=")
    url_vector <- paste0(result_url_xlsx, search_list)
    df_final_export <- result_dataframe

    create_hyperlink_df <- function(data_df, url_vec, count_vec, rows, cols) {
      temp_df <- data_df
      temp_df[] <- NA_character_

      for (j in seq_len(cols)) {
        # The slice of url_vec and count_vec corresponding to the current column (A[j])
        start_index <- (j - 1) * rows + 1
        end_index <- j * rows

        url_slice <- url_vec[start_index:end_index]
        count_slice <- count_vec[start_index:end_index]

        # Construct the HYPERLINK formula for each cell in the column
        temp_df[, j] <- paste0(
          "=HYPERLINK(\"",
          url_slice,
          "\",\"",
          count_slice,
          "\")"
        )
      }
      return(temp_df)
    }

    df_hyperlink <- create_hyperlink_df(
      data_df = result_dataframe,
      url_vec = url_vector,
      count_vec = z,
      rows = length(B),
      cols = length(A)
    )

    colnames(df_hyperlink) <- colnames(result_dataframe)
    rownames(df_hyperlink) <- rownames(result_dataframe)

    write.csv(df_hyperlink, file = paste0(outfile, "_result.csv"), row.names = TRUE)
  }

  return(result_dataframe)
}
