# Internal helper: parse count from an E-utilities XML response.
.pubmatrix_extract_count <- function(xml_doc) {
  count_node <- xml2::xml_find_first(xml_doc, ".//Count | .//count")
  if (inherits(count_node, "xml_missing")) {
    stop("NCBI response did not contain a <Count> field.", call. = FALSE)
  }

  count_text <- trimws(xml2::xml_text(count_node))
  if (!grepl("^[0-9]+$", count_text)) {
    stop("NCBI response contained a non-numeric count value.", call. = FALSE)
  }
  count_value <- as.numeric(count_text)

  count_value
}

.pubmatrix_fetch_count <- function(base_url, encoded_term, n_tries = 2L) {
  n_tries <- as.integer(n_tries)
  if (is.na(n_tries) || n_tries < 1L) {
    n_tries <- 1L
  }

  query_url <- paste0(base_url, "&term=", encoded_term, "&usehistory=y")
  last_error <- NULL

  for (attempt in seq_len(n_tries)) {
    result <- tryCatch(
      list(
        ok = TRUE,
        value = {
          doc <- xml2::read_xml(query_url)
          .pubmatrix_extract_count(doc)
        }
      ),
      error = function(e) {
        list(ok = FALSE, error = e)
      }
    )

    if (isTRUE(result$ok)) {
      return(result$value)
    }
    last_error <- result$error

    if (attempt < n_tries) {
      Sys.sleep(0.25)
    }
  }

  stop(
    "Failed to retrieve search count from NCBI after ",
    n_tries,
    " attempt(s): ",
    conditionMessage(last_error),
    call. = FALSE
  )
}

.pubmatrix_validate_daterange <- function(daterange) {
  if (is.null(daterange)) {
    return(NULL)
  }

  if (length(daterange) != 2L) {
    stop("daterange must be a numeric vector of length 2.", call. = FALSE)
  }

  if (!is.numeric(daterange) || anyNA(daterange) || any(!is.finite(daterange))) {
    stop("daterange must contain two finite numeric year values.", call. = FALSE)
  }

  daterange <- as.integer(round(daterange))
  if (daterange[1] > daterange[2]) {
    stop("daterange start year must be less than or equal to end year.", call. = FALSE)
  }

  daterange
}

#' Query PubMed or PMC and Build a Pairwise Co-occurrence Matrix
#'
#' `PubMatrix()` counts publications for all pairwise combinations of two term
#' sets using the NCBI Entrez E-utilities API. It returns a matrix-like data
#' frame with rows corresponding to terms in `B` and columns corresponding to
#' terms in `A`.
#'
#' @param file Optional path to a text file containing search terms. The file
#'   must contain a `#` separator line between the `A` and `B` term lists. Used
#'   only when `A` and `B` are both `NULL`.
#' @param A Character vector of search terms for matrix columns.
#' @param B Character vector of search terms for matrix rows.
#' @param API.key Optional NCBI API key.
#' @param Database Character scalar. One of `"pubmed"` or `"pmc"`.
#' @param daterange Optional numeric vector of length 2 giving
#'   `c(start_year, end_year)`.
#' @param outfile Optional output file stem used when `export_format` is set.
#' @param export_format Optional export format: `"csv"` or `"ods"`.
#'
#' @return A data frame of publication counts with rows named by `B` and columns
#'   named by `A`.
#'
#' @details Examples and vignettes should avoid live web queries during package
#' checks. This function performs live requests to NCBI and may fail when there
#' is no internet connectivity or when the service is unavailable.
#'
#' @importFrom pbapply pblapply
#' @importFrom readODS write_ods
#' @importFrom utils URLencode write.csv
#' @importFrom xml2 read_xml xml_find_first xml_text
#'
#' @examples
#' # Construct terms locally (live query example kept commented for checks)
#' A <- c("WNT1", "WNT2")
#' B <- c("FZD1", "FZD2")
#' # result <- PubMatrix(A = A, B = B, Database = "pubmed", daterange = c(2020, 2023))
#' # print(result)
#' message("Live query example is commented out to avoid web access during checks.")
#'
#' # Input validation examples (offline-safe)
#' try(PubMatrix(A = NULL, B = NULL, file = NULL))
#' try(PubMatrix(A = "a", B = "b", Database = "invalid_db"))
#'
#' @export
PubMatrix <- function(file = NULL, A = NULL, B = NULL, API.key = NULL,
                      Database = "pubmed", daterange = NULL, outfile = NULL, export_format = NULL) {
  # Validate export settings before any network activity.
  if (!is.null(export_format)) {
    export_format <- tolower(as.character(export_format)[1])
    if (!export_format %in% c("csv", "ods")) {
      stop("export_format must be either 'csv' or 'ods'.", call. = FALSE)
    }
    if (is.null(outfile) || !nzchar(as.character(outfile)[1])) {
      stop("outfile must be provided when export_format is specified.", call. = FALSE)
    }
  }

  # Validate database before constructing any query.
  Database <- tolower(as.character(Database)[1])
  if (!Database %in% c("pubmed", "pmc")) {
    stop("Database must be one of 'pubmed' or 'pmc'.", call. = FALSE)
  }

  daterange <- .pubmatrix_validate_daterange(daterange)

  # Resolve search terms from file or vectors.
  if (is.null(A) && is.null(B)) {
    if (missing(file) || is.null(file)) {
      stop("Either provide vectors A and B, or specify a file containing search terms.", call. = FALSE)
    }
    if (!file.exists(file)) {
      stop("file does not exist: ", file, call. = FALSE)
    }

    file_content <- readLines(file, warn = FALSE)
    sep_idx <- match("#", file_content)
    if (is.na(sep_idx)) {
      stop("File must contain '#' separator between A and B term lists.", call. = FALSE)
    }

    A <- file_content[seq_len(sep_idx - 1L)]
    B <- file_content[seq.int(sep_idx + 1L, length(file_content))]
  } else if (is.null(A) || is.null(B)) {
    stop("Provide both A and B when file is NULL.", call. = FALSE)
  }

  A <- as.character(A)
  B <- as.character(B)

  if (length(A) == 0L || length(B) == 0L) {
    stop("Both A and B must contain at least one search term.", call. = FALSE)
  }

  if (anyNA(A) || anyNA(B) || any(!nzchar(A)) || any(!nzchar(B))) {
    stop("A and B must contain non-empty, non-missing search terms.", call. = FALSE)
  }

  # Build search combinations and encoded query terms.
  # Use B-first expand.grid so rows are grouped by each A term (matching matrix assembly below).
  search_list_raw <- expand.grid(B = B, A = A, stringsAsFactors = FALSE)
  search_terms <- paste(search_list_raw$A, search_list_raw$B, sep = " AND ")
  encoded_search_terms <- vapply(
    search_terms,
    utils::URLencode,
    FUN.VALUE = character(1),
    reserved = TRUE
  )

  # Construct base E-utilities URL (do not message it to avoid leaking API keys).
  base_url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?",
    "db=", Database
  )
  if (!is.null(API.key) && nzchar(as.character(API.key)[1])) {
    base_url <- paste0(base_url, "&api_key=", utils::URLencode(as.character(API.key)[1], reserved = TRUE))
  }
  if (!is.null(daterange)) {
    base_url <- paste0(
      base_url,
      "&datetype=pdat",
      "&mindate=", daterange[1],
      "&maxdate=", daterange[2]
    )
  }

  n_tries <- getOption("PubMatrixR.n_tries", 2L)
  z <- unlist(
    pbapply::pblapply(
      encoded_search_terms,
      function(term) .pubmatrix_fetch_count(base_url, term, n_tries = n_tries)
    ),
    use.names = FALSE
  )

  expected_length <- length(A) * length(B)
  if (length(z) != expected_length) {
    stop(
      "Mismatched search results. Expected ", expected_length,
      " results, but received ", length(z),
      ". Check NCBI response or search terms.",
      call. = FALSE
    )
  }

  # Build the matrix-like result data frame (rows = B, columns = A).
  result_dataframe <- data.frame(B_term = B, stringsAsFactors = FALSE)
  for (i in seq_along(A)) {
    start_index <- (i - 1L) * length(B) + 1L
    end_index <- i * length(B)
    result_dataframe[[A[i]]] <- z[start_index:end_index]
  }
  rownames(result_dataframe) <- result_dataframe$B_term
  result_dataframe$B_term <- NULL

  # Optional export with hyperlink formulas.
  if (!is.null(outfile) && !is.null(export_format)) {
    outfile <- as.character(outfile)[1]
    if (grepl("\\.csv$|\\.ods$", outfile, ignore.case = TRUE)) {
      outfile <- sub("\\.csv$|\\.ods$", "", outfile, ignore.case = TRUE)
    }

    result_url_base <- paste0("https://www.ncbi.nlm.nih.gov/", Database, "/?term=")
    url_vector <- paste0(result_url_base, encoded_search_terms)

    create_hyperlink_df <- function(data_df, url_vec, count_vec, rows, cols) {
      temp_df <- data_df
      temp_df[] <- NA_character_

      for (j in seq_len(cols)) {
        start_index <- (j - 1L) * rows + 1L
        end_index <- j * rows

        url_slice <- url_vec[start_index:end_index]
        count_slice <- count_vec[start_index:end_index]

        temp_df[, j] <- paste0(
          "=HYPERLINK(\"",
          url_slice,
          "\",\"",
          count_slice,
          "\")"
        )
      }

      temp_df
    }

    df_hyperlink <- create_hyperlink_df(
      data_df = result_dataframe,
      url_vec = url_vector,
      count_vec = z,
      rows = length(B),
      cols = length(A)
    )

    if (identical(export_format, "csv")) {
      utils::write.csv(df_hyperlink, file = paste0(outfile, "_result.csv"), row.names = TRUE)
    } else {
      df_with_rownames <- data.frame(
        rowname = rownames(df_hyperlink),
        df_hyperlink,
        check.names = FALSE
      )
      readODS::write_ods(df_with_rownames, path = paste0(outfile, "_result.ods"))
    }
  }

  result_dataframe
}
