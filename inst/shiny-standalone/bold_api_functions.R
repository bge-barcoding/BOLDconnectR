# Standalone BOLD API functions for use in Shiny apps
# No BOLDconnectR dependency required.
# Required packages: httr, jsonlite, dplyr, data.table
#
# These functions replicate the core functionality of bold.public.search()
# and bold.fetch() from BOLDconnectR, calling the BOLD APIs directly.
#
# Usage:
#   source("bold_api_functions.R")
#   results <- bold_public_search(taxonomy = list("Panthera leo", "Panthera uncia"))
#   data    <- bold_fetch(api_key = "your-key", get_by = "processid",
#                         identifiers = results$processid)

library(httr)
library(jsonlite)
library(dplyr)
library(data.table)

# ---------------------------------------------------------------------------
# API endpoint constants
# ---------------------------------------------------------------------------
BOLD_PORTAL_PARSE        <- "https://portal.boldsystems.org/api/query/parse?query="
BOLD_PORTAL_PREPROCESS   <- "https://portal.boldsystems.org/api/query/preprocessor?query="
BOLD_PORTAL_SUMMARY      <- "https://portal.boldsystems.org/api/summary?query="
BOLD_PORTAL_QUERY        <- "https://portal.boldsystems.org/api/query?query="
BOLD_PORTAL_DOWNLOAD     <- "https://portal.boldsystems.org/api/documents/"
BOLD_DATA_RETRIEVE       <- "https://data.boldsystems.org/api/records/retrieve?"

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Step 1: Parse the query terms
.parse_query <- function(query_terms) {
  quoted <- paste0('%22', gsub(' ', '%20', query_terms), '%22')
  combined <- paste(quoted, collapse = '%20')
  full_url <- URLencode(paste0(BOLD_PORTAL_PARSE, combined))
  fromJSON(url(full_url))
}

# Step 2: Preprocess — resolve terms to BOLD triplets
.preprocess_query <- function(parsed) {
  encoded <- gsub(":", "%3A",
               gsub(";", "%3B",
                 gsub(",", "%2C",
                   gsub(" ", "%20", parsed$terms))))
  full_url <- URLencode(paste0(BOLD_PORTAL_PREPROCESS, encoded))

  res <- GET(url = full_url, add_headers('accept' = 'application/json'))
  stop_for_status(res)

  json_data <- fromJSON(content(res, "text", encoding = "UTF-8"))
  successful <- json_data$successful_terms
  successful$matched <- gsub(',.*', "", successful$matched)
  successful$names <- gsub("^na:na:", "", successful$submitted)

  # Verify no "ids:" prefix (indicates bad input)
  if (any(grepl("ids:", successful$matched))) {
    stop("Re-check search queries — some terms resolved to IDs instead of taxonomy/geography.")
  }

  successful
}

# Step 3: Count records per term
.counts_query <- function(preprocessed, taxonomy_only = FALSE) {
  encoded_terms <- gsub("/", "%2F",
                     gsub(" ", "%20",
                       gsub(";", "%3B",
                         gsub(":", "%3A", preprocessed$matched))))

  counts <- sapply(encoded_terms, function(term) {
    url <- paste0(BOLD_PORTAL_SUMMARY, term,
                  "&fields=specimens&reduce_operation=count")
    res <- GET(url = url, add_headers('accept' = 'application/json'))
    stop_for_status(res)
    json <- fromJSON(content(res, "text", encoding = "UTF-8"))
    ct <- json$counts$specimens
    if (is.null(ct)) 0L else as.integer(ct)
  }, USE.NAMES = FALSE)

  preprocessed$observations <- counts

  zero_mask <- preprocessed$observations == 0

  if (any(zero_mask, na.rm = TRUE)) {
    if (taxonomy_only) {
      missing <- preprocessed$names[zero_mask]
      warning("The following terms returned 0 records and were skipped: ",
              paste(missing, collapse = ", "), call. = FALSE)
      preprocessed <- preprocessed[!zero_mask, , drop = FALSE]
      if (nrow(preprocessed) == 0) return(NULL)
    } else {
      return(NULL)
    }
  }

  preprocessed
}

# Step 4: Generate query ID and build download URL
.generate_query_id <- function(counts_df) {
  non_zero <- counts_df %>%
    filter(observations > 0) %>%
    arrange(desc(observations))

  query_part <- gsub("/", "%2F",
                  gsub(" ", "%20",
                    gsub(":", "%3A",
                      gsub(";", "%3B",
                        gsub(",", "%2C",
                          paste(non_zero$matched, collapse = ";"))))))

  full_url <- paste0(BOLD_PORTAL_QUERY, query_part, "&extent=full")
  res <- GET(url = full_url, add_headers('accept' = 'application/json'))
  stop_for_status(res)

  query_id <- fromJSON(content(res, "text", encoding = "UTF-8"))$query_id

  paste0(BOLD_PORTAL_DOWNLOAD,
         gsub("=", "%3D", query_id),
         "/download?format=tsv&fields=processid,marker_code")
}

# Step 5: Download TSV data
.obtain_data <- function(download_url) {
  temp_file <- tempfile()
  on.exit(unlink(temp_file), add = TRUE)
  suppressWarnings(download.file(download_url, destfile = temp_file, quiet = TRUE))
  if (file.size(temp_file) == 0) return(NULL)
  read.delim(temp_file, sep = '\t', stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Public function: bold_public_search
# ---------------------------------------------------------------------------
#' Search publicly available data on BOLD
#'
#' @param taxonomy  List of taxonomic names (any rank)
#' @param geography List of country/province/state/region/site names
#' @param bins      List of BIN URIs
#' @param institutes List of institute names
#' @param dataset_codes List of dataset codes
#' @param project_codes List of project codes
#' @return Data frame with processid and marker_code columns, or NULL
bold_public_search <- function(taxonomy = NULL,
                               geography = NULL,
                               bins = NULL,
                               institutes = NULL,
                               dataset_codes = NULL,
                               project_codes = NULL) {

  args <- list(taxonomy = taxonomy,
               geography = geography,
               bins = bins,
               institutes = institutes,
               dataset_codes = dataset_codes,
               project_codes = project_codes)

  non_null_args <- Filter(Negate(is.null), args)

  if (length(non_null_args) == 0) stop("At least one search parameter must be provided.")
  if (any(!sapply(non_null_args, is.list))) stop("All inputs must be lists.")

  query_terms <- unname(unlist(non_null_args))

  is_taxonomy_only <- (length(non_null_args) == 1 && names(non_null_args) == "taxonomy")

  parsed       <- .parse_query(query_terms)
  preprocessed <- .preprocess_query(parsed)
  counts       <- .counts_query(preprocessed, taxonomy_only = is_taxonomy_only)

  if (is.null(counts)) return(NULL)

  download_url <- .generate_query_id(counts)
  data <- .obtain_data(download_url)

  data
}

# ---------------------------------------------------------------------------
# Public function: bold_public_search_batch
# ---------------------------------------------------------------------------
#' Batch search for multiple species with resilience to missing taxa
#'
#' Iterates over each species individually. Species not found on BOLD are
#' skipped with a warning. Results are combined and deduplicated.
#'
#' @param species_list Character vector of species names
#' @param geography   Optional list of geography filters (applied to all species)
#' @param bins        Optional list of BIN filters
#' @param institutes  Optional list of institute filters
#' @param dataset_codes Optional list of dataset code filters
#' @param project_codes Optional list of project code filters
#' @param quiet       Suppress per-species progress messages (default FALSE)
#' @param sleep       Seconds to pause between API calls (default 0.5)
#' @return List with $data (combined data frame) and $summary (text report)
bold_public_search_batch <- function(species_list,
                                     geography = NULL,
                                     bins = NULL,
                                     institutes = NULL,
                                     dataset_codes = NULL,
                                     project_codes = NULL,
                                     quiet = FALSE,
                                     sleep = 0.5) {

  results <- list()
  found <- character(0)
  missing <- character(0)

  for (i in seq_along(species_list)) {
    sp <- species_list[i]

    if (!quiet) message(sprintf("[%d/%d] Searching: %s", i, length(species_list), sp))

    res <- tryCatch(
      bold_public_search(
        taxonomy = list(sp),
        geography = geography,
        bins = bins,
        institutes = institutes,
        dataset_codes = dataset_codes,
        project_codes = project_codes
      ),
      error = function(e) NULL
    )

    if (!is.null(res) && nrow(res) > 0) {
      found <- c(found, sp)
      results[[length(results) + 1]] <- res
    } else {
      missing <- c(missing, sp)
      if (!quiet) warning("No records found for: ", sp, call. = FALSE)
    }

    if (i < length(species_list)) Sys.sleep(sleep)
  }

  if (length(results) == 0) {
    summary_text <- sprintf("No records found for any of the %d species searched.", length(species_list))
    return(list(data = NULL, summary = summary_text))
  }

  combined <- bind_rows(results) %>% distinct()

  summary_text <- sprintf(
    "Retrieved %d records for %d of %d species.%s",
    nrow(combined),
    length(found),
    length(species_list),
    if (length(missing) > 0) paste0("\nNot found on BOLD: ", paste(missing, collapse = ", ")) else ""
  )

  if (!quiet) message(summary_text)

  list(data = combined, summary = summary_text)
}

# ---------------------------------------------------------------------------
# Public function: bold_fetch
# ---------------------------------------------------------------------------
#' Fetch BCDM data from BOLD using an API key
#'
#' @param api_key     Your BOLD API key
#' @param get_by      One of "processid" or "sampleid"
#' @param identifiers Character vector of IDs to fetch
#' @param batch_size  Number of IDs per API call (default 5000)
#' @return Data frame in BCDM format
bold_fetch <- function(api_key,
                       get_by = "processid",
                       identifiers,
                       batch_size = 5000) {

  if (!get_by %in% c("processid", "sampleid")) {
    stop("get_by must be 'processid' or 'sampleid'")
  }

  identifiers <- unique(identifiers)

  query_params <- list(
    input_type = get_by,
    batch_size = 10000,
    offset = 0,
    record_sep = ",",
    last_updated_threshold = 0
  )

  # Split into batches
  n <- length(identifiers)
  batches <- split(identifiers, ceiling(seq_len(n) / batch_size))

  all_results <- list()

  for (b in seq_along(batches)) {
    message(sprintf("Downloading batch %d of %d", b, length(batches)))

    id_string <- paste(batches[[b]], collapse = ",")
    temp_file <- tempfile()
    writeLines(id_string, temp_file)

    res <- POST(
      url = BOLD_DATA_RETRIEVE,
      query = query_params,
      add_headers(
        'accept' = 'application/json',
        'api-key' = api_key,
        'Content-Type' = 'multipart/form-data'
      ),
      body = list(input_file = upload_file(temp_file))
    )

    unlink(temp_file)
    stop_for_status(res)

    json_text <- content(res, "text", encoding = "UTF-8")
    json_lines <- strsplit(json_text, "\n")[[1]]
    json_list <- lapply(json_lines, fromJSON)

    # Collapse multi-value columns
    collapse_cols <- c("bold_recordset_code_arr", "coord", "marker_code",
                       "primers_forward", "primers_reverse")
    json_list <- lapply(json_list, function(x) {
      for (col in collapse_cols) {
        if (!is.null(x[[col]])) {
          x[[col]] <- paste(unlist(x[[col]]), collapse = ",")
        }
      }
      x
    })

    batch_df <- rbindlist(json_list, fill = TRUE, use.names = TRUE) %>%
      as.data.frame()

    all_results[[b]] <- batch_df
  }

  combined <- bind_rows(all_results)
  rownames(combined) <- NULL

  message("Download complete. ", nrow(combined), " records retrieved.")
  combined
}
