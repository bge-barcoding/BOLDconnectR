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
# Uses httr::GET instead of fromJSON(url(...)) for better handling of long URLs
.parse_query <- function(query_terms) {
  quoted <- paste0('%22', gsub(' ', '%20', query_terms), '%22')
  combined <- paste(quoted, collapse = '%20')
  full_url <- URLencode(paste0(BOLD_PORTAL_PARSE, combined))
  res <- GET(url = full_url, add_headers('accept' = 'application/json'))
  stop_for_status(res)
  fromJSON(content(res, "text", encoding = "UTF-8"))
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

  # Filter out terms that resolved to raw IDs instead of taxonomy/geography.
  # This happens when BOLD can't match a name taxonomically (e.g. mites,
  # obscure taxa). Instead of failing the whole batch, drop those terms and
  # warn so the remaining valid species can still be searched.
  ids_mask <- grepl("ids:", successful$matched)
  if (any(ids_mask)) {
    bad_names <- successful$names[ids_mask]
    warning("The following terms could not be resolved taxonomically and were skipped: ",
            paste(bad_names, collapse = ", "), call. = FALSE)
    successful <- successful[!ids_mask, , drop = FALSE]
    if (nrow(successful) == 0) {
      stop("No terms could be resolved taxonomically. Re-check search queries.")
    }
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

# Step 4: Generate query ID(s) and build download URL(s)
# When many species are in the batch, the matched triplets (e.g.
# "taxonomy:Animalia,Chordata,...,Panthera leo") can be very long.
# Concatenating them all with semicolons can exceed the ~2048 char URL limit,
# causing HTTP 422. This function splits terms into sub-groups that each fit
# within the limit and returns a vector of download URLs.
.generate_query_id <- function(counts_df, max_url_length = 1500) {
  non_zero <- counts_df %>%
    filter(observations > 0) %>%
    arrange(desc(observations))

  if (nrow(non_zero) == 0) return(character(0))

  # Encode each matched term individually so we can measure lengths
  encode_term <- function(term) {
    gsub("/", "%2F",
      gsub(" ", "%20",
        gsub(":", "%3A",
          gsub(";", "%3B",
            gsub(",", "%2C", term)))))
  }

  encoded_terms <- vapply(non_zero$matched, encode_term, character(1),
                          USE.NAMES = FALSE)

  # Base: "https://portal.boldsystems.org/api/query?query=" (48) + "&extent=full" (12) = 60
  base_len <- nchar(BOLD_PORTAL_QUERY) + nchar("&extent=full")

  # Group terms into sub-batches that fit within max_url_length
  groups <- list()
  current_group <- character(0)
  current_len <- base_len

  for (i in seq_along(encoded_terms)) {
    term <- encoded_terms[i]
    # Separator between terms is encoded ";" = "%3B" (3 chars)
    sep_len <- if (length(current_group) > 0) 3 else 0
    term_len <- nchar(term)

    if (current_len + sep_len + term_len > max_url_length && length(current_group) > 0) {
      groups[[length(groups) + 1]] <- current_group
      current_group <- term
      current_len <- base_len + term_len
    } else {
      current_group <- c(current_group, term)
      current_len <- current_len + sep_len + term_len
    }
  }
  if (length(current_group) > 0) {
    groups[[length(groups) + 1]] <- current_group
  }

  # Make a query call per group and collect download URLs
  download_urls <- vapply(groups, function(group) {
    query_part <- paste(group, collapse = "%3B")
    full_url <- paste0(BOLD_PORTAL_QUERY, query_part, "&extent=full")
    res <- GET(url = full_url, add_headers('accept' = 'application/json'))
    stop_for_status(res)

    query_id <- fromJSON(content(res, "text", encoding = "UTF-8"))$query_id

    paste0(BOLD_PORTAL_DOWNLOAD,
           gsub("=", "%3D", query_id),
           "/download?format=tsv&fields=processid,marker_code")
  }, character(1))

  download_urls
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

  download_urls <- .generate_query_id(counts)
  if (length(download_urls) == 0) return(NULL)

  all_data <- lapply(download_urls, .obtain_data)
  all_data <- Filter(Negate(is.null), all_data)
  if (length(all_data) == 0) return(NULL)

  bind_rows(all_data) %>% distinct()
}

# ---------------------------------------------------------------------------
# Public function: bold_public_search_batch
# ---------------------------------------------------------------------------
#' Batch search for multiple species with resilience to missing taxa
#'
#' Sends species in batches of up to `batch_size` names per API call to stay
#' within URL length limits (~2048 chars). Within each batch, species not found
#' on BOLD are skipped with a warning (taxonomy-only search resilience).
#' If geography or other parameters are also provided, the function falls back
#' to one-species-at-a-time to preserve the multi-parameter safety guard.
#'
#' @param species_list Character vector of species names
#' @param geography   Optional list of geography filters (applied to all species)
#' @param bins        Optional list of BIN filters
#' @param institutes  Optional list of institute filters
#' @param dataset_codes Optional list of dataset code filters
#' @param project_codes Optional list of project code filters
#' @param max_url_length Max URL length per API call (default 1500). Species
#'                       are grouped into batches that keep URLs under this limit.
#'                       The BOLD portal has a ~2048 char URL limit; 1500 provides
#'                       headroom for encoding overhead. Only used for taxonomy-only
#'                       searches; ignored when other params are set.
#' @param quiet       Suppress per-species progress messages (default FALSE)
#' @param sleep       Seconds to pause between API calls (default 0.5)
#' @return List with $data (combined data frame) and $summary (text report)
bold_public_search_batch <- function(species_list,
                                     geography = NULL,
                                     bins = NULL,
                                     institutes = NULL,
                                     dataset_codes = NULL,
                                     project_codes = NULL,
                                     max_url_length = 1500,
                                     quiet = FALSE,
                                     sleep = 0.5) {

  has_other_params <- !is.null(geography) || !is.null(bins) ||
    !is.null(institutes) || !is.null(dataset_codes) || !is.null(project_codes)

  results <- list()
  all_missing <- character(0)

  if (!has_other_params) {
    # --- Taxonomy-only: batch by URL length ---
    # bold_public_search + .counts_query already skip zero-count terms with a
    # warning for taxonomy-only searches, so we can send many names at once.
    # We group species into batches that keep the parse URL under max_url_length.

    # Base URL length: "https://portal.boldsystems.org/api/query/parse?query=" = 53 chars
    base_len <- 53
    batches <- list()
    current_batch <- character(0)
    # Each species adds: %22Name%20Name%22 = 4 + nchar(name) + 3*(spaces) chars,
    # plus %20 separator (3 chars) between terms
    current_len <- base_len

    for (sp in species_list) {
      # Encoded length: %22 (3) + name with spaces as %20 (nchar + 2*spaces) + %22 (3)
      n_spaces <- lengths(regmatches(sp, gregexpr(" ", sp)))
      sp_encoded_len <- 3 + nchar(sp) + (n_spaces * 2) + 3
      sep_len <- if (length(current_batch) > 0) 3 else 0  # %20 separator

      if (current_len + sep_len + sp_encoded_len > max_url_length && length(current_batch) > 0) {
        batches[[length(batches) + 1]] <- current_batch
        current_batch <- sp
        current_len <- base_len + sp_encoded_len
      } else {
        current_batch <- c(current_batch, sp)
        current_len <- current_len + sep_len + sp_encoded_len
      }
    }
    if (length(current_batch) > 0) {
      batches[[length(batches) + 1]] <- current_batch
    }

    for (b in seq_along(batches)) {
      batch <- batches[[b]]

      if (!quiet) {
        message(sprintf("[Batch %d/%d] Searching %d species...",
                        b, length(batches), length(batch)))
      }

      res <- tryCatch({
        # Capture warnings to extract skipped species names
        batch_warnings <- character(0)
        result <- withCallingHandlers(
          bold_public_search(taxonomy = as.list(batch)),
          warning = function(w) {
            batch_warnings <<- c(batch_warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        )

        # Parse skipped species from warning messages
        for (wmsg in batch_warnings) {
          if (grepl("returned 0 records and were skipped", wmsg)) {
            skipped <- sub(".*skipped: ", "", wmsg)
            all_missing <<- c(all_missing, trimws(strsplit(skipped, ",")[[1]]))
          }
          if (grepl("could not be resolved taxonomically and were skipped", wmsg)) {
            skipped <- sub(".*skipped: ", "", wmsg)
            all_missing <<- c(all_missing, trimws(strsplit(skipped, ",")[[1]]))
          }
          if (!quiet) warning(wmsg, call. = FALSE)
        }

        result
      },
      error = function(e) {
        if (!quiet) warning("Batch failed: ", e$message, call. = FALSE)
        NULL
      })

      if (!is.null(res) && nrow(res) > 0) {
        results[[length(results) + 1]] <- res
      }

      if (b < length(batches)) Sys.sleep(sleep)
    }

  } else {
    # --- Multi-parameter: one species at a time ---
    # With geography/bins/etc, the zero-count guard must stay active to prevent
    # silent data corruption, so we search each species individually.

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
        results[[length(results) + 1]] <- res
      } else {
        all_missing <- c(all_missing, sp)
        if (!quiet) warning("No records found for: ", sp, call. = FALSE)
      }

      if (i < length(species_list)) Sys.sleep(sleep)
    }
  }

  # --- Combine results ---
  if (length(results) == 0) {
    summary_text <- sprintf("No records found for any of the %d species searched.",
                            length(species_list))
    return(list(data = NULL, summary = summary_text))
  }

  combined <- bind_rows(results) %>% distinct()
  n_found <- length(species_list) - length(all_missing)

  summary_text <- sprintf(
    "Retrieved %d records for %d of %d species.%s",
    nrow(combined),
    n_found,
    length(species_list),
    if (length(all_missing) > 0)
      paste0("\nNot found on BOLD: ", paste(all_missing, collapse = ", "))
    else ""
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
