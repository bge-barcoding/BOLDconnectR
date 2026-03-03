#' Helper functions for bold.public.search
#' @keywords internal
#' @importFrom utils download.file
#'
# URLs

base_url_parse<- "https://portal.boldsystems.org/api/query/parse?query="

base_url_preprocess<-'https://portal.boldsystems.org/api/query/preprocessor?query='

base_url_summary<-'https://portal.boldsystems.org/api/summary?query='

base_url_query<-'https://portal.boldsystems.org/api/query?query='

# BOLD API query-parameter character limit.
# The preprocessor, query, and summary endpoints enforce maxLength: 250
# on the "query" parameter value. We use 245 for a small safety margin.
bold_query_param_limit <- 245

# Rate-limited GET with exponential backoff retry on transient errors
# (HTTP 429, 503). Retries up to max_retries times with delays of
# 2, 4, 8, ... seconds. Also pauses between consecutive calls.
bold_api_get <- function(url, ..., max_retries = 4) {
  for (attempt in seq_len(max_retries + 1)) {
    res <- httr::GET(url = url, ...)

    if (httr::status_code(res) %in% c(429L, 503L) && attempt <= max_retries) {
      wait <- 2^attempt
      message(sprintf("  Rate limited (HTTP %d), retrying in %ds...",
                      httr::status_code(res), wait))
      Sys.sleep(wait)
      next
    }

    stop_for_status(res)
    return(res)
  }
}

# Utility: split a character vector of terms into groups where the
# semicolon-joined string of each group is <= max_chars.
split_terms <- function(terms, max_chars = bold_query_param_limit) {
  groups <- list()
  current <- character(0)
  current_len <- 0L

  for (term in terms) {
    term_len <- nchar(term)
    sep_len <- if (length(current) > 0) 1L else 0L

    if (current_len + sep_len + term_len > max_chars && length(current) > 0) {
      groups[[length(groups) + 1]] <- current
      current <- term
      current_len <- term_len
    } else {
      current <- c(current, term)
      current_len <- current_len + sep_len + term_len
    }
  }
  if (length(current) > 0) {
    groups[[length(groups) + 1]] <- current
  }
  groups
}


#1.Parse the query

# Remove the commas and substitute it with space

parse_query<-function(query)

{

  trial_query_internal<-gsub(",",
                             " ",
                             query)

  trial_query_quoted <-gsub(",",
                            " ",
                            trial_query_internal)%>%
    paste0('"',
           .,
           '"')%>%
    paste(.,
          collapse = " ")%>%
    gsub('"',
         '%22',
         .)%>%
    gsub(' ',
         '%20',
         .)

  # Full url parse

  full_url_parse <- URLencode(paste0(base_url_parse,
                                     trial_query_quoted,sep=""))

  result <- bold_api_get(full_url_parse,
                         add_headers('accept' = 'application/json'))

  get.data_parse <- fromJSON(content(result, "text", encoding = "UTF-8"))

  return(get.data_parse)

}

#2. Preprocess the query

preprocess_query<-function(parsed_query)
{
  # The preprocessor endpoint enforces maxLength: 250 on the query parameter,
  # so we split the parsed terms (semicolon-delimited) into chunks that fit.
  individual_terms <- strsplit(parsed_query$terms, ";")[[1]]

  groups <- split_terms(individual_terms)

  all_successful <- list()
  for (g_idx in seq_along(groups)) {
    if (g_idx > 1) Sys.sleep(0.5)
    group <- groups[[g_idx]]
    query_value <- paste(group, collapse = ";")
    query_preprocess <- gsub(",", "%2C", query_value) %>%
      gsub(":", "%3A", .) %>%
      gsub(";", "%3B", .) %>%
      gsub(' ', '%20', .)

    full_url_preprocess <- URLencode(paste0(base_url_preprocess,
                                            query_preprocess,
                                            sep=""))

    # Downloading the preprocess data

    get.data.pre <- bold_api_get(full_url_preprocess,
                                 add_headers('accept' = 'application/json'))

    suppressWarnings(suppressMessages(json_preprocess<-content(get.data.pre,
                                                               "text")))

    all_successful[[length(all_successful) + 1]] <- fromJSON(json_preprocess)$successful_terms
  }

  json_preprocess_data_final <- dplyr::bind_rows(all_successful)

  json_preprocess_data_final$matched<-gsub(',.*',"",json_preprocess_data_final$matched)

  # A separate column for names is created that will be used to print the query terms

  json_preprocess_data_final=json_preprocess_data_final%>%
    dplyr::mutate(names=gsub("^na:na:",'',.$submitted))

  # Filter out terms that resolved to raw IDs instead of taxonomy/geography.
  # This happens when BOLD can't match a name taxonomically (e.g. mites,
  # obscure taxa). Instead of failing the whole batch, drop those terms and
  # warn so the remaining valid species can still be searched.
  ids_mask <- grepl("ids:", json_preprocess_data_final$matched)
  if (any(ids_mask)) {
    bad_names <- json_preprocess_data_final$names[ids_mask]
    warning("The following terms could not be resolved taxonomically and were skipped: ",
            paste(bad_names, collapse = ", "), call. = FALSE)
    json_preprocess_data_final <- json_preprocess_data_final[!ids_mask, , drop = FALSE]
    if (nrow(json_preprocess_data_final) == 0) {
      stop("No terms could be resolved taxonomically. Re-check search queries.")
    }
  }

  return(json_preprocess_data_final)

}

#3. Count for a) selecting the number of records which have more than 0 counts for each term & b) displaying the total number of specimens data available for download

counts_query<-function (preprocessed_query, taxonomy_only = FALSE)
{

  #a. Record counts url (will be separate for each triplet)

  query_preprocess_summ<-gsub(":","%3A",preprocessed_query$matched)%>%
    gsub(";","%3B",.)%>%
    gsub(' ','%20',.)%>%
    gsub('/','%2F',.)

  # Function to GET the number of specimen data for each triplet. It takes the query_preprocess_summ as input

  get_counts<-function (summ_url)
  {

    full_url_preprocess_summ<-paste0(base_url_summary,
                                     summ_url,
                                     "&fields=specimens&reduce_operation=count",
                                     sep="")

    get.data.pre.summ <- bold_api_get(full_url_preprocess_summ,
                                      add_headers('accept' = 'application/json'))

    suppressWarnings(suppressMessages(json_preprocess_summ<-content(get.data.pre.summ,
                                                                    "text")))

    json_preprocess_summ_counts<-fromJSON(json_preprocess_summ)$counts$specimens

    json_preprocess_summ_counts <- if (is.null(json_preprocess_summ_counts)) 0 else json_preprocess_summ_counts

    result=unname(json_preprocess_summ_counts)

    return(result)

  }

  ## There are two aspects of taking the count. 1. Is to get numbers for each query term in each parameters searches so that the ones with zero can be removed (in step 4) and 2. to display the total number of specimen data available for the query search. The reasons for getting 0 specimens data could perhaps be due to data not being available on BOLD at the time of download or due to user misspelling any/all of the query terms. If this step is not implemented the whole query would yield a NULL result even due to a single error (Ex. Panthera leo + India vs Panthera leoss + India; the latter has a misspelled word).

  # Creating an empty result list

  result = list()

  # For use in the filtering in step4

  query_search_counts_values <- vapply(seq_along(query_preprocess_summ), function(i) {
    if (i > 1) Sys.sleep(0.5)
    get_counts(summ_url = query_preprocess_summ[i])
  }, numeric(1))

  query_search_counts_df=preprocessed_query%>%
    dplyr::mutate(observations=query_search_counts_values)

  result[['counts_df']]<-query_search_counts_df

  # For displaying number of specimen records available


  # Check for 0 observations. In multi-parameter queries (e.g. taxonomy + geography), a zero-count taxonomy term would be silently dropped, leaving the geography unconstrained and returning far more data than intended (e.g. "Panthera leoss" + "India" would return ALL India records). For taxonomy-only searches this risk does not exist — zero-count terms are simply species not on BOLD, and removing them from the OR query is safe.

  zero_mask <- result$counts_df[["observations"]] == 0

  if (any(zero_mask, na.rm = TRUE)) {
    if (taxonomy_only) {
      # Taxonomy-only search: skip missing terms with a warning
      missing_terms <- result$counts_df$names[zero_mask]
      warning(
        "The following terms returned 0 records and were skipped: ",
        paste(missing_terms, collapse = ", "),
        call. = FALSE
      )
      result$counts_df <- result$counts_df[!zero_mask, , drop = FALSE]
      # If ALL terms are zero, nothing to search
      if (nrow(result$counts_df) == 0) return(NULL)
    } else {
      # Multi-parameter search: fail to prevent silent data corruption
      return(NULL)
    }
  }

  return(result)

}


# Query terms validation (correct placement of terms)

parameter_validation <- function(df_counts, non_null_args)
{

  df_counts = df_counts$counts_df

  # Prefixes for the arguments used in the function

  expected_prefixes <- c(
    taxonomy = "tax",
    geography = "geo",
    bins = "bin",
    institutes = "inst",
    dataset_codes = "recordsetcode",
    project_codes = "recordsetcode"
  )

  # The first part of the triplet (bin,geo,tax) is extracted. A new column of the extracted string is added to the input

  df_counts$prefix = sub(":.*", "", df_counts$matched)

  # Loop which checks and compares the relevant prefixes and the expected prefixes

  for (query_param in names(non_null_args)) {

    prefix_expected <- expected_prefixes[[query_param]]

    # filter the df_counts by prefix column created above

    relevant_names <- df_counts$names[df_counts$prefix == prefix_expected]

    # Only validate terms that survived preprocessing and counting.
    # Terms can legitimately disappear (filtered as ids: or zero-count),
    # so we only check that surviving terms have the correct prefix.
    surviving_input <- intersect(non_null_args[[query_param]], df_counts$names)

    if (length(surviving_input) > 0 && !all(surviving_input %in% relevant_names)) stop("")
  }

}


#4. Generate a query id based on the matched terms

generate_query_id<-function (matched_terms)
{
  for_query<-matched_terms$counts_df

  matched_terms_non_zero<-for_query%>%
    dplyr::filter(observations>0)%>%
    dplyr::arrange(desc(observations))

  if (nrow(matched_terms_non_zero) == 0) return(character(0))

  # The /api/query endpoint enforces maxLength: 250 on the query parameter
  # value. Split matched terms into groups that fit within the limit.
  groups <- split_terms(matched_terms_non_zero$matched)

  # Make a query call per group and collect download URLs
  download_urls <- vapply(seq_along(groups), function(g_idx) {
    if (g_idx > 1) Sys.sleep(0.5)
    group <- groups[[g_idx]]
    query_value <- paste(group, collapse = ";")
    query_url_part1 <- gsub(",", "%2C", query_value) %>%
      gsub(";", "%3B", .) %>%
      gsub(":", "%3A", .) %>%
      gsub(" ", "%20", .) %>%
      gsub('/', '%2F', .)

    full_query <- paste0(base_url_query,
                         query_url_part1,
                         "&extent=full",
                         sep="")

    # Download the data

    get.data.query <- bold_api_get(full_query,
                                   add_headers('accept' = 'application/json'))

    # Extract the data

    suppressWarnings(suppressMessages(json_query<-content(get.data.query,
                                                          "text")))

    # Convert the data into text

    json_query_data<-fromJSON(json_query)

    out = json_query_data$query_id

    paste0("https://portal.boldsystems.org/api/documents/",
           out,
           "/download?format=tsv&fields=processid,marker_code")
  }, character(1))

  return(download_urls)

}


#5. Obtain the data based on the query

obtain_data<-function(download_url)
{
  res <- bold_api_get(download_url,
                      add_headers('accept' = 'text/tab-separated-values'))

  tsv_text <- content(res, "text", encoding = "UTF-8")

  # Check to see if there is data downloaded. If no data is available, it will return NULL

  if(is.null(tsv_text) || nchar(trimws(tsv_text)) == 0) return(NULL)

  final_data <- read.delim(text = tsv_text, sep = '\t')

  return(final_data)
}



