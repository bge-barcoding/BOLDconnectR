# Implementation Plan: Resilient Batch Search & Standalone Shiny Functions

## Problem Analysis

### Problem 1: Batch Species Search Fails When One Species Is Missing

**Root Cause:** In `bold.public.search()`, the internal helper `counts_query()`
(`helper.functions.4.bold.public.search.R:181`) does:

```r
if (any(result$counts_df[["observations"]] == 0, na.rm = TRUE)) return(NULL)
```

When *any* term in a batch has 0 observations, the **entire** query returns
`NULL`. The comment at line 161 explains why: in a multi-parameter search
(e.g. taxonomy + geography), dropping a zero-count taxonomy term would leave
geography unconstrained, returning all records for that country — a dangerous
silent data corruption.

**However**, when the user searches with **taxonomy only** (no geography, bins,
institutes, etc.), this risk does not exist. Dropping a zero-count species from a
taxonomy-only list simply means "that species isn't on BOLD", and the remaining
species can be searched safely. The existing guard should **only** apply when
non-taxonomy parameters are also present.

### Problem 2: Standalone Shiny Functions Without BOLDconnectR Dependency

The user wants to call the same BOLD APIs directly from a Shiny app using only
`httr` and `jsonlite`, without importing the BOLDconnectR package.

---

## Task 1: Taxonomy-Only Batch Resilience in `counts_query()`

### Approach: Conditional Skip vs. Fail

Modify `counts_query()` to accept a `taxonomy_only` flag. When `TRUE` (i.e. no
geography/bins/institutes/dataset/project parameters are set), zero-count terms
are **filtered out with a warning** instead of aborting. When `FALSE` (multi-
parameter search), the existing fail-fast behavior is preserved.

Then update `bold.public.search()` to detect whether the search is taxonomy-only
and pass the flag through.

### Implementation Steps

#### Step 1.1: Modify `counts_query()` in `helper.functions.4.bold.public.search.R`

Change the function signature to accept `taxonomy_only = FALSE`:

```r
counts_query <- function(preprocessed_query, taxonomy_only = FALSE)
```

Replace line 181 (`if (any(...)) return(NULL)`) with:

```r
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
```

**Why this is safe:** In a taxonomy-only search, all terms are "tax:..." triplets
combined with OR logic. Removing a zero-count term just removes an empty branch
from the OR — it cannot widen the search. In a multi-parameter search (taxonomy
AND geography), removing a taxonomy term *can* widen the geography constraint,
which is why we preserve the fail-fast behavior there.

#### Step 1.2: Modify `bold.public.search()` in `external.bold.public.search.R`

Detect whether the search is taxonomy-only and pass the flag to `counts_query()`.

In the single-parameter branch (`else if (length(non_null_args)==1)`), determine
if the only parameter is taxonomy:

```r
is_taxonomy_only <- (length(non_null_args) == 1 && names(non_null_args) == "taxonomy")
```

Then pass it to `counts_query()`:

```r
step3 = counts_query(step2, taxonomy_only = is_taxonomy_only)
```

The multi-parameter branch (`length(non_null_args)>1`) always passes
`taxonomy_only = FALSE` (the default), so existing behavior is preserved.

#### Step 1.3: No changes to `globals.R`, `NAMESPACE`, or documentation

This is an internal behavioral improvement, not a new function or new parameters.
The existing documentation for `bold.public.search()` says misspelled/missing
terms cause an error. We update that sentence to clarify the taxonomy-only
exception:

> "Misspelled queries or those for which no public data exists on BOLD at the
> time the function is executed will result in an error. **Exception**: for
> taxonomy-only searches (no other parameters), missing terms are skipped with a
> warning."

---

## Task 2: Standalone Shiny App Functions (No BOLDconnectR Dependency)

### Approach: Provide Self-Contained R Functions

Create a self-contained R script that directly calls the BOLD APIs using `httr`
and `jsonlite`. This can be `source()`d from any Shiny app without installing
BOLDconnectR.

### Implementation Steps

#### Step 2.1: Create `inst/shiny-standalone/bold_api_functions.R`

A single self-contained file. Required packages: `httr`, `jsonlite`, `dplyr`,
`data.table`.

**Functions:**

##### A. `bold_public_search(taxonomy, geography, bins, institutes, dataset_codes, project_codes)`

Replicates the 5-step portal API pipeline:

1. **Parse** — `GET https://portal.boldsystems.org/api/query/parse?query=`
   with URL-encoded `%22term%22` quoted search terms.
2. **Preprocess** — `GET https://portal.boldsystems.org/api/query/preprocessor?query=`
   using the `$terms` from step 1. Returns `successful_terms` with `submitted`,
   `matched`, and we add a `names` column.
3. **Count** — For each matched triplet, `GET https://portal.boldsystems.org/api/summary?query={triplet}&fields=specimens&reduce_operation=count`.
   Returns specimen count per term.
   - **Key enhancement:** If search is taxonomy-only, skip zero-count terms with
     a warning instead of aborting. If multi-parameter, abort (matching package
     behavior).
4. **Query ID** — `GET https://portal.boldsystems.org/api/query?query={terms}&extent=full`
   with non-zero matched terms joined by `%3B`. Returns `query_id`.
5. **Download** — `download.file()` from
   `https://portal.boldsystems.org/api/documents/{query_id}/download?format=tsv&fields=processid,marker_code`.
   Parse as TSV.

##### B. `bold_fetch(api_key, get_by, identifiers, batch_size = 5000)`

Replicates the data retrieval pipeline:

1. Split identifiers into batches of `batch_size`.
2. For each batch, write IDs to a temp file (comma-separated), then POST to
   `https://data.boldsystems.org/api/records/retrieve?` with:
   - Query: `input_type={get_by}`, `batch_size=10000`, `offset=0`,
     `record_sep=","`, `last_updated_threshold=0`
   - Headers: `accept: application/json`, `api-key: {api_key}`,
     `Content-Type: multipart/form-data`
   - Body: `input_file = upload_file(temp_file)`
3. Parse response: split by `\n`, `fromJSON()` each line, collapse multi-value
   columns (`bold_recordset_code_arr`, `coord`, `marker_code`,
   `primers_forward`, `primers_reverse`) into comma-separated strings,
   `rbindlist(fill=TRUE)`.
4. Combine batches with `bind_rows()`.

##### C. `bold_public_search_batch(species_list, geography, bins, institutes, dataset_codes, project_codes)`

Convenience wrapper that iterates per-species:

1. Loop over `species_list`, calling `bold_public_search(taxonomy = list(sp), ...)`
   for each.
2. `tryCatch()` each call — on `NULL` or error, log a warning and continue.
3. `bind_rows()` + `distinct()` to combine and deduplicate.
4. Return list with `$data` (dataframe) and `$summary` (text summary of
   found/missing species).

#### Step 2.2: Create `inst/shiny-standalone/app.R`

Minimal working Shiny app demonstrating:

- Text area for species list input (one per line)
- Optional geography filter
- API key input for `bold_fetch`
- "Search" button → calls `bold_public_search_batch()`
- "Fetch BCDM Data" button → calls `bold_fetch()` with processids from search
- Results displayed in `DT::datatable`
- Status messages showing which species were found/missing

---

## File Changes Summary

| File | Action | Description |
|------|--------|-------------|
| `R/helper.functions.4.bold.public.search.R` | EDIT | Add `taxonomy_only` param to `counts_query()`, skip zero-count terms when taxonomy-only |
| `R/external.bold.public.search.R` | EDIT | Detect taxonomy-only search, pass flag to `counts_query()`, update docs |
| `inst/shiny-standalone/bold_api_functions.R` | CREATE | Standalone API functions (no BOLDconnectR dependency) |
| `inst/shiny-standalone/app.R` | CREATE | Example Shiny app |

## API Endpoints Reference

| Endpoint | Method | Auth | Used By |
|----------|--------|------|---------|
| `portal.boldsystems.org/api/query/parse` | GET | No | public.search |
| `portal.boldsystems.org/api/query/preprocessor` | GET | No | public.search |
| `portal.boldsystems.org/api/summary` | GET | No | public.search |
| `portal.boldsystems.org/api/query` | GET | No | public.search |
| `portal.boldsystems.org/api/documents/{id}/download` | GET | No | public.search |
| `data.boldsystems.org/api/records/retrieve` | POST | API Key | fetch |

## Risks and Mitigations

1. **Multi-parameter safety preserved:** The zero-count guard is only relaxed
   for taxonomy-only searches. Multi-parameter searches (taxonomy + geography)
   retain the existing fail-fast behavior to prevent silent data corruption.
2. **Rate limiting for standalone batch:** Per-species iteration means N API
   calls. Mitigation: 0.5s sleep between calls in the batch wrapper.
3. **BOLD API changes:** Standalone functions hardcode URLs. Mitigation: URLs
   defined as constants at the top of the file.
4. **Large result sets:** Batch searches can accumulate large dataframes.
   Mitigation: `distinct()` deduplication and a warning if >1M records.
