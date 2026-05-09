#' Search NCBI for sequence availability
#'
#' Searches an NCBI Entrez database, `nuccore` by default, for one or more taxon
#' names. When a taxon index or outline is supplied, taxon names are first
#' resolved through [get_lineage()]. Results are cached by database, query, and
#' `retmax`.
#'
#' @param taxon Character vector of taxon names.
#' @param outline Optional standardized outline used to build a taxon index when
#'   `taxon_index` is not supplied.
#' @param taxon_index Optional taxon index from [build_taxon_index()].
#' @param db NCBI Entrez database. Defaults to `"nuccore"`.
#' @param match_synonym Logical. If `TRUE`, resolve synonym taxon names.
#' @param exact Logical. If `TRUE`, quote the taxon name in the NCBI Organism
#'   query.
#' @param extra_query Optional additional Entrez query clause combined with
#'   `AND`.
#' @param retmax Maximum number of record IDs to return.
#' @param use_cache Logical. If `TRUE`, read and write cached results.
#' @param cache_dir Optional cache directory.
#' @param delay_sec Delay in seconds between uncached NCBI requests.
#' @param api_key NCBI API key. Defaults to `Sys.getenv("NCBI_API_KEY")`.
#' @param search_fun Optional mock search function for tests. It must accept
#'   `db`, `query`, `retmax`, and `api_key` arguments and return a list or data
#'   frame with `count` and optional `ids`.
#' @param verbose Logical. If `TRUE`, emit cli progress messages.
#'
#' @return A tibble with one row per input taxon and columns including query,
#'   count, IDs, cache status, and errors.
#' @export
#'
#' @examples
#' \dontrun{
#' fungioutline::search_ncbi_sequences("Ascomycota", db = "nuccore")
#' }
search_ncbi_sequences <- function(
    taxon,
    outline = NULL,
    taxon_index = NULL,
    db = "nuccore",
    match_synonym = TRUE,
    exact = TRUE,
    extra_query = NULL,
    retmax = 20,
    use_cache = TRUE,
    cache_dir = NULL,
    delay_sec = 0.34,
    api_key = Sys.getenv("NCBI_API_KEY", unset = ""),
    search_fun = NULL,
    verbose = FALSE
) {
    if (missing(taxon) || !is.character(taxon)) {
        rlang::abort("`taxon` must be a character vector.")
    }

    if (!is.character(db) || length(db) != 1L || is.na(db) || !nzchar(db)) {
        rlang::abort("`db` must be a single non-empty NCBI database name.")
    }

    check_logical_scalar(match_synonym, "match_synonym")
    check_logical_scalar(exact, "exact")
    check_logical_scalar(use_cache, "use_cache")
    check_logical_scalar(verbose, "verbose")

    if (!is.numeric(retmax) || length(retmax) != 1L || is.na(retmax) || retmax < 0) {
        rlang::abort("`retmax` must be a single non-negative numeric value.")
    }

    if (!is.numeric(delay_sec) || length(delay_sec) != 1L || is.na(delay_sec) || delay_sec < 0) {
        rlang::abort("`delay_sec` must be a single non-negative numeric value.")
    }

    if (!is.character(api_key) || length(api_key) != 1L || is.na(api_key)) {
        rlang::abort("`api_key` must be a single character string.")
    }

    retmax <- as.integer(retmax)

    if (length(taxon) == 0L) {
        return(fo_empty_ncbi_result())
    }

    search_taxa <- fo_prepare_ncbi_taxa(
        taxon = taxon,
        outline = outline,
        taxon_index = taxon_index,
        match_synonym = match_synonym
    )

    result <- purrr::map_dfr(
        seq_len(nrow(search_taxa)),
        function(.row_id) {
            one_taxon <- search_taxa[.row_id, ]
            query <- fo_build_ncbi_query(
                taxon_name = one_taxon$accepted_name[[1]],
                rank = one_taxon$rank[[1]],
                db = db,
                exact = exact,
                extra_query = extra_query
            )
            cache_key <- fo_ncbi_cache_key(db = db, query = query, retmax = retmax)
            cached_result <- if (isTRUE(use_cache)) {
                cache_ncbi_result(cache_key, cache_dir = cache_dir)
            } else {
                NULL
            }

            cached <- !is.null(cached_result)

            search_result <- if (isTRUE(cached)) {
                cached_result
            } else {
                if (isTRUE(verbose)) {
                    cli::cli_inform(glue::glue("Searching NCBI {db}: {query}"))
                }

                live_result <- fo_ncbi_search_once(
                    db = db,
                    query = query,
                    retmax = retmax,
                    api_key = api_key,
                    search_fun = search_fun
                )
                live_result$query_time <- fo_ncbi_query_time()

                if (isTRUE(use_cache)) {
                    cache_ncbi_result(cache_key, live_result, cache_dir = cache_dir)
                }

                if (delay_sec > 0 && .row_id < nrow(search_taxa)) {
                    Sys.sleep(delay_sec)
                }

                live_result
            }

            query_time <- if (!is.null(search_result$query_time)) {
                search_result$query_time
            } else {
                NA_character_
            }

            count <- if (!is.null(search_result$count)) {
                as.integer(search_result$count[[1]])
            } else {
                NA_integer_
            }

            ids <- if (!is.null(search_result$ids)) {
                as.character(unlist(search_result$ids, use.names = FALSE))
            } else {
                character()
            }

            error <- if (!is.null(search_result$error)) {
                as.character(search_result$error[[1]])
            } else {
                NA_character_
            }

            tibble::tibble(
                input_taxon = one_taxon$input_taxon[[1]],
                accepted_name = one_taxon$accepted_name[[1]],
                rank = one_taxon$rank[[1]],
                match_type = one_taxon$match_type[[1]],
                db = db,
                query = query,
                count = count,
                ids = list(ids),
                has_sequences = !is.na(count) && count > 0L,
                cached = cached,
                query_time = query_time,
                error = dplyr::na_if(error, "")
            )
        }
    )

    result |>
        dplyr::select(dplyr::all_of(fo_ncbi_result_columns())) |>
        tibble::as_tibble()
}
