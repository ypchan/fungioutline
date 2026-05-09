#' Search NCBI sequence counts
#'
#' Count-only wrapper around [search_ncbi_sequences()]. It uses `retmax = 0` so
#' that only record counts are requested and returned.
#'
#' @param taxon Character vector of taxon names.
#' @param outline Optional standardized outline used to build a taxon index.
#' @param taxon_index Optional taxon index from [build_taxon_index()]. If both
#'   `outline` and `taxon_index` are `NULL`, the packaged
#'   [fungi_taxon_index] data is used.
#' @param db NCBI Entrez database. Defaults to `"nuccore"`.
#' @param match_synonym Logical. If `TRUE`, resolve synonym taxon names.
#' @param exact Logical. If `TRUE`, quote the taxon name in the NCBI Organism
#'   query.
#' @param extra_query Optional additional Entrez query clause combined with
#'   `AND`.
#' @param use_cache Logical. If `TRUE`, read and write cached results.
#' @param cache_dir Optional cache directory.
#' @param delay_sec Delay in seconds between uncached NCBI requests.
#' @param api_key NCBI API key. Defaults to `Sys.getenv("NCBI_API_KEY")`.
#' @param search_fun Optional mock search function for tests.
#' @param verbose Logical. If `TRUE`, emit cli progress messages.
#'
#' @return A tibble with one count row per input taxon.
#' @export
#'
#' @examples
#' \dontrun{
#' fungioutline::search_ncbi_sequence_counts("Ascomycota")
#' }
search_ncbi_sequence_counts <- function(
    taxon,
    outline = NULL,
    taxon_index = NULL,
    db = "nuccore",
    match_synonym = TRUE,
    exact = TRUE,
    extra_query = NULL,
    use_cache = TRUE,
    cache_dir = NULL,
    delay_sec = 0.34,
    api_key = Sys.getenv("NCBI_API_KEY", unset = ""),
    search_fun = NULL,
    verbose = FALSE
) {
    search_ncbi_sequences(
        taxon = taxon,
        outline = outline,
        taxon_index = taxon_index,
        db = db,
        match_synonym = match_synonym,
        exact = exact,
        extra_query = extra_query,
        retmax = 0,
        use_cache = use_cache,
        cache_dir = cache_dir,
        delay_sec = delay_sec,
        api_key = api_key,
        search_fun = search_fun,
        verbose = verbose
    ) |>
        dplyr::select(dplyr::all_of(fo_ncbi_count_columns())) |>
        tibble::as_tibble()
}
