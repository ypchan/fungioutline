#' Get genomes available for a taxon
#'
#' Resolves a taxon at any supported rank and returns matching genome metadata.
#' Matching uses genome rank columns and the genus-level taxon index built from
#' the curated outline.
#'
#' @param taxon Character vector of taxon names.
#' @param genome_metadata Genome metadata data frame. Defaults to the packaged
#'   [fgtdb_genome_metadata] data.
#' @param outline Optional standardized outline used to build a taxon index.
#' @param taxon_index Optional taxon index from [build_taxon_index()]. If both
#'   `outline` and `taxon_index` are `NULL`, the packaged
#'   [fungi_taxon_index] data is used.
#' @param match_synonym Logical. If `TRUE`, resolve synonym taxon names.
#' @param best_match Logical. If `TRUE`, keep one best taxon match per input.
#' @param include_lineage Logical. If `TRUE`, append matched outline lineage
#'   columns prefixed with `outline_`.
#'
#' @return A tibble of matching genome metadata rows.
#' @export
#'
#' @examples
#' outline <- tibble::tibble(kingdom = "Fungi", phylum = "Ascomycota", genus = "Fusarium")
#' idx <- fungioutline::build_taxon_index(outline)
#' genomes <- tibble::tibble(genome_label = "g1", phylum = "Ascomycota", genus = "Fusarium", C = 95, F = 2, M = 3)
#' fungioutline::get_genomes("Ascomycota", genomes, taxon_index = idx)
#'
#' fungioutline::get_genomes("Ascomycota")
get_genomes <- function(
    taxon,
    genome_metadata = fo_default_genome_metadata(),
    outline = NULL,
    taxon_index = NULL,
    match_synonym = TRUE,
    best_match = TRUE,
    include_lineage = TRUE
) {
    if (missing(taxon) || !is.character(taxon)) {
        rlang::abort("`taxon` must be a character vector.")
    }

    check_logical_scalar(match_synonym, "match_synonym")
    check_logical_scalar(best_match, "best_match")
    check_logical_scalar(include_lineage, "include_lineage")

    metadata <- fo_prepare_genome_metadata(genome_metadata)
    taxon_index <- fo_prepare_taxon_index(
        outline = outline,
        taxon_index = taxon_index,
        include_synonyms = match_synonym
    )

    lineages <- get_lineage(
        taxon = taxon,
        taxon_index = taxon_index,
        match_synonym = match_synonym,
        best_match = best_match,
        warn_ambiguous = !isTRUE(best_match)
    ) |>
        dplyr::filter(.data$match_type != "unmatched")

    if (nrow(lineages) == 0L) {
        return(fo_empty_genome_result(metadata))
    }

    result <- purrr::map_dfr(
        seq_len(nrow(lineages)),
        function(.row_id) {
            one_lineage <- lineages[.row_id, ]
            matched <- fo_filter_genomes_for_lineage(metadata, one_lineage, taxon_index)

            if (nrow(matched) == 0L) {
                return(fo_empty_genome_result(metadata))
            }

            matched <- matched |>
                dplyr::mutate(
                    input_taxon = one_lineage$input_taxon[[1]],
                    matched_name = one_lineage$matched_name[[1]],
                    accepted_name = one_lineage$accepted_name[[1]],
                    matched_rank = one_lineage$rank[[1]],
                    match_type = one_lineage$match_type[[1]]
                ) |>
                dplyr::select(
                    dplyr::all_of(c("input_taxon", "matched_name", "accepted_name", "matched_rank", "match_type")),
                    dplyr::everything()
                )

            if (isTRUE(include_lineage)) {
                for (column in fo_lineage_columns()) {
                    matched[[paste0("outline_", column)]] <- one_lineage[[column]][[1]]
                }

                matched$outline_source_row_id <- one_lineage$source_row_id[[1]]
            }

            matched
        }
    )

    result |>
        dplyr::distinct() |>
        tibble::as_tibble()
}
