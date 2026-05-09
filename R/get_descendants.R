#' Get descendant taxa below a taxon
#'
#' Resolves a taxon name and returns accepted descendant taxa below the matched
#' lineage. Synonym ancestor names are resolved before descendants are selected.
#'
#' @param taxon Character vector of taxon names.
#' @param target_rank Optional descendant rank to return. If `NULL`, all lower
#'   ranks are returned.
#' @param outline Optional standardized outline used to build a taxon index.
#' @param taxon_index Optional taxon index from [build_taxon_index()]. If both
#'   `outline` and `taxon_index` are `NULL`, the packaged
#'   [fungi_taxon_index] data is used.
#' @param match_synonym Logical. If `TRUE`, match synonym ancestor names.
#'
#' @return A tibble of descendant taxa and their lineage columns.
#' @export
#'
#' @examples
#' outline <- tibble::tibble(kingdom = "Fungi", phylum = "Ascomycota", genus = "Fusarium")
#' fungioutline::get_descendants("Ascomycota", target_rank = "genus")
#'
#' idx <- fungioutline::build_taxon_index(outline)
#' fungioutline::get_descendants("Ascomycota", target_rank = "genus", taxon_index = idx)
get_descendants <- function(
    taxon,
    target_rank = NULL,
    outline = NULL,
    taxon_index = NULL,
    match_synonym = TRUE
) {
    if (missing(taxon) || !is.character(taxon)) {
        rlang::abort("`taxon` must be a character vector.")
    }

    check_logical_scalar(match_synonym, "match_synonym")
    target_rank <- fo_check_rank(target_rank, allow_null = TRUE)

    taxon_index <- fo_prepare_taxon_index(
        outline = outline,
        taxon_index = taxon_index,
        include_synonyms = match_synonym
    )

    if (length(taxon) == 0L) {
        return(fo_empty_descendants())
    }

    resolved <- get_lineage(
        taxon = taxon,
        taxon_index = taxon_index,
        match_synonym = match_synonym,
        best_match = TRUE,
        warn_ambiguous = FALSE
    ) |>
        dplyr::filter(.data$match_type != "unmatched")

    if (nrow(resolved) == 0L) {
        return(fo_empty_descendants())
    }

    accepted_index <- taxon_index |>
        dplyr::filter(!.data$is_synonym)

    result <- purrr::map_dfr(
        seq_len(nrow(resolved)),
        function(.row_id) {
            one_ancestor <- resolved[.row_id, ]
            lower_ranks <- fo_lower_ranks(one_ancestor$rank[[1]])

            if (!is.null(target_rank)) {
                if (!target_rank %in% lower_ranks) {
                    cli::cli_warn(glue::glue(
                        "`target_rank` ({target_rank}) is not lower than matched rank {one_ancestor$rank[[1]]}."
                    ))
                    return(fo_empty_descendants())
                }

                lower_ranks <- target_rank
            }

            if (length(lower_ranks) == 0L) {
                return(fo_empty_descendants())
            }

            ancestor_rank <- one_ancestor$rank[[1]]
            ancestor_norm <- fo_normalize_taxon_name(one_ancestor$accepted_name[[1]])

            accepted_index |>
                dplyr::filter(
                    .data$rank %in% lower_ranks,
                    fo_normalize_taxon_name(.data[[ancestor_rank]]) == ancestor_norm,
                    !is.na(.data$taxon_name_norm),
                    nzchar(.data$taxon_name_norm)
                ) |>
                dplyr::transmute(
                    input_taxon = one_ancestor$input_taxon[[1]],
                    ancestor_name = one_ancestor$accepted_name[[1]],
                    ancestor_rank = ancestor_rank,
                    descendant_name = .data$taxon_name,
                    descendant_rank = .data$rank,
                    dplyr::across(dplyr::all_of(fo_lineage_columns())),
                    source_row_id = .data$source_row_id
                )
        }
    )

    result |>
        dplyr::select(dplyr::all_of(fo_descendant_output_columns()))
}
