#' Count descendant taxa below a taxon
#'
#' Counts distinct accepted descendant taxa below one or more ancestor taxon
#' names, optionally restricted to selected lower ranks.
#'
#' @param taxon Character vector of taxon names.
#' @param by_rank Optional character vector of ranks to count. If `NULL`, all
#'   lower ranks are counted for each matched ancestor.
#' @param outline Optional standardized outline used to build a taxon index.
#' @param taxon_index Optional taxon index from [build_taxon_index()]. If both
#'   `outline` and `taxon_index` are `NULL`, the packaged
#'   [fungi_taxon_index] data is used.
#' @param match_synonym Logical. If `TRUE`, match synonym ancestor names.
#'
#' @return A tibble with `input_taxon`, `ancestor_name`, `ancestor_rank`, `rank`,
#'   and `n_taxa`.
#' @export
#'
#' @examples
#' outline <- tibble::tibble(kingdom = "Fungi", phylum = "Ascomycota", genus = "Fusarium")
#' fungioutline::count_taxa("Ascomycota", by_rank = "genus")
#'
#' idx <- fungioutline::build_taxon_index(outline)
#' fungioutline::count_taxa("Ascomycota", by_rank = "genus", taxon_index = idx)
count_taxa <- function(
    taxon,
    by_rank = NULL,
    outline = NULL,
    taxon_index = NULL,
    match_synonym = TRUE
) {
    if (missing(taxon) || !is.character(taxon)) {
        rlang::abort("`taxon` must be a character vector.")
    }

    check_logical_scalar(match_synonym, "match_synonym")
    by_rank <- fo_check_rank_vector(by_rank, arg = "by_rank", allow_null = TRUE)

    taxon_index <- fo_prepare_taxon_index(
        outline = outline,
        taxon_index = taxon_index,
        include_synonyms = match_synonym
    )

    if (length(taxon) == 0L) {
        return(fo_empty_counts())
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
        return(fo_empty_counts())
    }

    descendants <- get_descendants(
        taxon = taxon,
        taxon_index = taxon_index,
        match_synonym = match_synonym
    )

    skeleton <- purrr::map_dfr(
        seq_len(nrow(resolved)),
        function(.row_id) {
            one_ancestor <- resolved[.row_id, ]
            lower_ranks <- fo_lower_ranks(one_ancestor$rank[[1]])

            if (!is.null(by_rank)) {
                lower_ranks <- by_rank[by_rank %in% lower_ranks]
            }

            if (length(lower_ranks) == 0L) {
                return(fo_empty_counts())
            }

            tibble::tibble(
                input_taxon = one_ancestor$input_taxon[[1]],
                ancestor_name = one_ancestor$accepted_name[[1]],
                ancestor_rank = one_ancestor$rank[[1]],
                rank = lower_ranks
            )
        }
    )

    if (nrow(skeleton) == 0L) {
        return(fo_empty_counts())
    }

    observed_counts <- descendants |>
        dplyr::filter(
            !is.na(.data$descendant_name),
            nzchar(.data$descendant_name)
        ) |>
        dplyr::transmute(
            input_taxon = .data$input_taxon,
            ancestor_name = .data$ancestor_name,
            ancestor_rank = .data$ancestor_rank,
            rank = .data$descendant_rank,
            descendant_name = .data$descendant_name
        ) |>
        dplyr::distinct() |>
        dplyr::group_by(.data$input_taxon, .data$ancestor_name, .data$ancestor_rank, .data$rank) |>
        dplyr::summarise(n_taxa = dplyr::n(), .groups = "drop")

    skeleton |>
        dplyr::left_join(
            observed_counts,
            by = c("input_taxon", "ancestor_name", "ancestor_rank", "rank")
        ) |>
        dplyr::mutate(n_taxa = tidyr::replace_na(.data$n_taxa, 0L)) |>
        dplyr::select(dplyr::all_of(fo_count_output_columns()))
}
