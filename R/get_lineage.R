#' Get lineage information for taxon names
#'
#' Resolves one or more taxon names against a fungioutline taxon index. Accepted
#' name matches are preferred over synonym matches, and ambiguous names are
#' returned as all candidate lineages unless `best_match = TRUE`.
#'
#' @param taxon Character vector of taxon names.
#' @param outline Optional standardized outline used to build a taxon index when
#'   `taxon_index` is not supplied.
#' @param taxon_index Optional taxon index from [build_taxon_index()].
#' @param match_synonym Logical. If `TRUE`, use synonym matches when no accepted
#'   name match is found.
#' @param ignore_case Logical. If `TRUE`, match using normalized lower-case
#'   names. If `FALSE`, require case-sensitive matches on `taxon_name`.
#' @param best_match Logical. If `TRUE`, return one best candidate per input.
#' @param warn_ambiguous Logical. If `TRUE`, warn when an input maps to multiple
#'   candidate lineages.
#'
#' @return A tibble with the input taxon, matched name, accepted name, rank,
#'   match type, ambiguity flag, lineage columns, source row ID, and update
#'   metadata.
#' @export
#'
#' @examples
#' outline <- tibble::tibble(
#'     kingdom = "Fungi",
#'     phylum = "Ascomycota",
#'     genus = "Fusarium",
#'     genus_syn = "Gibberella"
#' )
#' idx <- fungioutline::build_taxon_index(outline)
#' fungioutline::get_lineage("Gibberella", taxon_index = idx)
get_lineage <- function(
    taxon,
    outline = NULL,
    taxon_index = NULL,
    match_synonym = TRUE,
    ignore_case = TRUE,
    best_match = FALSE,
    warn_ambiguous = TRUE
) {
    if (missing(taxon) || !is.character(taxon)) {
        rlang::abort("`taxon` must be a character vector.")
    }

    check_logical_scalar(match_synonym, "match_synonym")
    check_logical_scalar(ignore_case, "ignore_case")
    check_logical_scalar(best_match, "best_match")
    check_logical_scalar(warn_ambiguous, "warn_ambiguous")

    taxon_index <- fo_prepare_taxon_index(
        outline = outline,
        taxon_index = taxon_index,
        include_synonyms = match_synonym
    ) |>
        dplyr::mutate(.candidate_order = dplyr::row_number())

    if (length(taxon) == 0L) {
        return(fo_empty_lineage_result())
    }

    input_tbl <- tibble::tibble(
        .input_order = seq_along(taxon),
        input_taxon = taxon,
        input_taxon_norm = fo_normalize_taxon_name(taxon)
    )

    result <- purrr::map_dfr(
        seq_len(nrow(input_tbl)),
        function(.row_id) {
            one_input <- input_tbl[.row_id, ]
            input_value <- one_input$input_taxon[[1]]
            input_norm <- one_input$input_taxon_norm[[1]]

            if (is.na(input_value) || !nzchar(stringr::str_squish(input_value))) {
                return(fo_unmatched_lineage_row(one_input))
            }

            if (isTRUE(ignore_case)) {
                accepted_candidates <- taxon_index |>
                    dplyr::filter(
                        !.data$is_synonym,
                        .data$taxon_name_norm == input_norm
                    )
            } else {
                accepted_candidates <- taxon_index |>
                    dplyr::filter(
                        !.data$is_synonym,
                        .data$taxon_name == input_value
                    )
            }

            if (nrow(accepted_candidates) > 0L) {
                candidates <- accepted_candidates |>
                    dplyr::mutate(match_type = "accepted")
            } else if (isTRUE(match_synonym)) {
                if (isTRUE(ignore_case)) {
                    candidates <- taxon_index |>
                        dplyr::filter(
                            .data$is_synonym,
                            .data$taxon_name_norm == input_norm
                        )
                } else {
                    candidates <- taxon_index |>
                        dplyr::filter(
                            .data$is_synonym,
                            .data$taxon_name == input_value
                        )
                }

                candidates <- candidates |>
                    dplyr::mutate(match_type = "synonym")
            } else {
                candidates <- taxon_index[0, ] |>
                    dplyr::mutate(match_type = character())
            }

            if (nrow(candidates) == 0L) {
                return(fo_unmatched_lineage_row(one_input))
            }

            is_ambiguous <- nrow(candidates) > 1L

            candidates <- candidates |>
                dplyr::mutate(
                    input_taxon = input_value,
                    matched_name = .data$taxon_name,
                    is_ambiguous = is_ambiguous,
                    .input_order = one_input$.input_order[[1]],
                    .rank_depth = match(.data$rank, fo_rank_levels())
                )

            if (isTRUE(best_match)) {
                candidates <- candidates |>
                    dplyr::arrange(
                        .data$is_synonym,
                        dplyr::desc(.data$.rank_depth),
                        .data$.candidate_order
                    ) |>
                    dplyr::slice_head(n = 1L)
            }

            candidates |>
                dplyr::arrange(.data$.candidate_order) |>
                dplyr::transmute(
                    input_taxon = .data$input_taxon,
                    matched_name = .data$matched_name,
                    accepted_name = .data$accepted_name,
                    rank = .data$rank,
                    match_type = .data$match_type,
                    is_synonym = .data$is_synonym,
                    is_ambiguous = .data$is_ambiguous,
                    dplyr::across(dplyr::all_of(fo_lineage_columns())),
                    source_row_id = .data$source_row_id,
                    dplyr::across(dplyr::all_of(outline_update_columns()))
                )
        }
    )

    ambiguous_inputs <- result |>
        dplyr::filter(.data$is_ambiguous) |>
        dplyr::distinct(.data$input_taxon) |>
        dplyr::pull(.data$input_taxon)

    if (length(ambiguous_inputs) > 0L && isTRUE(warn_ambiguous)) {
        cli::cli_warn(glue::glue(
            "Ambiguous taxon names matched multiple lineages: {paste(ambiguous_inputs, collapse = ', ')}."
        ))
    }

    result |>
        dplyr::select(dplyr::all_of(fo_lineage_output_columns()))
}

fo_unmatched_lineage_row <- function(one_input) {
    tibble::tibble(
        input_taxon = one_input$input_taxon[[1]],
        matched_name = NA_character_,
        accepted_name = NA_character_,
        rank = NA_character_,
        match_type = "unmatched",
        is_synonym = NA,
        is_ambiguous = FALSE,
        kingdom = NA_character_,
        subkingdom = NA_character_,
        phylum = NA_character_,
        subphylum = NA_character_,
        class = NA_character_,
        subclass = NA_character_,
        order = NA_character_,
        family = NA_character_,
        genus = NA_character_,
        species = NA_character_,
        source_row_id = NA_integer_,
        updated_time = NA_character_,
        update_type = NA_character_,
        update_note = NA_character_,
        update_link = NA_character_
    )
}
