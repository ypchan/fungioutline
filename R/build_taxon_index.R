#' Build a long-format taxon index
#'
#' Builds the core fungioutline taxon index from a standardized outline table.
#' Each non-missing accepted taxon at each available rank becomes one row.
#' Optional synonym columns are split on semicolons and added as synonym rows.
#'
#' @param outline A standardized fungal outline data frame.
#' @param include_synonyms Logical. If `TRUE`, include rows from available
#'   synonym columns.
#' @param drop_missing Logical. If `TRUE`, drop rows with missing taxon names.
#'
#' @return A tibble taxon index with accepted names, normalized names, rank,
#'   synonym status, full lineage columns, source row IDs, and update metadata.
#' @export
#'
#' @examples
#' outline <- tibble::tibble(
#'     kingdom = "Fungi",
#'     phylum = "Ascomycota",
#'     genus = "Fusarium",
#'     genus_syn = "Gibberella"
#' )
#' fungioutline::build_taxon_index(outline)
build_taxon_index <- function(outline, include_synonyms = TRUE, drop_missing = TRUE) {
    if (!is.data.frame(outline)) {
        rlang::abort("`outline` must be a data frame or tibble.")
    }

    check_logical_scalar(include_synonyms, "include_synonyms")
    check_logical_scalar(drop_missing, "drop_missing")

    outline <- standardize_outline(outline, verbose = FALSE)
    outline <- tibble::as_tibble(outline)

    rank_columns <- fo_lineage_columns()
    update_columns <- outline_update_columns()

    for (column in c(rank_columns, update_columns)) {
        if (!column %in% names(outline)) {
            outline[[column]] <- NA_character_
        }
    }

    outline <- outline |>
        dplyr::mutate(
            source_row_id = dplyr::row_number(),
            dplyr::across(dplyr::all_of(rank_columns), as.character),
            dplyr::across(
                dplyr::all_of(update_columns),
                function(.x) {
                    if (inherits(.x, "Date") || inherits(.x, "POSIXt")) {
                        return(as.character(.x))
                    }

                    as.character(.x)
                }
            )
        )

    accepted_rows <- purrr::map_dfr(
        rank_columns,
        function(.rank) {
            outline |>
                dplyr::transmute(
                    taxon_name = .data[[.rank]],
                    taxon_name_norm = fo_normalize_taxon_name(.data[[.rank]]),
                    accepted_name = .data[[.rank]],
                    accepted_name_norm = fo_normalize_taxon_name(.data[[.rank]]),
                    rank = .rank,
                    is_synonym = FALSE,
                    synonym_of = NA_character_,
                    dplyr::across(dplyr::all_of(rank_columns)),
                    source_row_id = .data$source_row_id,
                    dplyr::across(dplyr::all_of(update_columns))
                )
        }
    )

    if (isTRUE(drop_missing)) {
        accepted_rows <- accepted_rows |>
            dplyr::filter(!is.na(.data$taxon_name_norm), nzchar(.data$taxon_name_norm))
    }

    synonym_rows <- fo_empty_taxon_index()

    if (isTRUE(include_synonyms)) {
        synonym_rows <- purrr::map_dfr(
            setdiff(rank_columns, "kingdom"),
            function(.rank) {
                synonym_column <- paste0(.rank, "_syn")

                if (!synonym_column %in% names(outline)) {
                    return(fo_empty_taxon_index())
                }

                outline |>
                    dplyr::transmute(
                        accepted_name = .data[[.rank]],
                        accepted_name_norm = fo_normalize_taxon_name(.data[[.rank]]),
                        synonym_text = as.character(.data[[synonym_column]]),
                        rank = .rank,
                        dplyr::across(dplyr::all_of(rank_columns)),
                        source_row_id = .data$source_row_id,
                        dplyr::across(dplyr::all_of(update_columns))
                    ) |>
                    dplyr::filter(!is.na(.data$accepted_name_norm), nzchar(.data$accepted_name_norm)) |>
                    dplyr::mutate(
                        synonym_text = stringr::str_replace_all(.data$synonym_text, "\uFF1B", ";"),
                        taxon_name = stringr::str_split(.data$synonym_text, ";")
                    ) |>
                    tidyr::unnest_longer(col = dplyr::all_of("taxon_name")) |>
                    dplyr::mutate(
                        taxon_name = stringr::str_squish(.data$taxon_name),
                        taxon_name = dplyr::na_if(.data$taxon_name, ""),
                        taxon_name_norm = fo_normalize_taxon_name(.data$taxon_name),
                        is_synonym = TRUE,
                        synonym_of = .data$accepted_name
                    ) |>
                    dplyr::filter(!is.na(.data$taxon_name_norm), nzchar(.data$taxon_name_norm)) |>
                    dplyr::select(dplyr::all_of(fo_index_columns()))
            }
        )
    }

    dplyr::bind_rows(
        accepted_rows,
        synonym_rows
    ) |>
        dplyr::select(dplyr::all_of(fo_index_columns())) |>
        dplyr::distinct()
}
