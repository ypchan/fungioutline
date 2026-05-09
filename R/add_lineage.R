#' Add lineage columns to a data frame
#'
#' Annotates a data frame by resolving taxon names from one column against a
#' fungioutline taxon index. Bare column names and single column-name strings
#' are both supported.
#'
#' @param data A data frame or tibble.
#' @param taxon_col Column containing taxon names. Supports bare column names
#'   such as `genus` and strings such as `"genus"`.
#' @param outline Optional standardized outline used to build a taxon index when
#'   `taxon_index` is not supplied.
#' @param taxon_index Optional taxon index from [build_taxon_index()].
#' @param match_synonym Logical. If `TRUE`, match synonyms when no accepted name
#'   match exists.
#' @param best_match Logical. If `TRUE`, keep one best lineage per input row.
#' @param keep_unmatched Logical. If `TRUE`, keep rows with unmatched taxa.
#' @param suffix Suffix appended to lineage columns that would otherwise
#'   overwrite existing columns in `data`.
#'
#' @return A tibble containing the original data and appended lineage columns.
#' @export
#'
#' @examples
#' outline <- tibble::tibble(kingdom = "Fungi", phylum = "Ascomycota", genus = "Fusarium")
#' idx <- fungioutline::build_taxon_index(outline)
#' tibble::tibble(genus = "Fusarium") |>
#'     fungioutline::add_lineage(taxon_col = genus, taxon_index = idx)
add_lineage <- function(
    data,
    taxon_col,
    outline = NULL,
    taxon_index = NULL,
    match_synonym = TRUE,
    best_match = TRUE,
    keep_unmatched = TRUE,
    suffix = "_lineage"
) {
    if (!is.data.frame(data)) {
        rlang::abort("`data` must be a data frame or tibble.")
    }

    check_logical_scalar(match_synonym, "match_synonym")
    check_logical_scalar(best_match, "best_match")
    check_logical_scalar(keep_unmatched, "keep_unmatched")

    if (!is.character(suffix) || length(suffix) != 1L || is.na(suffix)) {
        rlang::abort("`suffix` must be a single character string.")
    }

    taxon_col_name <- fo_taxon_col_name(rlang::enexpr(taxon_col))
    data <- tibble::as_tibble(data)

    if (!taxon_col_name %in% names(data)) {
        rlang::abort(glue::glue("Column `{taxon_col_name}` was not found in `data`."))
    }

    row_id_col <- fo_unique_internal_name(".fo_row_id", names(data))

    taxon_index <- fo_prepare_taxon_index(
        outline = outline,
        taxon_index = taxon_index,
        include_synonyms = match_synonym
    )

    working_data <- data |>
        dplyr::mutate(!!row_id_col := dplyr::row_number())

    lineage <- if (nrow(working_data) == 0L) {
        fo_empty_lineage_result() |>
            dplyr::mutate(!!row_id_col := integer())
    } else {
        purrr::map2_dfr(
            as.character(working_data[[taxon_col_name]]),
            working_data[[row_id_col]],
            function(.taxon, .row_id) {
                get_lineage(
                    taxon = .taxon,
                    taxon_index = taxon_index,
                    match_synonym = match_synonym,
                    best_match = best_match,
                    warn_ambiguous = FALSE
                ) |>
                    dplyr::mutate(!!row_id_col := .row_id)
            }
        )
    }

    if (!isTRUE(keep_unmatched)) {
        lineage <- lineage |>
            dplyr::filter(.data$match_type != "unmatched")
    }

    lineage <- lineage |>
        dplyr::select(dplyr::all_of(row_id_col), dplyr::everything())

    names(lineage) <- fo_suffix_conflicting_names(
        names(lineage),
        existing_names = names(data),
        suffix = suffix,
        protected_names = row_id_col
    )

    working_data |>
        dplyr::inner_join(lineage, by = row_id_col) |>
        dplyr::arrange(.data[[row_id_col]]) |>
        dplyr::select(-dplyr::all_of(row_id_col)) |>
        tibble::as_tibble()
}

fo_suffix_conflicting_names <- function(names_to_check, existing_names, suffix, protected_names = character()) {
    new_names <- names_to_check

    for (index in seq_along(new_names)) {
        name <- new_names[[index]]

        if (name %in% protected_names || !name %in% existing_names) {
            next
        }

        candidate <- paste0(name, suffix)
        used_names <- c(existing_names, new_names[seq_len(index - 1L)])

        while (candidate %in% used_names) {
            candidate <- paste0(candidate, suffix)
        }

        new_names[[index]] <- candidate
    }

    new_names
}

fo_unique_internal_name <- function(prefix, existing_names) {
    candidate <- prefix

    while (candidate %in% existing_names) {
        candidate <- paste0(candidate, "_")
    }

    candidate
}
