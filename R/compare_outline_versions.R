#' Compare two fungal outline versions
#'
#' Compares previous and new standardized outline tables and returns a
#' long-format change history. Taxa are matched by rank, normalized accepted
#' name, and higher-rank lineage context rather than by Excel row number.
#'
#' @param old_outline Previous standardized outline tibble. May be `NULL`.
#' @param new_outline New standardized outline tibble. May be `NULL`.
#' @param version_id Optional version ID. If `NULL`, one is generated.
#' @param change_time Timestamp for the comparison.
#'
#' @return A tibble with canonical outline history columns.
#' @export
#'
#' @examples
#' old <- tibble::tibble(kingdom = "Fungi", phylum = "Ascomycota", genus = "Fusarium")
#' new <- tibble::tibble(kingdom = "Fungi", phylum = "Ascomycota", genus = "Fusarium")
#' fungioutline::compare_outline_versions(old, new)
compare_outline_versions <- function(
    old_outline,
    new_outline,
    version_id = NULL,
    change_time = Sys.time()
) {
    if (is.null(version_id)) {
        version_id <- fo_make_version_id(time = change_time)
    }

    if (!is.character(version_id) || length(version_id) != 1L || is.na(version_id) || !nzchar(version_id)) {
        rlang::abort("`version_id` must be a single non-empty character string.")
    }

    change_time <- fo_change_time_text(change_time)
    old_outline <- fo_prepare_outline_for_history(old_outline)
    new_outline <- fo_prepare_outline_for_history(new_outline)

    old_index <- if (is.null(old_outline) || nrow(old_outline) == 0L) {
        fo_empty_taxon_index()
    } else {
        build_taxon_index(old_outline, include_synonyms = TRUE)
    }

    new_index <- if (is.null(new_outline) || nrow(new_outline) == 0L) {
        fo_empty_taxon_index()
    } else {
        build_taxon_index(new_outline, include_synonyms = TRUE)
    }

    old_accepted <- old_index |>
        dplyr::filter(!.data$is_synonym) |>
        fo_build_history_key() |>
        dplyr::mutate(name_key = paste(.data$rank, .data$accepted_name_norm, sep = "||")) |>
        dplyr::distinct(.data$taxon_key, .keep_all = TRUE)

    new_accepted <- new_index |>
        dplyr::filter(!.data$is_synonym) |>
        fo_build_history_key() |>
        dplyr::mutate(name_key = paste(.data$rank, .data$accepted_name_norm, sep = "||")) |>
        dplyr::distinct(.data$taxon_key, .keep_all = TRUE)

    changed_lineage <- fo_compare_changed_lineage(
        old_accepted = old_accepted,
        new_accepted = new_accepted,
        version_id = version_id,
        change_time = change_time
    )

    moved_name_keys <- changed_lineage |>
        dplyr::transmute(name_key = paste(.data$rank, fo_normalize_taxon_name(.data$taxon_name), sep = "||")) |>
        dplyr::distinct(.data$name_key) |>
        dplyr::pull(.data$name_key)

    added_taxa <- new_accepted |>
        dplyr::anti_join(old_accepted, by = "taxon_key") |>
        dplyr::filter(!.data$name_key %in% moved_name_keys) |>
        fo_taxa_change_rows(
            version_id = version_id,
            change_time = change_time,
            change_type = "added_taxon",
            old_value_column = NA_character_,
            new_value_column = "accepted_name"
        )

    removed_taxa <- old_accepted |>
        dplyr::anti_join(new_accepted, by = "taxon_key") |>
        dplyr::filter(!.data$name_key %in% moved_name_keys) |>
        fo_taxa_change_rows(
            version_id = version_id,
            change_time = change_time,
            change_type = "removed_taxon",
            old_value_column = "accepted_name",
            new_value_column = NA_character_
        )

    common_accepted <- old_accepted |>
        dplyr::inner_join(
            new_accepted,
            by = "taxon_key",
            suffix = c("_old", "_new")
        )

    changed_metadata <- fo_compare_changed_metadata(
        common_accepted = common_accepted,
        version_id = version_id,
        change_time = change_time
    )

    changed_synonym <- fo_compare_changed_synonyms(
        old_index = old_index,
        new_index = new_index,
        common_accepted = common_accepted,
        version_id = version_id,
        change_time = change_time
    )

    history <- dplyr::bind_rows(
        added_taxa,
        removed_taxa,
        changed_lineage,
        changed_synonym,
        changed_metadata
    )

    if (nrow(history) == 0L) {
        return(fo_empty_history())
    }

    fo_add_missing_history_columns(history) |>
        dplyr::distinct()
}

fo_taxa_change_rows <- function(
    data,
    version_id,
    change_time,
    change_type,
    old_value_column,
    new_value_column
) {
    if (nrow(data) == 0L) {
        return(fo_empty_history())
    }

    data |>
        dplyr::transmute(
            version_id = version_id,
            change_time = change_time,
            change_type = change_type,
            rank = .data$rank,
            taxon_name = .data$accepted_name,
            field = "taxon_name",
            old_value = if (is.na(old_value_column)) NA_character_ else .data[[old_value_column]],
            new_value = if (is.na(new_value_column)) NA_character_ else .data[[new_value_column]],
            update_type = .data$update_type,
            update_note = .data$update_note,
            update_link = .data$update_link,
            source_row_id_old = if (identical(change_type, "removed_taxon")) .data$source_row_id else NA_integer_,
            source_row_id_new = if (identical(change_type, "added_taxon")) .data$source_row_id else NA_integer_
        )
}

fo_compare_changed_lineage <- function(old_accepted, new_accepted, version_id, change_time) {
    if (nrow(old_accepted) == 0L || nrow(new_accepted) == 0L) {
        return(fo_empty_history())
    }

    old_by_name <- old_accepted |>
        dplyr::distinct(.data$name_key, .keep_all = TRUE)
    new_by_name <- new_accepted |>
        dplyr::distinct(.data$name_key, .keep_all = TRUE)

    moved <- old_by_name |>
        dplyr::inner_join(
            new_by_name,
            by = "name_key",
            suffix = c("_old", "_new")
        ) |>
        dplyr::filter(.data$taxon_key_old != .data$taxon_key_new)

    if (nrow(moved) == 0L) {
        return(fo_empty_history())
    }

    purrr::map_dfr(
        seq_len(nrow(moved)),
        function(.row_id) {
            one_change <- moved[.row_id, ]
            higher_ranks <- fo_higher_rank_columns(one_change$rank_new[[1]])

            if (length(higher_ranks) == 0L) {
                return(fo_empty_history())
            }

            changed_fields <- purrr::keep(
                higher_ranks,
                function(.field) {
                    old_value <- one_change[[paste0(.field, "_old")]]
                    new_value <- one_change[[paste0(.field, "_new")]]
                    !identical(fo_compare_value(old_value), fo_compare_value(new_value))
                }
            )

            if (length(changed_fields) == 0L) {
                return(fo_empty_history())
            }

            purrr::map_dfr(
                changed_fields,
                function(.field) {
                    fo_history_row(
                        version_id = version_id,
                        change_time = change_time,
                        change_type = "changed_lineage",
                        rank = one_change$rank_new[[1]],
                        taxon_name = one_change$accepted_name_new[[1]],
                        field = .field,
                        old_value = one_change[[paste0(.field, "_old")]][[1]],
                        new_value = one_change[[paste0(.field, "_new")]][[1]],
                        update_type = one_change$update_type_new[[1]],
                        update_note = one_change$update_note_new[[1]],
                        update_link = one_change$update_link_new[[1]],
                        source_row_id_old = one_change$source_row_id_old[[1]],
                        source_row_id_new = one_change$source_row_id_new[[1]]
                    )
                }
            )
        }
    )
}

fo_compare_changed_metadata <- function(common_accepted, version_id, change_time) {
    if (nrow(common_accepted) == 0L) {
        return(fo_empty_history())
    }

    metadata_fields <- c("updated_time", "update_type", "update_note", "update_link")

    purrr::map_dfr(
        metadata_fields,
        function(.field) {
            old_column <- paste0(.field, "_old")
            new_column <- paste0(.field, "_new")

            common_accepted |>
                dplyr::filter(fo_compare_value(.data[[old_column]]) != fo_compare_value(.data[[new_column]])) |>
                dplyr::transmute(
                    version_id = version_id,
                    change_time = change_time,
                    change_type = "changed_metadata",
                    rank = .data$rank_new,
                    taxon_name = .data$accepted_name_new,
                    field = .field,
                    old_value = .data[[old_column]],
                    new_value = .data[[new_column]],
                    update_type = .data$update_type_new,
                    update_note = .data$update_note_new,
                    update_link = .data$update_link_new,
                    source_row_id_old = .data$source_row_id_old,
                    source_row_id_new = .data$source_row_id_new
                )
        }
    )
}

fo_compare_changed_synonyms <- function(old_index, new_index, common_accepted, version_id, change_time) {
    if (nrow(common_accepted) == 0L) {
        return(fo_empty_history())
    }

    old_synonyms <- fo_synonym_set_table(old_index) |>
        dplyr::rename(synonyms_old = dplyr::all_of("synonyms"))
    new_synonyms <- fo_synonym_set_table(new_index) |>
        dplyr::rename(synonyms_new = dplyr::all_of("synonyms"))

    common_accepted |>
        dplyr::transmute(
            taxon_key = .data$taxon_key,
            rank = .data$rank_new,
            taxon_name = .data$accepted_name_new,
            update_type = .data$update_type_new,
            update_note = .data$update_note_new,
            update_link = .data$update_link_new,
            source_row_id_old = .data$source_row_id_old,
            source_row_id_new = .data$source_row_id_new
        ) |>
        dplyr::left_join(old_synonyms, by = "taxon_key") |>
        dplyr::left_join(new_synonyms, by = "taxon_key") |>
        dplyr::mutate(
            synonyms_old = dplyr::coalesce(.data$synonyms_old, ""),
            synonyms_new = dplyr::coalesce(.data$synonyms_new, "")
        ) |>
        dplyr::filter(.data$synonyms_old != .data$synonyms_new) |>
        dplyr::transmute(
            version_id = version_id,
            change_time = change_time,
            change_type = "changed_synonym",
            rank = .data$rank,
            taxon_name = .data$taxon_name,
            field = purrr::map_chr(.data$rank, fo_synonym_field_for_rank),
            old_value = dplyr::na_if(.data$synonyms_old, ""),
            new_value = dplyr::na_if(.data$synonyms_new, ""),
            update_type = .data$update_type,
            update_note = .data$update_note,
            update_link = .data$update_link,
            source_row_id_old = .data$source_row_id_old,
            source_row_id_new = .data$source_row_id_new
        )
}

fo_compare_value <- function(x) {
    x <- as.character(x)
    dplyr::coalesce(x, "")
}
