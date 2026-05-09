fo_make_version_id <- function(prefix = "outline", time = Sys.time()) {
    if (!is.character(prefix) || length(prefix) != 1L || is.na(prefix) || !nzchar(prefix)) {
        rlang::abort("`prefix` must be a single non-empty character string.")
    }

    time <- as.POSIXct(time)

    if (length(time) != 1L || is.na(time)) {
        rlang::abort("`time` must be a single valid date-time value.")
    }

    version_id <- paste0(prefix, "_", format(time, "%Y%m%d_%H%M%S"))
    version_id <- stringr::str_replace_all(version_id, "[^A-Za-z0-9_.-]+", "_")
    stringr::str_replace_all(version_id, "_+", "_")
}

fo_history_columns <- function() {
    c(
        "version_id",
        "change_time",
        "change_type",
        "rank",
        "taxon_name",
        "field",
        "old_value",
        "new_value",
        "update_type",
        "update_note",
        "update_link",
        "source_row_id_old",
        "source_row_id_new"
    )
}

fo_empty_history <- function() {
    tibble::tibble(
        version_id = character(),
        change_time = character(),
        change_type = character(),
        rank = character(),
        taxon_name = character(),
        field = character(),
        old_value = character(),
        new_value = character(),
        update_type = character(),
        update_note = character(),
        update_link = character(),
        source_row_id_old = integer(),
        source_row_id_new = integer()
    )
}

fo_add_missing_history_columns <- function(x) {
    if (is.null(x)) {
        x <- fo_empty_history()
    }

    if (!is.data.frame(x)) {
        rlang::abort("`history` must be a data frame or tibble.")
    }

    x <- tibble::as_tibble(x)

    for (column in fo_history_columns()) {
        if (column %in% names(x)) {
            next
        }

        if (column %in% c("source_row_id_old", "source_row_id_new")) {
            x[[column]] <- NA_integer_
        } else {
            x[[column]] <- NA_character_
        }
    }

    x |>
        dplyr::mutate(
            dplyr::across(
                dplyr::all_of(setdiff(fo_history_columns(), c("source_row_id_old", "source_row_id_new"))),
                as.character
            ),
            source_row_id_old = as.integer(.data$source_row_id_old),
            source_row_id_new = as.integer(.data$source_row_id_new)
        ) |>
        dplyr::select(
            dplyr::all_of(fo_history_columns()),
            dplyr::everything()
        )
}

fo_safe_read_rds <- function(path) {
    if (is.null(path) || !fs::file_exists(path)) {
        return(NULL)
    }

    readRDS(path)
}

fo_safe_write_rds <- function(x, path) {
    if (missing(path) || !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
        rlang::abort("`path` must be a single non-empty file path.")
    }

    parent <- fs::path_dir(path)
    fs::dir_create(parent)
    saveRDS(x, path)
    invisible(x)
}

fo_change_time_text <- function(change_time) {
    change_time <- as.POSIXct(change_time)

    if (length(change_time) != 1L || is.na(change_time)) {
        rlang::abort("`change_time` must be a single valid date-time value.")
    }

    format(change_time, "%Y-%m-%d %H:%M:%S %Z")
}

fo_prepare_outline_for_history <- function(outline) {
    if (is.null(outline)) {
        return(NULL)
    }

    if (!is.data.frame(outline)) {
        rlang::abort("Outline inputs must be data frames or tibbles.")
    }

    outline <- standardize_outline(outline, verbose = FALSE)
    outline <- tibble::as_tibble(outline)

    for (column in c(fo_lineage_columns(), fo_synonym_columns(), outline_update_columns())) {
        if (!column %in% names(outline)) {
            outline[[column]] <- NA_character_
        }
    }

    outline
}

fo_history_identity_columns <- function() {
    paste0("identity_", fo_lineage_columns())
}

fo_higher_rank_columns <- function(rank) {
    rank <- fo_check_rank(rank)
    rank_position <- match(rank, fo_rank_levels())

    if (rank_position <= 1L) {
        return(character())
    }

    fo_rank_levels()[seq_len(rank_position - 1L)]
}

fo_build_history_key <- function(index) {
    keyed_index <- index |>
        dplyr::mutate(
            dplyr::across(
                dplyr::all_of(fo_lineage_columns()),
                fo_normalize_taxon_name,
                .names = "identity_{.col}"
            )
        )

    identity_context <- if (nrow(keyed_index) == 0L) {
        character()
    } else {
        context_data <- keyed_index |>
            dplyr::select(
                dplyr::all_of(c("rank", fo_history_identity_columns()))
            )

        purrr::pmap_chr(
            context_data,
            function(rank, ...) {
                values <- list(...)
                higher_columns <- paste0("identity_", fo_higher_rank_columns(rank))
                context_values <- values[higher_columns]
                context_values <- purrr::map_chr(context_values, ~ dplyr::coalesce(.x, ""))
                paste(context_values, collapse = "||")
            }
        )
    }

    keyed_index |>
        dplyr::mutate(
            identity_key = identity_context,
            taxon_key = paste(.data$rank, .data$accepted_name_norm, .data$identity_key, sep = "||")
        )
}

fo_empty_history_index <- function() {
    fo_build_history_key(fo_empty_taxon_index())
}

fo_prepare_accepted_history_index <- function(outline) {
    if (is.null(outline) || nrow(outline) == 0L) {
        return(fo_empty_history_index())
    }

    build_taxon_index(outline, include_synonyms = TRUE) |>
        dplyr::filter(!.data$is_synonym) |>
        fo_build_history_key() |>
        dplyr::distinct(.data$taxon_key, .keep_all = TRUE)
}

fo_history_row <- function(
    version_id,
    change_time,
    change_type,
    rank,
    taxon_name,
    field,
    old_value,
    new_value,
    update_type = NA_character_,
    update_note = NA_character_,
    update_link = NA_character_,
    source_row_id_old = NA_integer_,
    source_row_id_new = NA_integer_
) {
    tibble::tibble(
        version_id = version_id,
        change_time = change_time,
        change_type = change_type,
        rank = rank,
        taxon_name = taxon_name,
        field = field,
        old_value = as.character(old_value),
        new_value = as.character(new_value),
        update_type = update_type,
        update_note = update_note,
        update_link = update_link,
        source_row_id_old = as.integer(source_row_id_old),
        source_row_id_new = as.integer(source_row_id_new)
    )
}

fo_synonym_field_for_rank <- function(rank) {
    if (is.na(rank) || !nzchar(rank) || identical(rank, "kingdom")) {
        return("synonyms")
    }

    paste0(rank, "_syn")
}

fo_synonym_set_table <- function(index) {
    synonym_rows <- index |>
        dplyr::filter(.data$is_synonym) |>
        fo_build_history_key()

    if (nrow(synonym_rows) == 0L) {
        return(tibble::tibble(
            taxon_key = character(),
            synonyms = character()
        ))
    }

    synonym_rows |>
        dplyr::distinct(.data$taxon_key, .data$taxon_name_norm) |>
        dplyr::arrange(.data$taxon_key, .data$taxon_name_norm) |>
        dplyr::group_by(.data$taxon_key) |>
        dplyr::summarise(
            synonyms = paste(.data$taxon_name_norm, collapse = "; "),
            .groups = "drop"
        )
}

fo_read_history_by_extension <- function(path) {
    extension <- stringr::str_to_lower(fs::path_ext(path))

    switch(
        extension,
        tsv = readr::read_tsv(path, show_col_types = FALSE),
        csv = readr::read_csv(path, show_col_types = FALSE),
        rds = readRDS(path),
        rlang::abort("Unsupported history file extension. Use .tsv, .csv, or .rds.")
    )
}

fo_write_history_by_extension <- function(history, path) {
    extension <- stringr::str_to_lower(fs::path_ext(path))
    parent <- fs::path_dir(path)
    fs::dir_create(parent)

    switch(
        extension,
        tsv = readr::write_tsv(history, path),
        csv = readr::write_csv(history, path),
        rds = saveRDS(history, path),
        rlang::abort("Unsupported history file extension. Use .tsv, .csv, or .rds.")
    )

    invisible(history)
}
