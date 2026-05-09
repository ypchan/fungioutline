outline_required_columns <- function() {
    c(
        "kingdom",
        "subkingdom",
        "subkingdom_syn",
        "phylum",
        "phylum_syn",
        "subphylum",
        "subphylum_syn",
        "class",
        "class_syn",
        "subclass",
        "subclass_syn",
        "order",
        "family",
        "family_syn",
        "genus",
        "genus_syn",
        "updated_time",
        "update_type",
        "update_note",
        "update_link"
    )
}

outline_future_columns <- function() {
    c("species", "species_syn")
}

outline_supported_columns <- function() {
    unique(c(outline_required_columns(), outline_future_columns()))
}

outline_rank_columns <- function(include_species = TRUE) {
    columns <- c(
        "kingdom",
        "subkingdom",
        "phylum",
        "subphylum",
        "class",
        "subclass",
        "order",
        "family",
        "genus"
    )

    if (isTRUE(include_species)) {
        columns <- c(columns, "species")
    }

    columns
}

outline_synonym_columns <- function(include_species = TRUE) {
    columns <- c(
        "subkingdom_syn",
        "phylum_syn",
        "subphylum_syn",
        "class_syn",
        "subclass_syn",
        "family_syn",
        "genus_syn"
    )

    if (isTRUE(include_species)) {
        columns <- c(columns, "species_syn")
    }

    columns
}

outline_update_columns <- function() {
    c("updated_time", "update_type", "update_note", "update_link")
}

clean_column_names <- function(x) {
    x <- as.character(x)
    x <- stringr::str_replace_all(x, "([a-z0-9])([A-Z])", "\\1_\\2")
    x <- stringr::str_replace_all(x, "[^A-Za-z0-9]+", "_")
    x <- stringr::str_replace_all(x, "_+", "_")
    x <- stringr::str_replace_all(x, "^_|_$", "")
    stringr::str_to_lower(x)
}

check_required_columns <- function(data, required = outline_required_columns()) {
    if (!is.data.frame(data)) {
        return(outline_report_row(
            check = "input_type",
            status = "fail",
            severity = "fatal",
            column = NA_character_,
            n = NA_integer_,
            message = "`data` must be a data frame or tibble."
        ))
    }

    available_columns <- clean_column_names(names(data))
    required <- clean_column_names(required)
    missing_columns <- setdiff(required, available_columns)

    if (length(missing_columns) > 0L) {
        return(outline_report_row(
            check = "required_columns",
            status = "fail",
            severity = "fatal",
            column = paste(missing_columns, collapse = ", "),
            n = length(missing_columns),
            message = "Required outline columns are missing."
        ))
    }

    outline_report_row(
        check = "required_columns",
        status = "pass",
        severity = "info",
        column = NA_character_,
        n = length(required),
        message = "All required outline columns are present."
    )
}

check_duplicate_keys <- function(data, keys, check = "duplicate_keys") {
    if (!is.data.frame(data)) {
        return(outline_report_row(
            check = check,
            status = "fail",
            severity = "fatal",
            column = NA_character_,
            n = NA_integer_,
            message = "`data` must be a data frame or tibble."
        ))
    }

    if (missing(keys) || length(keys) == 0L) {
        rlang::abort("`keys` must contain at least one column name.")
    }

    data_names <- clean_column_names(names(data))
    keys <- clean_column_names(keys)
    missing_keys <- setdiff(keys, data_names)

    if (length(missing_keys) > 0L) {
        return(outline_report_row(
            check = check,
            status = "skip",
            severity = "info",
            column = paste(missing_keys, collapse = ", "),
            n = length(missing_keys),
            message = "Duplicate key check skipped because one or more key columns are absent."
        ))
    }

    checked_data <- tibble::as_tibble(data)
    names(checked_data) <- data_names

    duplicate_keys <- checked_data |>
        dplyr::mutate(
            dplyr::across(
                dplyr::all_of(keys),
                function(.x) {
                    if (is.character(.x)) {
                        return(dplyr::na_if(stringr::str_squish(.x), ""))
                    }

                    .x
                }
            )
        ) |>
        dplyr::filter(!dplyr::if_all(dplyr::all_of(keys), ~ is.na(.x))) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
        dplyr::summarise(duplicate_count = dplyr::n(), .groups = "drop") |>
        dplyr::filter(.data$duplicate_count > 1L)

    if (nrow(duplicate_keys) > 0L) {
        return(outline_report_row(
            check = check,
            status = "fail",
            severity = "warning",
            column = paste(keys, collapse = ", "),
            n = nrow(duplicate_keys),
            message = "Duplicate taxonomic key combinations were found."
        ))
    }

    outline_report_row(
        check = check,
        status = "pass",
        severity = "info",
        column = paste(keys, collapse = ", "),
        n = 0L,
        message = "No duplicate taxonomic key combinations were found."
    )
}

check_logical_scalar <- function(x, arg) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
        rlang::abort(glue::glue("`{arg}` must be TRUE or FALSE."))
    }

    invisible(x)
}

outline_report_row <- function(check, status, severity, column, n, message) {
    tibble::tibble(
        check = check,
        status = status,
        severity = severity,
        column = column,
        n = n,
        message = message
    )
}
