#' Validate a fungal outline table
#'
#' Checks whether a fungal outline table has the required columns, unique column
#' names after standardization, at least one row, and at least one populated
#' taxonomic rank value. The function accepts either original Excel-style
#' column names such as `Kingdom` or standardized names such as `kingdom`.
#'
#' @param data A data frame containing a fungal outline table.
#' @param error Logical. If `TRUE`, abort when fatal validation checks fail.
#'   If `FALSE`, return the validation report without aborting.
#' @param verbose Logical. If `TRUE`, emit an informational message.
#'
#' @return A tibble validation report with columns `check`, `status`,
#'   `severity`, `column`, `n`, and `message`.
#' @export
#'
#' @examples
#' outline <- tibble::tibble(Kingdom = "Fungi", Genus = "Fusarium")
#' fungioutline::validate_fungi_outline(outline, error = FALSE)
validate_fungi_outline <- function(data, error = TRUE, verbose = FALSE) {
    check_logical_scalar(error, "error")
    check_logical_scalar(verbose, "verbose")

    if (!is.data.frame(data)) {
        report <- outline_report_row(
            check = "input_type",
            status = "fail",
            severity = "fatal",
            column = NA_character_,
            n = NA_integer_,
            message = "`data` must be a data frame or tibble."
        )

        if (isTRUE(error)) {
            rlang::abort(
                "`data` must be a data frame or tibble.",
                class = "fungioutline_validation_error",
                report = report
            )
        }

        return(report)
    }

    clean_names <- clean_column_names(names(data))
    duplicated_names <- unique(clean_names[duplicated(clean_names)])
    available_columns <- unique(clean_names)
    report <- list()

    report[[length(report) + 1L]] <- if (length(duplicated_names) > 0L) {
        outline_report_row(
            check = "duplicate_columns",
            status = "fail",
            severity = "fatal",
            column = paste(duplicated_names, collapse = ", "),
            n = length(duplicated_names),
            message = "Column names must be unique after standardization."
        )
    } else {
        outline_report_row(
            check = "duplicate_columns",
            status = "pass",
            severity = "info",
            column = NA_character_,
            n = 0L,
            message = "Column names are unique after standardization."
        )
    }

    report[[length(report) + 1L]] <- check_required_columns(data)

    report[[length(report) + 1L]] <- if (nrow(data) == 0L) {
        outline_report_row(
            check = "row_count",
            status = "fail",
            severity = "fatal",
            column = NA_character_,
            n = 0L,
            message = "The outline table must contain at least one row."
        )
    } else {
        outline_report_row(
            check = "row_count",
            status = "pass",
            severity = "info",
            column = NA_character_,
            n = nrow(data),
            message = "The outline table contains one or more rows."
        )
    }

    species_columns <- outline_future_columns()
    missing_species_columns <- setdiff(species_columns, available_columns)

    report[[length(report) + 1L]] <- if (length(missing_species_columns) == 0L) {
        outline_report_row(
            check = "species_columns",
            status = "pass",
            severity = "info",
            column = paste(species_columns, collapse = ", "),
            n = length(species_columns),
            message = "Optional species columns are present."
        )
    } else {
        outline_report_row(
            check = "species_columns",
            status = "pass",
            severity = "info",
            column = paste(missing_species_columns, collapse = ", "),
            n = length(missing_species_columns),
            message = "Optional species columns are absent; this is allowed."
        )
    }

    if (length(duplicated_names) == 0L) {
        outline <- tibble::as_tibble(data)
        names(outline) <- clean_names
        rank_columns <- intersect(outline_rank_columns(include_species = TRUE), names(outline))
        rank_values <- unlist(outline[rank_columns], use.names = FALSE)
        rank_values <- stringr::str_squish(as.character(rank_values))
        has_taxon_content <- any(!is.na(rank_values) & nzchar(rank_values))
    } else {
        rank_columns <- character()
        has_taxon_content <- FALSE
    }

    report[[length(report) + 1L]] <- if (isTRUE(has_taxon_content)) {
        outline_report_row(
            check = "taxon_content",
            status = "pass",
            severity = "info",
            column = paste(rank_columns, collapse = ", "),
            n = length(rank_columns),
            message = "At least one taxonomic rank column contains a value."
        )
    } else {
        outline_report_row(
            check = "taxon_content",
            status = "fail",
            severity = "fatal",
            column = paste(rank_columns, collapse = ", "),
            n = length(rank_columns),
            message = "At least one taxonomic rank column must contain a value."
        )
    }

    if (length(duplicated_names) == 0L) {
        key_columns <- intersect(outline_rank_columns(include_species = TRUE), available_columns)
        if (length(key_columns) > 0L) {
            report[[length(report) + 1L]] <- check_duplicate_keys(
                data = data,
                keys = key_columns,
                check = "duplicate_taxon_keys"
            )
        }
    }

    report <- dplyr::bind_rows(report)

    if (isTRUE(verbose)) {
        cli::cli_inform(glue::glue(
            "Outline validation completed with {sum(report$status == 'pass')} passing checks."
        ))
    }

    fatal_failures <- dplyr::filter(
        report,
        .data$status == "fail",
        .data$severity == "fatal"
    )

    if (nrow(fatal_failures) > 0L && isTRUE(error)) {
        rlang::abort(
            paste(
                c("Fungal outline validation failed.", fatal_failures$message),
                collapse = "\n"
            ),
            class = "fungioutline_validation_error",
            report = report
        )
    }

    report
}
