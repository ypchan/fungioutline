#' Validate fungal genome metadata
#'
#' Checks whether genome metadata contains required columns, has unique column
#' names after standardization, contains at least one row, and has valid BUSCO
#' percentage values where available.
#'
#' @param data A data frame containing genome metadata.
#' @param error Logical. If `TRUE`, abort when fatal validation checks fail.
#' @param verbose Logical. If `TRUE`, emit an informational message.
#'
#' @return A tibble validation report with columns `check`, `status`,
#'   `severity`, `column`, `n`, and `message`.
#' @export
#'
#' @examples
#' genomes <- tibble::tibble(
#'     genome_label = "Fusarium test",
#'     scientific_name = "Fusarium oxysporum",
#'     accession = "GCA_000000001.1",
#'     phylum = "Ascomycota",
#'     class = "Sordariomycetes",
#'     order = "Hypocreales",
#'     family = "Nectriaceae",
#'     genus = "Fusarium",
#'     species = "Fusarium oxysporum",
#'     C = 95,
#'     F = 2,
#'     M = 3,
#'     complete_buscos = 950,
#'     total_buscos = 1000,
#'     ok = TRUE
#' )
#' fungioutline::validate_genome_metadata(genomes, error = FALSE)
validate_genome_metadata <- function(data, error = TRUE, verbose = FALSE) {
    check_logical_scalar(error, "error")
    check_logical_scalar(verbose, "verbose")

    if (!is.data.frame(data)) {
        report <- fo_genome_report_row(
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
                class = "fungioutline_genome_validation_error",
                report = report
            )
        }

        return(report)
    }

    clean_names <- fo_clean_genome_column_names(names(data))
    duplicated_names <- unique(clean_names[duplicated(clean_names)])
    available_columns <- unique(clean_names)
    missing_required <- setdiff(fo_genome_required_columns(), available_columns)
    missing_expected <- setdiff(fo_genome_expected_columns(), available_columns)
    report <- list()

    report[[length(report) + 1L]] <- if (length(duplicated_names) > 0L) {
        fo_genome_report_row(
            check = "duplicate_columns",
            status = "fail",
            severity = "fatal",
            column = paste(duplicated_names, collapse = ", "),
            n = length(duplicated_names),
            message = "Column names must be unique after standardization."
        )
    } else {
        fo_genome_report_row(
            check = "duplicate_columns",
            status = "pass",
            severity = "info",
            column = NA_character_,
            n = 0L,
            message = "Column names are unique after standardization."
        )
    }

    report[[length(report) + 1L]] <- if (length(missing_required) > 0L) {
        fo_genome_report_row(
            check = "required_columns",
            status = "fail",
            severity = "fatal",
            column = paste(missing_required, collapse = ", "),
            n = length(missing_required),
            message = "Required genome metadata columns are missing."
        )
    } else {
        fo_genome_report_row(
            check = "required_columns",
            status = "pass",
            severity = "info",
            column = NA_character_,
            n = length(fo_genome_required_columns()),
            message = "All required genome metadata columns are present."
        )
    }

    report[[length(report) + 1L]] <- if (nrow(data) == 0L) {
        fo_genome_report_row(
            check = "row_count",
            status = "fail",
            severity = "fatal",
            column = NA_character_,
            n = 0L,
            message = "Genome metadata must contain at least one row."
        )
    } else {
        fo_genome_report_row(
            check = "row_count",
            status = "pass",
            severity = "info",
            column = NA_character_,
            n = nrow(data),
            message = "Genome metadata contains one or more rows."
        )
    }

    report[[length(report) + 1L]] <- if (length(missing_expected) > 0L) {
        fo_genome_report_row(
            check = "expected_columns",
            status = "pass",
            severity = "info",
            column = paste(missing_expected, collapse = ", "),
            n = length(missing_expected),
            message = "Some optional expected genome metadata columns are absent."
        )
    } else {
        fo_genome_report_row(
            check = "expected_columns",
            status = "pass",
            severity = "info",
            column = NA_character_,
            n = length(fo_genome_expected_columns()),
            message = "All expected genome metadata columns are present."
        )
    }

    if (length(duplicated_names) == 0L) {
        metadata <- tibble::as_tibble(data)
        names(metadata) <- clean_names
        busco_columns <- intersect(fo_genome_busco_percent_columns(), names(metadata))
        invalid_busco <- purrr::map_int(
            busco_columns,
            function(.column) {
                values <- fo_as_numeric_column(metadata[[.column]])
                sum(!is.na(values) & (values < 0 | values > 100))
            }
        )
        n_invalid_busco <- sum(invalid_busco)
    } else {
        busco_columns <- character()
        n_invalid_busco <- 0L
    }

    report[[length(report) + 1L]] <- if (n_invalid_busco > 0L) {
        fo_genome_report_row(
            check = "busco_percent_range",
            status = "fail",
            severity = "fatal",
            column = paste(busco_columns, collapse = ", "),
            n = n_invalid_busco,
            message = "BUSCO percentage columns must be between 0 and 100."
        )
    } else {
        fo_genome_report_row(
            check = "busco_percent_range",
            status = "pass",
            severity = "info",
            column = paste(busco_columns, collapse = ", "),
            n = 0L,
            message = "BUSCO percentage columns are within range."
        )
    }

    if ("accession" %in% available_columns && length(duplicated_names) == 0L) {
        metadata <- tibble::as_tibble(data)
        names(metadata) <- clean_names
        n_duplicate_accessions <- metadata |>
            dplyr::filter(!is.na(.data$accession), nzchar(as.character(.data$accession))) |>
            dplyr::count(.data$accession, name = "n") |>
            dplyr::filter(.data$n > 1L) |>
            nrow()
    } else {
        n_duplicate_accessions <- 0L
    }

    report[[length(report) + 1L]] <- if (n_duplicate_accessions > 0L) {
        fo_genome_report_row(
            check = "duplicate_accessions",
            status = "fail",
            severity = "warning",
            column = "accession",
            n = n_duplicate_accessions,
            message = "Duplicate genome accession values were found."
        )
    } else {
        fo_genome_report_row(
            check = "duplicate_accessions",
            status = "pass",
            severity = "info",
            column = "accession",
            n = 0L,
            message = "No duplicate genome accession values were found."
        )
    }

    report <- dplyr::bind_rows(report)

    if (isTRUE(verbose)) {
        cli::cli_inform(glue::glue(
            "Genome metadata validation completed with {sum(report$status == 'pass')} passing checks."
        ))
    }

    fatal_failures <- report |>
        dplyr::filter(.data$status == "fail", .data$severity == "fatal")

    if (nrow(fatal_failures) > 0L && isTRUE(error)) {
        rlang::abort(
            paste(c("Genome metadata validation failed.", fatal_failures$message), collapse = "\n"),
            class = "fungioutline_genome_validation_error",
            report = report
        )
    }

    report
}
