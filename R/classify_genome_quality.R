#' Classify genome quality from BUSCO completeness
#'
#' Adds a categorical genome quality column using BUSCO complete and missing
#' percentages. The default classes are `high_quality`, `medium_quality`,
#' `low_quality`, and `unknown`.
#'
#' @param data A genome metadata data frame.
#' @param complete_col Column containing BUSCO complete percentage. Defaults to
#'   `C`.
#' @param missing_col Column containing BUSCO missing percentage. Defaults to
#'   `M`.
#' @param fragmented_col Column containing BUSCO fragmented percentage. Defaults
#'   to `F`.
#' @param high_complete Minimum complete percentage for `high_quality`.
#' @param high_missing Maximum missing percentage for `high_quality`.
#' @param medium_complete Minimum complete percentage for `medium_quality`.
#' @param medium_missing Maximum missing percentage for `medium_quality`.
#' @param output_col Name of the output quality column.
#'
#' @return A tibble with an added quality column.
#' @export
#'
#' @examples
#' genomes <- tibble::tibble(C = c(95, 75, 50), F = c(2, 10, 20), M = c(3, 15, 30))
#' fungioutline::classify_genome_quality(genomes)
classify_genome_quality <- function(
    data,
    complete_col = "C",
    missing_col = "M",
    fragmented_col = "F",
    high_complete = 90,
    high_missing = 10,
    medium_complete = 70,
    medium_missing = 30,
    output_col = "genome_quality"
) {
    if (!is.data.frame(data)) {
        rlang::abort("`data` must be a data frame or tibble.")
    }

    data <- fo_standardize_genome_metadata(data)
    complete_col <- fo_clean_genome_column_names(complete_col)
    missing_col <- fo_clean_genome_column_names(missing_col)
    fragmented_col <- fo_clean_genome_column_names(fragmented_col)

    fo_check_genome_column_exists(data, complete_col, "complete_col")
    fo_check_genome_column_exists(data, missing_col, "missing_col")
    fo_check_genome_column_exists(data, fragmented_col, "fragmented_col")

    if (!is.character(output_col) || length(output_col) != 1L || is.na(output_col) || !nzchar(output_col)) {
        rlang::abort("`output_col` must be a single non-empty column name.")
    }

    complete <- fo_as_numeric_column(data[[complete_col]])
    missing <- fo_as_numeric_column(data[[missing_col]])

    data |>
        dplyr::mutate(
            !!output_col := dplyr::case_when(
                is.na(complete) | is.na(missing) ~ "unknown",
                complete >= high_complete & missing <= high_missing ~ "high_quality",
                complete >= medium_complete & missing <= medium_missing ~ "medium_quality",
                TRUE ~ "low_quality"
            )
        ) |>
        tibble::as_tibble()
}
