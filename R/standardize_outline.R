#' Standardize a fungal outline table
#'
#' Converts fungal outline column names to snake case, trims character values,
#' converts empty strings to `NA`, and returns a tibble. Required outline
#' columns are moved before extra columns when present. Future `species` and
#' `species_syn` columns are preserved if they are present and ignored if absent.
#'
#' @param data A data frame containing a fungal outline table.
#' @param trim_values Logical. If `TRUE`, trim and squish whitespace in
#'   character columns.
#' @param empty_to_na Logical. If `TRUE`, convert empty strings in character
#'   columns to `NA`.
#' @param verbose Logical. If `TRUE`, emit an informational message.
#'
#' @return A tibble with standardized column names.
#' @export
#'
#' @examples
#' outline <- tibble::tibble(Kingdom = "Fungi", Genus = " Fusarium ")
#' fungioutline::standardize_outline(outline)
standardize_outline <- function(data, trim_values = TRUE, empty_to_na = TRUE, verbose = FALSE) {
    if (!is.data.frame(data)) {
        rlang::abort("`data` must be a data frame or tibble.")
    }

    check_logical_scalar(trim_values, "trim_values")
    check_logical_scalar(empty_to_na, "empty_to_na")
    check_logical_scalar(verbose, "verbose")

    clean_names <- clean_column_names(names(data))
    duplicated_names <- unique(clean_names[duplicated(clean_names)])

    if (length(duplicated_names) > 0L) {
        rlang::abort(glue::glue(
            "Column names must be unique after standardization: {paste(duplicated_names, collapse = ', ')}."
        ))
    }

    names(data) <- clean_names
    outline <- tibble::as_tibble(data)

    if (isTRUE(trim_values) || isTRUE(empty_to_na)) {
        outline <- dplyr::mutate(
            outline,
            dplyr::across(
                dplyr::where(is.character),
                function(.x) {
                    if (isTRUE(trim_values)) {
                        .x <- stringr::str_squish(.x)
                    }

                    if (isTRUE(empty_to_na)) {
                        .x <- dplyr::na_if(.x, "")
                    }

                    .x
                }
            )
        )
    }

    supported_columns <- outline_supported_columns()
    ordered_columns <- c(
        intersect(supported_columns, names(outline)),
        setdiff(names(outline), supported_columns)
    )
    outline <- dplyr::select(outline, dplyr::all_of(ordered_columns))

    if (isTRUE(verbose)) {
        cli::cli_inform(glue::glue(
            "Standardized fungal outline with {nrow(outline)} rows and {ncol(outline)} columns."
        ))
    }

    outline
}
