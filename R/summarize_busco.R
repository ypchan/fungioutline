#' Summarize BUSCO metrics
#'
#' Summarizes BUSCO percentage and count columns for all genomes or by one or
#' more grouping columns.
#'
#' @param data A genome metadata data frame.
#' @param by Optional character vector of grouping column names.
#'
#' @return A tibble containing BUSCO summary statistics.
#' @export
#'
#' @examples
#' genomes <- tibble::tibble(phylum = c("Ascomycota", "Ascomycota"), C = c(95, 90), F = c(2, 5), M = c(3, 5))
#' fungioutline::summarize_busco(genomes, by = "phylum")
summarize_busco <- function(data, by = NULL) {
    if (!is.data.frame(data)) {
        rlang::abort("`data` must be a data frame or tibble.")
    }

    data <- fo_standardize_genome_metadata(data)

    if (!is.null(by)) {
        if (!is.character(by) || length(by) == 0L || any(is.na(by)) || any(!nzchar(by))) {
            rlang::abort("`by` must be a character vector of grouping column names.")
        }

        by <- fo_clean_genome_column_names(by)
        missing_by <- setdiff(by, names(data))

        if (length(missing_by) > 0L) {
            rlang::abort(glue::glue(
                "`by` contains columns not found in `data`: {paste(missing_by, collapse = ', ')}."
            ))
        }
    }

    for (column in c(fo_genome_busco_percent_columns(), fo_genome_busco_count_columns())) {
        if (!column %in% names(data)) {
            data[[column]] <- NA_real_
        }

        data[[column]] <- fo_as_numeric_column(data[[column]])
    }

    grouped <- if (is.null(by)) {
        data
    } else {
        dplyr::group_by(data, dplyr::across(dplyr::all_of(by)))
    }

    grouped |>
        dplyr::summarise(
            n_genomes = dplyr::n(),
            mean_C = mean(.data$C, na.rm = TRUE),
            median_C = stats::median(.data$C, na.rm = TRUE),
            mean_S = mean(.data$S, na.rm = TRUE),
            mean_D = mean(.data$D, na.rm = TRUE),
            mean_F = mean(.data$F, na.rm = TRUE),
            mean_M = mean(.data$M, na.rm = TRUE),
            mean_complete_buscos = mean(.data$complete_buscos, na.rm = TRUE),
            mean_total_buscos = mean(.data$total_buscos, na.rm = TRUE),
            .groups = "drop"
        ) |>
        dplyr::mutate(
            dplyr::across(
                dplyr::where(is.numeric),
                ~ dplyr::if_else(is.nan(.x), NA_real_, .x)
            )
        ) |>
        tibble::as_tibble()
}
