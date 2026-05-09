#' Filter genomes by BUSCO quality thresholds
#'
#' Filters genome metadata using BUSCO complete, missing, and fragmented
#' percentages, with optional filtering by categorical quality labels and `ok`
#' status.
#'
#' @param data A genome metadata data frame.
#' @param min_complete Minimum BUSCO complete percentage.
#' @param max_missing Maximum BUSCO missing percentage.
#' @param max_fragmented Maximum BUSCO fragmented percentage.
#' @param require_ok Logical. If `TRUE`, keep only rows where `ok` is true.
#' @param quality Optional character vector of quality classes to keep.
#'
#' @return A filtered tibble.
#' @export
#'
#' @examples
#' genomes <- tibble::tibble(C = c(95, 60), F = c(2, 15), M = c(3, 25), ok = c(TRUE, FALSE))
#' fungioutline::filter_genomes_by_quality(genomes, min_complete = 80)
filter_genomes_by_quality <- function(
    data,
    min_complete = 80,
    max_missing = 20,
    max_fragmented = Inf,
    require_ok = FALSE,
    quality = NULL
) {
    if (!is.data.frame(data)) {
        rlang::abort("`data` must be a data frame or tibble.")
    }

    check_logical_scalar(require_ok, "require_ok")

    if (!is.numeric(min_complete) || length(min_complete) != 1L || is.na(min_complete)) {
        rlang::abort("`min_complete` must be a single numeric value.")
    }

    if (!is.numeric(max_missing) || length(max_missing) != 1L || is.na(max_missing)) {
        rlang::abort("`max_missing` must be a single numeric value.")
    }

    if (!is.numeric(max_fragmented) || length(max_fragmented) != 1L || is.na(max_fragmented)) {
        rlang::abort("`max_fragmented` must be a single numeric value.")
    }

    if (!is.null(quality)) {
        if (!is.character(quality) || length(quality) == 0L || any(is.na(quality))) {
            rlang::abort("`quality` must be a character vector of quality labels.")
        }

        unsupported_quality <- setdiff(quality, fo_genome_quality_levels())

        if (length(unsupported_quality) > 0L) {
            rlang::abort(glue::glue(
                "`quality` contains unsupported labels: {paste(unsupported_quality, collapse = ', ')}."
            ))
        }
    }

    data <- classify_genome_quality(data)
    ok_values <- if ("ok" %in% names(data)) fo_as_logical_column(data$ok) else rep(NA, nrow(data))

    filtered <- data |>
        dplyr::mutate(
            .fo_complete = fo_as_numeric_column(.data$C),
            .fo_missing = fo_as_numeric_column(.data$M),
            .fo_fragmented = fo_as_numeric_column(.data$F),
            .fo_ok = ok_values
        ) |>
        dplyr::filter(
            !is.na(.data$.fo_complete),
            !is.na(.data$.fo_missing),
            !is.na(.data$.fo_fragmented),
            .data$.fo_complete >= min_complete,
            .data$.fo_missing <= max_missing,
            .data$.fo_fragmented <= max_fragmented
        )

    if (isTRUE(require_ok)) {
        filtered <- filtered |>
            dplyr::filter(.data$.fo_ok)
    }

    if (!is.null(quality)) {
        filtered <- filtered |>
            dplyr::filter(.data$genome_quality %in% quality)
    }

    filtered |>
        dplyr::select(-dplyr::all_of(c(".fo_complete", ".fo_missing", ".fo_fragmented", ".fo_ok"))) |>
        tibble::as_tibble()
}
