#' Plot NCBI sequence availability counts
#'
#' Visualizes count results from [search_ncbi_sequence_counts()] or
#' [search_ncbi_sequences()].
#'
#' @param ncbi_counts A data frame with NCBI count columns.
#' @param output_path Optional file path. If supplied, the plot is saved.
#' @param width Plot width used when saving.
#' @param height Plot height used when saving.
#' @param dpi Plot resolution used when saving.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' counts <- tibble::tibble(input_taxon = "Fusarium", accepted_name = "Fusarium", count = 10, has_sequences = TRUE)
#' fungioutline::plot_ncbi_sequence_counts(counts)
plot_ncbi_sequence_counts <- function(
    ncbi_counts,
    output_path = NULL,
    width = 8,
    height = 5,
    dpi = 300
) {
    fo_check_ggplot2()
    fo_check_output_plot_args(width, height, dpi)
    ncbi_counts <- fo_check_plot_data_frame(ncbi_counts, "ncbi_counts")

    required_columns <- c("input_taxon", "accepted_name", "count", "has_sequences")
    missing_columns <- setdiff(required_columns, names(ncbi_counts))

    if (length(missing_columns) > 0L) {
        rlang::abort(glue::glue(
            "`ncbi_counts` is missing required columns: {paste(missing_columns, collapse = ', ')}."
        ))
    }

    plot_data <- ncbi_counts |>
        dplyr::filter(!is.na(.data$count)) |>
        dplyr::mutate(
            display_taxon = dplyr::coalesce(.data$accepted_name, .data$input_taxon),
            display_taxon = stats::reorder(.data$display_taxon, .data$count),
            availability = dplyr::if_else(.data$has_sequences, "available", "not found")
        )
    fo_check_nonempty_plot_data(
        plot_data,
        "No NCBI count rows with non-missing counts were found; nothing can be plotted."
    )

    plot <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = .data$display_taxon, y = .data$count, fill = .data$availability)
    ) +
        ggplot2::geom_col(width = 0.72, color = "white") +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = c(available = "#2B8CBE", `not found` = "#BDBDBD")) +
        ggplot2::labs(
            title = "NCBI sequence availability",
            x = "Taxon",
            y = "NCBI records",
            fill = "Status"
        ) +
        fo_plot_theme()

    fo_save_plot_if_requested(plot, output_path, width, height, dpi)
}
