#' Plot BUSCO quality distributions
#'
#' Visualizes a BUSCO metric, `C` by default, as a histogram or as grouped
#' boxplots when `group_col` is supplied.
#'
#' @param genome_metadata Genome metadata data frame.
#' @param metric BUSCO metric column to plot. Defaults to `C`.
#' @param group_col Optional grouping column for boxplots.
#' @param bins Number of histogram bins when `group_col` is `NULL`.
#' @param output_path Optional file path. If supplied, the plot is saved.
#' @param width Plot width used when saving.
#' @param height Plot height used when saving.
#' @param dpi Plot resolution used when saving.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' genomes <- tibble::tibble(C = c(95, 80), F = c(2, 10), M = c(3, 10), phylum = c("A", "B"))
#' fungioutline::plot_busco_quality(genomes)
plot_busco_quality <- function(
    genome_metadata,
    metric = "C",
    group_col = NULL,
    bins = 30,
    output_path = NULL,
    width = 8,
    height = 5,
    dpi = 300
) {
    fo_check_ggplot2()
    fo_check_output_plot_args(width, height, dpi)

    if (!is.character(metric) || length(metric) != 1L || is.na(metric) || !nzchar(metric)) {
        rlang::abort("`metric` must be a single metric column name.")
    }

    if (!is.numeric(bins) || length(bins) != 1L || is.na(bins) || bins <= 0) {
        rlang::abort("`bins` must be a single positive numeric value.")
    }

    data <- fo_prepare_genome_metadata(genome_metadata) |>
        classify_genome_quality()
    metric <- fo_clean_genome_column_names(metric)
    fo_check_genome_column_exists(data, metric, "metric")
    data[[metric]] <- fo_as_numeric_column(data[[metric]])

    if (is.null(group_col)) {
        plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[metric]], fill = .data$genome_quality)) +
            ggplot2::geom_histogram(bins = as.integer(bins), color = "white", alpha = 0.88) +
            ggplot2::scale_fill_manual(
                values = c(
                    high_quality = "#2B8CBE",
                    medium_quality = "#A6BDDB",
                    low_quality = "#F4A582",
                    unknown = "#BDBDBD"
                )
            ) +
            ggplot2::labs(
                title = "BUSCO completeness distribution",
                x = metric,
                y = "Number of genomes",
                fill = "Genome quality"
            ) +
            fo_plot_theme()
    } else {
        if (!is.character(group_col) || length(group_col) != 1L || is.na(group_col) || !nzchar(group_col)) {
            rlang::abort("`group_col` must be NULL or a single grouping column name.")
        }

        group_col <- fo_clean_genome_column_names(group_col)
        fo_check_genome_column_exists(data, group_col, "group_col")

        plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[group_col]], y = .data[[metric]], fill = .data[[group_col]])) +
            ggplot2::geom_boxplot(width = 0.62, outlier.alpha = 0.55) +
            ggplot2::guides(fill = "none") +
            ggplot2::labs(
                title = "BUSCO quality by group",
                x = group_col,
                y = metric
            ) +
            fo_plot_theme()
    }

    fo_save_plot_if_requested(plot, output_path, width, height, dpi)
}
