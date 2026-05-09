#' Plot outline update history
#'
#' Summarizes outline history rows by change type and version. The input should
#' usually come from [read_outline_history()] or [compare_outline_versions()].
#'
#' @param history A history data frame with canonical history columns.
#' @param output_path Optional file path. If supplied, the plot is saved.
#' @param width Plot width used when saving.
#' @param height Plot height used when saving.
#' @param dpi Plot resolution used when saving.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' history <- fungioutline::compare_outline_versions(NULL, tibble::tibble(kingdom = "Fungi"))
#' fungioutline::plot_update_history(history)
plot_update_history <- function(
    history,
    output_path = NULL,
    width = 8,
    height = 5,
    dpi = 300
) {
    fo_check_ggplot2()
    fo_check_output_plot_args(width, height, dpi)

    history <- fo_add_missing_history_columns(history)
    plot_data <- history |>
        dplyr::group_by(.data$version_id, .data$change_type) |>
        dplyr::summarise(n_changes = dplyr::n(), .groups = "drop") |>
        dplyr::filter(!is.na(.data$change_type), nzchar(.data$change_type))

    plot <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = .data$change_type, y = .data$n_changes, fill = .data$version_id)
    ) +
        ggplot2::geom_col(width = 0.72, color = "white", position = "dodge") +
        ggplot2::coord_flip() +
        ggplot2::labs(
            title = "Outline update history",
            x = "Change type",
            y = "Number of changes",
            fill = "Version"
        ) +
        fo_plot_theme()

    fo_save_plot_if_requested(plot, output_path, width, height, dpi)
}
