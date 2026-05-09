fo_check_ggplot2 <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        rlang::abort("Package `ggplot2` is required for plotting functions.")
    }

    invisible(TRUE)
}

fo_save_plot_if_requested <- function(plot, output_path = NULL, width = 8, height = 5, dpi = 300) {
    if (is.null(output_path)) {
        return(plot)
    }

    if (!is.character(output_path) || length(output_path) != 1L || is.na(output_path) || !nzchar(output_path)) {
        rlang::abort("`output_path` must be NULL or a single non-empty file path.")
    }

    fs::dir_create(fs::path_dir(output_path))
    ggplot2::ggsave(
        filename = output_path,
        plot = plot,
        width = width,
        height = height,
        dpi = dpi
    )
    plot
}

fo_rank_factor <- function(rank) {
    factor(rank, levels = fo_rank_levels(), ordered = TRUE)
}

fo_plot_theme <- function() {
    ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            plot.title.position = "plot",
            plot.title = ggplot2::element_text(face = "bold"),
            axis.title = ggplot2::element_text(face = "bold")
        )
}

fo_check_plot_data_frame <- function(data, arg = "data") {
    if (!is.data.frame(data)) {
        rlang::abort(glue::glue("`{arg}` must be a data frame or tibble."))
    }

    tibble::as_tibble(data)
}

fo_check_nonempty_plot_data <- function(data, message) {
    if (!is.data.frame(data) || nrow(data) == 0L) {
        rlang::abort(message)
    }

    invisible(data)
}

fo_check_output_plot_args <- function(width, height, dpi) {
    if (!is.numeric(width) || length(width) != 1L || is.na(width) || width <= 0) {
        rlang::abort("`width` must be a single positive numeric value.")
    }

    if (!is.numeric(height) || length(height) != 1L || is.na(height) || height <= 0) {
        rlang::abort("`height` must be a single positive numeric value.")
    }

    if (!is.numeric(dpi) || length(dpi) != 1L || is.na(dpi) || dpi <= 0) {
        rlang::abort("`dpi` must be a single positive numeric value.")
    }

    invisible(TRUE)
}
