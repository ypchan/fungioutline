#' Plot the fungioutline framework diagram
#'
#' Creates a compact package workflow diagram in an editorial scientific style.
#' The diagram shows how curated outlines, genome metadata, BUSCO results, NCBI
#' searches, R-native packaged data, and downstream visualization functions
#' connect.
#'
#' @param output_path Optional file path. If supplied, the plot is saved.
#' @param width Plot width used when saving.
#' @param height Plot height used when saving.
#' @param dpi Plot resolution used when saving.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' fungioutline::plot_fungioutline_framework()
plot_fungioutline_framework <- function(
    output_path = NULL,
    width = 10,
    height = 6,
    dpi = 300
) {
    fo_check_ggplot2()
    fo_check_output_plot_args(width, height, dpi)

    nodes <- tibble::tribble(
        ~id, ~label, ~x, ~y, ~group,
        "outline", "Curated outline\nExcel", 0, 2.2, "input",
        "genomes", "Genome metadata\nBUSCO", 0, 0.9, "input",
        "data", "Fast R data\n.rda / .rds", 2, 3.25, "data",
        "index", "Taxon index\naccepted + synonyms", 2, 2.2, "core",
        "lineage", "Lineage lookup\nDescendants", 4.25, 2.2, "core",
        "coverage", "Genome coverage\nQuality filters", 4.25, 0.9, "core",
        "ncbi", "NCBI sequence\navailability", 4.25, -0.35, "external",
        "history", "Update history\nversioned changes", 2, -0.35, "core",
        "plots", "Phylogenomic sampling\nand paper figures", 6.5, 1.15, "output"
    )

    edges <- tibble::tribble(
        ~from, ~to,
        "outline", "index",
        "outline", "data",
        "outline", "history",
        "data", "index",
        "index", "lineage",
        "lineage", "coverage",
        "genomes", "coverage",
        "lineage", "ncbi",
        "coverage", "plots",
        "ncbi", "plots",
        "history", "plots"
    ) |>
        dplyr::left_join(
            nodes |> dplyr::transmute(from = .data$id, x_start = .data$x, y_start = .data$y),
            by = "from"
        ) |>
        dplyr::left_join(
            nodes |> dplyr::transmute(to = .data$id, x_end = .data$x, y_end = .data$y),
            by = "to"
        )

    plot <- ggplot2::ggplot() +
        ggplot2::geom_segment(
            data = edges,
            ggplot2::aes(x = .data$x_start, y = .data$y_start, xend = .data$x_end, yend = .data$y_end),
            linewidth = 0.55,
            color = "#4A5568",
            arrow = grid::arrow(length = grid::unit(0.13, "inches"), type = "closed")
        ) +
        ggplot2::geom_label(
            data = nodes,
            ggplot2::aes(x = .data$x, y = .data$y, label = .data$label, fill = .data$group),
            label.size = 0.22,
            label.r = grid::unit(0.08, "inches"),
            size = 3.2,
            color = "#1F1F1F",
            lineheight = 0.95
        ) +
        ggplot2::scale_fill_manual(
            values = c(
                input = "#E8F2F7",
                data = "#F5F1E5",
                core = "#F3E8D2",
                external = "#E9E4F3",
                output = "#DDEEDC"
            )
        ) +
        ggplot2::labs(
            title = "fungioutline framework for reproducible fungal systematics",
            x = NULL,
            y = NULL
        ) +
        ggplot2::coord_cartesian(xlim = c(-0.7, 7.1), ylim = c(-0.9, 3.65), clip = "off") +
        ggplot2::theme_void(base_size = 11) +
        ggplot2::theme(
            legend.position = "none",
            plot.background = ggplot2::element_rect(fill = "#FBFBF8", color = NA),
            panel.background = ggplot2::element_rect(fill = "#FBFBF8", color = NA),
            plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 12))
        )

    fo_save_plot_if_requested(plot, output_path, width, height, dpi)
}
