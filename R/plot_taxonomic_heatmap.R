#' Plot a taxonomic genome coverage heatmap
#'
#' Builds a compact heatmap of genome coverage for descendant taxa below one or
#' more focal taxa. The heatmap uses [check_genome_coverage()] internally.
#'
#' @param taxon Character vector of focal taxon names.
#' @param genome_metadata Genome metadata data frame.
#' @param target_rank Descendant rank to check.
#' @param outline Optional standardized outline used to build a taxon index when
#'   `taxon_index` is not supplied.
#' @param taxon_index Optional taxon index from [build_taxon_index()].
#' @param match_synonym Logical. If `TRUE`, resolve synonym taxon names.
#' @param output_path Optional file path. If supplied, the plot is saved.
#' @param width Plot width used when saving.
#' @param height Plot height used when saving.
#' @param dpi Plot resolution used when saving.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' outline <- tibble::tibble(kingdom = "Fungi", phylum = "Ascomycota", genus = "Fusarium")
#' idx <- fungioutline::build_taxon_index(outline)
#' genomes <- tibble::tibble(phylum = "Ascomycota", genus = "Fusarium", C = 95, F = 2, M = 3)
#' fungioutline::plot_taxonomic_heatmap("Ascomycota", genomes, target_rank = "genus", taxon_index = idx)
plot_taxonomic_heatmap <- function(
    taxon,
    genome_metadata,
    target_rank = "genus",
    outline = NULL,
    taxon_index = NULL,
    match_synonym = TRUE,
    output_path = NULL,
    width = 9,
    height = 5,
    dpi = 300
) {
    fo_check_ggplot2()
    fo_check_output_plot_args(width, height, dpi)

    coverage <- check_genome_coverage(
        taxon = taxon,
        genome_metadata = genome_metadata,
        target_rank = target_rank,
        outline = outline,
        taxon_index = taxon_index,
        match_synonym = match_synonym
    ) |>
        dplyr::mutate(
            ancestor_label = paste(.data$input_taxon, .data$ancestor_rank, sep = " | "),
            taxon_name = stats::reorder(.data$taxon_name, .data$n_genomes)
        )

    plot <- ggplot2::ggplot(
        coverage,
        ggplot2::aes(x = .data$taxon_name, y = .data$ancestor_label, fill = .data$n_genomes)
    ) +
        ggplot2::geom_tile(color = "white", linewidth = 0.35) +
        ggplot2::scale_fill_gradient(low = "#F0F0F0", high = "#2B8CBE") +
        ggplot2::labs(
            title = "Taxonomic genome coverage heatmap",
            x = target_rank,
            y = "Ancestor",
            fill = "Genomes"
        ) +
        fo_plot_theme() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    fo_save_plot_if_requested(plot, output_path, width, height, dpi)
}
