#' Plot genome coverage for descendant taxa
#'
#' Visualizes genome counts and coverage status for descendants of a focal
#' taxon, using [check_genome_coverage()].
#'
#' @param taxon Character vector of focal taxon names.
#' @param genome_metadata Genome metadata data frame. Defaults to the packaged
#'   [fgtdb_genome_metadata] data.
#' @param target_rank Descendant rank to check.
#' @param outline Optional standardized outline used to build a taxon index.
#' @param taxon_index Optional taxon index from [build_taxon_index()]. If both
#'   `outline` and `taxon_index` are `NULL`, the packaged
#'   [fungi_taxon_index] data is used.
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
#' fungioutline::plot_genome_coverage("Ascomycota", genomes, target_rank = "genus", taxon_index = idx)
#'
#' fungioutline::plot_genome_coverage("Ascomycota", target_rank = "genus")
plot_genome_coverage <- function(
    taxon,
    genome_metadata = fo_default_genome_metadata(),
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
    )

    coverage <- coverage |>
        dplyr::filter(!is.na(.data$taxon_name), nzchar(.data$taxon_name)) |>
        dplyr::mutate(
            taxon_name = stats::reorder(.data$taxon_name, .data$n_genomes),
            coverage_status = dplyr::if_else(.data$has_genome, "available", "missing")
        )
    fo_check_nonempty_plot_data(
        coverage,
        "No descendant taxa were found for the requested `taxon` and `target_rank`; nothing can be plotted."
    )

    plot <- ggplot2::ggplot(
        coverage,
        ggplot2::aes(x = .data$taxon_name, y = .data$n_genomes, fill = .data$coverage_status)
    ) +
        ggplot2::geom_col(width = 0.72, color = "white") +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = c(available = "#2B8CBE", missing = "#BDBDBD")) +
        ggplot2::labs(
            title = "Genome coverage by taxon",
            x = target_rank,
            y = "Number of genomes",
            fill = "Coverage"
        ) +
        fo_plot_theme()

    fo_save_plot_if_requested(plot, output_path, width, height, dpi)
}
