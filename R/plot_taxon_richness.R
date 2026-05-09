#' Plot taxon richness by rank
#'
#' Visualizes accepted taxon richness across ranks for a full taxon index or
#' descendant richness below one or more focal taxa.
#'
#' @param outline Optional standardized outline used to build a taxon index when
#'   `taxon_index` is not supplied.
#' @param taxon_index Optional taxon index from [build_taxon_index()].
#' @param taxon Optional focal taxon names. If supplied, descendant counts are
#'   plotted with [count_taxa()].
#' @param by_rank Optional ranks to count when `taxon` is supplied.
#' @param match_synonym Logical. If `TRUE`, resolve synonym focal taxon names.
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
#' fungioutline::plot_taxon_richness(taxon_index = idx)
plot_taxon_richness <- function(
    outline = NULL,
    taxon_index = NULL,
    taxon = NULL,
    by_rank = NULL,
    match_synonym = TRUE,
    output_path = NULL,
    width = 8,
    height = 5,
    dpi = 300
) {
    fo_check_ggplot2()
    check_logical_scalar(match_synonym, "match_synonym")
    fo_check_output_plot_args(width, height, dpi)

    taxon_index <- fo_prepare_taxon_index(
        outline = outline,
        taxon_index = taxon_index,
        include_synonyms = match_synonym
    )

    plot_data <- if (is.null(taxon)) {
        taxon_index |>
            dplyr::filter(!.data$is_synonym) |>
            dplyr::filter(!is.na(.data$accepted_name_norm), nzchar(.data$accepted_name_norm)) |>
            dplyr::distinct(.data$rank, .data$accepted_name_norm) |>
            dplyr::group_by(.data$rank) |>
            dplyr::summarise(n_taxa = dplyr::n(), .groups = "drop") |>
            dplyr::mutate(input_taxon = "all", rank = fo_rank_factor(.data$rank))
    } else {
        if (!is.character(taxon)) {
            rlang::abort("`taxon` must be NULL or a character vector.")
        }

        count_taxa(
            taxon = taxon,
            by_rank = by_rank,
            taxon_index = taxon_index,
            match_synonym = match_synonym
        ) |>
            dplyr::mutate(rank = fo_rank_factor(.data$rank))
    }

    plot <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = .data$rank, y = .data$n_taxa, fill = .data$input_taxon)
    ) +
        ggplot2::geom_col(width = 0.72, color = "white") +
        ggplot2::labs(
            title = "Taxon richness by rank",
            x = "Rank",
            y = "Number of taxa",
            fill = "Input taxon"
        ) +
        fo_plot_theme()

    fo_save_plot_if_requested(plot, output_path, width, height, dpi)
}
