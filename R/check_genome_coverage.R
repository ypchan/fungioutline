#' Check genome coverage for descendant taxa
#'
#' Resolves a clade and reports whether descendant taxa at a target rank have
#' public genomes represented in genome metadata.
#'
#' @param taxon Character vector of taxon names.
#' @param genome_metadata Genome metadata data frame. Defaults to the packaged
#'   [fgtdb_genome_metadata] data.
#' @param target_rank Descendant rank to check. Must be one of `phylum`,
#'   `class`, `order`, `family`, or `genus`.
#' @param outline Optional standardized outline used to build a taxon index.
#' @param taxon_index Optional taxon index from [build_taxon_index()]. If both
#'   `outline` and `taxon_index` are `NULL`, the packaged
#'   [fungi_taxon_index] data is used.
#' @param match_synonym Logical. If `TRUE`, resolve synonym ancestor names.
#'
#' @return A tibble with descendant taxa, genome counts, and coverage status.
#' @export
#'
#' @examples
#' outline <- tibble::tibble(kingdom = "Fungi", phylum = "Ascomycota", genus = "Fusarium")
#' idx <- fungioutline::build_taxon_index(outline)
#' genomes <- tibble::tibble(phylum = "Ascomycota", genus = "Fusarium", C = 95, F = 2, M = 3)
#' fungioutline::check_genome_coverage("Ascomycota", genomes, target_rank = "genus", taxon_index = idx)
#'
#' fungioutline::check_genome_coverage("Ascomycota", target_rank = "genus")
check_genome_coverage <- function(
    taxon,
    genome_metadata = fo_default_genome_metadata(),
    target_rank = "genus",
    outline = NULL,
    taxon_index = NULL,
    match_synonym = TRUE
) {
    if (missing(taxon) || !is.character(taxon)) {
        rlang::abort("`taxon` must be a character vector.")
    }

    check_logical_scalar(match_synonym, "match_synonym")
    target_rank <- fo_check_rank(target_rank)

    if (!target_rank %in% fo_genome_rank_columns()) {
        rlang::abort("`target_rank` must be one of the supported outline ranks represented in genome metadata: phylum, class, order, family, genus.")
    }

    metadata <- fo_prepare_genome_metadata(genome_metadata)
    taxon_index <- fo_prepare_taxon_index(
        outline = outline,
        taxon_index = taxon_index,
        include_synonyms = match_synonym
    )

    descendants <- get_descendants(
        taxon = taxon,
        target_rank = target_rank,
        taxon_index = taxon_index,
        match_synonym = match_synonym
    )

    if (nrow(descendants) == 0L) {
        return(fo_empty_genome_coverage())
    }

    genome_counts <- metadata |>
        dplyr::mutate(taxon_name_norm = fo_normalize_taxon_name(.data[[target_rank]])) |>
        dplyr::filter(!is.na(.data$taxon_name_norm), nzchar(.data$taxon_name_norm)) |>
        dplyr::count(.data$taxon_name_norm, name = "n_genomes")

    descendants |>
        dplyr::mutate(taxon_name_norm = fo_normalize_taxon_name(.data$descendant_name)) |>
        dplyr::left_join(genome_counts, by = "taxon_name_norm") |>
        dplyr::mutate(
            n_genomes = tidyr::replace_na(.data$n_genomes, 0L),
            has_genome = .data$n_genomes > 0L
        ) |>
        dplyr::transmute(
            input_taxon = .data$input_taxon,
            ancestor_name = .data$ancestor_name,
            ancestor_rank = .data$ancestor_rank,
            taxon_name = .data$descendant_name,
            rank = .data$descendant_rank,
            n_genomes = .data$n_genomes,
            has_genome = .data$has_genome,
            dplyr::across(dplyr::all_of(fo_lineage_columns())),
            source_row_id = .data$source_row_id
        ) |>
        tibble::as_tibble()
}
