#' Summarize genome availability and quality
#'
#' Summarizes genome availability, accessions, taxonomic sampling, `ok` status,
#' and high-quality genomes. Summaries can be generated for all genomes, grouped
#' by a genome rank column, or scoped to one or more taxa.
#'
#' @param genome_metadata Genome metadata data frame. Defaults to the packaged
#'   [fgtdb_genome_metadata] data.
#' @param taxon Optional character vector of taxon names to resolve through the
#'   taxon index before summarizing.
#' @param outline Optional standardized outline used to build a taxon index.
#' @param taxon_index Optional taxon index from [build_taxon_index()]. If both
#'   `outline` and `taxon_index` are `NULL`, the packaged
#'   [fungi_taxon_index] data is used.
#' @param by_rank Optional genome rank column for grouped summaries when
#'   `taxon` is `NULL`.
#' @param match_synonym Logical. If `TRUE`, resolve synonym taxon names.
#'
#' @return A tibble genome summary.
#' @export
#'
#' @examples
#' genomes <- tibble::tibble(phylum = "Ascomycota", genus = "Fusarium", accession = "GCA_1", C = 95, F = 2, M = 3, ok = TRUE)
#' fungioutline::summarize_genomes(genomes, by_rank = "phylum")
#'
#' fungioutline::summarize_genomes(by_rank = "phylum")
summarize_genomes <- function(
    genome_metadata = fo_default_genome_metadata(),
    taxon = NULL,
    outline = NULL,
    taxon_index = NULL,
    by_rank = NULL,
    match_synonym = TRUE
) {
    check_logical_scalar(match_synonym, "match_synonym")
    metadata <- fo_prepare_genome_metadata(genome_metadata)

    if (!is.null(by_rank)) {
        by_rank <- fo_check_genome_rank(by_rank, arg = "by_rank")

        if (!by_rank %in% fo_genome_rank_columns()) {
            rlang::abort("`by_rank` must be one of the genome metadata ranks: phylum, class, order, family, genus, species.")
        }
    }

    if (!is.null(taxon)) {
        if (!is.character(taxon)) {
            rlang::abort("`taxon` must be a character vector when supplied.")
        }

        taxon_index <- fo_prepare_taxon_index(
            outline = outline,
            taxon_index = taxon_index,
            include_synonyms = match_synonym
        )

        lineages <- get_lineage(
            taxon = taxon,
            taxon_index = taxon_index,
            match_synonym = match_synonym,
            best_match = TRUE,
            warn_ambiguous = FALSE
        )

        genomes <- get_genomes(
            taxon = taxon,
            genome_metadata = metadata,
            taxon_index = taxon_index,
            match_synonym = match_synonym,
            best_match = TRUE
        )

        skeleton <- lineages |>
            dplyr::transmute(
                input_taxon = .data$input_taxon,
                accepted_name = .data$accepted_name,
                rank = .data$rank
            )

        observed <- if (nrow(genomes) == 0L) {
            fo_empty_genome_summary()
        } else {
            genomes |>
                dplyr::group_by(.data$input_taxon, .data$accepted_name, rank = .data$matched_rank) |>
                dplyr::group_modify(~ fo_summary_metrics(.x)) |>
                dplyr::ungroup()
        }

        return(skeleton |>
            dplyr::left_join(observed, by = c("input_taxon", "accepted_name", "rank")) |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::all_of(c("n_genomes", "n_accessions", "n_species", "n_genera", "n_ok", "n_high_quality")),
                    ~ tidyr::replace_na(.x, 0L)
                )
            ) |>
            tibble::as_tibble())
    }

    if (is.null(by_rank)) {
        metrics <- fo_summary_metrics(metadata)
        return(tibble::tibble(
            input_taxon = NA_character_,
            accepted_name = "all",
            rank = "all"
        ) |>
            dplyr::bind_cols(metrics) |>
            tibble::as_tibble())
    }

    metadata |>
        dplyr::mutate(.fo_group = .data[[by_rank]]) |>
        dplyr::group_by(.data$.fo_group) |>
        dplyr::group_modify(~ fo_summary_metrics(.x)) |>
        dplyr::ungroup() |>
        dplyr::transmute(
            input_taxon = NA_character_,
            accepted_name = .data$.fo_group,
            rank = by_rank,
            dplyr::across(dplyr::all_of(c(
                "n_genomes",
                "n_accessions",
                "n_species",
                "n_genera",
                "n_ok",
                "n_high_quality"
            )))
        ) |>
        tibble::as_tibble()
}
