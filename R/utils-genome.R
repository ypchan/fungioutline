fo_genome_expected_columns <- function() {
    c(
        "genome_label",
        "scientific_name",
        "species_rep",
        "genus_rep",
        "family_rep",
        "order_rep",
        "class_rep",
        "phylum",
        "class",
        "order",
        "family",
        "genus",
        "species",
        "accession",
        "long_label",
        "n_scaffolds",
        "n_contigs",
        "scaf_bp",
        "contig_bp",
        "gap_pct",
        "scaf_N50",
        "scaf_L50",
        "ctg_N50",
        "ctg_L50",
        "scaf_L90",
        "scaf_N90",
        "ctg_N90",
        "ctg_L90",
        "scaf_max",
        "ctg_max",
        "scaf_n_gt50K",
        "scaf_pct_gt50K",
        "gc_avg",
        "gc_std",
        "C",
        "S",
        "D",
        "F",
        "M",
        "complete_buscos",
        "complete_singlecopy_buscos",
        "complete_duplicated_buscos",
        "fragmented_buscos",
        "missing_buscos",
        "total_buscos",
        "ok",
        "is_type_strain_likely",
        "organism",
        "strain",
        "biosample_accession",
        "error",
        "evidence_summary",
        "taxid",
        "note"
    )
}

fo_genome_required_columns <- function() {
    c(
        "genome_label",
        "scientific_name",
        "accession",
        "phylum",
        "class",
        "order",
        "family",
        "genus",
        "species",
        "C",
        "F",
        "M",
        "complete_buscos",
        "total_buscos",
        "ok"
    )
}

fo_genome_rank_columns <- function() {
    c("phylum", "class", "order", "family", "genus", "species")
}

fo_genome_busco_percent_columns <- function() {
    c("C", "S", "D", "F", "M")
}

fo_genome_busco_count_columns <- function() {
    c(
        "complete_buscos",
        "complete_singlecopy_buscos",
        "complete_duplicated_buscos",
        "fragmented_buscos",
        "missing_buscos",
        "total_buscos"
    )
}

fo_clean_genome_column_names <- function(x) {
    clean_names <- clean_column_names(x)
    canonical_columns <- fo_genome_expected_columns()
    canonical_map <- stats::setNames(canonical_columns, clean_column_names(canonical_columns))
    matched_columns <- clean_names %in% names(canonical_map)
    clean_names[matched_columns] <- unname(canonical_map[clean_names[matched_columns]])
    clean_names
}

fo_standardize_genome_metadata <- function(data, trim_values = TRUE, empty_to_na = TRUE) {
    if (!is.data.frame(data)) {
        rlang::abort("`data` must be a data frame or tibble.")
    }

    check_logical_scalar(trim_values, "trim_values")
    check_logical_scalar(empty_to_na, "empty_to_na")

    clean_names <- fo_clean_genome_column_names(names(data))
    duplicated_names <- unique(clean_names[duplicated(clean_names)])

    if (length(duplicated_names) > 0L) {
        rlang::abort(glue::glue(
            "Genome metadata column names must be unique after standardization: {paste(duplicated_names, collapse = ', ')}."
        ))
    }

    names(data) <- clean_names
    metadata <- tibble::as_tibble(data)

    if (isTRUE(trim_values) || isTRUE(empty_to_na)) {
        metadata <- metadata |>
            dplyr::mutate(
                dplyr::across(
                    dplyr::where(is.character),
                    function(.x) {
                        if (isTRUE(trim_values)) {
                            .x <- stringr::str_squish(.x)
                        }

                        if (isTRUE(empty_to_na)) {
                            .x <- dplyr::na_if(.x, "")
                        }

                        .x
                    }
                )
            )
    }

    expected_columns <- fo_genome_expected_columns()
    ordered_columns <- c(
        intersect(expected_columns, names(metadata)),
        setdiff(names(metadata), expected_columns)
    )

    metadata |>
        dplyr::select(dplyr::all_of(ordered_columns))
}

fo_add_missing_genome_rank_columns <- function(data) {
    data <- tibble::as_tibble(data)

    for (column in fo_genome_rank_columns()) {
        if (!column %in% names(data)) {
            data[[column]] <- NA_character_
        }
    }

    data
}

fo_genome_report_row <- function(check, status, severity, column, n, message) {
    tibble::tibble(
        check = check,
        status = status,
        severity = severity,
        column = column,
        n = n,
        message = message
    )
}

fo_check_genome_column_exists <- function(data, column, arg = "column") {
    if (!is.character(column) || length(column) != 1L || is.na(column) || !nzchar(column)) {
        rlang::abort(glue::glue("`{arg}` must be a single column name."))
    }

    if (!column %in% names(data)) {
        rlang::abort(glue::glue("Column `{column}` was not found in `data`."))
    }

    invisible(column)
}

fo_as_numeric_column <- function(x) {
    suppressWarnings(as.numeric(x))
}

fo_as_logical_column <- function(x) {
    if (is.logical(x)) {
        return(x)
    }

    x_chr <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
    dplyr::case_when(
        x_chr %in% c("true", "t", "yes", "y", "1", "ok") ~ TRUE,
        x_chr %in% c("false", "f", "no", "n", "0", "not_ok", "not ok") ~ FALSE,
        TRUE ~ NA
    )
}

fo_empty_genome_metadata <- function() {
    tibble::as_tibble(stats::setNames(
        rep(list(character()), length(fo_genome_expected_columns())),
        fo_genome_expected_columns()
    ))
}

fo_empty_genome_result <- function(genome_metadata = NULL) {
    base <- if (is.null(genome_metadata)) {
        fo_empty_genome_metadata()
    } else {
        tibble::as_tibble(genome_metadata)[0, ]
    }

    tibble::tibble(
        input_taxon = character(),
        matched_name = character(),
        accepted_name = character(),
        matched_rank = character(),
        match_type = character()
    ) |>
        dplyr::bind_cols(base)
}

fo_empty_genome_summary <- function() {
    tibble::tibble(
        input_taxon = character(),
        accepted_name = character(),
        rank = character(),
        n_genomes = integer(),
        n_accessions = integer(),
        n_species = integer(),
        n_genera = integer(),
        n_ok = integer(),
        n_high_quality = integer(),
        mean_busco_complete = double(),
        median_busco_complete = double()
    )
}

fo_empty_busco_summary <- function() {
    tibble::tibble(
        n_genomes = integer(),
        mean_C = double(),
        median_C = double(),
        mean_S = double(),
        mean_D = double(),
        mean_F = double(),
        mean_M = double(),
        mean_complete_buscos = double(),
        mean_total_buscos = double()
    )
}

fo_empty_genome_coverage <- function() {
    tibble::tibble(
        input_taxon = character(),
        ancestor_name = character(),
        ancestor_rank = character(),
        taxon_name = character(),
        rank = character(),
        n_genomes = integer(),
        has_genome = logical(),
        kingdom = character(),
        subkingdom = character(),
        phylum = character(),
        subphylum = character(),
        class = character(),
        subclass = character(),
        order = character(),
        family = character(),
        genus = character(),
        species = character(),
        source_row_id = integer()
    )
}

fo_prepare_genome_metadata <- function(genome_metadata) {
    if (!is.data.frame(genome_metadata)) {
        rlang::abort("`genome_metadata` must be a data frame or tibble.")
    }

    fo_standardize_genome_metadata(genome_metadata) |>
        fo_add_missing_genome_rank_columns()
}

fo_deepest_genome_rank <- function(data) {
    ranks <- rev(fo_genome_rank_columns())
    available <- purrr::keep(
        ranks,
        function(.rank) {
            .rank %in% names(data) &&
                any(!is.na(data[[.rank]]) & nzchar(stringr::str_squish(as.character(data[[.rank]]))))
        }
    )

    if (length(available) == 0L) {
        return(NA_character_)
    }

    available[[1]]
}

fo_filter_genomes_for_lineage <- function(metadata, lineage, taxon_index) {
    if (nrow(lineage) == 0L || identical(lineage$match_type[[1]], "unmatched")) {
        return(metadata[0, ])
    }

    matched_rank <- lineage$rank[[1]]
    accepted_norm <- fo_normalize_taxon_name(lineage$accepted_name[[1]])

    if (matched_rank %in% fo_genome_rank_columns()) {
        return(metadata |>
            dplyr::filter(fo_normalize_taxon_name(.data[[matched_rank]]) == accepted_norm))
    }

    lower_genome_ranks <- intersect(fo_lower_ranks(matched_rank), fo_genome_rank_columns())

    if (length(lower_genome_ranks) == 0L) {
        return(metadata[0, ])
    }

    descendant_rank <- lower_genome_ranks[[1]]
    descendants <- get_descendants(
        lineage$accepted_name[[1]],
        target_rank = descendant_rank,
        taxon_index = taxon_index,
        match_synonym = FALSE
    )

    descendant_names <- descendants |>
        dplyr::filter(.data$descendant_rank == descendant_rank) |>
        dplyr::mutate(descendant_name_norm = fo_normalize_taxon_name(.data$descendant_name)) |>
        dplyr::pull(.data$descendant_name_norm) |>
        unique()

    if (length(descendant_names) == 0L) {
        return(metadata[0, ])
    }

    metadata |>
        dplyr::filter(fo_normalize_taxon_name(.data[[descendant_rank]]) %in% descendant_names)
}

fo_genome_quality_levels <- function() {
    c("high_quality", "medium_quality", "low_quality", "unknown")
}

fo_summary_metrics <- function(data) {
    data <- tibble::as_tibble(data)

    if (nrow(data) == 0L) {
        return(tibble::tibble(
            n_genomes = 0L,
            n_accessions = 0L,
            n_species = 0L,
            n_genera = 0L,
            n_ok = 0L,
            n_high_quality = 0L,
            mean_busco_complete = NA_real_,
            median_busco_complete = NA_real_
        ))
    }

    data <- data |>
        classify_genome_quality()

    C_values <- if ("C" %in% names(data)) fo_as_numeric_column(data$C) else rep(NA_real_, nrow(data))
    ok_values <- if ("ok" %in% names(data)) fo_as_logical_column(data$ok) else rep(NA, nrow(data))

    tibble::tibble(
        n_genomes = nrow(data),
        n_accessions = if ("accession" %in% names(data)) dplyr::n_distinct(data$accession, na.rm = TRUE) else 0L,
        n_species = if ("species" %in% names(data)) dplyr::n_distinct(data$species, na.rm = TRUE) else 0L,
        n_genera = if ("genus" %in% names(data)) dplyr::n_distinct(data$genus, na.rm = TRUE) else 0L,
        n_ok = sum(ok_values, na.rm = TRUE),
        n_high_quality = sum(data$genome_quality == "high_quality", na.rm = TRUE),
        mean_busco_complete = mean(C_values, na.rm = TRUE),
        median_busco_complete = stats::median(C_values, na.rm = TRUE)
    ) |>
        dplyr::mutate(
            mean_busco_complete = dplyr::if_else(is.nan(.data$mean_busco_complete), NA_real_, .data$mean_busco_complete),
            median_busco_complete = dplyr::if_else(is.nan(.data$median_busco_complete), NA_real_, .data$median_busco_complete)
        )
}
