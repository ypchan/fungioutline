fo_rank_levels <- function() {
    c(
        "kingdom",
        "subkingdom",
        "phylum",
        "subphylum",
        "class",
        "subclass",
        "order",
        "family",
        "genus"
    )
}

fo_synonym_columns <- function(rank_levels = fo_rank_levels()) {
    rank_levels <- setdiff(rank_levels, "kingdom")
    paste0(rank_levels, "_syn")
}

fo_normalize_taxon_name <- function(x) {
    x <- as.character(x)
    x <- stringr::str_squish(x)
    x <- stringr::str_trim(x)
    stringr::str_to_lower(x)
}

fo_detect_available_ranks <- function(outline) {
    if (!is.data.frame(outline)) {
        rlang::abort("`outline` must be a data frame or tibble.")
    }

    intersect(fo_rank_levels(), clean_column_names(names(outline)))
}

fo_check_rank <- function(rank, allow_null = FALSE) {
    check_logical_scalar(allow_null, "allow_null")

    if (is.null(rank) && isTRUE(allow_null)) {
        return(NULL)
    }

    if (!is.character(rank) || length(rank) != 1L || is.na(rank) || !nzchar(rank)) {
        rlang::abort("`rank` must be a single supported rank name.")
    }

    rank <- fo_normalize_taxon_name(rank)

    if (!rank %in% fo_rank_levels()) {
        rlang::abort(glue::glue(
            "`rank` must be one of: {paste(fo_rank_levels(), collapse = ', ')}."
        ))
    }

    rank
}

fo_check_rank_vector <- function(ranks, arg = "rank", allow_null = FALSE) {
    check_logical_scalar(allow_null, "allow_null")

    if (is.null(ranks) && isTRUE(allow_null)) {
        return(NULL)
    }

    if (!is.character(ranks) || length(ranks) == 0L || any(is.na(ranks)) || any(!nzchar(ranks))) {
        rlang::abort(glue::glue("`{arg}` must contain supported rank names."))
    }

    ranks <- fo_normalize_taxon_name(ranks)
    unsupported_ranks <- setdiff(ranks, fo_rank_levels())

    if (length(unsupported_ranks) > 0L) {
        rlang::abort(glue::glue(
            "`{arg}` contains unsupported ranks: {paste(unsupported_ranks, collapse = ', ')}."
        ))
    }

    unique(ranks)
}

fo_lineage_columns <- function() {
    fo_rank_levels()
}

fo_lower_ranks <- function(rank) {
    rank <- fo_check_rank(rank)
    rank_position <- match(rank, fo_rank_levels())

    if (is.na(rank_position) || rank_position >= length(fo_rank_levels())) {
        return(character())
    }

    fo_rank_levels()[seq.int(rank_position + 1L, length(fo_rank_levels()))]
}

fo_index_columns <- function() {
    c(
        "taxon_name",
        "taxon_name_norm",
        "accepted_name",
        "accepted_name_norm",
        "rank",
        "is_synonym",
        "synonym_of",
        fo_lineage_columns(),
        "source_row_id",
        outline_update_columns()
    )
}

fo_lineage_output_columns <- function() {
    c(
        "input_taxon",
        "matched_name",
        "accepted_name",
        "rank",
        "match_type",
        "is_synonym",
        "is_ambiguous",
        fo_lineage_columns(),
        "source_row_id",
        outline_update_columns()
    )
}

fo_descendant_output_columns <- function() {
    c(
        "input_taxon",
        "ancestor_name",
        "ancestor_rank",
        "descendant_name",
        "descendant_rank",
        fo_lineage_columns(),
        "source_row_id"
    )
}

fo_count_output_columns <- function() {
    c("input_taxon", "ancestor_name", "ancestor_rank", "rank", "n_taxa")
}

fo_empty_taxon_index <- function() {
    tibble::tibble(
        taxon_name = character(),
        taxon_name_norm = character(),
        accepted_name = character(),
        accepted_name_norm = character(),
        rank = character(),
        is_synonym = logical(),
        synonym_of = character(),
        kingdom = character(),
        subkingdom = character(),
        phylum = character(),
        subphylum = character(),
        class = character(),
        subclass = character(),
        order = character(),
        family = character(),
        genus = character(),
        source_row_id = integer(),
        updated_time = character(),
        update_type = character(),
        update_note = character(),
        update_link = character()
    )
}

fo_empty_lineage_result <- function() {
    tibble::tibble(
        input_taxon = character(),
        matched_name = character(),
        accepted_name = character(),
        rank = character(),
        match_type = character(),
        is_synonym = logical(),
        is_ambiguous = logical(),
        kingdom = character(),
        subkingdom = character(),
        phylum = character(),
        subphylum = character(),
        class = character(),
        subclass = character(),
        order = character(),
        family = character(),
        genus = character(),
        source_row_id = integer(),
        updated_time = character(),
        update_type = character(),
        update_note = character(),
        update_link = character()
    )
}

fo_empty_descendants <- function() {
    tibble::tibble(
        input_taxon = character(),
        ancestor_name = character(),
        ancestor_rank = character(),
        descendant_name = character(),
        descendant_rank = character(),
        kingdom = character(),
        subkingdom = character(),
        phylum = character(),
        subphylum = character(),
        class = character(),
        subclass = character(),
        order = character(),
        family = character(),
        genus = character(),
        source_row_id = integer()
    )
}

fo_empty_counts <- function() {
    tibble::tibble(
        input_taxon = character(),
        ancestor_name = character(),
        ancestor_rank = character(),
        rank = character(),
        n_taxa = integer()
    )
}

fo_prepare_taxon_index <- function(outline = NULL, taxon_index = NULL, include_synonyms = TRUE) {
    check_logical_scalar(include_synonyms, "include_synonyms")

    if (is.null(taxon_index)) {
        if (is.null(outline)) {
            taxon_index <- fo_default_taxon_index()
        } else {
            taxon_index <- build_taxon_index(outline, include_synonyms = include_synonyms)
        }
    } else if (!is.data.frame(taxon_index)) {
        rlang::abort("`taxon_index` must be a data frame or tibble.")
    } else {
        taxon_index <- tibble::as_tibble(taxon_index)
    }

    missing_columns <- setdiff(fo_index_columns(), names(taxon_index))

    if (length(missing_columns) > 0L) {
        rlang::abort(glue::glue(
            "`taxon_index` is missing required columns: {paste(missing_columns, collapse = ', ')}."
        ))
    }

    if (!isTRUE(include_synonyms)) {
        taxon_index <- taxon_index |>
            dplyr::filter(!.data$is_synonym)
    }

    taxon_index
}

fo_taxon_col_name <- function(taxon_col) {
    if (rlang::is_symbol(taxon_col)) {
        return(rlang::as_name(taxon_col))
    }

    if (is.character(taxon_col) && length(taxon_col) == 1L && !is.na(taxon_col) && nzchar(taxon_col)) {
        return(taxon_col)
    }

    rlang::abort("`taxon_col` must be a bare column name or a single column-name string.")
}
