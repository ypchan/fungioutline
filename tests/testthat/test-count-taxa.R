test_that("count_taxa counts lower ranks correctly", {
    idx <- build_taxon_index(toy_outline_phase2())
    result <- count_taxa("Ascomycota", by_rank = "genus", taxon_index = idx)

    expect_s3_class(result, "tbl_df")
    expect_true(all(phase2_count_columns() %in% names(result)))
    expect_equal(result$n_taxa[[1]], 3L)
})

test_that("count_taxa supports by_rank", {
    idx <- build_taxon_index(toy_outline_phase2())
    result <- count_taxa("Ascomycota", by_rank = c("family", "genus"), taxon_index = idx)

    family_count <- dplyr::filter(result, .data$rank == "family")$n_taxa[[1]]
    genus_count <- dplyr::filter(result, .data$rank == "genus")$n_taxa[[1]]

    expect_equal(family_count, 2L)
    expect_equal(genus_count, 3L)
})

test_that("count_taxa rejects species because the outline is genus-level", {
    idx <- build_taxon_index(toy_outline_phase2())

    expect_error(
        count_taxa("Fusarium", by_rank = "species", taxon_index = idx),
        "unsupported ranks: species"
    )
})

test_that("count_taxa supports vector taxon input", {
    idx <- build_taxon_index(toy_outline_phase2())
    result <- count_taxa(c("Ascomycota", "Basidiomycota"), by_rank = "genus", taxon_index = idx)

    expect_equal(nrow(result), 2L)
    expect_equal(
        dplyr::filter(result, .data$input_taxon == "Ascomycota")$n_taxa[[1]],
        3L
    )
    expect_equal(
        dplyr::filter(result, .data$input_taxon == "Basidiomycota")$n_taxa[[1]],
        1L
    )
})

test_that("count_taxa returns a tibble with required columns", {
    idx <- build_taxon_index(toy_outline_phase2())
    result <- count_taxa("Ascomycota", taxon_index = idx)

    expect_s3_class(result, "tbl_df")
    expect_true(all(phase2_count_columns() %in% names(result)))
})
