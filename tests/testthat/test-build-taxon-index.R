test_that("build_taxon_index returns a tibble with required columns", {
    idx <- build_taxon_index(toy_outline_phase2())

    expect_s3_class(idx, "tbl_df")
    expect_true(all(phase2_index_columns() %in% names(idx)))
})

test_that("build_taxon_index includes accepted names and synonyms", {
    idx <- build_taxon_index(toy_outline_phase2())

    expect_true("Fusarium" %in% idx$taxon_name)
    expect_true("Ascomycota" %in% idx$taxon_name)
    expect_true("Fusaria" %in% idx$taxon_name)
    expect_true("Gibberella" %in% idx$taxon_name)
    expect_true("Pratella" %in% idx$taxon_name)
    expect_true(any(idx$taxon_name == "Gibberella" & idx$is_synonym))
})

test_that("build_taxon_index handles absent species columns gracefully", {
    outline <- toy_outline_phase2()
    expect_false("species" %in% names(outline))

    idx <- build_taxon_index(outline)

    expect_true("species" %in% names(idx))
    expect_true(all(is.na(idx$species)))
    expect_false(any(idx$rank == "species"))
})

test_that("build_taxon_index drops empty synonym entries", {
    idx <- build_taxon_index(toy_outline_phase2())

    expect_false(any(is.na(idx$taxon_name)))
    expect_false(any(idx$taxon_name == ""))
})

test_that("build_taxon_index preserves source rows and update metadata", {
    idx <- build_taxon_index(toy_outline_phase2())

    expect_true(all(seq_len(nrow(toy_outline_phase2())) %in% idx$source_row_id))
    fusarium <- dplyr::filter(idx, .data$taxon_name == "Fusarium", .data$rank == "genus")
    expect_identical(fusarium$updated_time[[1]], "2026-01-01")
    expect_identical(fusarium$update_type[[1]], "seed")
    expect_identical(fusarium$update_note[[1]], "First record")
    expect_identical(fusarium$update_link[[1]], "https://example.org/1")
})
