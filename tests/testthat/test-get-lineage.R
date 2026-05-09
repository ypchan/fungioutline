test_that("get_lineage resolves an accepted exact match", {
    idx <- build_taxon_index(toy_outline_phase2())
    result <- get_lineage("Fusarium", taxon_index = idx)

    expect_s3_class(result, "tbl_df")
    expect_true(all(phase2_lineage_columns() %in% names(result)))
    expect_identical(result$match_type[[1]], "accepted")
    expect_identical(result$accepted_name[[1]], "Fusarium")
    expect_identical(result$rank[[1]], "genus")
})

test_that("get_lineage resolves a synonym match", {
    idx <- build_taxon_index(toy_outline_phase2())
    result <- get_lineage("Gibberella", taxon_index = idx)

    expect_identical(result$match_type[[1]], "synonym")
    expect_true(result$is_synonym[[1]])
    expect_identical(result$accepted_name[[1]], "Fusarium")
})

test_that("get_lineage returns an unmatched row for unknown taxa", {
    idx <- build_taxon_index(toy_outline_phase2())
    result <- get_lineage("Unknown taxon", taxon_index = idx)

    expect_equal(nrow(result), 1L)
    expect_identical(result$match_type[[1]], "unmatched")
    expect_true(is.na(result$matched_name[[1]]))
})

test_that("get_lineage supports vector input", {
    idx <- build_taxon_index(toy_outline_phase2())
    result <- get_lineage(c("Agaricus", "Fusarium"), taxon_index = idx)

    expect_s3_class(result, "tbl_df")
    expect_equal(result$input_taxon, c("Agaricus", "Fusarium"))
})

test_that("get_lineage returns multiple ambiguous rows when best_match is FALSE", {
    idx <- build_taxon_index(toy_outline_phase2())
    result <- get_lineage("Ambigua", taxon_index = idx, best_match = FALSE, warn_ambiguous = FALSE)

    expect_gt(nrow(result), 1L)
    expect_true(all(result$is_ambiguous))
    expect_true(all(c("class", "genus") %in% result$rank))
})

test_that("get_lineage returns one row per input with best_match TRUE", {
    idx <- build_taxon_index(toy_outline_phase2())
    result <- get_lineage(
        c("Ambigua", "Fusaria", "Unknown taxon"),
        taxon_index = idx,
        best_match = TRUE,
        warn_ambiguous = FALSE
    )

    expect_equal(nrow(result), 3L)
    expect_identical(result$rank[[1]], "genus")
    expect_identical(result$match_type[[2]], "synonym")
    expect_identical(result$match_type[[3]], "unmatched")
})

test_that("get_lineage output has required columns", {
    idx <- build_taxon_index(toy_outline_phase2())
    result <- get_lineage("Fusarium", taxon_index = idx)

    expect_true(all(phase2_lineage_columns() %in% names(result)))
})
