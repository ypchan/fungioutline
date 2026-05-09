test_that("add_lineage supports bare column names", {
    idx <- build_taxon_index(toy_outline_phase2())
    data <- tibble::tibble(sample_id = 1L, genus = "Fusarium")

    result <- add_lineage(data, genus, taxon_index = idx)

    expect_s3_class(result, "tbl_df")
    expect_identical(result$sample_id, 1L)
    expect_identical(result$accepted_name[[1]], "Fusarium")
})

test_that("add_lineage supports string column names", {
    idx <- build_taxon_index(toy_outline_phase2())
    data <- tibble::tibble(sample_id = 1L, genus = "Fusaria")

    result <- add_lineage(data, "genus", taxon_index = idx)

    expect_identical(result$match_type[[1]], "synonym")
    expect_identical(result$accepted_name[[1]], "Fusarium")
})

test_that("add_lineage preserves row order", {
    idx <- build_taxon_index(toy_outline_phase2())
    data <- tibble::tibble(sample_id = c("b", "a", "c"), genus = c("Agaricus", "Fusarium", "Aspergillus"))

    result <- add_lineage(data, genus, taxon_index = idx)

    expect_equal(result$sample_id, data$sample_id)
})

test_that("add_lineage keeps unmatched rows by default", {
    idx <- build_taxon_index(toy_outline_phase2())
    data <- tibble::tibble(sample_id = 1:2, genus = c("Fusarium", "Unknown taxon"))

    result <- add_lineage(data, genus, taxon_index = idx)

    expect_equal(nrow(result), 2L)
    expect_true("unmatched" %in% result$match_type)
})

test_that("add_lineage can drop unmatched rows", {
    idx <- build_taxon_index(toy_outline_phase2())
    data <- tibble::tibble(sample_id = 1:2, genus = c("Fusarium", "Unknown taxon"))

    result <- add_lineage(data, genus, taxon_index = idx, keep_unmatched = FALSE)

    expect_equal(nrow(result), 1L)
    expect_false("unmatched" %in% result$match_type)
})

test_that("add_lineage suffixes conflicting lineage columns", {
    idx <- build_taxon_index(toy_outline_phase2())
    data <- tibble::tibble(sample_id = 1L, genus = "Fusarium", rank = "input rank")

    result <- add_lineage(data, genus, taxon_index = idx)

    expect_true("genus_lineage" %in% names(result))
    expect_true("rank_lineage" %in% names(result))
    expect_identical(result$genus[[1]], "Fusarium")
    expect_identical(result$rank[[1]], "input rank")
})

test_that("add_lineage returns a tibble", {
    idx <- build_taxon_index(toy_outline_phase2())
    data <- tibble::tibble(sample_id = 1L, genus = "Fusarium")

    result <- add_lineage(data, genus, taxon_index = idx)

    expect_s3_class(result, "tbl_df")
})
