test_that("get_descendants returns descendants for a high-rank taxon", {
    idx <- build_taxon_index(toy_outline_phase2())
    result <- get_descendants("Ascomycota", taxon_index = idx)

    expect_s3_class(result, "tbl_df")
    expect_true(all(phase2_descendant_columns() %in% names(result)))
    expect_true("Fusarium" %in% result$descendant_name)
    expect_true("Aspergillus" %in% result$descendant_name)
})

test_that("get_descendants returns only the requested target rank", {
    idx <- build_taxon_index(toy_outline_phase2())
    result <- get_descendants("Ascomycota", target_rank = "genus", taxon_index = idx)

    expect_true(all(result$descendant_rank == "genus"))
    expect_setequal(result$descendant_name, c("Fusarium", "Aspergillus", "Ambigua"))
})

test_that("get_descendants returns an empty tibble when target rank is not lower", {
    idx <- build_taxon_index(toy_outline_phase2())

    expect_warning(
        result <- get_descendants("Fusarium", target_rank = "kingdom", taxon_index = idx),
        "not lower"
    )

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0L)
    expect_true(all(phase2_descendant_columns() %in% names(result)))
})

test_that("get_descendants does not include the ancestor itself", {
    idx <- build_taxon_index(toy_outline_phase2())
    result <- get_descendants("Ascomycota", taxon_index = idx)

    expect_false(any(result$descendant_name == "Ascomycota" & result$descendant_rank == "phylum"))
})

test_that("get_descendants works through synonym ancestors", {
    idx <- build_taxon_index(toy_outline_phase2())
    result <- get_descendants("Pyrenomycetes", target_rank = "genus", taxon_index = idx)

    expect_s3_class(result, "tbl_df")
    expect_true("Fusarium" %in% result$descendant_name)
    expect_true(all(result$ancestor_rank == "class"))
})
