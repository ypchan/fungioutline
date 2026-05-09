test_that("check_genome_coverage reports genome availability by descendant rank", {
    result <- check_genome_coverage(
        "Ascomycota",
        phase4_genome_metadata(),
        target_rank = "genus",
        taxon_index = phase4_taxon_index()
    )

    expect_s3_class(result, "tbl_df")
    expect_true(all(phase4_coverage_columns() %in% names(result)))
    expect_true("Neurospora" %in% result$taxon_name)
    expect_false(result$has_genome[result$taxon_name == "Neurospora"][[1]])
})

test_that("check_genome_coverage works through synonym ancestors", {
    result <- check_genome_coverage(
        "Sac fungi",
        phase4_genome_metadata(),
        target_rank = "genus",
        taxon_index = phase4_taxon_index()
    )

    expect_true(all(result$ancestor_name == "Ascomycota"))
    expect_true("Fusarium" %in% result$taxon_name)
})

test_that("check_genome_coverage rejects invalid inputs", {
    expect_error(
        check_genome_coverage(123, phase4_genome_metadata(), taxon_index = phase4_taxon_index()),
        "`taxon` must be a character vector"
    )

    expect_error(
        check_genome_coverage("Ascomycota", phase4_genome_metadata(), target_rank = "subclass", taxon_index = phase4_taxon_index()),
        "genome metadata ranks"
    )
})

test_that("check_genome_coverage returns empty tibble when target rank is not lower", {
    expect_warning(
        result <- check_genome_coverage(
            "Fusarium",
            phase4_genome_metadata(),
            target_rank = "genus",
            taxon_index = phase4_taxon_index()
        ),
        "not lower"
    )

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0L)
    expect_true(all(phase4_coverage_columns() %in% names(result)))
})
