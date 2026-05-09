test_that("filter_genomes_by_quality keeps genomes meeting thresholds", {
    result <- filter_genomes_by_quality(
        phase4_genome_metadata(),
        min_complete = 90,
        max_missing = 10
    )

    expect_s3_class(result, "tbl_df")
    expect_true(all(result$C >= 90))
    expect_true(all(result$M <= 10))
})

test_that("filter_genomes_by_quality rejects invalid inputs", {
    expect_error(
        filter_genomes_by_quality(list(C = 95)),
        "`data` must be a data frame or tibble"
    )

    expect_error(
        filter_genomes_by_quality(phase4_genome_metadata(), quality = "excellent"),
        "unsupported labels"
    )
})

test_that("filter_genomes_by_quality handles no matching rows", {
    result <- filter_genomes_by_quality(
        phase4_genome_metadata(),
        min_complete = 99
    )

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0L)
})

test_that("filter_genomes_by_quality can require ok genomes", {
    result <- filter_genomes_by_quality(
        phase4_genome_metadata(),
        min_complete = 40,
        max_missing = 100,
        max_fragmented = 100,
        require_ok = TRUE
    )

    expect_true(all(result$ok))
    expect_true("genome_quality" %in% names(result))
})
