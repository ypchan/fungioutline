test_that("classify_genome_quality adds expected quality labels", {
    result <- classify_genome_quality(phase4_genome_metadata())

    expect_s3_class(result, "tbl_df")
    expect_true("genome_quality" %in% names(result))
    expect_true(all(c("high_quality", "medium_quality", "low_quality") %in% result$genome_quality))
})

test_that("classify_genome_quality rejects invalid inputs", {
    expect_error(
        classify_genome_quality(list(C = 95, F = 2, M = 3)),
        "`data` must be a data frame or tibble"
    )

    expect_error(
        classify_genome_quality(tibble::tibble(C = 95, F = 2), missing_col = "M"),
        "Column `M` was not found"
    )
})

test_that("classify_genome_quality marks missing BUSCO values as unknown", {
    genomes <- tibble::tibble(C = NA_real_, F = 1, M = NA_real_)
    result <- classify_genome_quality(genomes)

    expect_identical(result$genome_quality[[1]], "unknown")
})

test_that("classify_genome_quality returns a tibble with required columns", {
    result <- classify_genome_quality(phase4_genome_metadata())

    expect_s3_class(result, "tbl_df")
    expect_true(all(c("C", "F", "M", "genome_quality") %in% names(result)))
})
