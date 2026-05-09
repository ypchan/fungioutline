test_that("summarize_busco summarizes by group", {
    result <- summarize_busco(phase4_genome_metadata(), by = "phylum")

    expect_s3_class(result, "tbl_df")
    expect_true(all(c("phylum", phase4_busco_summary_columns()) %in% names(result)))
    expect_true("Ascomycota" %in% result$phylum)
})

test_that("summarize_busco rejects invalid inputs", {
    expect_error(
        summarize_busco(list(C = 95)),
        "`data` must be a data frame or tibble"
    )

    expect_error(
        summarize_busco(phase4_genome_metadata(), by = "missing_column"),
        "not found"
    )
})

test_that("summarize_busco handles missing optional BUSCO columns", {
    genomes <- tibble::tibble(C = c(90, 80), F = c(5, 10), M = c(5, 10))
    result <- summarize_busco(genomes)

    expect_s3_class(result, "tbl_df")
    expect_true(all(phase4_busco_summary_columns() %in% names(result)))
    expect_equal(result$n_genomes[[1]], 2L)
})

test_that("summarize_busco returns a tibble with required columns", {
    result <- summarize_busco(phase4_genome_metadata())

    expect_s3_class(result, "tbl_df")
    expect_true(all(phase4_busco_summary_columns() %in% names(result)))
})
