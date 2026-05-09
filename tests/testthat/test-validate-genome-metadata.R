test_that("validate_genome_metadata reports a normal table", {
    report <- validate_genome_metadata(phase4_genome_metadata())

    expect_s3_class(report, "tbl_df")
    expect_true(all(phase4_validation_columns() %in% names(report)))
    expect_false(any(report$status == "fail" & report$severity == "fatal"))
})

test_that("validate_genome_metadata rejects invalid input", {
    report <- validate_genome_metadata(list(genome_label = "g1"), error = FALSE)

    expect_s3_class(report, "tbl_df")
    expect_true(any(report$check == "input_type" & report$status == "fail"))

    expect_error(
        validate_genome_metadata(list(genome_label = "g1")),
        class = "fungioutline_genome_validation_error"
    )
})

test_that("validate_genome_metadata reports missing required columns", {
    incomplete <- tibble::tibble(genome_label = "g1")
    report <- validate_genome_metadata(incomplete, error = FALSE)

    expect_true(any(report$check == "required_columns" & report$status == "fail"))

    expect_error(
        validate_genome_metadata(incomplete),
        class = "fungioutline_genome_validation_error"
    )
})

test_that("validate_genome_metadata catches an empty table edge case", {
    empty <- phase4_genome_metadata()[0, ]
    report <- validate_genome_metadata(empty, error = FALSE)

    expect_s3_class(report, "tbl_df")
    expect_true(any(report$check == "row_count" & report$status == "fail"))
})

test_that("validate_genome_metadata catches invalid BUSCO values", {
    invalid <- phase4_genome_metadata()
    invalid$C[[1]] <- 120
    report <- validate_genome_metadata(invalid, error = FALSE)

    expect_true(any(report$check == "busco_percent_range" & report$status == "fail"))
})
