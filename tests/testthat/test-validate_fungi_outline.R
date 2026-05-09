test_that("validate_fungi_outline returns a report for a normal outline table", {
    report <- validate_fungi_outline(minimal_outline())

    expect_s3_class(report, "tbl_df")
    expect_true(all(expected_validation_columns() %in% names(report)))
    expect_false(any(report$status == "fail"))
})

test_that("validate_fungi_outline reports and errors on invalid input", {
    report <- validate_fungi_outline(list(Kingdom = "Fungi"), error = FALSE)

    expect_s3_class(report, "tbl_df")
    expect_true(all(expected_validation_columns() %in% names(report)))
    expect_true(any(report$check == "input_type" & report$status == "fail"))

    expect_error(
        validate_fungi_outline(list(Kingdom = "Fungi")),
        class = "fungioutline_validation_error"
    )
})

test_that("validate_fungi_outline reports missing required columns", {
    incomplete <- tibble::tibble(Kingdom = "Fungi")
    report <- validate_fungi_outline(incomplete, error = FALSE)

    expect_true(any(report$check == "required_columns" & report$status == "fail"))

    expect_error(
        validate_fungi_outline(incomplete),
        class = "fungioutline_validation_error"
    )
})

test_that("validate_fungi_outline handles an empty outline edge case", {
    empty_outline <- minimal_outline()[0, ]
    report <- validate_fungi_outline(empty_outline, error = FALSE)

    expect_s3_class(report, "tbl_df")
    expect_true(all(expected_validation_columns() %in% names(report)))
    expect_true(any(report$check == "row_count" & report$status == "fail"))
})

test_that("validate_fungi_outline reports duplicate taxonomic keys as a warning", {
    duplicated_outline <- dplyr::bind_rows(minimal_outline(), minimal_outline())
    report <- validate_fungi_outline(duplicated_outline, error = FALSE)

    expect_s3_class(report, "tbl_df")
    expect_true(all(expected_validation_columns() %in% names(report)))
    expect_true(any(report$check == "duplicate_taxon_keys" & report$status == "fail"))
    expect_true(any(report$check == "duplicate_taxon_keys" & report$severity == "warning"))
})

test_that("validate_fungi_outline returns the expected class and required columns", {
    report <- validate_fungi_outline(minimal_outline())

    expect_s3_class(report, "tbl_df")
    expect_true(all(expected_validation_columns() %in% names(report)))
})
