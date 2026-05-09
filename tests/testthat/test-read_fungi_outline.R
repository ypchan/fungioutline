test_that("read_fungi_outline reads and standardizes a normal fixture", {
    path <- testthat::test_path("fixtures", "minimal_outline.csv")

    result <- read_fungi_outline(path)

    expect_s3_class(result, "tbl_df")
    expect_true(all(expected_outline_columns() %in% names(result)))
    expect_identical(result$genus, "Fusarium")
})

test_that("read_fungi_outline rejects invalid paths and extensions", {
    expect_error(
        read_fungi_outline("missing-outline.xlsx"),
        "does not exist"
    )

    unsupported_file <- tempfile(fileext = ".json")
    writeLines("{}", unsupported_file)

    expect_error(
        read_fungi_outline(unsupported_file),
        "Unsupported outline file extension"
    )
})

test_that("read_fungi_outline can preserve raw names when requested", {
    path <- testthat::test_path("fixtures", "minimal_outline.csv")

    result <- read_fungi_outline(path, standardize = FALSE, validate = FALSE)

    expect_s3_class(result, "tbl_df")
    expect_true("Kingdom" %in% names(result))
    expect_false("kingdom" %in% names(result))
})

test_that("read_fungi_outline returns the expected class and required columns", {
    path <- testthat::test_path("fixtures", "minimal_outline.csv")
    result <- read_fungi_outline(path)

    expect_s3_class(result, "tbl_df")
    expect_true(all(expected_outline_columns() %in% names(result)))
})
