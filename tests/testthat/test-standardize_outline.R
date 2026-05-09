test_that("standardize_outline handles a normal outline table", {
    outline <- minimal_outline()
    outline$Genus <- "  Fusarium  "

    result <- standardize_outline(outline)

    expect_s3_class(result, "tbl_df")
    expect_true(all(expected_outline_columns() %in% names(result)))
    expect_identical(result$genus, "Fusarium")
})

test_that("standardize_outline rejects invalid inputs", {
    expect_error(
        standardize_outline(list(Kingdom = "Fungi")),
        "`data` must be a data frame or tibble"
    )

    duplicate_columns <- tibble::tibble(Kingdom = "Fungi", kingdom = "Fungi")

    expect_error(
        standardize_outline(duplicate_columns),
        "unique after standardization"
    )
})

test_that("standardize_outline drops species columns and handles empty strings", {
    outline <- minimal_outline()
    outline$Genus_syn <- ""
    outline$Species <- "Fusarium oxysporum"
    outline$Species_syn <- "Fusarium cubense"

    result <- standardize_outline(outline)

    expect_false("species" %in% names(result))
    expect_false("species_syn" %in% names(result))
    expect_true(is.na(result$genus_syn[[1]]))
})

test_that("standardize_outline returns the expected class and required columns", {
    result <- standardize_outline(minimal_outline())

    expect_s3_class(result, "tbl_df")
    expect_true(all(expected_outline_columns() %in% names(result)))
})
