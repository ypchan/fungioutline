test_that("read_genome_metadata reads and standardizes a CSV fixture", {
    path <- tempfile(fileext = ".csv")
    readr::write_csv(phase4_genome_metadata(), path)

    result <- read_genome_metadata(path)

    expect_s3_class(result, "tbl_df")
    expect_true(all(phase4_required_genome_columns() %in% names(result)))
    expect_true("C" %in% names(result))
})

test_that("read_genome_metadata rejects invalid paths and extensions", {
    expect_error(
        read_genome_metadata("missing-genomes.csv"),
        "does not exist"
    )

    unsupported_file <- tempfile(fileext = ".json")
    writeLines("[]", unsupported_file)

    expect_error(
        read_genome_metadata(unsupported_file),
        "Unsupported genome metadata file extension"
    )
})

test_that("read_genome_metadata can preserve raw names when requested", {
    path <- tempfile(fileext = ".csv")
    readr::write_csv(phase4_genome_metadata(), path)

    result <- read_genome_metadata(path, standardize = FALSE, validate = FALSE)

    expect_s3_class(result, "tbl_df")
    expect_true("genome_label" %in% names(result))
})

test_that("read_genome_metadata returns the expected class and required columns", {
    path <- tempfile(fileext = ".csv")
    readr::write_csv(phase4_genome_metadata(), path)

    result <- read_genome_metadata(path)

    expect_s3_class(result, "tbl_df")
    expect_true(all(phase4_required_genome_columns() %in% names(result)))
})
