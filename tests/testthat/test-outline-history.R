test_that("read_outline_history returns empty canonical history for missing files", {
    path <- file.path(tempdir(), "missing-outline-history.tsv")
    history <- read_outline_history(path)

    expect_s3_class(history, "tbl_df")
    expect_equal(nrow(history), 0L)
    expect_identical(names(history), phase3_history_columns())
})

test_that("write_outline_history writes and read_outline_history reads TSV", {
    path <- tempfile(fileext = ".tsv")
    history <- compare_outline_versions(NULL, phase3_outline(), version_id = "outline_tsv")

    written <- write_outline_history(history, path, append = FALSE)
    read_back <- read_outline_history(path)

    expect_s3_class(written, "tbl_df")
    expect_equal(nrow(read_back), nrow(history))
    expect_identical(names(read_back)[seq_along(phase3_history_columns())], phase3_history_columns())
})

test_that("write_outline_history writes and read_outline_history reads CSV", {
    path <- tempfile(fileext = ".csv")
    history <- compare_outline_versions(NULL, phase3_outline(), version_id = "outline_csv")

    write_outline_history(history, path, append = FALSE)
    read_back <- read_outline_history(path)

    expect_equal(nrow(read_back), nrow(history))
    expect_identical(names(read_back)[seq_along(phase3_history_columns())], phase3_history_columns())
})

test_that("write_outline_history writes and read_outline_history reads RDS", {
    path <- tempfile(fileext = ".rds")
    history <- compare_outline_versions(NULL, phase3_outline(), version_id = "outline_rds")

    write_outline_history(history, path, append = FALSE)
    read_back <- read_outline_history(path)

    expect_equal(nrow(read_back), nrow(history))
    expect_identical(names(read_back)[seq_along(phase3_history_columns())], phase3_history_columns())
})

test_that("write_outline_history appends to existing history", {
    path <- tempfile(fileext = ".tsv")
    first <- compare_outline_versions(NULL, phase3_outline(), version_id = "outline_first")
    second <- compare_outline_versions(phase3_outline(), phase3_outline_added(), version_id = "outline_second")

    write_outline_history(first, path, append = FALSE)
    write_outline_history(second, path, append = TRUE)
    read_back <- read_outline_history(path)

    expect_true(all(c("outline_first", "outline_second") %in% read_back$version_id))
})

test_that("write_outline_history overwrites when append is FALSE", {
    path <- tempfile(fileext = ".tsv")
    first <- compare_outline_versions(NULL, phase3_outline(), version_id = "outline_first")
    second <- compare_outline_versions(phase3_outline(), phase3_outline_added(), version_id = "outline_second")

    write_outline_history(first, path, append = FALSE)
    write_outline_history(second, path, append = FALSE)
    read_back <- read_outline_history(path)

    expect_false("outline_first" %in% read_back$version_id)
    expect_true("outline_second" %in% read_back$version_id)
})

test_that("write_outline_history preserves extra columns after canonical columns", {
    path <- tempfile(fileext = ".rds")
    history <- compare_outline_versions(NULL, phase3_outline(), version_id = "outline_extra") |>
        dplyr::mutate(curator = "manual")

    write_outline_history(history, path, append = FALSE)
    read_back <- read_outline_history(path)

    expect_true("curator" %in% names(read_back))
    expect_identical(names(read_back)[seq_along(phase3_history_columns())], phase3_history_columns())
    expect_true(all(read_back$curator == "manual"))
})

test_that("read_outline_history errors on unsupported extensions", {
    path <- tempfile(fileext = ".json")
    writeLines("[]", path)

    expect_error(
        read_outline_history(path),
        "Unsupported history file extension"
    )

    expect_error(
        write_outline_history(compare_outline_versions(NULL, phase3_outline(), version_id = "outline_json"), path),
        "Unsupported history file extension"
    )
})
