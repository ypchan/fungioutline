test_that("update_outline_data works when no previous outline exists", {
    testthat::skip_if_not_installed("writexl")

    output_dir <- tempfile("fungioutline-update-")
    excel_path <- tempfile(fileext = ".xlsx")
    phase3_write_excel(phase3_outline(), excel_path)

    result <- update_outline_data(
        excel_path = excel_path,
        output_dir = output_dir,
        version_id = "outline_20260101_000000",
        write_history = TRUE
    )

    expect_named(result, c("version_id", "outline", "taxon_index", "history", "paths", "summary"))
    expect_s3_class(result$outline, "tbl_df")
    expect_s3_class(result$taxon_index, "tbl_df")
    expect_s3_class(result$history, "tbl_df")
    expect_s3_class(result$summary, "tbl_df")
    expect_true(all(phase3_summary_columns() %in% names(result$summary)))
    expect_true(file.exists(file.path(output_dir, "outline_current.rds")))
    expect_true(file.exists(file.path(output_dir, "outline_index.rds")))
    expect_true(file.exists(file.path(output_dir, "outline_outline_20260101_000000.rds")))
    expect_true(file.exists(file.path(output_dir, "outline_index_outline_20260101_000000.rds")))
    expect_true(file.exists(file.path(output_dir, "outline_history.tsv")))
})

test_that("update_outline_data works when previous outline exists and detects changes", {
    testthat::skip_if_not_installed("writexl")

    output_dir <- tempfile("fungioutline-update-")
    first_excel <- tempfile(fileext = ".xlsx")
    second_excel <- tempfile(fileext = ".xlsx")
    phase3_write_excel(phase3_outline(), first_excel)
    phase3_write_excel(phase3_outline_synonym_changed(), second_excel)

    update_outline_data(
        excel_path = first_excel,
        output_dir = output_dir,
        version_id = "outline_20260101_000001"
    )

    result <- update_outline_data(
        excel_path = second_excel,
        output_dir = output_dir,
        version_id = "outline_20260101_000002"
    )

    expect_true(any(result$history$change_type == "changed_synonym"))
    expect_gt(result$summary$n_changes[[1]], 0L)
})

test_that("update_outline_data does not fail with zero changes", {
    testthat::skip_if_not_installed("writexl")

    output_dir <- tempfile("fungioutline-update-")
    first_excel <- tempfile(fileext = ".xlsx")
    second_excel <- tempfile(fileext = ".xlsx")
    phase3_write_excel(phase3_outline(), first_excel)
    phase3_write_excel(phase3_outline(), second_excel)

    update_outline_data(
        excel_path = first_excel,
        output_dir = output_dir,
        version_id = "outline_20260101_000003"
    )

    result <- update_outline_data(
        excel_path = second_excel,
        output_dir = output_dir,
        version_id = "outline_20260101_000004"
    )

    expect_s3_class(result$history, "tbl_df")
    expect_equal(nrow(result$history), 0L)
    expect_equal(result$summary$n_changes[[1]], 0L)
})
