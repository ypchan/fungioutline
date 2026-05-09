test_that("cache_ncbi_result returns NULL for missing cache entries", {
    cache_dir <- tempfile("ncbi-cache-")

    result <- cache_ncbi_result("missing_key", cache_dir = cache_dir)

    expect_null(result)
})

test_that("cache_ncbi_result writes and reads cached objects", {
    cache_dir <- tempfile("ncbi-cache-")
    value <- list(count = 2L, ids = c("1", "2"))

    written <- cache_ncbi_result("fusarium_key", value, cache_dir = cache_dir)
    read_back <- cache_ncbi_result("fusarium_key", cache_dir = cache_dir)

    expect_equal(written, value)
    expect_equal(read_back, value)
    expect_true(dir.exists(cache_dir))
})

test_that("cache_ncbi_result respects overwrite FALSE", {
    cache_dir <- tempfile("ncbi-cache-")

    cache_ncbi_result("same_key", list(count = 1L), cache_dir = cache_dir)
    cache_ncbi_result("same_key", list(count = 99L), cache_dir = cache_dir, overwrite = FALSE)
    read_back <- cache_ncbi_result("same_key", cache_dir = cache_dir)

    expect_equal(read_back$count, 1L)
})

test_that("cache_ncbi_result rejects invalid inputs", {
    expect_error(
        cache_ncbi_result(""),
        "`cache_key` must be a single non-empty character string"
    )

    expect_error(
        cache_ncbi_result("key", list(count = 1L), overwrite = NA),
        "`overwrite` must be TRUE or FALSE"
    )
})
