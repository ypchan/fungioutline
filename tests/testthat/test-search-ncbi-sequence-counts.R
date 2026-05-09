test_that("search_ncbi_sequence_counts returns count-only rows", {
    result <- search_ncbi_sequence_counts(
        "Fusarium",
        taxon_index = phase4_taxon_index(),
        use_cache = FALSE,
        delay_sec = 0,
        search_fun = phase5_mock_ncbi_search
    )

    expect_s3_class(result, "tbl_df")
    expect_true(all(phase5_ncbi_count_columns() %in% names(result)))
    expect_false("ids" %in% names(result))
    expect_equal(result$count[[1]], 2L)
    expect_true(result$has_sequences[[1]])
})

test_that("search_ncbi_sequence_counts requests retmax zero", {
    seen_retmax <- NA_integer_
    mock_counts <- function(db, query, retmax, api_key) {
        seen_retmax <<- retmax
        phase5_mock_ncbi_search(db, query, retmax, api_key)
    }

    search_ncbi_sequence_counts(
        "Ascomycota",
        taxon_index = phase4_taxon_index(),
        use_cache = FALSE,
        delay_sec = 0,
        search_fun = mock_counts
    )

    expect_equal(seen_retmax, 0L)
})

test_that("search_ncbi_sequence_counts supports vector input", {
    result <- search_ncbi_sequence_counts(
        c("Ascomycota", "Unknown taxon"),
        taxon_index = phase4_taxon_index(),
        use_cache = FALSE,
        delay_sec = 0,
        search_fun = phase5_mock_ncbi_search
    )

    expect_equal(result$input_taxon, c("Ascomycota", "Unknown taxon"))
    expect_equal(result$count, c(5L, 0L))
})

test_that("search_ncbi_sequence_counts uses cache", {
    cache_dir <- tempfile("ncbi-cache-")
    calls <- 0L
    counting_search <- function(db, query, retmax, api_key) {
        calls <<- calls + 1L
        phase5_mock_ncbi_search(db, query, retmax, api_key)
    }

    search_ncbi_sequence_counts(
        "Fusarium",
        taxon_index = phase4_taxon_index(),
        cache_dir = cache_dir,
        delay_sec = 0,
        search_fun = counting_search
    )
    cached <- search_ncbi_sequence_counts(
        "Fusarium",
        taxon_index = phase4_taxon_index(),
        cache_dir = cache_dir,
        delay_sec = 0,
        search_fun = counting_search
    )

    expect_equal(calls, 1L)
    expect_true(cached$cached[[1]])
})

test_that("search_ncbi_sequence_counts rejects invalid input", {
    expect_error(
        search_ncbi_sequence_counts(123, search_fun = phase5_mock_ncbi_search),
        "`taxon` must be a character vector"
    )
})
