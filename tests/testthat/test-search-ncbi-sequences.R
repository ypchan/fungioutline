test_that("search_ncbi_sequences returns sequence availability rows", {
    result <- search_ncbi_sequences(
        "Fusarium",
        taxon_index = phase4_taxon_index(),
        retmax = 2,
        use_cache = FALSE,
        delay_sec = 0,
        search_fun = phase5_mock_ncbi_search
    )

    expect_s3_class(result, "tbl_df")
    expect_true(all(phase5_ncbi_result_columns() %in% names(result)))
    expect_equal(result$count[[1]], 2L)
    expect_equal(result$ids[[1]], c("200", "201"))
    expect_true(result$has_sequences[[1]])
    expect_false(result$cached[[1]])
    expect_true(stringr::str_detect(result$query[[1]], '"Fusarium"\\[Organism\\]'))
})

test_that("search_ncbi_sequences resolves synonyms through the taxon index", {
    result <- search_ncbi_sequences(
        "Gibberella",
        taxon_index = phase4_taxon_index(),
        retmax = 2,
        use_cache = FALSE,
        delay_sec = 0,
        search_fun = phase5_mock_ncbi_search
    )

    expect_equal(result$accepted_name[[1]], "Fusarium")
    expect_equal(result$match_type[[1]], "synonym")
    expect_equal(result$count[[1]], 2L)
})

test_that("search_ncbi_sequences supports vector input", {
    result <- search_ncbi_sequences(
        c("Ascomycota", "Agaricus"),
        taxon_index = phase4_taxon_index(),
        retmax = 1,
        use_cache = FALSE,
        delay_sec = 0,
        search_fun = phase5_mock_ncbi_search
    )

    expect_equal(result$input_taxon, c("Ascomycota", "Agaricus"))
    expect_equal(result$count, c(5L, 1L))
    expect_equal(length(result$ids[[1]]), 1L)
})

test_that("search_ncbi_sequences uses cached results on repeated queries", {
    cache_dir <- tempfile("ncbi-cache-")
    calls <- 0L
    counting_search <- function(db, query, retmax, api_key) {
        calls <<- calls + 1L
        phase5_mock_ncbi_search(db, query, retmax, api_key)
    }

    first <- search_ncbi_sequences(
        "Fusarium",
        taxon_index = phase4_taxon_index(),
        retmax = 2,
        cache_dir = cache_dir,
        delay_sec = 0,
        search_fun = counting_search
    )
    second <- search_ncbi_sequences(
        "Fusarium",
        taxon_index = phase4_taxon_index(),
        retmax = 2,
        cache_dir = cache_dir,
        delay_sec = 0,
        search_fun = counting_search
    )

    expect_equal(calls, 1L)
    expect_false(first$cached[[1]])
    expect_true(second$cached[[1]])
    expect_equal(second$count[[1]], 2L)
})

test_that("search_ncbi_sequences handles zero-count results", {
    result <- search_ncbi_sequences(
        "Unknown taxon",
        retmax = 2,
        use_cache = FALSE,
        delay_sec = 0,
        search_fun = phase5_mock_ncbi_search
    )

    expect_equal(result$count[[1]], 0L)
    expect_false(result$has_sequences[[1]])
    expect_equal(result$ids[[1]], character())
})

test_that("search_ncbi_sequences rejects invalid inputs", {
    expect_error(
        search_ncbi_sequences(123, search_fun = phase5_mock_ncbi_search),
        "`taxon` must be a character vector"
    )

    expect_error(
        search_ncbi_sequences("Fusarium", db = "", search_fun = phase5_mock_ncbi_search),
        "`db` must be a single non-empty NCBI database name"
    )

    expect_error(
        search_ncbi_sequences("Fusarium", retmax = -1, search_fun = phase5_mock_ncbi_search),
        "`retmax` must be a single non-negative numeric value"
    )
})
