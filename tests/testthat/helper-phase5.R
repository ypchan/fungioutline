phase5_ncbi_result_columns <- function() {
    c(
        "input_taxon",
        "accepted_name",
        "rank",
        "match_type",
        "db",
        "query",
        "count",
        "ids",
        "has_sequences",
        "cached",
        "query_time",
        "error"
    )
}

phase5_ncbi_count_columns <- function() {
    c(
        "input_taxon",
        "accepted_name",
        "rank",
        "match_type",
        "db",
        "query",
        "count",
        "has_sequences",
        "cached",
        "query_time",
        "error"
    )
}

phase5_mock_ncbi_search <- function(db, query, retmax, api_key) {
    count <- dplyr::case_when(
        stringr::str_detect(query, "Fusarium") ~ 2L,
        stringr::str_detect(query, "Ascomycota") ~ 5L,
        stringr::str_detect(query, "Agaricus") ~ 1L,
        TRUE ~ 0L
    )
    ids <- if (count == 5L) {
        c("100", "101", "102", "103", "104")
    } else if (count == 2L) {
        c("200", "201")
    } else if (count == 1L) {
        c("300")
    } else {
        character()
    }

    if (retmax == 0L) {
        ids <- character()
    } else {
        ids <- utils::head(ids, retmax)
    }

    list(
        count = count,
        ids = ids,
        error = NA_character_
    )
}
