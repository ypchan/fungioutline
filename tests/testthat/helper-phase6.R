phase6_skip_if_no_ggplot2 <- function() {
    testthat::skip_if_not_installed("ggplot2")
}

phase6_expect_ggplot <- function(plot) {
    testthat::expect_s3_class(plot, "ggplot")
}

phase6_ncbi_counts <- function() {
    tibble::tibble(
        input_taxon = c("Fusarium", "Unknown taxon"),
        accepted_name = c("Fusarium", "Unknown taxon"),
        rank = c("genus", NA_character_),
        match_type = c("accepted", "raw"),
        db = "nuccore",
        query = c("\"Fusarium\"[Organism]", "\"Unknown taxon\"[Organism]"),
        count = c(12L, 0L),
        has_sequences = c(TRUE, FALSE),
        cached = c(FALSE, FALSE),
        query_time = c(NA_character_, NA_character_),
        error = c(NA_character_, NA_character_)
    )
}
