test_that("plot_ncbi_sequence_counts returns ggplot", {
    phase6_skip_if_no_ggplot2()

    plot <- plot_ncbi_sequence_counts(phase6_ncbi_counts())

    phase6_expect_ggplot(plot)
})

test_that("plot_ncbi_sequence_counts supports zero counts", {
    phase6_skip_if_no_ggplot2()

    counts <- phase6_ncbi_counts() |>
        dplyr::mutate(count = 0L, has_sequences = FALSE)
    plot <- plot_ncbi_sequence_counts(counts)

    phase6_expect_ggplot(plot)
})

test_that("plot_ncbi_sequence_counts rejects missing required columns", {
    phase6_skip_if_no_ggplot2()

    expect_error(
        plot_ncbi_sequence_counts(tibble::tibble(input_taxon = "Fusarium")),
        "missing required columns"
    )
})
