test_that("plot_genome_coverage returns ggplot", {
    phase6_skip_if_no_ggplot2()

    plot <- plot_genome_coverage(
        "Ascomycota",
        genome_metadata = phase4_genome_metadata(),
        target_rank = "genus",
        taxon_index = phase4_taxon_index()
    )

    phase6_expect_ggplot(plot)
})

test_that("plot_genome_coverage works through synonym ancestors", {
    phase6_skip_if_no_ggplot2()

    plot <- plot_genome_coverage(
        "Sac fungi",
        genome_metadata = phase4_genome_metadata(),
        target_rank = "genus",
        taxon_index = phase4_taxon_index()
    )

    phase6_expect_ggplot(plot)
})

test_that("plot_genome_coverage rejects invalid genome metadata", {
    phase6_skip_if_no_ggplot2()

    expect_error(
        plot_genome_coverage("Ascomycota", genome_metadata = 123, taxon_index = phase4_taxon_index()),
        "`genome_metadata` must be a data frame or tibble"
    )
})
