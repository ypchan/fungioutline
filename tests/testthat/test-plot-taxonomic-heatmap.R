test_that("plot_taxonomic_heatmap returns ggplot", {
    phase6_skip_if_no_ggplot2()

    plot <- plot_taxonomic_heatmap(
        "Ascomycota",
        genome_metadata = phase4_genome_metadata(),
        target_rank = "genus",
        taxon_index = phase4_taxon_index()
    )

    phase6_expect_ggplot(plot)
})

test_that("plot_taxonomic_heatmap supports vector taxon input", {
    phase6_skip_if_no_ggplot2()

    plot <- plot_taxonomic_heatmap(
        c("Ascomycota", "Basidiomycota"),
        genome_metadata = phase4_genome_metadata(),
        target_rank = "genus",
        taxon_index = phase4_taxon_index()
    )

    phase6_expect_ggplot(plot)
})

test_that("plot_taxonomic_heatmap rejects invalid target rank", {
    phase6_skip_if_no_ggplot2()

    expect_error(
        plot_taxonomic_heatmap(
            "Ascomycota",
            genome_metadata = phase4_genome_metadata(),
            target_rank = "subclass",
            taxon_index = phase4_taxon_index()
        ),
        "genome metadata ranks"
    )
})
