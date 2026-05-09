test_that("plot_taxon_richness returns ggplot for full index", {
    phase6_skip_if_no_ggplot2()

    plot <- plot_taxon_richness(taxon_index = phase4_taxon_index())

    phase6_expect_ggplot(plot)
})

test_that("plot_taxon_richness returns ggplot for focal taxon counts", {
    phase6_skip_if_no_ggplot2()

    plot <- plot_taxon_richness(
        taxon = "Ascomycota",
        by_rank = "genus",
        taxon_index = phase4_taxon_index()
    )

    phase6_expect_ggplot(plot)
})

test_that("plot_taxon_richness rejects invalid taxon input", {
    phase6_skip_if_no_ggplot2()

    expect_error(
        plot_taxon_richness(taxon = 123, taxon_index = phase4_taxon_index()),
        "`taxon` must be NULL or a character vector"
    )
})

test_that("plot_taxon_richness can save when output_path is supplied", {
    phase6_skip_if_no_ggplot2()

    output_path <- tempfile(fileext = ".png")
    plot <- plot_taxon_richness(
        taxon_index = phase4_taxon_index(),
        output_path = output_path,
        width = 4,
        height = 3
    )

    phase6_expect_ggplot(plot)
    expect_true(file.exists(output_path))
})
