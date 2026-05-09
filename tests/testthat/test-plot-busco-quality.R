test_that("plot_busco_quality returns ggplot for histogram", {
    phase6_skip_if_no_ggplot2()

    plot <- plot_busco_quality(phase4_genome_metadata())

    phase6_expect_ggplot(plot)
})

test_that("plot_busco_quality returns ggplot for grouped boxplot", {
    phase6_skip_if_no_ggplot2()

    plot <- plot_busco_quality(
        phase4_genome_metadata(),
        metric = "C",
        group_col = "phylum"
    )

    phase6_expect_ggplot(plot)
})

test_that("plot_busco_quality rejects invalid metric", {
    phase6_skip_if_no_ggplot2()

    expect_error(
        plot_busco_quality(phase4_genome_metadata(), metric = "not_a_metric"),
        "Column `not_a_metric` was not found"
    )
})

test_that("plot_busco_quality rejects invalid bins", {
    phase6_skip_if_no_ggplot2()

    expect_error(
        plot_busco_quality(phase4_genome_metadata(), bins = 0),
        "`bins` must be a single positive numeric value"
    )
})
