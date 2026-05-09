test_that("plot_update_history returns ggplot for history rows", {
    phase6_skip_if_no_ggplot2()

    history <- compare_outline_versions(
        old_outline = NULL,
        new_outline = phase3_outline(),
        version_id = "outline_plot"
    )
    plot <- plot_update_history(history)

    phase6_expect_ggplot(plot)
})

test_that("plot_update_history returns ggplot for empty history", {
    phase6_skip_if_no_ggplot2()

    history <- compare_outline_versions(phase3_outline(), phase3_outline())
    plot <- plot_update_history(history)

    phase6_expect_ggplot(plot)
})

test_that("plot_update_history rejects invalid output dimensions", {
    phase6_skip_if_no_ggplot2()

    history <- compare_outline_versions(phase3_outline(), phase3_outline())
    expect_error(
        plot_update_history(history, width = -1),
        "`width` must be a single positive numeric value"
    )
})
