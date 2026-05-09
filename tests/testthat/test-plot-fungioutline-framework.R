test_that("plot_fungioutline_framework returns ggplot", {
    phase6_skip_if_no_ggplot2()

    plot <- plot_fungioutline_framework()

    phase6_expect_ggplot(plot)
})

test_that("plot_fungioutline_framework rejects invalid output path", {
    phase6_skip_if_no_ggplot2()

    expect_error(
        plot_fungioutline_framework(output_path = NA_character_),
        "`output_path` must be NULL or a single non-empty file path"
    )
})

test_that("plot_fungioutline_framework can save when output_path is supplied", {
    phase6_skip_if_no_ggplot2()

    output_path <- tempfile(fileext = ".png")
    plot <- plot_fungioutline_framework(
        output_path = output_path,
        width = 4,
        height = 3
    )

    phase6_expect_ggplot(plot)
    expect_true(file.exists(output_path))
})
