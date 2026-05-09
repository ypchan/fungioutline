test_that("summarize_genomes summarizes all genomes", {
    result <- summarize_genomes(phase4_genome_metadata())

    expect_s3_class(result, "tbl_df")
    expect_true(all(phase4_genome_summary_columns() %in% names(result)))
    expect_equal(result$n_genomes[[1]], 4L)
})

test_that("summarize_genomes summarizes by rank", {
    result <- summarize_genomes(phase4_genome_metadata(), by_rank = "phylum")

    expect_s3_class(result, "tbl_df")
    expect_true("Ascomycota" %in% result$accepted_name)
    expect_true(all(result$rank == "phylum"))
})

test_that("summarize_genomes summarizes a resolved taxon", {
    result <- summarize_genomes(
        phase4_genome_metadata(),
        taxon = "Ascomycota",
        taxon_index = phase4_taxon_index()
    )

    expect_equal(result$input_taxon[[1]], "Ascomycota")
    expect_equal(result$n_genomes[[1]], 3L)
    expect_gt(result$n_high_quality[[1]], 0L)
    expect_false("mean_busco_complete" %in% names(result))
    expect_false("median_busco_complete" %in% names(result))
})

test_that("summarize_genomes rejects invalid inputs", {
    expect_error(
        summarize_genomes(list()),
        "`genome_metadata` must be a data frame or tibble"
    )

    expect_error(
        summarize_genomes(phase4_genome_metadata(), by_rank = "subclass"),
        "genome metadata ranks"
    )
})

test_that("summarize_genomes returns zero for unmatched taxa", {
    result <- summarize_genomes(
        phase4_genome_metadata(),
        taxon = "Unknown taxon",
        taxon_index = phase4_taxon_index()
    )

    expect_s3_class(result, "tbl_df")
    expect_equal(result$n_genomes[[1]], 0L)
    expect_true(all(phase4_genome_summary_columns() %in% names(result)))
})
