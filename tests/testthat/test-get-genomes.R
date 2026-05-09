test_that("get_genomes returns genomes for a high-rank taxon", {
    result <- get_genomes(
        "Ascomycota",
        phase4_genome_metadata(),
        taxon_index = phase4_taxon_index()
    )

    expect_s3_class(result, "tbl_df")
    expect_true(all(c("input_taxon", "accepted_name", "matched_rank", "genome_label", "accession") %in% names(result)))
    expect_equal(nrow(result), 3L)
})

test_that("get_genomes works through synonym taxa", {
    result <- get_genomes(
        "Sac fungi",
        phase4_genome_metadata(),
        taxon_index = phase4_taxon_index()
    )

    expect_s3_class(result, "tbl_df")
    expect_equal(unique(result$accepted_name), "Ascomycota")
    expect_equal(nrow(result), 3L)
})

test_that("get_genomes rejects invalid inputs", {
    expect_error(
        get_genomes(123, phase4_genome_metadata(), taxon_index = phase4_taxon_index()),
        "`taxon` must be a character vector"
    )

    expect_error(
        get_genomes("Ascomycota", list(), taxon_index = phase4_taxon_index()),
        "`genome_metadata` must be a data frame or tibble"
    )
})

test_that("get_genomes returns an empty tibble for unmatched taxa", {
    result <- get_genomes(
        "Unknown taxon",
        phase4_genome_metadata(),
        taxon_index = phase4_taxon_index()
    )

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0L)
    expect_true(all(c("input_taxon", "matched_name", "accepted_name", "matched_rank", "match_type") %in% names(result)))
})

test_that("get_genomes returns a tibble with lineage columns when requested", {
    result <- get_genomes(
        "Fusarium",
        phase4_genome_metadata(),
        taxon_index = phase4_taxon_index(),
        include_lineage = TRUE
    )

    expect_s3_class(result, "tbl_df")
    expect_true("outline_phylum" %in% names(result))
    expect_true(all(result$genus == "Fusarium"))
})
