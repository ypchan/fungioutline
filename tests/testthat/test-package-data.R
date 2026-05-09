test_that("packaged outline data is available as fast R data", {
    data("fungi_outline", package = "fungioutline")

    expect_s3_class(fungi_outline, "tbl_df")
    expect_true(all(outline_required_columns() %in% names(fungi_outline)))
    expect_false(any(c("species", "species_syn") %in% names(fungi_outline)))
    expect_gt(nrow(fungi_outline), 0L)
})

test_that("packaged taxon index is available and usable", {
    data("fungi_taxon_index", package = "fungioutline")

    expect_s3_class(fungi_taxon_index, "tbl_df")
    expect_true(all(fo_index_columns() %in% names(fungi_taxon_index)))
    expect_false("species" %in% names(fungi_taxon_index))
    expect_false(any(fungi_taxon_index$rank == "species"))
    expect_gt(nrow(fungi_taxon_index), 0L)
    expect_true(any(!fungi_taxon_index$is_synonym))
})

test_that("packaged genome metadata is available as fast R data", {
    data("fgtdb_genome_metadata", package = "fungioutline")

    expect_s3_class(fgtdb_genome_metadata, "tbl_df")
    expect_true(all(phase4_required_genome_columns() %in% names(fgtdb_genome_metadata)))
    expect_gt(nrow(fgtdb_genome_metadata), 0L)
})

test_that("processed RDS mirrors are installed in extdata", {
    paths <- system.file(
        "extdata",
        c("fungi_outline.rds", "fungi_taxon_index.rds", "fgtdb_genome_metadata.rds"),
        package = "fungioutline"
    )

    expect_true(all(nzchar(paths)))
    expect_true(all(file.exists(paths)))
})

test_that("lineage helpers use packaged taxon index by default", {
    lineage <- get_lineage("Ascomycota", best_match = TRUE, warn_ambiguous = FALSE)
    descendants <- get_descendants("Ascomycota", target_rank = "genus")

    expect_s3_class(lineage, "tbl_df")
    expect_identical(lineage$match_type[[1]], "accepted")
    expect_identical(lineage$accepted_name[[1]], "Ascomycota")

    expect_s3_class(descendants, "tbl_df")
    expect_gt(nrow(descendants), 0L)
    expect_true(all(descendants$descendant_rank == "genus"))
})

test_that("genome helpers use packaged genome metadata by default", {
    genomes <- get_genomes("Ascomycota")
    summary <- summarize_genomes(taxon = "Ascomycota")
    busco <- summarize_busco()

    expect_s3_class(genomes, "tbl_df")
    expect_gt(nrow(genomes), 0L)

    expect_s3_class(summary, "tbl_df")
    expect_gt(summary$n_genomes[[1]], 0L)
    expect_gt(summary$n_high_quality[[1]], 0L)
    expect_false("mean_busco_complete" %in% names(summary))

    expect_s3_class(busco, "tbl_df")
    expect_gt(busco$n_genomes[[1]], 0L)
})
