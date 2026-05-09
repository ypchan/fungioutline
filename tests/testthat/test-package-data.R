test_that("packaged outline data is available as fast R data", {
    data("fungi_outline", package = "fungioutline")

    expect_s3_class(fungi_outline, "tbl_df")
    expect_true(all(outline_required_columns() %in% names(fungi_outline)))
    expect_true(all(c("species", "species_syn") %in% names(fungi_outline)))
    expect_gt(nrow(fungi_outline), 0L)
})

test_that("packaged taxon index is available and usable", {
    data("fungi_taxon_index", package = "fungioutline")

    expect_s3_class(fungi_taxon_index, "tbl_df")
    expect_true(all(fo_index_columns() %in% names(fungi_taxon_index)))
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
