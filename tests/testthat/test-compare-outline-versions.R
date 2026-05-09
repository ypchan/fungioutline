test_that("compare_outline_versions returns canonical columns", {
    history <- compare_outline_versions(
        phase3_outline(),
        phase3_outline_added(),
        version_id = "outline_test"
    )

    expect_s3_class(history, "tbl_df")
    expect_true(all(phase3_history_columns() %in% names(history)))
    expect_identical(names(history), phase3_history_columns())
})

test_that("compare_outline_versions detects added taxa", {
    history <- compare_outline_versions(
        phase3_outline(),
        phase3_outline_added(),
        version_id = "outline_test"
    )

    expect_true(any(history$change_type == "added_taxon"))
    expect_true(any(history$taxon_name == "Agaricus"))
})

test_that("compare_outline_versions detects removed taxa", {
    history <- compare_outline_versions(
        phase3_outline_added(),
        phase3_outline(),
        version_id = "outline_test"
    )

    expect_true(any(history$change_type == "removed_taxon"))
    expect_true(any(history$taxon_name == "Agaricus"))
})

test_that("compare_outline_versions detects changed lineage", {
    history <- compare_outline_versions(
        phase3_outline(),
        phase3_outline_lineage_changed(),
        version_id = "outline_test"
    )

    expect_true(any(history$change_type == "changed_lineage"))
    expect_true(any(history$taxon_name == "Fusarium" & history$field == "family"))
})

test_that("compare_outline_versions detects changed synonyms", {
    history <- compare_outline_versions(
        phase3_outline(),
        phase3_outline_synonym_changed(),
        version_id = "outline_test"
    )

    expect_true(any(history$change_type == "changed_synonym"))
    expect_true(any(history$field == "genus_syn"))
})

test_that("compare_outline_versions detects changed metadata", {
    history <- compare_outline_versions(
        phase3_outline(),
        phase3_outline_metadata_changed(),
        version_id = "outline_test"
    )

    expect_true(any(history$change_type == "changed_metadata"))
    expect_true(any(history$field == "update_note"))
})

test_that("compare_outline_versions returns empty history when there are no changes", {
    history <- compare_outline_versions(
        phase3_outline(),
        phase3_outline(),
        version_id = "outline_test"
    )

    expect_s3_class(history, "tbl_df")
    expect_equal(nrow(history), 0L)
    expect_identical(names(history), phase3_history_columns())
})

test_that("compare_outline_versions handles NULL old outline as all added", {
    history <- compare_outline_versions(
        NULL,
        phase3_outline(),
        version_id = "outline_test"
    )

    expect_true(all(history$change_type == "added_taxon"))
    expect_true("Fusarium" %in% history$taxon_name)
})

test_that("compare_outline_versions handles NULL new outline as all removed", {
    history <- compare_outline_versions(
        phase3_outline(),
        NULL,
        version_id = "outline_test"
    )

    expect_true(all(history$change_type == "removed_taxon"))
    expect_true("Fusarium" %in% history$taxon_name)
})
