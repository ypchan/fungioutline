phase3_history_columns <- function() {
    c(
        "version_id",
        "change_time",
        "change_type",
        "rank",
        "taxon_name",
        "field",
        "old_value",
        "new_value",
        "update_type",
        "update_note",
        "update_link",
        "source_row_id_old",
        "source_row_id_new"
    )
}

phase3_summary_columns <- function() {
    c(
        "version_id",
        "n_outline_rows",
        "n_index_rows",
        "n_changes",
        "n_added_taxa",
        "n_removed_taxa",
        "n_changed_lineage",
        "n_changed_synonym",
        "n_changed_metadata",
        "output_dir",
        "history_path"
    )
}

phase3_outline <- function() {
    tibble::tibble(
        kingdom = "Fungi",
        subkingdom = "Dikarya",
        subkingdom_syn = NA_character_,
        phylum = "Ascomycota",
        phylum_syn = NA_character_,
        subphylum = "Pezizomycotina",
        subphylum_syn = NA_character_,
        class = "Sordariomycetes",
        class_syn = NA_character_,
        subclass = "Hypocreomycetidae",
        subclass_syn = NA_character_,
        order = "Hypocreales",
        family = "Nectriaceae",
        family_syn = NA_character_,
        genus = "Fusarium",
        genus_syn = "Gibberella",
        updated_time = "2026-01-01",
        update_type = "seed",
        update_note = "Original record",
        update_link = "https://example.org/original"
    )
}

phase3_outline_added <- function() {
    dplyr::bind_rows(
        phase3_outline(),
        tibble::tibble(
            kingdom = "Fungi",
            subkingdom = "Dikarya",
            subkingdom_syn = NA_character_,
            phylum = "Basidiomycota",
            phylum_syn = NA_character_,
            subphylum = "Agaricomycotina",
            subphylum_syn = NA_character_,
            class = "Agaricomycetes",
            class_syn = NA_character_,
            subclass = "Agaricomycetidae",
            subclass_syn = NA_character_,
            order = "Agaricales",
            family = "Agaricaceae",
            family_syn = NA_character_,
            genus = "Agaricus",
            genus_syn = "Psalliota",
            updated_time = "2026-01-02",
            update_type = "add",
            update_note = "Added Agaricus",
            update_link = "https://example.org/added"
        )
    )
}

phase3_outline_lineage_changed <- function() {
    phase3_outline() |>
        dplyr::mutate(
            family = "Clavicipitaceae",
            updated_time = "2026-01-03",
            update_type = "move",
            update_note = "Moved Fusarium family",
            update_link = "https://example.org/move"
        )
}

phase3_outline_synonym_changed <- function() {
    phase3_outline() |>
        dplyr::mutate(
            genus_syn = "Gibberella; Fusaria",
            updated_time = "2026-01-04",
            update_type = "synonym",
            update_note = "Added synonym",
            update_link = "https://example.org/synonym"
        )
}

phase3_outline_metadata_changed <- function() {
    phase3_outline() |>
        dplyr::mutate(
            updated_time = "2026-01-05",
            update_type = "metadata",
            update_note = "Updated note",
            update_link = "https://example.org/metadata"
        )
}

phase3_write_excel <- function(outline, path) {
    writexl::write_xlsx(outline, path)
    path
}
