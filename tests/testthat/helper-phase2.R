toy_outline_phase2 <- function() {
    tibble::tribble(
        ~kingdom, ~subkingdom, ~subkingdom_syn, ~phylum, ~phylum_syn, ~subphylum, ~subphylum_syn, ~class, ~class_syn, ~subclass, ~subclass_syn, ~order, ~family, ~family_syn, ~genus, ~genus_syn, ~updated_time, ~update_type, ~update_note, ~update_link,
        "Fungi", "Dikarya", NA_character_, "Ascomycota", "Asco; Sac fungi", "Pezizomycotina", NA_character_, "Sordariomycetes", "Pyrenomycetes", "Hypocreomycetidae", NA_character_, "Hypocreales", "Nectriaceae", NA_character_, "Fusarium", "Fusaria; Gibberella； ", "2026-01-01", "seed", "First record", "https://example.org/1",
        "Fungi", "Dikarya", NA_character_, "Ascomycota", NA_character_, "Pezizomycotina", NA_character_, "Eurotiomycetes", NA_character_, "Eurotiomycetidae", NA_character_, "Eurotiales", "Trichocomaceae", NA_character_, "Aspergillus", "Eurotium", "2026-01-02", "seed", "Second record", "https://example.org/2",
        "Fungi", "Dikarya", NA_character_, "Basidiomycota", NA_character_, "Agaricomycotina", NA_character_, "Agaricomycetes", NA_character_, "Agaricomycetidae", NA_character_, "Agaricales", "Agaricaceae", NA_character_, "Agaricus", "Psalliota; Pratella", "2026-01-03", "seed", "Third record", "https://example.org/3",
        "Fungi", "Dikarya", NA_character_, "Basidiomycota", NA_character_, "Agaricomycotina", NA_character_, "Agaricomycetes", NA_character_, "Agaricomycetidae", NA_character_, "Boletales", "Boletaceae", NA_character_, NA_character_, "", "2026-01-04", "seed", "Missing genus record", "https://example.org/4",
        "Fungi", "Dikarya", NA_character_, "Ascomycota", NA_character_, "Pezizomycotina", NA_character_, "Ambigua", NA_character_, "Hypocreomycetidae", NA_character_, "Hypocreales", "Nectriaceae", NA_character_, "Ambigua", "Ambigua syn", "2026-01-05", "seed", "Ambiguous name record", "https://example.org/5"
    )
}

phase2_index_columns <- function() {
    c(
        "taxon_name",
        "taxon_name_norm",
        "accepted_name",
        "accepted_name_norm",
        "rank",
        "is_synonym",
        "synonym_of",
        "kingdom",
        "subkingdom",
        "phylum",
        "subphylum",
        "class",
        "subclass",
        "order",
        "family",
        "genus",
        "source_row_id",
        "updated_time",
        "update_type",
        "update_note",
        "update_link"
    )
}

phase2_lineage_columns <- function() {
    c(
        "input_taxon",
        "matched_name",
        "accepted_name",
        "rank",
        "match_type",
        "is_synonym",
        "is_ambiguous",
        "kingdom",
        "subkingdom",
        "phylum",
        "subphylum",
        "class",
        "subclass",
        "order",
        "family",
        "genus",
        "source_row_id",
        "updated_time",
        "update_type",
        "update_note",
        "update_link"
    )
}

phase2_descendant_columns <- function() {
    c(
        "input_taxon",
        "ancestor_name",
        "ancestor_rank",
        "descendant_name",
        "descendant_rank",
        "kingdom",
        "subkingdom",
        "phylum",
        "subphylum",
        "class",
        "subclass",
        "order",
        "family",
        "genus",
        "source_row_id"
    )
}

phase2_count_columns <- function() {
    c("input_taxon", "ancestor_name", "ancestor_rank", "rank", "n_taxa")
}
