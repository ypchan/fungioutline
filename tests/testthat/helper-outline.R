minimal_outline <- function() {
    tibble::tibble(
        Kingdom = "Fungi",
        Subkingdom = "Dikarya",
        Subkingdom_syn = NA_character_,
        Phylum = "Ascomycota",
        Phylum_syn = NA_character_,
        Subphylum = "Pezizomycotina",
        Subphylum_syn = NA_character_,
        Class = "Sordariomycetes",
        Class_syn = NA_character_,
        Subclass = "Hypocreomycetidae",
        Subclass_syn = NA_character_,
        Order = "Hypocreales",
        Family = "Nectriaceae",
        Family_syn = NA_character_,
        Genus = "Fusarium",
        Genus_syn = NA_character_,
        Updated_time = "2025-10-20",
        Update_type = "manual",
        Update_note = "Seed record",
        Update_link = "https://example.org/update"
    )
}

expected_outline_columns <- function() {
    c(
        "kingdom",
        "subkingdom",
        "subkingdom_syn",
        "phylum",
        "phylum_syn",
        "subphylum",
        "subphylum_syn",
        "class",
        "class_syn",
        "subclass",
        "subclass_syn",
        "order",
        "family",
        "family_syn",
        "genus",
        "genus_syn",
        "updated_time",
        "update_type",
        "update_note",
        "update_link"
    )
}

expected_validation_columns <- function() {
    c("check", "status", "severity", "column", "n", "message")
}
