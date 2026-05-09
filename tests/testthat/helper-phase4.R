phase4_outline <- function() {
    tibble::tribble(
        ~kingdom, ~subkingdom, ~subkingdom_syn, ~phylum, ~phylum_syn, ~subphylum, ~subphylum_syn, ~class, ~class_syn, ~subclass, ~subclass_syn, ~order, ~order_syn, ~family, ~family_syn, ~genus, ~genus_syn, ~updated_time, ~update_type, ~update_note, ~update_link,
        "Fungi", "Dikarya", NA_character_, "Ascomycota", "Sac fungi", "Pezizomycotina", NA_character_, "Sordariomycetes", NA_character_, "Hypocreomycetidae", NA_character_, "Hypocreales", NA_character_, "Nectriaceae", NA_character_, "Fusarium", "Gibberella", "2026-01-01", "seed", "Fusarium record", "https://example.org/fusarium",
        "Fungi", "Dikarya", NA_character_, "Ascomycota", NA_character_, "Pezizomycotina", NA_character_, "Eurotiomycetes", NA_character_, "Eurotiomycetidae", NA_character_, "Eurotiales", NA_character_, "Trichocomaceae", NA_character_, "Aspergillus", "Eurotium", "2026-01-02", "seed", "Aspergillus record", "https://example.org/aspergillus",
        "Fungi", "Dikarya", NA_character_, "Ascomycota", NA_character_, "Pezizomycotina", NA_character_, "Sordariomycetes", NA_character_, "Sordariomycetidae", NA_character_, "Sordariales", NA_character_, "Sordariaceae", NA_character_, "Neurospora", NA_character_, "2026-01-03", "seed", "No genome record", "https://example.org/neurospora",
        "Fungi", "Dikarya", NA_character_, "Basidiomycota", NA_character_, "Agaricomycotina", NA_character_, "Agaricomycetes", NA_character_, "Agaricomycetidae", NA_character_, "Agaricales", NA_character_, "Agaricaceae", NA_character_, "Agaricus", "Psalliota", "2026-01-04", "seed", "Agaricus record", "https://example.org/agaricus"
    )
}

phase4_taxon_index <- function() {
    build_taxon_index(phase4_outline())
}

phase4_genome_metadata <- function() {
    tibble::tibble(
        genome_label = c("Fusarium genome", "Aspergillus genome", "Agaricus genome", "Low quality Fusarium"),
        scientific_name = c("Fusarium oxysporum", "Aspergillus niger", "Agaricus bisporus", "Fusarium testlow"),
        species_rep = c(TRUE, TRUE, TRUE, FALSE),
        genus_rep = c(TRUE, TRUE, TRUE, FALSE),
        family_rep = c(TRUE, TRUE, TRUE, FALSE),
        order_rep = c(TRUE, TRUE, TRUE, FALSE),
        class_rep = c(TRUE, TRUE, TRUE, FALSE),
        phylum = c("Ascomycota", "Ascomycota", "Basidiomycota", "Ascomycota"),
        class = c("Sordariomycetes", "Eurotiomycetes", "Agaricomycetes", "Sordariomycetes"),
        order = c("Hypocreales", "Eurotiales", "Agaricales", "Hypocreales"),
        family = c("Nectriaceae", "Trichocomaceae", "Agaricaceae", "Nectriaceae"),
        genus = c("Fusarium", "Aspergillus", "Agaricus", "Fusarium"),
        species = c("Fusarium oxysporum", "Aspergillus niger", "Agaricus bisporus", "Fusarium testlow"),
        accession = c("GCA_000000001.1", "GCA_000000002.1", "GCA_000000003.1", "GCA_000000004.1"),
        long_label = c("Fusarium long", "Aspergillus long", "Agaricus long", "Fusarium low long"),
        n_scaffolds = c(10, 100, 20, 1000),
        n_contigs = c(10, 100, 20, 1000),
        scaf_bp = c(45000000, 35000000, 30000000, 20000000),
        contig_bp = c(45000000, 35000000, 30000000, 20000000),
        gap_pct = c(0.1, 0.5, 0.2, 5),
        scaf_N50 = c(5000000, 1000000, 3000000, 10000),
        scaf_L50 = c(4, 15, 5, 400),
        ctg_N50 = c(5000000, 1000000, 3000000, 10000),
        ctg_L50 = c(4, 15, 5, 400),
        scaf_L90 = c(8, 50, 12, 900),
        scaf_N90 = c(1000000, 200000, 800000, 1000),
        ctg_N90 = c(1000000, 200000, 800000, 1000),
        ctg_L90 = c(8, 50, 12, 900),
        scaf_max = c(10000000, 4000000, 7000000, 50000),
        ctg_max = c(10000000, 4000000, 7000000, 50000),
        scaf_n_gt50K = c(10, 80, 20, 4),
        scaf_pct_gt50K = c(99, 85, 98, 10),
        gc_avg = c(48, 50, 46, 52),
        gc_std = c(1, 2, 1.5, 8),
        C = c(96, 78, 92, 45),
        S = c(94, 76, 90, 40),
        D = c(2, 2, 2, 5),
        F = c(2, 10, 4, 20),
        M = c(2, 12, 4, 35),
        complete_buscos = c(960, 780, 920, 450),
        complete_singlecopy_buscos = c(940, 760, 900, 400),
        complete_duplicated_buscos = c(20, 20, 20, 50),
        fragmented_buscos = c(20, 100, 40, 200),
        missing_buscos = c(20, 120, 40, 350),
        total_buscos = c(1000, 1000, 1000, 1000),
        ok = c(TRUE, TRUE, TRUE, FALSE),
        is_type_strain_likely = c(FALSE, FALSE, TRUE, FALSE),
        organism = c("Fusarium oxysporum strain A", "Aspergillus niger strain B", "Agaricus bisporus strain C", "Fusarium testlow strain D"),
        strain = c("A", "B", "C", "D"),
        biosample_accession = c("SAMN1", "SAMN2", "SAMN3", "SAMN4"),
        error = c(NA_character_, NA_character_, NA_character_, "fragmented"),
        evidence_summary = c("assembly", "assembly", "assembly", "assembly warning"),
        taxid = c("5507", "5061", "5341", "999999"),
        note = c("high", "medium", "high", "low")
    )
}

phase4_validation_columns <- function() {
    c("check", "status", "severity", "column", "n", "message")
}

phase4_required_genome_columns <- function() {
    c(
        "genome_label",
        "scientific_name",
        "accession",
        "phylum",
        "class",
        "order",
        "family",
        "genus",
        "species",
        "C",
        "F",
        "M",
        "complete_buscos",
        "total_buscos",
        "ok"
    )
}

phase4_genome_summary_columns <- function() {
    c(
        "input_taxon",
        "accepted_name",
        "rank",
        "n_genomes",
        "n_accessions",
        "n_species",
        "n_genera",
        "n_ok",
        "n_high_quality",
        "mean_busco_complete",
        "median_busco_complete"
    )
}

phase4_busco_summary_columns <- function() {
    c(
        "n_genomes",
        "mean_C",
        "median_C",
        "mean_S",
        "mean_D",
        "mean_F",
        "mean_M",
        "mean_complete_buscos",
        "mean_total_buscos"
    )
}

phase4_coverage_columns <- function() {
    c(
        "input_taxon",
        "ancestor_name",
        "ancestor_rank",
        "taxon_name",
        "rank",
        "n_genomes",
        "has_genome",
        "kingdom",
        "subkingdom",
        "phylum",
        "subphylum",
        "class",
        "subclass",
        "order",
        "family",
        "genus",
        "species",
        "source_row_id"
    )
}
