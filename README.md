# fungioutline

`fungioutline` is an R package for reproducible fungal systematics,
phylogenomic sampling, genome quality screening, and taxonomy update tracking.
It connects an expert-curated fungal outline with public genome metadata, BUSCO
quality summaries, optional NCBI sequence availability checks, and publication
figures.

![fungioutline framework](man/figures/fungioutline_framework_nature_methods.svg)

The package is designed around a simple principle: the raw Excel files are
curated and archived once, then converted into fast R-native data objects for
analysis. Users should not need to repeatedly parse large Excel workbooks during
ordinary package use.

## What fungioutline Provides

- Curated fungal taxonomy import, validation, standardization, and update
  history.
- A long-format taxon index with accepted names, synonyms, normalized matching
  keys, ranks, and full lineage context.
- Lineage lookup for any supported rank, with exact matching before synonym
  matching and ambiguity-preserving outputs.
- Genome metadata and BUSCO quality tools for clade-level sampling decisions.
- Optional cached NCBI `nuccore` searches for sequence availability.
- ggplot2 visualization functions and a publication-style framework diagram.
- Built-in package data for high-speed use after installation.

## Installation

During local development, install dependencies first:

```r
install.packages(c(
    "devtools", "roxygen2", "cli", "dplyr", "fs", "glue", "lifecycle",
    "purrr", "readr", "readxl", "rlang", "stringr", "tibble", "tidyr",
    "ggplot2", "ggrepel", "patchwork", "scales", "testthat", "rentrez",
    "writexl", "knitr", "rmarkdown", "pkgdown"
))
```

Install the local package:

```r
library(remotes)
remotes::install_github("ypchan/fungioutline")
```

## Built-In Data

The two original Excel workbooks are stored as raw source files:

- `data-raw/outline_2025.10.20.xlsx`
- `data-raw/fgtdb_genome_metadata.xlsx`

They are processed once by `data-raw/prepare_package_data.R` into fast package
data:

- `data/fungi_outline.rda`
- `data/fungi_taxon_index.rda`
- `data/fgtdb_genome_metadata.rda`

The curated outline and taxon index stop at genus. Species labels are retained
only in genome metadata, where they describe genome records rather than outline
taxonomy.

RDS mirrors are also written for direct file-based workflows:

- `inst/extdata/fungi_outline.rds`
- `inst/extdata/fungi_taxon_index.rds`
- `inst/extdata/fgtdb_genome_metadata.rds`

After installation, the built-in data are available by their exported names, and
core helpers use them by default:

```r
library(fungioutline)

fungioutline::fungi_outline
fungioutline::fungi_taxon_index
fungioutline::fgtdb_genome_metadata

fungioutline::get_lineage("Ascomycota")
fungioutline::get_genomes("Ascomycota")
```

This avoids repeated Excel parsing and makes ordinary lineage, genome coverage,
and plotting workflows much faster.

## Quick Start

```r
library(fungioutline)

fungioutline::get_lineage("Ascomycota")

fungioutline::count_taxa(
    "Ascomycota"
)

fungioutline::summarize_genomes(
    taxon = "Ascomycota"
)
```

## Use Case 1: Lineage Lookup for Any Taxon

Resolve accepted names and synonyms while preserving ambiguity.

```r
lineages <- fungioutline::get_lineage(
    c("Ascomycota", "Basidiomycota", "Fusarium", "Gibberella"),
    match_synonym = TRUE,
    best_match = FALSE
)

lineages |>
    dplyr::select(
        "input_taxon",
        "matched_name",
        "accepted_name",
        "rank",
        "match_type",
        "phylum",
        "class",
        "order",
        "family",
        "genus"
    )
```

Use `best_match = TRUE` when each input taxon should produce one best row:

```r
fungioutline::get_lineage(
    c("Agaricus", "Fusarium"),
    best_match = TRUE
)
```

## Use Case 2: Add Lineages to a Sampling Table

`add_lineage()` supports both bare and string column names.

```r
sampling_plan <- tibble::tibble(
    isolate_id = c("iso_001", "iso_002", "iso_003"),
    genus = c("Fusarium", "Agaricus", "Unknown")
)

sampling_plan |>
    fungioutline::add_lineage(taxon_col = genus) |>
    dplyr::select(
        "isolate_id",
        "genus",
        "match_type",
        "phylum",
        "class",
        "order",
        "family"
    )
```

String input is equivalent:

```r
sampling_plan |>
    fungioutline::add_lineage(taxon_col = "genus")
```

## Use Case 3: Count and Retrieve Descendant Taxa

Count richness below a clade:

```r
fungioutline::count_taxa(
    "Ascomycota"
)
```

Retrieve descendants at a target rank:

```r
ascomycete_genera <- fungioutline::get_descendants(
    "Ascomycota",
    target_rank = "genus"
)

ascomycete_genera |>
    dplyr::arrange(family, descendant_name)
```

## Use Case 4: Genome Availability and BUSCO Filtering

Summarize genome availability for one or more clades:

```r
fungioutline::summarize_genomes(
    taxon = c("Ascomycota", "Basidiomycota")
)
```

Classify and filter genomes:

```r
high_quality_genomes <- fungioutline::fgtdb_genome_metadata |>
    fungioutline::classify_genome_quality() |>
    fungioutline::filter_genomes_by_quality(
        min_complete = 90,
        max_missing = 10,
        require_ok = TRUE
    )
```

Check which descendant genera under a clade have public genomes:

```r
coverage <- fungioutline::check_genome_coverage(
    "Ascomycota",
    target_rank = "genus"
)

coverage |>
    dplyr::filter(!has_genome)
```

## Use Case 5: BUSCO Summaries by Rank

```r
fungioutline::summarize_busco(
    by = "phylum"
)

fungioutline::summarize_busco(
    by = c("phylum", "class")
)
```

Visualize BUSCO completeness:

```r
fungioutline::plot_busco_quality()

fungioutline::plot_busco_quality(
    metric = "C",
    group_col = "phylum"
)
```

## Use Case 6: Optional NCBI Sequence Availability

NCBI searches are serial by default, cached, and use
`Sys.getenv("NCBI_API_KEY")` when available.

```r
ncbi_counts <- fungioutline::search_ncbi_sequence_counts(
    c("Agaricus", "Fusarium"),
    db = "nuccore",
    cache_dir = "inst/extdata/ncbi-cache",
    delay_sec = 0.34
)

ncbi_counts |>
    dplyr::select(
        "input_taxon",
        "accepted_name",
        "count",
        "has_sequences",
        "cached"
    )
```

Retrieve IDs when needed:

```r
fungioutline::search_ncbi_sequences(
    "Fusarium",
    db = "nuccore",
    retmax = 20
)
```

## Use Case 7: Phylogenomic Sampling Table

Combine coverage, quality, and optional sequence availability to find sampling
gaps.

```r
coverage_quality <- coverage |>
    dplyr::left_join(
        high_quality_genomes |>
            dplyr::count(genus, name = "n_high_quality_genomes"),
        by = c("taxon_name" = "genus")
    ) |>
    dplyr::mutate(
        n_high_quality_genomes = tidyr::replace_na(n_high_quality_genomes, 0L),
        sampling_priority = dplyr::case_when(
            n_high_quality_genomes > 0L ~ "genome_available",
            has_genome ~ "genome_needs_quality_review",
            TRUE ~ "sampling_gap"
        )
    )

coverage_quality |>
    dplyr::arrange(sampling_priority, taxon_name)
```

## Use Case 8: Publication Figures

Plot functions return ggplot objects and save files only when `output_path` is
supplied.

```r
fungioutline::plot_taxon_richness()

fungioutline::plot_genome_coverage(
    "Ascomycota",
    target_rank = "genus"
)

fungioutline::plot_taxonomic_heatmap(
    "Ascomycota",
    target_rank = "genus"
)

fungioutline::plot_fungioutline_framework(
    output_path = "man/figures/fungioutline_framework.png",
    width = 10,
    height = 6,
    dpi = 600
)
```

A vector framework figure for manuscript preparation is included at:

```text
man/figures/fungioutline_framework_nature_methods.svg
```

## Updating the Curated Outline

When the expert-curated Excel file changes, update the processed data and
history.

```r
outline_update <- fungioutline::update_outline_data(
    excel_path = "data-raw/outline_2025.10.20.xlsx",
    output_dir = "inst/extdata",
    overwrite_current = TRUE,
    write_versioned = TRUE,
    write_history = TRUE,
    verbose = TRUE
)

outline_update$summary
outline_update$history
```

Then regenerate package data:

```r
source("data-raw/prepare_package_data.R")
```

Read accumulated history:

```r
history <- fungioutline::read_outline_history(
    "inst/extdata/outline_history.tsv"
)

fungioutline::plot_update_history(history)
```

## Function Overview

| Module | Main functions |
| --- | --- |
| Outline import | `read_fungi_outline()`, `standardize_outline()`, `validate_fungi_outline()` |
| Taxonomy core | `build_taxon_index()`, `get_lineage()`, `add_lineage()`, `get_descendants()`, `count_taxa()` |
| Update history | `update_outline_data()`, `compare_outline_versions()`, `read_outline_history()`, `write_outline_history()` |
| Genome metadata | `read_genome_metadata()`, `validate_genome_metadata()`, `get_genomes()`, `summarize_genomes()` |
| BUSCO quality | `classify_genome_quality()`, `filter_genomes_by_quality()`, `summarize_busco()` |
| Coverage | `check_genome_coverage()` |
| NCBI | `search_ncbi_sequences()`, `search_ncbi_sequence_counts()`, `cache_ncbi_result()` |
| Visualization | `plot_taxon_richness()`, `plot_genome_coverage()`, `plot_busco_quality()`, `plot_taxonomic_heatmap()`, `plot_update_history()`, `plot_ncbi_sequence_counts()`, `plot_fungioutline_framework()` |

## Vignettes

The package includes five workflow guides:

- Getting started with `fungioutline`
- Updating the outline Excel file
- Genome coverage and BUSCO filtering
- NCBI sequence availability
- Phylogenomic sampling support

Examples that depend on local curated files, internet access, or larger genome
metadata are shown as reproducible templates and are not executed by default.

## Manual Development Commands

Run these commands manually from the package root. Package functions do not call
them automatically.

```r
setwd("D:/BaiduSyncdisk/github/fungioutline")

source("data-raw/prepare_package_data.R")

devtools::document()
devtools::test()
devtools::build_vignettes()
pkgdown::build_site()
devtools::check()
```

## Roadmap

1. Package skeleton, outline import, standardization, validation, and tests.
2. Taxonomic index, lineage lookup, descendants, and taxon counts.
3. Outline update workflow and change history.
4. Genome metadata and BUSCO quality modules.
5. Cached NCBI sequence availability searches.
6. Visualization functions.
7. Vignettes and expanded documentation.
8. Package data, manuscript figure, pkgdown, CI, citation, license, and checks.
