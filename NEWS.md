# fungioutline 0.0.0.9000

## Package Infrastructure

- Added fast built-in package data objects for the curated outline, prebuilt
  taxon index, and FGTDB genome metadata.
- Moved raw Excel source workbooks to `data-raw/` and added a reproducible data
  preparation script.
- Added a publication-style framework SVG suitable for manuscript preparation.
- Added pkgdown site configuration for reference grouping and workflow
  articles.
- Added GitHub Actions R-CMD-check workflow.
- Added citation metadata in Citation File Format.
- Added GPL-3-or-later repository license file.

## Documentation

- Added five vignettes covering getting started, outline updates, genome
  coverage and BUSCO filtering, NCBI sequence availability, and phylogenomic
  sampling support.
- Expanded README with installation, function overview, visualization examples,
  development commands, and roadmap.

## Visualization

- Added ggplot2 plot helpers for taxon richness, genome coverage, BUSCO
  quality, taxonomic heatmaps, update history, NCBI sequence counts, and the
  package framework diagram.

## NCBI Sequence Availability

- Added cached serial NCBI sequence search helpers, count-only searches, and
  test support for mocked NCBI responses.

## Genome Metadata and BUSCO

- Added genome metadata import and validation, genome quality classification,
  BUSCO summaries, clade-level genome retrieval, and coverage checks.

## Outline Updates

- Added update workflow for manually curated Excel outlines, versioned RDS
  outputs, and reproducible long-format change history.

## Core Taxonomy

- Added taxon index construction, exact and synonym matching, lineage lookup,
  lineage annotation, descendant retrieval, and taxon counting.

## Outline Import

- Added package skeleton, DESCRIPTION, defensive `.onLoad()`, outline import,
  standardization, validation, README, and testthat edition 3 tests.
