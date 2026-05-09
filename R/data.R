#' Standardized curated fungal outline
#'
#' A standardized, R-native version of the expert-curated fungal outline Excel
#' table. The object is built from `data-raw/outline_2025.10.20.xlsx` by
#' `data-raw/prepare_package_data.R`.
#'
#' @format A tibble with one row per curated outline record and standardized
#'   taxonomy, synonym, and update metadata columns.
#' @source `data-raw/outline_2025.10.20.xlsx`
#' @examples
#' data(fungi_outline)
#' head(fungi_outline)
"fungi_outline"

#' Prebuilt fungal taxon index
#'
#' A long-format index generated from [fungi_outline]. Accepted taxon names and
#' synonyms are indexed for fast lineage lookup, descendant retrieval, and
#' clade-level summaries without rebuilding the index from Excel.
#'
#' @format A tibble with taxon names, normalized matching keys, accepted names,
#'   ranks, synonym status, lineage columns, source row IDs, and update
#'   metadata.
#' @source Generated from [fungi_outline] by `data-raw/prepare_package_data.R`.
#' @examples
#' data(fungi_taxon_index)
#' head(fungi_taxon_index)
"fungi_taxon_index"

#' Standardized FGTDB genome metadata
#'
#' A standardized, R-native version of the public fungal genome metadata table.
#' It is built from `data-raw/fgtdb_genome_metadata.xlsx` and saved as package
#' data so workflows do not need to repeatedly parse the raw Excel workbook.
#'
#' @format A tibble with one row per genome record and taxonomy, assembly,
#'   BUSCO, quality, accession, strain, and evidence metadata columns.
#' @source `data-raw/fgtdb_genome_metadata.xlsx`
#' @examples
#' data(fgtdb_genome_metadata)
#' head(fgtdb_genome_metadata)
"fgtdb_genome_metadata"
