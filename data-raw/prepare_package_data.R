# Prepare packaged data for fungioutline.
#
# Run this script manually from the package root after editing the raw Excel
# files. It creates fast R-native data objects in data/ and RDS mirrors in
# inst/extdata/. Package installation should include these processed files, so
# users do not need to read Excel workbooks every time they load the package.

if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package `readxl` is required to prepare package data.", call. = FALSE)
}
if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package `tibble` is required to prepare package data.", call. = FALSE)
}

source_package_r_files <- function(root) {
    r_files <- list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)
    invisible(lapply(r_files, source))
}

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(root, "DESCRIPTION"))) {
    stop("Run this script from the fungioutline package root.", call. = FALSE)
}

source_package_r_files(root)

outline_xlsx <- file.path(root, "data-raw", "outline_2025.10.20.xlsx")
genome_xlsx <- file.path(root, "data-raw", "fgtdb_genome_metadata.xlsx")

fungi_outline <- read_fungi_outline(
    outline_xlsx,
    standardize = TRUE,
    validate = TRUE
)

fungi_taxon_index <- build_taxon_index(fungi_outline)

fgtdb_genome_metadata <- read_genome_metadata(
    genome_xlsx,
    standardize = TRUE,
    validate = FALSE
)

dir.create(file.path(root, "data"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(root, "inst", "extdata"), showWarnings = FALSE, recursive = TRUE)

save(fungi_outline, file = file.path(root, "data", "fungi_outline.rda"), compress = "xz")
save(fungi_taxon_index, file = file.path(root, "data", "fungi_taxon_index.rda"), compress = "xz")
save(fgtdb_genome_metadata, file = file.path(root, "data", "fgtdb_genome_metadata.rda"), compress = "xz")

saveRDS(fungi_outline, file.path(root, "inst", "extdata", "fungi_outline.rds"), compress = "xz")
saveRDS(fungi_taxon_index, file.path(root, "inst", "extdata", "fungi_taxon_index.rds"), compress = "xz")
saveRDS(fgtdb_genome_metadata, file.path(root, "inst", "extdata", "fgtdb_genome_metadata.rds"), compress = "xz")

message("Prepared fungi_outline rows: ", nrow(fungi_outline))
message("Prepared fungi_taxon_index rows: ", nrow(fungi_taxon_index))
message("Prepared fgtdb_genome_metadata rows: ", nrow(fgtdb_genome_metadata))
