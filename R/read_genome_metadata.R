#' Read fungal genome metadata
#'
#' Imports public fungal genome metadata from Excel, CSV, TSV, TXT, or RDS and
#' returns a standardized tibble. Column names are converted to package-standard
#' snake case while BUSCO percentage columns `C`, `S`, `D`, `F`, and `M` are
#' preserved in upper case.
#'
#' @param file Path to a `.xlsx`, `.xls`, `.csv`, `.tsv`, `.txt`, or `.rds`
#'   genome metadata file.
#' @param sheet Sheet name or position for Excel files. If `NULL`, the first
#'   sheet is used.
#' @param standardize Logical. If `TRUE`, standardize column names and character
#'   values.
#' @param validate Logical. If `TRUE`, validate the imported metadata with
#'   [validate_genome_metadata()].
#' @param .name_repair Name repair strategy passed to `readxl` or `readr`.
#' @param verbose Logical. If `TRUE`, emit informational messages.
#'
#' @return A tibble containing genome metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' genomes <- fungioutline::read_genome_metadata("fgtdb_genome_metadata.xlsx")
#' }
read_genome_metadata <- function(
    file,
    sheet = NULL,
    standardize = TRUE,
    validate = TRUE,
    .name_repair = "unique",
    verbose = FALSE
) {
    check_logical_scalar(standardize, "standardize")
    check_logical_scalar(validate, "validate")
    check_logical_scalar(verbose, "verbose")

    if (missing(file) || !is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
        rlang::abort("`file` must be a single non-empty file path.")
    }

    file <- path.expand(file)

    if (!file.exists(file)) {
        rlang::abort(glue::glue("Genome metadata file does not exist: {file}"))
    }

    extension <- stringr::str_to_lower(fs::path_ext(file))

    metadata <- switch(
        extension,
        xlsx = {
            if (is.null(sheet)) {
                readxl::read_excel(file, .name_repair = .name_repair)
            } else {
                readxl::read_excel(file, sheet = sheet, .name_repair = .name_repair)
            }
        },
        xls = {
            if (is.null(sheet)) {
                readxl::read_excel(file, .name_repair = .name_repair)
            } else {
                readxl::read_excel(file, sheet = sheet, .name_repair = .name_repair)
            }
        },
        csv = readr::read_csv(file, show_col_types = FALSE, name_repair = .name_repair),
        tsv = readr::read_tsv(file, show_col_types = FALSE, name_repair = .name_repair),
        txt = readr::read_tsv(file, show_col_types = FALSE, name_repair = .name_repair),
        rds = readRDS(file),
        rlang::abort("Unsupported genome metadata file extension. Use .xlsx, .xls, .csv, .tsv, .txt, or .rds.")
    )

    metadata <- tibble::as_tibble(metadata)

    if (isTRUE(standardize)) {
        metadata <- fo_standardize_genome_metadata(metadata)
    }

    if (isTRUE(validate)) {
        validate_genome_metadata(metadata, error = TRUE, verbose = verbose)
    }

    if (isTRUE(verbose)) {
        cli::cli_inform(glue::glue("Read genome metadata with {nrow(metadata)} rows."))
    }

    metadata
}
