#' Read a fungal outline file
#'
#' Imports a fungal taxonomic outline from an Excel workbook, CSV file, or TSV
#' file and returns a tibble. Excel workbooks are the primary user-editable
#' format; delimited files are supported for reproducible exports and tests.
#'
#' @param file Path to an `.xlsx`, `.xls`, `.csv`, `.tsv`, or `.txt` outline
#'   file.
#' @param sheet Sheet name or position for Excel files. If `NULL`, the first
#'   sheet is used.
#' @param standardize Logical. If `TRUE`, standardize column names and character
#'   values with [standardize_outline()].
#' @param validate Logical. If `TRUE`, validate the imported outline with
#'   [validate_fungi_outline()].
#' @param .name_repair Name repair strategy passed to `readxl` or `readr`.
#' @param verbose Logical. If `TRUE`, emit informational messages.
#'
#' @return A tibble containing the imported outline.
#' @export
#'
#' @examples
#' \dontrun{
#' outline <- fungioutline::read_fungi_outline("outline_2025.10.20.xlsx")
#' }
read_fungi_outline <- function(
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
        rlang::abort(glue::glue("Outline file does not exist: {file}"))
    }

    extension <- stringr::str_to_lower(fs::path_ext(file))

    outline <- switch(
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
        rlang::abort(glue::glue(
            "Unsupported outline file extension: .{extension}. Use .xlsx, .xls, .csv, .tsv, or .txt."
        ))
    )

    outline <- tibble::as_tibble(outline)

    if (isTRUE(standardize)) {
        outline <- standardize_outline(outline, verbose = FALSE)
    }

    if (isTRUE(validate)) {
        validate_fungi_outline(outline, error = TRUE, verbose = verbose)
    }

    if (isTRUE(verbose)) {
        cli::cli_inform(glue::glue("Read fungal outline with {nrow(outline)} rows."))
    }

    outline
}
