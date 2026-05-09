#' Read outline change history
#'
#' Reads a fungal outline change-history table from TSV, CSV, or RDS. Missing
#' files return an empty history tibble with the canonical columns.
#'
#' @param path Path to a `.tsv`, `.csv`, or `.rds` history file.
#'
#' @return A tibble with canonical history columns, plus any extra columns after
#'   the canonical columns.
#' @export
#'
#' @examples
#' fungioutline::read_outline_history("inst/extdata/outline_history.tsv")
read_outline_history <- function(path) {
    if (missing(path) || !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
        rlang::abort("`path` must be a single non-empty file path.")
    }

    extension <- stringr::str_to_lower(fs::path_ext(path))

    if (!extension %in% c("tsv", "csv", "rds")) {
        rlang::abort("Unsupported history file extension. Use .tsv, .csv, or .rds.")
    }

    if (!fs::file_exists(path)) {
        return(fo_empty_history())
    }

    history <- fo_read_history_by_extension(path)
    fo_add_missing_history_columns(history)
}
