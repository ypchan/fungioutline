#' Write outline change history
#'
#' Writes a fungal outline change-history table to TSV, CSV, or RDS. Parent
#' directories are created automatically. When `append = TRUE`, existing history
#' is read, combined with the new rows, deduplicated, and written back.
#'
#' @param history A data frame containing history rows.
#' @param path Output path ending in `.tsv`, `.csv`, or `.rds`.
#' @param append Logical. If `TRUE`, append to an existing history file.
#'
#' @return The written history tibble, invisibly.
#' @export
#'
#' @examples
#' history <- fungioutline::compare_outline_versions(NULL, tibble::tibble(kingdom = "Fungi"))
#' fungioutline::write_outline_history(history, tempfile(fileext = ".tsv"), append = FALSE)
write_outline_history <- function(history, path, append = TRUE) {
    check_logical_scalar(append, "append")

    if (missing(path) || !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
        rlang::abort("`path` must be a single non-empty file path.")
    }

    extension <- stringr::str_to_lower(fs::path_ext(path))

    if (!extension %in% c("tsv", "csv", "rds")) {
        rlang::abort("Unsupported history file extension. Use .tsv, .csv, or .rds.")
    }

    history <- fo_add_missing_history_columns(history)

    if (isTRUE(append) && fs::file_exists(path)) {
        existing_history <- read_outline_history(path)
        history <- dplyr::bind_rows(existing_history, history) |>
            dplyr::distinct()
    }

    fo_write_history_by_extension(history, path)
    invisible(history)
}
