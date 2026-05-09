#' Update curated fungal outline data
#'
#' Runs the Phase 3 outline update workflow for a manually edited Excel file:
#' read, validate, standardize, compare with the previous outline, build a taxon
#' index, and write current/versioned RDS files and optional change history.
#'
#' @param excel_path Path to the manually curated Excel outline file.
#' @param previous_outline Optional previous outline data frame. If `NULL`,
#'   `outline_current.rds` is read from `output_dir` when present.
#' @param output_dir Directory for generated RDS files and default history.
#' @param version_id Optional version ID. If `NULL`, one is generated.
#' @param sheet Excel sheet name or position passed to [read_fungi_outline()].
#' @param history_path Optional history path. Defaults to
#'   `file.path(output_dir, "outline_history.tsv")`.
#' @param overwrite_current Logical. If `TRUE`, write current outline and index
#'   RDS files.
#' @param write_versioned Logical. If `TRUE`, write versioned outline and index
#'   RDS files.
#' @param write_history Logical. If `TRUE`, append comparison history.
#' @param verbose Logical. If `TRUE`, emit cli progress messages.
#'
#' @return A named list with `version_id`, `outline`, `taxon_index`, `history`,
#'   `paths`, and `summary`.
#' @export
#'
#' @examples
#' \dontrun{
#' fungioutline::update_outline_data(
#'     excel_path = "data-raw/outline_2025.10.20.xlsx",
#'     output_dir = "inst/extdata"
#' )
#' }
update_outline_data <- function(
    excel_path,
    previous_outline = NULL,
    output_dir = "inst/extdata",
    version_id = NULL,
    sheet = 1,
    history_path = NULL,
    overwrite_current = TRUE,
    write_versioned = TRUE,
    write_history = TRUE,
    verbose = FALSE
) {
    check_logical_scalar(overwrite_current, "overwrite_current")
    check_logical_scalar(write_versioned, "write_versioned")
    check_logical_scalar(write_history, "write_history")
    check_logical_scalar(verbose, "verbose")

    if (is.null(version_id)) {
        version_id <- fo_make_version_id()
    }

    if (!is.character(version_id) || length(version_id) != 1L || is.na(version_id) || !nzchar(version_id)) {
        rlang::abort("`version_id` must be a single non-empty character string.")
    }

    if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir) || !nzchar(output_dir)) {
        rlang::abort("`output_dir` must be a single non-empty directory path.")
    }

    if (is.null(history_path)) {
        history_path <- fs::path(output_dir, "outline_history.tsv")
    }

    if (isTRUE(verbose)) {
        cli::cli_inform("Reading curated outline.")
    }

    outline <- read_fungi_outline(
        file = excel_path,
        sheet = sheet,
        standardize = TRUE,
        validate = TRUE,
        verbose = verbose
    )

    current_outline_path <- fs::path(output_dir, "outline_current.rds")

    if (is.null(previous_outline)) {
        previous_outline <- fo_safe_read_rds(current_outline_path)
    }

    if (!is.null(previous_outline)) {
        previous_outline <- standardize_outline(previous_outline, verbose = FALSE)
    }

    if (isTRUE(verbose)) {
        cli::cli_inform("Comparing outline versions.")
    }

    history <- compare_outline_versions(
        old_outline = previous_outline,
        new_outline = outline,
        version_id = version_id
    )

    if (isTRUE(verbose)) {
        cli::cli_inform("Building taxon index.")
    }

    taxon_index <- build_taxon_index(outline, include_synonyms = TRUE)
    fs::dir_create(output_dir)

    paths <- list(
        output_dir = output_dir,
        outline_current = current_outline_path,
        outline_index_current = fs::path(output_dir, "outline_index.rds"),
        outline_versioned = fs::path(output_dir, paste0("outline_", version_id, ".rds")),
        outline_index_versioned = fs::path(output_dir, paste0("outline_index_", version_id, ".rds")),
        history = history_path
    )

    if (isTRUE(write_versioned)) {
        fo_safe_write_rds(outline, paths$outline_versioned)
        fo_safe_write_rds(taxon_index, paths$outline_index_versioned)
    }

    if (isTRUE(overwrite_current)) {
        fo_safe_write_rds(outline, paths$outline_current)
        fo_safe_write_rds(taxon_index, paths$outline_index_current)
    }

    if (isTRUE(write_history)) {
        write_outline_history(history, history_path, append = TRUE)
    }

    summary <- tibble::tibble(
        version_id = version_id,
        n_outline_rows = nrow(outline),
        n_index_rows = nrow(taxon_index),
        n_changes = nrow(history),
        n_added_taxa = sum(history$change_type == "added_taxon", na.rm = TRUE),
        n_removed_taxa = sum(history$change_type == "removed_taxon", na.rm = TRUE),
        n_changed_lineage = sum(history$change_type == "changed_lineage", na.rm = TRUE),
        n_changed_synonym = sum(history$change_type == "changed_synonym", na.rm = TRUE),
        n_changed_metadata = sum(history$change_type == "changed_metadata", na.rm = TRUE),
        output_dir = output_dir,
        history_path = history_path
    )

    if (isTRUE(verbose)) {
        cli::cli_inform(glue::glue(
            "Outline update {version_id} completed with {nrow(history)} history rows."
        ))
    }

    list(
        version_id = version_id,
        outline = outline,
        taxon_index = taxon_index,
        history = history,
        paths = paths,
        summary = summary
    )
}
