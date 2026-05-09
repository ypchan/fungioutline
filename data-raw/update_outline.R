# Update curated fungal outline data
#
# 1. Edit data-raw/outline_2025.10.20.xlsx manually.
# 2. Run this script interactively.
# 3. Review the generated history table.
# 4. Run devtools::document() and devtools::test() manually.

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
