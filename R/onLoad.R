# R/onLoad.R
.onLoad <- function(libname, pkgname) {
  # Load outline
  # Get outline.xlsx from the fungioutline package
  data_path <- list.files(system.file("data", package = "fungioutline"), pattern = "\\.xlsx$", full.names = TRUE)
  xlsx_file <- file.path(data_path, "outline_2025.2.20.xlsx")
  outline <<- readxl::read_excel(xlsx_file)
}
