# R/onLoad.R
.onLoad <- function(libname, pkgname) {
  # Load outline
  # Get outline.xlsx from the fungioutline package
  xlsx_file <- file.path(system.file("data", package = "fungioutline"), "outline_2025.2.20.xlsx")
  outline <<- readxl::read_excel(xlsx_file)
}
