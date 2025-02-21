# R/onLoad.R
.onLoad <- function(libname, pkgname) {
  # Load outline
  # Get outline.xlsx from the fungioutline package
  xlsx_files <- list.files(system.file("data_raw", package = "fungioutline"), pattern = "\\.xlsx$", full.names = TRUE)
  
  # sort table by time
  latest_file <- xlsx_files[order(file.info(xlsx_files)$mtime, decreasing = TRUE)][1]
  outline <<- readxl::read_excel(latest_file)
}