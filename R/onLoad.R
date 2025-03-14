# R/onLoad.R
.onLoad <- function(libname, pkgname) {
  library(tidyverse)
  # Load outline
  # Get outline.xlsx from the fungioutline package
  xlsx_file <- list.files(system.file("data/outline_2025.2.20.xlsx", package = "fungioutline"), pattern = "\\.xlsx$", full.names = TRUE)
  outline <<- readxl::read_excel(latest_file)
}