# R/onLoad.R
.onLoad <- function(libname, pkgname) {
  library(tidyverse)
  # Load outline
  # Get outline.xlsx from the fungioutline package
  xlsx_file <- list.files(system.file("data", package = "fungioutline"),
                            pattern = "^outline.*xlsx")
  outline <<- readxl::read_excel(xlsx_file)
  
  red_bold <- "\033[1;31m"  
  reset <- "\033[0m"       
  cat(red_bold, "Last update: March 15, 2025", reset, "\n")
}
