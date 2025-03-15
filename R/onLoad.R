# R/onLoad.R
.onLoad <- function(libname, pkgname) {
  library(tidyverse)
  # Load outline
  # Get outline.xlsx from the fungioutline package
  xlsx_file <- list.files(system.file("data", package = "fungioutline"),
                            pattern = "^outline.*xlsx")
  outline <<- readxl::read_excel(system.file("data", package = "fungioutline", xlsx_file)) %>%
      select(Kingdom, Subkingdoms, Phyla, Subphyla, Classes, Subclasses, Orders, Families, Genera)
  red_bold <- "\033[1;31m"  
  reset <- "\033[0m"       
  cat(red_bold, "Last update: March 15, 2025", reset, "\n")
}
