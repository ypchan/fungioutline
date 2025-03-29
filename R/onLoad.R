# R/onLoad.R
.onLoad <- function(libname, pkgname) {
  library(tidyverse)
  # Load outline
  # Get outline.xlsx from the fungioutline package
  xlsx_file <- list.files(system.file("data", package = "fungioutline"),
                            pattern = "^outline.*xlsx")
  red_bold <- "\033[1;31m"  
  reset <- "\033[0m"       
  cat("\n", red_bold, "Updated fungioutline: March 15, 2025", reset, "\n")
  outline <- readxl::read_excel(system.file("data", package = "fungioutline", xlsx_file), 
                                col_types = rep("text", 21)) %>%
      select(Kingdom, Subkingdoms, Phyla, Subphyla, Classes, Subclasses, Orders, Families, Genera) 
  assign("outline", outline, envir = .GlobalEnv)
}
#detach()

#devtools::install_github("ypchan/fungioutline")
#library(fungioutline)
#outline
