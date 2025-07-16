# R/onLoad.R
.onLoad <- function(libname, pkgname) {
  library(tidyverse)
  # Load outline
  # Get outline.xlsx from the fungioutline package
  xlsx_file <- list.files(system.file("data", package = "fungioutline"),
                            pattern = "^outline.*xlsx")
  outline <- readxl::read_excel(system.file("data", package = "fungioutline", xlsx_file), 
                                col_types = rep("text", 21)) %>%
      select(Kingdom, Subkingdom, Phylum, Subphylum, Class, Subclass, Order, Family, Genus) 
  assign("outline", outline, envir = .GlobalEnv)
}
#detach()

#devtools::install_github("ypchan/fungioutline")
#library(fungioutline)
#outline
