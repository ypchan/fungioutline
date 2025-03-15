# fungioutline -- a tiny R package that integrates up-to-date taxonomic studies

[![Built with R](https://img.shields.io/badge/powered_by-R-6362c2.svg?style=flat)](https://www.r-project.org/)
[![Github Releases](https://img.shields.io/github/downloads/ypchan/fungioutline/latest/total.svg?maxAge=3600)](https://github.com/ypchan/fungioutline/releases)

## Installitation and Update
```
library(devtools)
devtools::install_github(""ypchan/fungioutline) # To update, just redo installitation
```
## Usage
### Load and take a look at the outline
```
library(fungioutline)
head(outline)
```

### Update outline
   
Update data/outline.<update>.xlsx

### Summary

- Count all taxonomic levels
   ```
   outline %>%
     summarise(across(c(Kingdom, Subkingdoms, Phyla, Subphyla,Classes,Subclasses,Orders,Families,Genera), ~length(unique(.))))
   ```
- Count by group names
  ```
  outline %>%
    filter(Classes == "Sordariomycetes") %>%
    summarise(across(c(Subclasses,Orders,Families,Genera), ~length(unique(.))))
  ```
- List by group names
  ```
  outline %>%
    filter(Classes == "Sordariomycetes") %>%
    select(Subclasses, Orders, Familes, Genera)
  ```
- Validate taxonomic names
  ```
  # first check the suffixes, for most taxon
  # family - aceae
  # order - ales
  # class - mycetes
  # subclass - mycetidae
  # subphyla - mycotina
  # phyla - mycota
  ## to validate a family name: Septochytriaceae
  outline %>% filter(Families == "Septochytriaceae")
  ## To validate a genus name: Periconia
  outline %>% filter(Genera == "Periconia")
  ```

- Assign lineage information to a table by taxa name at different levels.
  ```
  # Assign lineage information from family to phyla
  outline %>%
    select(Kingdom, Subkingdoms, Phyla, Subphyla,Classes,Subclasses,Orders,Families,Genera) -> lineage_tbl
  
  tbl_to_assign_by_genera_name %>%
    left_join(lineage_tbl, by = c("Genus" = "Genera")) -> assigned_tbl
  write_xlsx(assigned_tbl, "assigned_tbl.xlsx")
  ```
## Updates
2025-3 Updated Phylum and Classes [doi](https://doi.org/10.1007/s13225-024-00540-z)
  
  
   
