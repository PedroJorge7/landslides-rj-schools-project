library(geobr)
library(sf)
library(dplyr)
library(stringi)


# ------------------------------------------------------------
# 1) Municípios (polígonos)
# ------------------------------------------------------------
year_ref <- 2020
muni <- geobr::read_municipality(year = year_ref, simplified = FALSE)
muni$area <- st_area(muni)
muni <- st_drop_geometry(muni)
muni$name_muni <- NULL
muni$code_state <- NULL
muni$abbrev_state <- NULL
muni$name_state <- NULL
muni$code_region <- NULL
muni$name_region <- NULL
arrow::write_parquet(muni,'muni_area.parquet')
