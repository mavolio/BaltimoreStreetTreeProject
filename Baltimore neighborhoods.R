#read in shapefile by bmore city with neighborhood delineations and combine with street tree data
#started 6/12
library(tidyverse)
library(sf)
setwd("C:\\Users\\trini\\Documents\\Street Trees")

#reading in shapefiles
bmore_nb <- st_read("Neighborhood.shp")
street_tree <- st_read("bc_forestry_trees_20190319.shp")

#combining files
combined <- street_tree %>%
  st_join(bmore_nb) %>%
  st_drop_geometry()
