#read in shapefile by bmore city with neighborhood delineations and combine with street tree data
#started 6/12
library(tidyverse)
library(sf)
library(mapview)
setwd("C:\\Users\\trini\\Documents\\Street Trees")

#reading in shapefiles
street_tree <- st_read("bc_forestry_trees_20190319.shp")
# bmore_nb <- st_read("Neighborhood.shp") |> st_transform(crs = st_crs(street_tree))
bmore_nb <- st_read("Neighborhood.geojson") |> st_transform(crs = st_crs(street_tree))


#combining files
combined <- street_tree %>%
  st_join(bmore_nb) %>%
  st_drop_geometry()

street_tree %>% sample_n(100) %>% mapview()
bmore_nb %>% mapview()
