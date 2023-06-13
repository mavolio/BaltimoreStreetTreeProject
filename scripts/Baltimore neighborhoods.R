#read in shapefile by bmore city with neighborhood delineations and combine with street tree data
#started 6/12

# load packages

library(tidyverse)
library(sf)
library(mapview)

# get oriented
# setwd("C:\\Users\\trini\\Documents\\Street Trees") # there is another way to do this I'll (DHL) show you
getwd()
list.files()
list.files('../BaltimoreStreetTreeProject_Large_Data/') # means "up one level"


# read spatial data 
street_tree <- 
  st_read("../BaltimoreStreetTreeProject_Large_Data/bc_forestry_trees_20190319/bc_forestry_trees_20190319.shp"
          , as_tibble = TRUE)

bmore_nb <- 
  st_read("../BaltimoreStreetTreeProject_Large_Data/Neighborhood/Neighborhood.shp") |> 
  st_make_valid() |> 
  st_transform(crs = st_crs(street_tree))


# double checks
street_tree |> glimpse()
street_tree |> 
  sample_n(1000) |> # just grab 1000 random points to avoid over-plotting and sensory overload
  mapview(zcol = 'GENUS') # lets color the dots by genus, why not?

bmore_nb |> glimpse()
bmore_nb |> plot() # works
# bmore_nb |> mapview() # fails, gdal errors (Geospatial Data Abstraction Library, https://gdal.org/, open source spatial library)
# bmore_nb |> st_is_valid()
# bmore_nb |> st_make_valid() |> mapview(zcol = 'Population') # seems to work now! updated read in step above


# combining files
combined <- 
  street_tree %>%
  st_join(bmore_nb) %>%
  st_drop_geometry()

# did that joining work?
combined
combined |> glimpse()
