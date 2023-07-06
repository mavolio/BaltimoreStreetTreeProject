library(sf); library(tidyverse)

# # temp land cover 
# lc <- st_read('../../../../_students/SPonte/git_SPonte/GSI_LCC_STEW/intermediate_data/Tabulate_balt_LU_2022_05_25.dbf', as_tibble = TRUE)

# neighs_no_match <- 
#   bmore_nb |> 
#   anti_join(lc, by = c('Name' = 'LABEL')) |> 
#   arrange(Name) |> 
#   pull(Name)
# 
# neighs_no_match_2 <- 
#   lc |> 
#   anti_join(bmore_nb, by = c('LABEL' = 'Name')) |> 
#   arrange(LABEL) |> 
#   pull(LABEL)
# 
# 
# cbind(neighs_no_match, neighs_no_match_2) # AACk!
# 
# rm(list = ls()) # don't do this in practice.

# create our own land cover summaries.
# street_tree <- 
#   st_read("../BaltimoreStreetTreeProject_Large_Data/bc_forestry_trees_20190319/bc_forestry_trees_20190319.shp"
#           , as_tibble = TRUE) %>% 
#   filter(LOC_TYPE == "Street") 

lc <- 
  raster::raster('../BaltimoreStreetTreeProject_Large_Data/balt_24510_lc_2018/balt_24510_landcover_2018.tif')

lc@data
lc@data@attributes

# bmore_nb <- 
#   st_read("../BaltimoreStreetTreeProject_Large_Data/Neighborhood/Neighborhood.shp") |> 
#   st_make_valid() |> 
#   st_transform(crs = st_crs(lc))

bmore_nb <- 
  st_read("../BaltimoreStreetTreeProject_Large_Data/Community_Statistical_Areas_(CSAs)__Reference_Boundaries/Community_Statistical_Areas_(CSAs)__Reference_Boundaries.shp") |> 
  # st_make_valid() |> 
  st_transform(crs = st_crs(lc))

# tictoc::tic(); for(i in bmore_nb$Name){
tictoc::tic(); for(i in bmore_nb$Community){
  print(i)
  
  vals <- 
    # terra::mask(terra::crop(lc, bmore_nb |> filter(Name == i)), bmore_nb |> filter(Name == i)) |> 
    terra::mask(terra::crop(lc, bmore_nb |> filter(Community == i)), bmore_nb |> filter(Community == i)) |> 
    raster::getValues()
  
  vals |>
    table() |> 
    as_tibble() |> 
    mutate(  id = i
           , lc_class = 
             case_when(
                   vals == '1' ~                            'Water'
                 , vals == '2' ~                'Emergent Wetlands'
                 , vals == '3' ~                      'Tree Canopy'
                 , vals == '4' ~                     'Scrub\\Shrub'
                 , vals == '5' ~                       'Herbaceous'
                 , vals == '6' ~                           'Barren'
                 , vals == '7' ~                       'Structures'
                 , vals == '8' ~                 'Other Impervious'
                 , vals == '9' ~                            'Roads'
                 , vals == '10'~      'Tree Canopy over Structures'
                 , vals == '11'~ 'Tree Canopy over Other Impervious'
                 , vals == '12'~            'Tree Canopy over Roads'
                 , TRUE ~ NA
                 )
             ) |> 
    select(id, n, lc_class) |>  # names should be id, n, lc_class
    # readr::write_csv('../BaltimoreStreetTreeProject_Large_Data/lc_neigh_summaries/neighs.csv' # TODO move to regular folder
    readr::write_csv('../BaltimoreStreetTreeProject_Large_Data/lc_neigh_summaries/neighs_bnia.csv' # TODO move to regular folder
                       , append = TRUE)
    }; tictoc::tic()

# read neighborhood land cover summaries in long-form
(test <- 
    # read_csv('../BaltimoreStreetTreeProject_Large_Data/lc_neigh_summaries/neighs.csv' # TODO and update here.
    read_csv('../BaltimoreStreetTreeProject_Large_Data/lc_neigh_summaries/neighs_bnia.csv' # TODO and update here.
             , col_names = c('id', 'n', 'lc_class')) |> 
    tidylog::filter(lc_class != 0) |> 
    tidylog::filter(!is.na(lc_class)) |> 
    tidylog::pivot_wider(names_from = lc_class, values_from = n, values_fill = 0) %>% # need old pipe
    tidylog::mutate(total_area_in_pixels = rowSums(.[,-1]))                           # for dot notation to work here
  )


test |> glimpse()

bmore_nb |> tidylog::left_join(test, by = c('Name' = 'id'))

# how similar are the estimates of area
# vector using st_area() vs number of pixels from raster
# 1) correlation
library(magrittr)
bmore_nb %>% 
  mutate(neigh_area_m2 = as.double(st_area(.))) |> 
  tidylog::left_join(test, by = c('Name' = 'id')) %$% # used when functions don't have data argument first
  cor.test(.$neigh_area_m2, .$total_area_in_pixels)

# 2) vizualize
bmore_nb %>% 
  mutate(neigh_area_m2 = as.double(st_area(.))) |> 
  tidylog::left_join(test, by = c('Name' = 'id')) |> 
  ggplot(aes(neigh_area_m2, total_area_in_pixels)) +
  geom_point() +
  geom_abline()
  

# 
# 
# tidylog::filter(lc_class !=0) %>% # move up? polygons dont match up with the raster around the edst
#   mutate(lc_class = recode(lc_class,
#                            '1' = 'tree',
#                            '2' = 'grass',
#                            '3' = 'soil',
#                            '4' = 'water',
#                            '5' = 'building',
#                            '6' = 'road',
#                            '7' = 'other_paved'),
#          area = n*pxl_conv) %>% 
#   pivot_wider(id_cols = id, names_from = lc_class, values_from = area, values_fill = 0, 
#               names_prefix = 'a_') %>%
#   mutate(a_total = rowSums(.[,-1])) %>% # TODO;make kthis more specific?
#   dplyr::select(id, # FIXME what if a land cover class is not present?
#                 a_tree, a_grass, a_soil, a_water, a_building, a_road, a_other_paved,
#                 a_total) %>% # just cosmetic reordering
#   mutate(p_tree        = 100*(a_tree        / a_total),
#          p_grass       = 100*(a_grass       / a_total),
#          p_soil        = 100*(a_soil        / a_total),
#          p_water       = 100*(a_water       / a_total),
#          p_building    = 100*(a_building    / a_total),
#          p_road        = 100*(a_road        / a_total),
#          p_other_paved = 100*(a_other_paved / a_total)) -> lc_tbl; toc()

