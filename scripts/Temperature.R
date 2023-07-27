library(sf); library(tidyverse)



#PM Baltimore temperature raster
temperature <- 
  raster::raster('../../BaltimoreTrees/BaltimoreStreetTreeProject_Large_Data\\baltimore\\temperature surfaces\\bal_pm.tif')
 

#BNIA Neighborhoods 
bmore_nb <- 
  st_read("../../BaltimoreTrees/BaltimoreStreetTreeProject_Large_Data/Community Statistical Areas (BNIA neighborhoods)/Community_Statistical_Areas_(CSAs)__Reference_Boundaries.shp") |> 
  st_transform(crs = st_crs(temperature))

#loop
tictoc::tic(); for(i in bmore_nb$Community){
  print(i)
  
   values <- 
     terra::mask(terra::crop(temperature, bmore_nb |> filter(Community == i)), bmore_nb |> filter(Community == i)) |> 
     raster::getValues()
   
#TO FIX lines 23-32 - I used a slightly different approach than what we discussed I think, is there a better way to do this?
   values |>
     table() |> 
     as_tibble() |>
     mutate(Neighborhood = i, 
            avg_temp = mean(values, na.rm = TRUE)) |>
     select(Neighborhood, n, avg_temp) #|>
    #readr::write_csv(paste0('../../BaltimoreTrees/BaltimoreStreetTreeProject_Large_Data/baltimore/temperature surfaces/NB_pm_temp_test', Sys.Date(), '.csv') # I have this commented out until I'm sure about the loop system
#                    , append = TRUE)
  
}; tictoc::tic()


#pivot still a work in progress 
(test <- 
    read_csv('../../BaltimoreTrees/BaltimoreStreetTreeProject_Large_Data/baltimore/temperature surfaces/NB_pm_temp_test2023-07-26.csv' 
             , col_names = c('id', 'n', 'avg_temp')) |> 
#    tidylog::filter(max_temp != 0) |> 
    tidylog::filter(!is.na(max_temp)) |> 
    tidylog::pivot_wider(names_from = , values_from = n, values_fill = 0)) #%>% # need old pipe
#    tidylog::mutate(total_area_in_pixels = rowSums(.[,-1]))                           # for dot notation to work here


test |> glimpse()

