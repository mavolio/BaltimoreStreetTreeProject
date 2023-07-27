library(sf); library(tidyverse)



#PM Baltimore temperature raster
temperature <- 
  # raster::raster('../../BaltimoreTrees/BaltimoreStreetTreeProject_Large_Data\\baltimore\\temperature surfaces\\bal_pm.tif') # please stop moving file hierarchy
  raster::raster('../BaltimoreStreetTreeProject_Large_Data/baltimore/temperature surfaces/bal_pm.tif')

 

#BNIA Neighborhoods 
bmore_nb <- 
  # st_read("../../BaltimoreTrees/BaltimoreStreetTreeProject_Large_Data/Community Statistical Areas (BNIA neighborhoods)/Community_Statistical_Areas_(CSAs)__Reference_Boundaries.shp") |> # it is critical to keep the paths the same. Its cumbersome for us each to change this back and forth. Let me know if I (DHL) can help explain how this fits together and why. The intended system saves us all time.
  st_read("../BaltimoreStreetTreeProject_Large_Data/Community_Statistical_Areas_(CSAs)__Reference_Boundaries/Community_Statistical_Areas_(CSAs)__Reference_Boundaries.shp") |> 
  st_transform(crs = st_crs(temperature))

#loop
tictoc::tic(); for(i in bmore_nb$Community){
  print(i)

   values <- 
     terra::mask(terra::crop(temperature, bmore_nb |> filter(Community == i)), bmore_nb |> filter(Community == i)) |> 
     raster::getValues()
   
#TO FIX lines 23-32 - I used a slightly different approach than what we discussed I think, is there a better way to do this?
   # really great attempt! I think "table()" left over from last time was the issue. 
   # I (DHL) used a slightly different approach (make a tibble with desired values rather than make a tibble and sequentially modify)
   # to demonstrate an alternative workflow, show nested piping, and introduce the nrow function.
   # Many streams, one river. Namaste.
   # values |> 
     # table() |> 
     # as_tibble() |>
   tibble(  Neighborhood = i
          , ave_temp = mean(values, na.rm = TRUE) # add the degrees '_c' or '_f' for Celsius or Farh..
          , n_pixel = values |> as_tibble() |> tidylog::filter(!is.na(value)) |> nrow()
          ) |> 
     # mutate(Neighborhood = i, 
     #        avg_temp = mean(values, na.rm = TRUE)) |>
     # select(Neighborhood, n, avg_temp) #|>
# readr::write_csv(paste0('../../BaltimoreTrees/BaltimoreStreetTreeProject_Large_Data/baltimore/temperature surfaces/NB_pm_temp_test', Sys.Date(), '.csv') # I have this commented out until I'm sure about the loop system
#                    , append = TRUE)
   readr::write_csv(paste0('input_data/NB_pm_temp_', Sys.Date(), '.csv') # moved to input (not Large_Data)
                    , append = TRUE)
  
}; tictoc::tic()


#pivot still a work in progress 
# no pivot needed! 
(temperature_by_neigh <- 
    read_csv('input_data/NB_pm_temp_2023-07-27.csv' 
             , col_names = c('Neighborhood', 'avg_temp', 'n_pixel'))
)

temperature_by_neigh |> glimpse() # NICE!
temperature_by_neigh |> summary()
    
# read_csv('../../BaltimoreTrees/BaltimoreStreetTreeProject_Large_Data/baltimore/temperature surfaces/NB_pm_temp_test2023-07-26.csv' 
             # , col_names = c('id', 'n', 'avg_temp')) |> 
# #    tidylog::filter(max_temp != 0) |> 
#     tidylog::filter(!is.na(max_temp)) |> 
#     tidylog::pivot_wider(names_from = , values_from = n, values_fill = 0)) #%>% # need old pipe
# #    tidylog::mutate(total_area_in_pixels = rowSums(.[,-1]))                           # for dot notation to work here


# test |> glimpse()

