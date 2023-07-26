library(sf); library(tidyverse)

heat_severity <- 
  raster::raster('../../BaltimoreTrees/BaltimoreStreetTreeProject_Large_Data/Urban Heat Island Mapping/Urban Heat Island Severity for U.S. cities - 2019.tiff') #temporarily using this data because it included a raster file



bmore_nb <- 
  st_read("../../BaltimoreTrees/BaltimoreStreetTreeProject_Large_Data/Community Statistical Areas (BNIA neighborhoods)/Community_Statistical_Areas_(CSAs)__Reference_Boundaries.shp") |> 
  st_transform(crs = st_crs(temperature))

tictoc::tic(); for(i in bmore_nb$Community){
  print(i)
  
  vals <- 
    terra::mask(terra::crop(heat_severity, bmore_nb |> filter(Community == i)), bmore_nb |> filter(Community == i)) |> 
    raster::getValues()
  
  vals |>
    summary() |>
    as_tibble() |> 
    mutate(  id = i
             , severity_class = #Fix- would I have to use this for this data? Severity ranges 1-5
               case_when( 
                 vals == '1' ~                            
                 , vals == '2' ~                
                 , vals == '3' ~                      
                 , vals == '4' ~                    
                 , vals == '5' ~                      
                 , TRUE ~ NA
               )
    ) |> 
    select(id, n, severity_class) 
  
}; tictoc::tic()
