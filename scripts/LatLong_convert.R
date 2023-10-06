# Coordinate conversion from Degrees Decimal.Minutes to DD using R
# Laura Feyrer 2023
#opens in assignRegions project

# #libraries
# library(readr)
# library(here)
# 
# input_file <-"CoriolisMultiBeamValidatedEntry2023.csv"
# species = "speciesCodes.csv" 

#read in csv of data with Degrees Decimal Minute Lat/Long in the first 4 separate variables 

convert_to_decimal_degrees <- function(coord) {
  # Check if the coordinate is already in decimal format
  if (grepl("^-?\\d+\\.\\d+$", coord)) {
    return(as.numeric(coord))
  }
  
  # Split the coordinate into its components (degrees, minutes, seconds)
  components <- as.numeric(unlist(strsplit(gsub("[^0-9. ]", "", coord), " ")))
  
  # Calculate the decimal degrees based on the number of components
  if (length(components) == 3) {
    return(components[1] + components[2]/60 + components[3]/3600)
  } else if (length(components) == 2) {
    return(components[1] + components[2]/60)
  } else {
    return(NA)
  }
}
#apply function to clean coordinates and ensure LONGITUDE is negative

# variation = DM_coords%>%mutate(LATITUDE = sapply(LATITUDE, convert_to_decimal_degrees), LONGITUDE = abs(sapply(LONGITUDE, convert_to_decimal_degrees))*-1)
# 
# # # rename so variables are generic
# # colnames(DM_coords)[1:4] <- paste0("new_col", 1:4)
# # 
# # #convert DdM to DD
# # 
# # DM_coords = DM_coords%>%  mutate(across(.cols = 1:4, .fns = ~ as.numeric(gsub("[^0-9.-]", "", .)))) %>% 
# #   mutate(LATITUDE = new_col1 + (new_col2 / 60),
# #     LONGITUDE = new_col3 + (new_col4 / 60) * -1
# #   ) %>%
# #   dplyr::select(LATITUDE, LONGITUDE)
# 
# #save output in output directory as csv
# print(DM_coords)
# 
# write.csv(DM_coords, here("output", "LatLong_data.csv"))


