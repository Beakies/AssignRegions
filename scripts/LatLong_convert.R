# Coordinate conversion from Degrees Decimal.Minutes to DD using R
# Laura Feyrer 2023
#opens in assignRegions project

#libraries
library(readr)
library(here)

#read in csv of data with Degrees Decimal Minute Lat/Long in the first 4 separate variables 

DM_coords = read_csv(here("input", "LatLong_data.csv"), col_types = cols(.default ="c")) 
# rename so variables are generic
colnames(DM_coords)[1:4] <- paste0("new_col", 1:4)

#convert DdM to DD

DM_coords = DM_coords%>%  mutate(across(.cols = 1:4, .fns = ~ as.numeric(gsub("[^0-9.-]", "", .)))) %>% 
  mutate(LATITUDE = new_col1 + (new_col2 / 60),
    LONGITUDE = new_col3 + (new_col4 / 60) * -1
  ) %>%
  dplyr::select(LATITUDE, LONGITUDE)

#save output in output directory as csv
print(DM_coords)

write.csv(DM_coords, here("output", "LatLong_data.csv"))


