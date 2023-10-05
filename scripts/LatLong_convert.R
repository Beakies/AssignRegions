# Coordinate conversion from Degrees Decimal.Minutes to DD using R
# Laura Feyrer 2023
#opens in assignRegions project

#libraries
library(readr)
library(here)

#read in csv of data with Degrees Decimal Minute Lat/Long in separate variables and out put dataset of Decimal Degrees

DM_coords = read_csv(here("input", "LatLong_data.csv"), col_types = cols(.default ="c")) %>%
  mutate(LATITUDE.D = as.numeric(gsub("[^0-9.-]", "", `Lat. Degrees`)), LATITUDE.M =as.numeric(gsub("[^0-9.-]", "", Lat.Min.m)),
         LONGITUDE.D = as.numeric(gsub("[^0-9.-]", "", `Long. Degrees`)), LONGITUDE.M = as.numeric(gsub("[^0-9.-]", "", Long.Min.m) ))%>% 
  mutate(LATITUDE = (LATITUDE.D+(LATITUDE.M/60)), LONGITUDE = (LONGITUDE.D+(LONGITUDE.M/60))*-1)%>%dplyr::select(LATITUDE, LONGITUDE)

#save output in output directory as csv

write.csv(DM_coords, here("output", "LatLong_data.csv"))


