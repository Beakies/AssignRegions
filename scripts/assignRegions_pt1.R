
# Script to assign region code to each sighting location and view all sighting locations on map

# J. Stanistreet 2022-12-07
# updated by L. Feyrer 2023-06-07

#####################################################

# REQUIRED FOLDERS:

# input: formatted Excel sheet with sighting coordinates to check/assign to regions

# output: resulting .csv file with region codes

# Y:\shapefiles: folder containing the following Y:\shapefiles:

#   DFO Regions: these are based on  NAFO regions (2022) and manually edited based on Gulf/ QC input and to remove overlap with land,
#       snap edges together, etc.
#   
#   AC_1M: land area shapefile, sourced from National Atlas files on opengov.ca


# Region codes applied by this script:

#MAR    DFO-Maritimes
#QC     DFO-Quebec
#NFLD   DFO-Newfoundland
#GULF   DFO-Gulf
#AR     DFO-Arctic
#CAN    Within Canada but outside current regional boundaries (e.g., Pacific, Ontario)
#OT     Other - anywhere outside Canada
#land   Location appears to be on land - check for errors in coordinates


#####################################################

### TO RUN SCRIPT, MODIFY THESE LINES:

# # specify xls input file
# needs to have a tab named "val_entry"
#coords should be named "LONGITUDE" and "LATITUDE"
# input_file <-"FWDtoGLFValidatedEntry2022.xlsx"

# install pacman package - only necessary the first time code is run, otherwise comment out this line
#install.packages("pacman")

# run all code below!


######## PART 1: assign region ########

# use pacman to install and load required packages

pacman::p_load(sf, tidyverse, readxl, here, leaflet, scales, terra, ggrepel)
sf_use_s2(FALSE)


# load via mapped Y drive using OPP12 to find shapefiles
# or if shapefiles folder is stored locally: "shapefiles/DFO_NAFO_EEZ_Land.shp"
regions <-read_sf("Y:/shapefiles/DFO_NAFO_EEZ_Land.shp")

# load hi res land data (sourced from Open Gov Atlas, saved as shapefile)
Canada<-read_sf("Y:/shapefiles/canada.shp") %>% 
  st_transform(4326) %>% 
  transmute(REGION = "land", geometry)

# load input WS data
    WS_data <- read_excel(here("input", input_file), sheet = sheet)%>%select(-COMMONNAME, -URI, -SCIENTIF)
    
# check species, add common names and scientific names based on codes
    SP_data <- read_excel(here("input", species), sheet = "species codes")
    WS_data = left_join(WS_data, SP_data, by = "SPECIES_CD")
    
    #create shapefile based on coords
    
    WS_coords <- WS_data %>% mutate(ROWNUMBER = row_number())%>%  
      filter(!is.na(LATITUDE), !is.na(LONGITUDE))%>%select(ROWNUMBER, everything())

#check any coordinates with NAs?
    WS_NA_coords <- WS_data %>% mutate(ROWNUMBER = row_number())%>% 
    filter(is.na(LATITUDE), is.na(LONGITUDE))%>%select(ROWNUMBER, everything())

# create shapefile of sighting coordinates
    WS_coords <- st_as_sf(WS_coords, coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(regions))%>%dplyr::mutate(LONGITUDE = sf::st_coordinates(.)[,1],
                                                                                                                 LATITUDE = sf::st_coordinates(.)[,2])

      
# Perform spatial join between sightings and DFO Regions
    WS_coords <- st_join(WS_coords, regions)
        
      # Identify points with multiple regions?
        overlap_Regions <- WS_coords[duplicated(WS_coords$ROWNUMBER) | duplicated(WS_coords$ROWNUMBER, fromLast = TRUE), ]
        possible_Dups = WS_coords[duplicated(WS_coords[c("LATITUDE","LONGITUDE", "WS_DATE","WS_TIME")] ) |
                                  duplicated(WS_coords[c("LATITUDE","LONGITUDE", "WS_DATE","WS_TIME")], fromLast = T), ]
 
        

# Assign the region codes to REGION_CD variable
        WS_coords = WS_coords%>%mutate(REGION_CD = ifelse(is.na(DFO_REGION), "OT",
                              DFO_REGION))

#check if there may be overlap on land
      # Perform spatial join
        WS_coords <- st_join(WS_coords, Canada)

      # Create a new column 'land' in the points object and assign the land codes
      WS_coords <- WS_coords%>%mutate(LAND = ifelse(is.na(REGION), "Ok",
                                 "check land"))

WS_coords = WS_coords%>%select(ROWNUMBER, LAND, LATITUDE, LONGITUDE, everything())
WS_coords1 = WS_coords%>%select(-c(,72:82))%>%st_drop_geometry()

# output as .csv file
outfilename<-str_match(input_file, "(.*)\\..*$")[,2]
write_csv(WS_coords1, here("output", paste0(outfilename, "-REGIONCODES.csv.")), na="")
write_csv(WS_NA_coords, here("output", paste0(outfilename, "-NA_COORDS.csv.")))
write_csv(overlap_Regions, here("output", paste0(outfilename, "-MULTIPLE_REGIONS.csv.")), na="")
write_csv(possible_Dups, here("output", paste0(outfilename, "-possible_Dups.csv.")), na="")
