
# Script to prep whale sightings data for entry in the WSDB
# includes steps to: 
  ## clean lat longs
  ## check for internal duplicates
  ## check for land whales
  ## prep time field for SQL script
  ## populate scientific and common names based on species codes
  ## assign DFO region to each sighting location and view all sighting locations on map 
#
# by L. Feyrer 2023-11-29

# credit to assign regions leaflet maps: J. Stanistreet 2022-12-07

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
#NL   DFO-Newfoundland
#GULF   DFO-Gulf
#AR     DFO-Arctic
#CAN    Within Canada but outside current regional boundaries (e.g., Pacific, Ontario)
#OTHER     Other - anywhere outside Canada
#land   Location appears to be on land - check for errors in coordinates


#####################################################

### TO RUN SCRIPT, MODIFY THESE LINES:

# # specify csv input file
#coords should be named "LONGITUDE" and "LATITUDE"

# install pacman package - only necessary the first time code is run, otherwise comment out this line
#install.packages("pacman")

# run all code below!


######## PART 1: prep data ########

# use pacman to install and load required packages

pacman::p_load(sf, tidyverse, readxl, here, leaflet, scales, terra, ggrepel)
sf_use_s2(FALSE)


# load via mapped Y drive using OPP12 to find shapefiles
# or if shapefiles folder is stored locally: "shapefiles/DFO_NAFO_EEZ_Land.shp"
regions <-read_sf("shapefiles/DFO_NAFO_EEZ_Land.shp")

# load hi res land data (sourced from Open Gov Atlas, saved as shapefile)
Canada<-read_sf("shapefiles/canada.shp") %>% 
  st_transform(4326) %>% 
  transmute(REGION = "land", geometry)

# load input WS data
options(digits = 5)
    WS_data <- read_csv(here("input", input_file), col_types = cols(.default ="c"))%>%dplyr::select(-COMMONNAME, -URI, -SCIENTIF)%>%
      mutate(LATITUDE = sapply(LATITUDE, convert_to_decimal_degrees), LONGITUDE = abs(sapply(LONGITUDE, convert_to_decimal_degrees))*-1,
             #apply function to clean coordinates and ensure LONGITUDE is negative, if long is POSTITIVE this will be wrong.
      SPECIES_CD = as.numeric(SPECIES_CD))%>%filter(!is.na(SPECIES_CD))
  

# check species, add common names and scientific names based on codes
    SP_data <- read_csv(here("input", species), show_col_types = F)
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
        
      # Identify possible duplicate records based on all fields matching perfectly  
        possible_Dups = WS_coords[duplicated(WS_coords[c("LATITUDE","LONGITUDE", "WS_DATE","WS_TIME", "SPECIES_CD")] ) |
                                  duplicated(WS_coords[c("LATITUDE","LONGITUDE", "WS_DATE","WS_TIME", "SPECIES_CD")], fromLast = T), ]
        
        #check if coords rounded to first decimal place ###NEED TO DO THIS###
 
# Assign the region codes to REGION_CD variable
        WS_coords = WS_coords%>%mutate(REGION_CD = ifelse(is.na(DFO_REGION), "OTHER",
                              DFO_REGION))

#check if there may be overlap on land
      # Perform spatial join
        WS_coords <- st_join(WS_coords, Canada)

      # Create a new column 'land' in the points object and assign the land codes
      WS_coords <- WS_coords%>%mutate(LAND = ifelse(is.na(REGION), "Ok",
                                 "check land"))
      
      
      ### remove colons from "Time" field
      WS_coords$WS_TIME <- gsub(":", "", WS_coords$WS_TIME)
      
      ### format Date as excel date number
      # Convert WS_DATE to Date object in R (assuming dates are in m/d/y format)
      WS_coords <- WS_coords %>% 
        mutate(WS_DATE = as.Date(WS_DATE, format = "%m/%d/%Y"))
      
      # Excel's date number starts from 1899-12-30
      excel_epoch <- as.Date("1899-12-30")
      
      # Calculate Excel date number - populates in new field named WS_DATE_EXCEL
      WS_coords <- WS_coords %>%
        mutate(WS_DATE_EXCEL = as.numeric(WS_DATE - excel_epoch))
      
      ### Take SPECIES_CD to populate COMMONNAME, URI, SCIENTIF fields from species codes.csv
      
      WS_coords_sp = WS_coords%>%select(-URI, -SCIENTIF, -COMMONNAME)%>%left_join(SP_data, by = "SPECIES_CD")
      ##clean up df

      WS_coords = WS_coords_sp%>%dplyr::select(ROWNUMBER, LAND, LATITUDE, LONGITUDE, DFO_REGION, everything())%>%
        mutate(WS_DATE1 = WS_DATE, WS_DATE = WS_DATE_EXCEL) #keep WS_DATE in original format just in case
      #remove the random shapefile fields
      WS_coords1 = WS_coords%>%st_drop_geometry()%>%
        dplyr::select(-c(FID_DFO_NA,FID_DFO_Re,	Region_FR,	Region_EN,	WS_DATE_EXCEL, Region_INU, Shape_Leng,	Shape_Le_1,	Shape_Area,	REGION))


# output as .csv file
outfilename<-str_match(input_file, "(.*)\\..*$")[,2]
write_csv(WS_coords1, here("output", paste0(outfilename, "-REGIONCODES.csv.")), na="")
write_csv(WS_NA_coords, here("output", paste0(outfilename, "-NA_COORDS.csv.")))
write_csv(overlap_Regions, here("output", paste0(outfilename, "-MULTIPLE_REGIONS.csv.")), na="")
write_csv(possible_Dups, here("output", paste0(outfilename, "-possible_Dups.csv.")), na="")
