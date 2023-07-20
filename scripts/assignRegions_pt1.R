
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
  transmute(DFO_REGION = "land", geometry)

# load input WS data
WS_data <- read_excel(here("input", input_file), sheet = "val_entry")

WS_coords <- WS_data %>%
  mutate(ROWNUMBER = row_number())%>%  
  filter(!is.na(LATITUDE), !is.na(LONGITUDE))

# create shapefile of sighting coordinates
points_sf <- st_as_sf(WS_coords, coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(regions))

# Perform spatial join
join_region <- st_join(points_sf, regions)

# Extract the region code from the joined polygons
region_code <- join_region$DFO_REGION

# Identify points with multiple regions
overlap_points <- join_region[duplicated(join_region$ROWNUMBER) | duplicated(join_region$ROWNUMBER, fromLast = TRUE), ]


# Create a new column 'region' in the points object and assign the region codes
points_sf$region <- ifelse(is.na(region_code), "OT",
                           region_code)

#check if there may be overlap on land
# Perform spatial join
join_land <- st_join(points_sf, Canada)
# Extract the region code from the joined polygons
land_code <- join_land$DFO_REGION

# Create a new column 'land' in the points object and assign the land codes
points_sf$land <- ifelse(is.na(land_code), "Ok",
                           "check land")

#add the codes back to the datasheet
WS_coords$REGION_CD <-as_factor(points_sf$region)
WS_coords$LAND<-as_factor(points_sf$land)


# output as .csv file
outfilename<-str_match(input_file, "(.*)\\..*$")[,2]
write_csv(WS_coords, here("output", paste0(outfilename, "-REGIONCODES.csv.")))

