
# Script to assign region code to each sighting location and view all sighting locations on map

# J. Stanistreet 2022-12-07
# updated by L. Feyrer 2023-06-07

#####################################################

# REQUIRED FOLDERS:

# input: formatted Excel sheet with sighting coordinates to check/assign to regions

# output: resulting .csv file with region codes

# shapefiles: folder containing the following shapefiles:

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

# specify input file
input_file <-"FWDtoGLFValidatedEntry2022.xlsx"

# install pacman package - only necessary the first time code is run, otherwise comment out this line
#install.packages("pacman")

# run all code below!


######## PART 1: assign region ########

# use pacman to install and load required packages

pacman::p_load(sf, tidyverse, readxl, here, leaflet, scales, terra, ggrepel)

# load DFO Region shapefiles
sf_use_s2(FALSE)
regions <-read_sf("shapefiles/DFO_NAFO_EEZ_1.shp")
plot(st_geometry(regions))

# load hi res land data (sourced from Open Gov Atlas, saved as shapefile)
land<-read_sf("shapefiles/land.shp") %>% 
  st_transform(4326) %>% 
  transmute(DFO_REGION = "land", geometry)

# load input WS data
WS_data <- read_excel(here("input", input_file), sheet = "val_entry")

WS_coords <- WS_data %>%
  mutate(ROWNUMBER = row_number()) %>%
  select(ROWNUMBER, LONGITUDE, LATITUDE) %>%  
  drop_na()

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
join_land <- st_join(points_sf, land)
# Extract the region code from the joined polygons
land_code <- join_land$DFO_REGION

# Create a new column 'region' in the points object and assign the region codes
points_sf$land <- ifelse(is.na(land_code), "Ok",
                           "check land")

#add the codes back to the datasheet
WS_coords$REGION_CD <-as_factor(points_sf$region)
WS_coords$LAND<-as_factor(points_sf$land)


# output as .csv file
outfilename<-str_match(input_file, "(.*)\\..*$")[,2]
write_csv(WS_coords, here("output", paste0(outfilename, "-REGIONCODES.csv.")))

######## PART 2: generate maps to check results ########

# set up regions for map

region_labels<-c("AR","GULF", "MAR", "NFLD", "QC", "OT")
land_check = c("Ok", "check land")

mapregions<-regions %>% 
  mutate(DFO_REGION = fct_expand(as_factor(DFO_REGION), region_labels))

mapland<-points_sf %>% 
  mutate(LAND = fct_expand(as_factor(land), land_check))

# set up color palettes for map

colorpal<-c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#ffff33","#666666", "grey60")
colorpal2<-c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#ffff33","black", "white")
colorpal3<-c("red","grey")

names(colorpal) <-region_labels
names(colorpal3) <-land_check
regioncolors<-colorFactor(colorpal, mapregions$DFO_REGION)
regioncolors_points<-colorFactor(colorpal2, mapregions$DFO_REGION)
landcolors_points<-colorFactor(colorpal3, points_sf$land)

# build map to check regions---------

WSmap <- leaflet() %>% 
  
  # add ocean basemap
  addProviderTiles(
    "Esri.OceanBasemap",
    options = providerTileOptions(
      variant = "Ocean/World_Ocean_Base")) %>%
  
  # focus map in a certain area / zoom level
  setView(lng = -63, lat = 44, zoom = 3) %>% 
  
  # add regions
  addPolygons(data = mapregions,
              stroke = T,
              weight = 1,
              color = ~regioncolors(DFO_REGION),
              fill = T,
              fillColor = ~regioncolors(DFO_REGION),
              fillOpacity = 0.4,
              smoothFactor = 3) %>%
  
  # add WS points
  addCircleMarkers(data = WS_coords,
                   ~LONGITUDE, ~LATITUDE,
                   weight = 1,
                   col = 'black',
                   fillColor = ~regioncolors_points(REGION_CD),
                   radius = 4,
                   fillOpacity = 1,
                   stroke = T,
                   popup=(paste0(
                     "Row Number: ", WS_coords$ROWNUMBER, "<br>",
                     "Latitude: ", WS_coords$LATITUDE, "<br>",
                     "Longitude: ", WS_coords$LONGITUDE, "<br>",
                     "Region: ", WS_coords$REGION_CD)))


# show map with regions
WSmap

WSmap%>%
  
  # Map with land points colored red-----
  
# add points coded for land
addCircleMarkers(data = WS_coords,
                 ~LONGITUDE, ~LATITUDE,
                 weight = 1,
                 col = ~landcolors_points(LAND),
                 fillColor = NA,
                 radius = 4,
                 fillOpacity = 1,
                 stroke = F,
                 popup=(paste0(
                   "Row Number: ", WS_coords$ROWNUMBER, "<br>",
                   "Latitude: ", WS_coords$LATITUDE, "<br>",
                   "Longitude: ", WS_coords$LONGITUDE, "<br>",
                   "Region: ", WS_coords$REGION_CD)))



# save map - Joy to add this later if htmlwidgets library is updated??
# 
# if (save_map == TRUE) {
#   saveWidget(widget = map, file = here("output", paste0(outfilename, ".html")), selfcontained = TRUE)
# }
# 
