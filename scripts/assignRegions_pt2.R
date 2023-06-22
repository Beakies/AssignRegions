######## PART 2: generate maps to check results ########
library(htmlwidgets)

#requires part 1!
# set up regions for map

region_labels<-c("AR","GULF", "MAR", "NFLD", "QC", "PAC", "O&P", "OT")
land_check = c("Ok", "check land")

mapregions<-regions %>% 
  mutate(DFO_REGION = factor(DFO_REGION, levels = region_labels))

mapland<-points_sf %>% 
  mutate(LAND = factor(land, levels = land_check))

# set up color palettes for map
colorpal<-c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02", "#7B3E19", "grey")
colorpal3<-c("red","grey")
# show_col(colorpal)


names(colorpal) <-factor(region_labels, levels = region_labels)
colorpal_fct = factor(region_labels, levels = region_labels)
names(colorpal3) <-rev(land_check)
regioncolors<-colorFactor(colorpal, mapregions$DFO_REGION)
regioncolors_points<-colorFactor(colorpal, mapregions$DFO_REGION)
landcolors_points<-colorFactor(colorpal3, points_sf$land)

# build map to check regions---------

WSmap1 <- leaflet() %>% 
  
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
              fillOpacity = 0.5,
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
                     "Region: ", WS_coords$REGION_CD)))%>%
  addLegend("bottomright", 
            pal = regioncolors_points,
            values = colorpal_fct,
                     title = "DFO Regions",
            opacity = .5)


# show map with regions
WSmap1

if (save_map == TRUE) {
    saveWidget(widget = WSmap1, file = here("output", paste0(outfilename, ".html")), selfcontained = TRUE)}

# Map with land points colored red-----

WSmap2 = WSmap1%>% # add regions
  addPolygons(data = Canada,
              stroke = T,
              weight = 1,
              color = "black",
              fill = F,
              fillColor = "gray",
              fillOpacity = 1,
              smoothFactor = 3) %>%
  

# add points coded for land
addCircleMarkers(data = WS_coords,
                 ~LONGITUDE, ~LATITUDE,
                 weight = 1,
                 col = "black",
                 fillColor = ~landcolors_points(LAND),
                 radius = 4,
                 fillOpacity = .75,
                 stroke = F,
                 popup=(paste0(
                   "Row Number: ", WS_coords$ROWNUMBER, "<br>",
                   "Latitude: ", WS_coords$LATITUDE, "<br>",
                   "Longitude: ", WS_coords$LONGITUDE, "<br>",
                   "Region: ", WS_coords$REGION_CD)))%>%
  addLegend("bottomright", 
            colors = colorpal3,
            labels = c("Check", "OK"),
            title = "Land whales?",
            opacity = .5)


WSmap2


# 
