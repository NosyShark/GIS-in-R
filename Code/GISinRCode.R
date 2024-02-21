# This is the GIS in R workshop with Jasper

install.packages(c("tidyverse", "sp", "raster", "sf", "lwgeom", "terra", "stars", "exactextractr"))
install.packages(c("cowplot", "hrbrthemes", "knitr", "leaflet", "htmltools", "rosm", "ggspatial", "rnaturalearth", "mapview", "tmap"))

# Reading in data from iNaturalist
puffyshark <- get_inat_obs(taxon_name = "Haploblepharus edwardsii", maxresults = 600)
brownshark <- get_inat_obs(taxon_name = "Haploblepharus fuscus", maxresults = 600)
darkshark <- get_inat_obs(taxon_name = "Haploblepharus pictus", maxresults = 600)

# Filtering observations to research grade
puffyshark <- puffyshark %>% filter(positional_accuracy < 100 & latitude <0 & !is.na(latitude) & quality_grade == "research")
brownshark <- brownshark %>% filter(positional_accuracy < 100 & latitude <0 & !is.na(latitude) & quality_grade == "research")
darkshark <- darkshark %>% filter(positional_accuracy < 100 & latitude <0 & !is.na(latitude) & quality_grade == "research")

# Make the dataframes spatial objects of class = "sf"
puffyshark <- st_as_sf(puffyshark, coords = c("longitude", "latitude"), crs = 4326)
brownshark <- st_as_sf(brownshark, coords = c("longitude", "latitude"), crs = 4326)
darkshark <- st_as_sf(darkshark, coords = c("longitude", "latitude"), crs = 4326)

# Check the class
class(puffyshark)
class(brownshark)
class(darkshark)

# Plot these bois
ggplot() + 
  annotation_map_tile(type = "osm", progress = "none") + 
  geom_sf(data = puffyshark, color = "#85B22C")
ggplot() +
  annotation_map_tile(type = "osm", progress = "none") + 
  geom_sf(data = brownshark, color = "#2C85B2")
ggplot() + 
  annotation_map_tile(type = "osm", progress = "none") + 
  geom_sf(data = darkshark, color = "#422CB2")

# Let's try overlay these data
ggplot() + 
  annotation_map_tile(type = "osm", progress = "none") + 
  geom_sf(data = puffyshark, color = "#85B22C") + 
  geom_sf(data = brownshark, color = "#2C85B2") + 
  geom_sf(data = darkshark, color = "#422CB2")

# Interactive maps
leaflet() %>% 
  addTiles(group = "Default") %>%
  addCircleMarkers(data = puffyshark, 
                   group = "Haploblepharus edwardsii", 
                   radius = 1, 
                   color = "#85B22C")
leaflet() %>%
  addTiles(group = "Default") %>%
  addCircleMarkers(data = brownshark, 
                   group = "Haploblepharus fuscus", 
                   radius = 1,
                   color = "#2C85B2")
leaflet() %>%
  addTiles(group = "Default") %>%
  addCircleMarkers(data = darkshark, 
                   group = "Haploblepharus pictus", 
                   radius = 1, 
                   color = "#422CB2")

# Maybe even trying to layer this?
leaflet() %>% 
  addTiles(group = "Default") %>%
  addCircleMarkers(data = puffyshark, 
                   group = "Haploblepharus edwardsii", 
                   radius = 1, 
                   color = "#85B22C")%>%
  addCircleMarkers(data = brownshark, 
                 group = "Haploblepharus fuscus", 
                 radius = 1,
                 color = "#2C85B2")%>%
  addCircleMarkers(data = darkshark, 
                   group = "Haploblepharus pictus", 
                   radius = 1, 
                   color = "#422CB2")

# Different interactive maps
mapview(puffyshark, 
        popup = 
          popupTable(puffyshark, 
                     zcol = c("user_login", "url")))
mapview(brownshark, 
        popup = 
          popupTable(brownshark, 
                     zcol = c("user_login", "url")))
mapview(darkshark, 
        popup = 
          popupTable(darkshark,
                     zcol = c("user_login", "url")))

