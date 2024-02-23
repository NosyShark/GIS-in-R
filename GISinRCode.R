# This is the GIS in R workshop with Jasper
rm(list=ls())

install.packages(c("tidyverse", "sp", "raster", "sf", "lwgeom", "terra", "stars", "exactextractr"))
install.packages(c("cowplot", "hrbrthemes", "knitr", "leaflet", "htmltools", "rosm", "ggspatial", "rnaturalearth", "mapview", "tmap"))
install.packages("rmarkdown", "prettymapr", "leaflet", "htmltools", "rinat", "rnaturalearth", "rnaturalearthdata")

my_packages <- c("tidyverse", "sp", "sf", "raster", "lwgeom", "terra", "stars", "exactextractr", "hrbrthemes","cowplot", "rinat", "prettymapr", "ggspatial", "leaflet", "mapview", "htmltools", "leafpop", "rnaturalearth", "rnaturalearthdata")
lapply(my_packages, require, character.only = TRUE)

## After going through this process once and using mapview to evaluate how many sharks were observed on land I have decided to remove brown shysharks from the analysis as 5 out of 8 observations were "land sharks" ##

# Reading in data from iNaturalist
puffyshark <- get_inat_obs(taxon_name = "Haploblepharus edwardsii", maxresults = 600)
darkshark <- get_inat_obs(taxon_name = "Haploblepharus pictus", maxresults = 600)


# Filtering observations to research grade
puffyshark <- puffyshark %>% filter(positional_accuracy < 100 & latitude <0 & !is.na(latitude) & quality_grade == "research")
darkshark <- darkshark %>% filter(positional_accuracy < 100 & latitude <0 & !is.na(latitude) & quality_grade == "research")

# Make the dataframes spatial objects of class = "sf"
puffyshark <- st_as_sf(puffyshark, coords = c("longitude", "latitude"), crs = 4326)
darkshark <- st_as_sf(darkshark, coords = c("longitude", "latitude"), crs = 4326)

# Check the class
class(puffyshark)
class(darkshark) # all gucci
# Check the CRS
st_crs(puffyshark)
st_crs(darkshark) # yay! all of them are 4326!

# Some more filtering -> I want to exclude observations of *sharks* made *on land* (shoutout to ChatGPT for the help)
# Getting land area (to exclude observations on land)
world_land <- ne_countries(scale = "medium", returnclass = "sf", country = "south africa")

# Spatial join to ID observations on land for puffies
landsharks_ps <- st_join(puffyshark, world_land, join = st_disjoint) %>%
  filter(!is.na(iso_a2))
# Exclude land shark observations from puffyshark
oceanshark_ps <- puffyshark[puffyshark$geometry %in% landsharks_ps$geometry,]
# Spatial join to ID observations on land for darksharks
landsharks_ds <- st_join(darkshark, world_land, join = st_disjoint) %>%
  filter(!is.na(iso_a2))
# Exclude land shark observations from darkshark
oceanshark_ds <- darkshark[darkshark$geometry %in% landsharks_ds$geometry,]

# Plot these bois
ggplot() + 
  annotation_map_tile(type = "osm", progress = "none") + 
  geom_sf(data = oceanshark_ps, color = "#85B22C")
ggplot() + 
  annotation_map_tile(type = "osm", progress = "none") + 
  geom_sf(data = oceanshark_ds, color = "#422CB2")

# Let's try overlay these data
ggplot() + 
  annotation_map_tile(type = "osm", progress = "none") + 
  geom_sf(data = oceanshark_ps, color = "#85B22C") + 
  geom_sf(data = oceanshark_ds, color = "#422CB2")

# Interactive maps
leaflet() %>% 
  addTiles(group = "Default") %>%
  addCircleMarkers(data = oceanshark_ps, 
                   group = "Haploblepharus edwardsii", 
                   radius = 1, 
                   color = "#85B22C")
leaflet() %>%
  addTiles(group = "Default") %>%
  addCircleMarkers(data = oceanshark_ds, 
                   group = "Haploblepharus pictus", 
                   radius = 1, 
                   color = "#422CB2")

# Interactive map for both species
leaflet() %>% 
  addTiles(group = "Default") %>%
  addCircleMarkers(data = oceanshark_ps, 
                   group = "Haploblepharus edwardsii", 
                   radius = 1, 
                   color = "#85B22C")%>%
  addCircleMarkers(data = oceanshark_ds, 
                   group = "Haploblepharus pictus", 
                   radius = 1, 
                   color = "#422CB2")%>% 
  addLegend(position = "topright", colors = c("#85B22C", "#422CB2"), labels = c("Puffadder shyshark", "Dark shyshark"))


# Creating dataframes with links to iNaturalist observations
loceanshark_ps <- oceanshark_ps %>%
  mutate(click_url = paste("<b><a href='", url, "'>Link to iNat observation</a></b>"))
loceanshark_ds <- oceanshark_ds %>%
  mutate(click_url = paste("<b><a href='", url, "'>Link to iNat observation</a></b>"))

# Interactive map that has popup links to iNaturalist observations for both species
mapview(oceanshark_ps, col.regions = "#85B22C", layer.name = "Puffadder shyshark", legend = TRUE,
        popup = 
          popupTable(loceanshark_ps,
                     zcol = c("user_login", "click_url", "id"))) + 
mapview(oceanshark_ds, col.regions = "#422CB2", layer.name = "Dark shyshark", legend = TRUE,
        popup = 
          popupTable(loceanshark_ds, 
                     zcol = c("user_login", "click_url", "id")))
