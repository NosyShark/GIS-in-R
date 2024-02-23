# This is the GIS in R workshop with Jasper
rm(list=ls())

install.packages(c("tidyverse", "sp", "raster", "sf", "lwgeom", "terra", "stars", "exactextractr"))
install.packages(c("cowplot", "hrbrthemes", "knitr", "leaflet", "htmltools", "rosm", "ggspatial", "rnaturalearth", "mapview", "tmap"))
install.packages("rmarkdown", "prettymapr", "leaflet", "htmltools", "rinat", "rnaturalearth")

my_packages <- c("tidyverse", "sp", "sf", "raster", "lwgeom", "terra", "stars", "exactextractr", "hrbrthemes","cowplot", "rinat", "prettymapr", "ggspatial", "leaflet", "mapview", "htmltools", "leafpop", "rnaturalearth")
lapply(my_packages, require, character.only = TRUE)

## After using mapview to evaluate how many sharks were observed on land I have decided to remove brownsharks from the analysis as 5 out of 8 were "land sharks" - the deciding code can be seen in line 97-103. I'm leaving the code in here though for future use

# Reading in data from iNaturalist
puffyshark <- get_inat_obs(taxon_name = "Haploblepharus edwardsii", maxresults = 600)
# brownshark <- get_inat_obs(taxon_name = "Haploblepharus fuscus", maxresults = 600)
darkshark <- get_inat_obs(taxon_name = "Haploblepharus pictus", maxresults = 600)


# Filtering observations to research grade
puffyshark <- puffyshark %>% filter(positional_accuracy < 10 & latitude <0 & !is.na(latitude) & quality_grade == "research")
#brownshark <- brownshark %>% filter(positional_accuracy < 10 & latitude <0 & !is.na(latitude) & quality_grade == "research")
darkshark <- darkshark %>% filter(positional_accuracy < 10 & latitude <0 & !is.na(latitude) & quality_grade == "research")

# Make the dataframes spatial objects of class = "sf"
puffyshark <- st_as_sf(puffyshark, coords = c("longitude", "latitude"), crs = 4326)
#brownshark <- st_as_sf(brownshark, coords = c("longitude", "latitude"), crs = 4326)
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

# Maybe even trying to layer this?
leaflet() %>% 
  addTiles(group = "Default") %>%
  addCircleMarkers(data = oceanshark_ps, 
                   group = "Haploblepharus edwardsii", 
                   radius = 1, 
                   color = "#85B22C")%>%
  addCircleMarkers(data = oceanshark_ds, 
                   group = "Haploblepharus pictus", 
                   radius = 1, 
                   color = "#422CB2")

# Interactive map where you can access links to iNat observations
loceanshark_ps <- oceanshark_ps %>%
  mutate(click_url = paste("<b><a href='", url, "'>Link to iNat observation</a></b>"))
mapview(oceanshark_ps, 
        popup = 
          popupTable(loceanshark_ps,
                     zcol = c("user_login", "click_url", "id")))
loceanshark_ds <- oceanshark_ds %>%
  mutate(click_url = paste("<b><a href='", url, "'>Link to iNat observation</a></b>"))
mapview(oceanshark_ds, 
        popup = 
          popupTable(loceanshark_ds,
                     zcol = c("user_login", "click_url", "id")))

# Layering the mapview maps to see if I can be both species on one map
mapview(oceanshark_ps, col.regions = "#85B22C", layer.name = "Puffadder shyshark", legend = TRUE,
        popup = 
          popupTable(loceanshark_ps,
                     zcol = c("user_login", "click_url", "id"))) + 
mapview(oceanshark_ds, col.regions = "#422CB2", layer.name = "Dark shyshark", legend = TRUE,
        popup = 
          popupTable(loceanshark_ds, 
                     zcol = c("user_login", "click_url", "id")))


# DO NOT RUN THIS!!! This is the code I used before filtering out the observations that were on land!! 
# Different interactive maps
lpuffyshark <- puffyshark %>%
  mutate(click_url = paste("<b><a href='", url, "'>Link to iNat observation</a></b>"))
mapview(puffyshark, 
        popup = 
          popupTable(lpuffyshark,
                     zcol = c("user_login", "click_url", "id")))
# Honourable mention to user_login pbsouthwood who clocked ~ 30 puffadder shysharks on land... I hope we never meet - have to remove a total of 55

#lbrownshark <- brownshark %>%
#mutate(click_url = paste("<b><a href='", url, "'>Link to iNat observation</a></b>"))
#mapview(brownshark, 
        #popup = 
          #popupTable(lbrownshark,
                     #zcol = c("user_login", "click_url", "id")))
# Have to remove 5 out of 8 ... will remove brown sharks all together

ldarkshark <- darkshark %>%
  mutate(click_url = paste("<b><a href='", url, "'>Link to iNat observation</a></b>"))

mapview(darkshark, 
        popup = 
          popupTable(ldarkshark,
                     zcol = c("user_login", "click_url", "id")))
# Have to remove 51 (out of 85) observations (of which pbsouthwood logged 38)
# This is the code for non-link mapview
mapview(puffyshark, 
        popup = 
          popupTable(puffyshark, 
                      zcol = c("user_login", "url")))
mapview(darkshark, 
        popup = 
          popupTable(darkshark, 
                     zcol = c("user_login", "url")))

