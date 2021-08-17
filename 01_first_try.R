library(tidyverse)
library(sf)
library(stars)

library(ggspatial)
library(raster)

library(httr)

roads_nl <- st_read("data/nwbwegen/geogegevens/shapefile/nederland_totaal/wegvakken/wegvakken.shp")

street <- 
  roads_nl %>% 
  filter(
    gme_naam == "Utrecht", 
    stt_naam == "Einsteindreef"
  )

# create 300m region of interest
street_roi <- 
  street %>% 
  st_geometry() %>% 
  st_combine() %>% 
  st_buffer(300)

roads_in_roi <- 
  roads_nl %>%
  st_filter(street_roi) %>% 
  st_crop(street_roi)




# example of WFS access
st_bbox(street_roi)
get_bag_pand <- function(bounding_box) {
  
}

# pand
req <- parse_url("https://geodata.nationaalgeoregister.nl/bag/wfs/v1_1")

req$query <- list(
  request = "getFeature",
  service = "WFS",
  version = "2.0.0",
  typenames = "bag:pand",
  srsName = "EPSG:28992",
  bbox    = paste(st_bbox(street_roi), collapse = ","), 
  count   = 5000
)


n_pand <- st_read(build_url(req))


pand <- st_read(build_url(req))


ggplot() +
  annotation_map_tile("cartolight", zoom = 15) +
  geom_sf(data = street, colour = "orange", size = 2) +
  geom_sf(data = roads_in_roi, colour = "black", size = 1) +
  geom_sf(data = pand, fill = "light seagreen") +
  #geom_sf(data = verblijfsobject, fill = "light seagreen") +
  theme_void() +
  annotation_scale(location = "br")
