# Load packages --------------------------------------------------
library(tidyverse)
library(sf)
library(stars)

library(ggspatial)
library(raster)

library(httr)

library(rgdal)

# Load data ------------------------------------------------------

## Load shapefile of all roads in NL (Nationaal Wegen Bestand (NWB))
roads_nl <- st_read("data/nwbwegen/geogegevens/shapefile/nederland_totaal/wegvakken/wegvakken.shp")

## Filter for municipality (gme_naam) and road (stt_name) we are interested in
street <- 
  roads_nl %>% 
  filter(
    gme_naam == "Utrecht", 
    stt_naam == "Einsteindreef"
  )

## Create 300m region of interest
street_roi <- 
  street %>% 
  st_geometry() %>% 
  st_combine() %>% 
  st_buffer(300)

roads_in_roi <- 
  roads_nl %>%
  st_filter(street_roi) %>% 
  st_crop(street_roi)

# WFS -----------------------------------------

# Example of WFS access ------------------------------------------

## Get pand from BAG (Basisregistratie Adressen en Gebouwen)

req <- parse_url("https://geodata.nationaalgeoregister.nl/bag/wfs/v1_1")
req$query <- list(
  request = "getFeature",
  service = "WFS",
  version = "2.0.0",
  typenames = "bag:pand",
  srsName = "EPSG:28992",
  bbox    = paste(st_bbox(street_roi), collapse = ","), 
  startIndex = 1001,
  count = 800
)
n_pand <- st_read(build_url(req))
pand <- st_read(build_url(req))

# Get verblijfsobject from BAG

req <- parse_url("https://geodata.nationaalgeoregister.nl/bag/wfs/v1_1")
req$query <- list(
  request = "getFeature",
  service = "WFS",
  version = "2.0.0",
  typenames = "bag:verblijfsobject",
  srsName = "EPSG:28992",
  bbox    = paste(st_bbox(street_roi), collapse = ",")
)

n_verblijfsobject <- st_read(build_url(req))
verblijfsobject <- st_read(build_url(req))

# WFS Function -------------------------------------------------

get_wfs_features <- function(index, bounding_box, wfs_url, featuretype) {
  req <- parse_url(wfs_url)
  
  req$query <- list(
    request = "getFeature",
    service = "WFS",
    version = "2.0.0",
    typenames = featuretype,
    srsName = "EPSG:28992",
    bbox    = paste(st_bbox(bounding_box), collapse = ","),
    startIndex=index,
    count=1000
  )
  
  gd <- st_read(build_url(req))
  return(gd)
}

pand1 <- get_wfs_features(0, street_roi, "https://geodata.nationaalgeoregister.nl/bag/wfs/v1_1", "bag:pand")
pand2 <- get_wfs_features(1000, street_roi, "https://geodata.nationaalgeoregister.nl/bag/wfs/v1_1", "bag:pand")

# WFS function with pagination ---------------------------------

get_panden <- function(index = 0) {
  wfs <- "https://geodata.nationaalgeoregister.nl/bag/wfs/v1_1"
  
  req <- parse_url(wfs)
  
  req$query <- list(
    request = "getFeature",
    service = "WFS",
    version = "2.0.0",
    typenames = "bag:pand",
    srsName = "EPSG:28992",
    bbox    = paste(st_bbox(street_roi), collapse = ","),
    startIndex=index,
    count=1000
  )
  
  panden <- st_read(build_url(req))
  return(panden)
}

panden <- map_dfr(.x = seq(0, 4000, 1000),
              .f = get_panden
)

panden

# WMS ------------------------------------------------------

# Example of WMS access ------------------------------------

library(leaflet)

# Get geluidsdata from RIVM
wms_rivm <- "http://geodata.rivm.nl/geoserver/alo/wms?"

wms_geluid <- leaflet() %>% 
  setView(lng = 5.110368840441155, lat = 52.11727569662975, zoom = 15) %>% 
  addWMSTiles(
    wms_rivm,
    layers = "rivm_20191112_g_geluidkaart_lden_wegverkeer",
    options = WMSTileOptions(format = "image/png", transparent = TRUE)
  )

wms_geluid

# WMS function ---------------------------------------------


# Map -------------------------------------------------------

ggplot() +
  annotation_map_tile("cartolight", zoom = 15) +
  geom_sf(data = street, colour = "orange", size = 2) +
  geom_sf(data = roads_in_roi, colour = "black", size = 1) +
  geom_sf(data = panden, fill = "light seagreen") +
  #geom_raster(data = test) +
  #geom_sf(data = gd, colour = "blue", size = 0.3) +
  theme_void() +
  annotation_scale(location = "br")


