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

# Load files Netwerkgegevens
#hectointervallen <- read_csv("data/Bron/Netwerkgegevens/hectointervallen.txt")
#hectopunten <- read_csv("data/Bron/Netwerkgegevens/hectopunten.txt")
#junctiehectometrering <- read_csv("data/Bron/Netwerkgegevens/junctiehectometrering.txt")
#juncties <- read_csv("data/Bron/Netwerkgegevens/juncties.txt")
puntlocaties <- read_csv("data/Bron/Netwerkgegevens/puntlocaties.txt")
#wegvakken <- read_csv("data/Bron/Netwerkgegevens/wegvakken.txt")

# Load files Ongevallengegevens
ongevallen <- read_csv("data/Bron/Ongevallengegevens/ongevallen.txt")
#partijen <- read_csv("data/Bron/Ongevallengegevens/partijen.txt")

# Load files ReferentiebestandenNetwerk
#ref_netwerk_rel <- read_csv("data/Bron/ReferentiebestandenNetwerk/relatieveposities.txt")
#ref_netwerk_baan <- read_csv("data/Bron/ReferentiebestandenNetwerk/baansubsoorten.txt")

# Load files ReferentiebestandenOngevallen
aardongevallen <- read_csv("data/Bron/ReferentiebestandenOngevallen/aardongevallen.txt")
aflopen3 <- read_csv("data/Bron/ReferentiebestandenOngevallen/aflopen3.txt")

### Nieuwe bestand

ongevallen_fixed <- ongevallen %>%
  left_join(aardongevallen) %>%
  left_join(aflopen3) %>%
  left_join(puntlocaties) %>%
  dplyr::select(VKL_NUMMER, JAAR_VKL, X_COORD, Y_COORD, FK_VELD5, JTE_ID, WVK_ID, HECTOMETER, ANTL_PTJ, AP3_OMS, AOL_OMS, NIVEAUKOP)



# Map -------------------------------------------------------

# make coordinates into geom_sf

ggplot() +
  annotation_map_tile("cartolight", zoom = 15) +
  geom_sf(data = street, colour = "orange", size = 2) +
  geom_sf(data = roads_in_roi, colour = "black", size = 1) +
  geom_sf(data = ongevallen_fixed, colour = "blue")
  #geom_raster(data = test) +
  #geom_sf(data = gd, colour = "blue", size = 0.3) +
  theme_void() +
  annotation_scale(location = "br")


