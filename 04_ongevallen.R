# Load packages --------------------------------------------------
library(tidyverse)
library(sf)
library(stars)

# Load data ------------------------------------------------------

  #2020
  ongevallen_2020 <- read_csv("data/Bron/2020/Ongevallengegevens/ongevallen.txt")
  
  partijen_2020 <- read_csv("data/Bron/2020/Ongevallengegevens/partijen.txt")
  aardongevallen_2020 <- read_csv("data/Bron/2020/ReferentiebestandenOngevallen/aardongevallen.txt")
  aflopen3_2020 <- read_csv("data/Bron/2020/ReferentiebestandenOngevallen/aflopen3.txt")
  objecttypes_2020 <- read_csv("data/Bron/2020/ReferentiebestandenOngevallen/objecttypes.txt")
  puntlocaties_2020 <- read_csv("data/Bron/2020/Netwerkgegevens/puntlocaties.txt")
  wegvakken_2020 <- read_csv("data/Bron/2020/Netwerkgegevens/wegvakken.txt")
  wegvakgeo_2020 <- st_read("data/Bron/2020/Netwerkgegevens/wegvakgeografie.shp")
  
  ongevallen_fixed_2020 <- ongevallen_2020 %>%
    left_join(aardongevallen_2020) %>%
    left_join(aflopen3_2020) %>%
    left_join(puntlocaties_2020) %>%
    left_join(partijen_2020) %>%
    left_join(objecttypes_2020) %>%
    left_join(wegvakken_2020) %>%
    left_join(wegvakgeo_2020) %>%
    dplyr::select(VKL_NUMMER, JAAR_VKL, X_COORD, Y_COORD, geometry, ANTL_PTJ, OTE_OMS, AP3_OMS, AOL_OMS, NIVEAUKOP)
  
  #2019
  ongevallen_2019 <- read_csv("data/Bron/2019/Ongevallengegevens/ongevallen.txt")
  
  partijen_2019 <- read_csv("data/Bron/2019/Ongevallengegevens/partijen.txt")
  aardongevallen_2019 <- read_csv("data/Bron/2019/ReferentiebestandenOngevallen/aardongevallen.txt")
  aflopen3_2019 <- read_csv("data/Bron/2019/ReferentiebestandenOngevallen/aflopen3.txt")
  objecttypes_2019 <- read_csv("data/Bron/2019/ReferentiebestandenOngevallen/objecttypes.txt")
  puntlocaties_2019 <- read_csv("data/Bron/2019/Netwerkgegevens/puntlocaties.txt")
  wegvakken_2019 <- read_csv("data/Bron/2019/Netwerkgegevens/wegvakken.txt")
  wegvakgeo_2019 <- st_read("data/Bron/2019/Netwerkgegevens/wegvakgeografie.shp")
  
  ongevallen_fixed_2019 <- ongevallen_2019 %>%
    left_join(aardongevallen_2019) %>%
    left_join(aflopen3_2019) %>%
    left_join(puntlocaties_2019) %>%
    left_join(partijen_2019) %>%
    left_join(objecttypes_2019) %>%
    left_join(wegvakken_2019) %>%
    left_join(wegvakgeo_2019) %>%
    dplyr::select(VKL_NUMMER, JAAR_VKL, X_COORD, Y_COORD, geometry, ANTL_PTJ, OTE_OMS, AP3_OMS, AOL_OMS, NIVEAUKOP)
  
  #2018
  ongevallen_2018 <- read_csv("data/Bron/2018/Ongevallengegevens/ongevallen.txt")
  
  partijen_2018 <- read_csv("data/Bron/2018/Ongevallengegevens/partijen.txt")
  aardongevallen_2018 <- read_csv("data/Bron/2018/ReferentiebestandenOngevallen/aardongevallen.txt")
  aflopen3_2018 <- read_csv("data/Bron/2018/ReferentiebestandenOngevallen/aflopen3.txt")
  objecttypes_2018 <- read_csv("data/Bron/2018/ReferentiebestandenOngevallen/objecttypes.txt")
  puntlocaties_2018 <- read_csv("data/Bron/2018/Netwerkgegevens/puntlocaties.txt")
  wegvakken_2018 <- read_csv("data/Bron/2018/Netwerkgegevens/wegvakken.txt")
  wegvakgeo_2018 <- st_read("data/Bron/2018/Netwerkgegevens/wegvakgeografie.shp")
  
  ongevallen_fixed_2018 <- ongevallen_2018 %>%
    left_join(aardongevallen_2018) %>%
    left_join(aflopen3_2018) %>%
    left_join(puntlocaties_2018) %>%
    left_join(partijen_2018) %>%
    left_join(objecttypes_2018) %>%
    left_join(wegvakken_2018) %>%
    left_join(wegvakgeo_2018) %>%
    dplyr::select(VKL_NUMMER, JAAR_VKL, X_COORD, Y_COORD, geometry, ANTL_PTJ, OTE_OMS, AP3_OMS, AOL_OMS, NIVEAUKOP)
  
  #2017
  ongevallen_2017 <- read_csv("data/Bron/2017/Ongevallengegevens/ongevallen.txt")
  
  partijen_2017 <- read_csv("data/Bron/2017/Ongevallengegevens/partijen.txt")
  aardongevallen_2017 <- read_csv("data/Bron/2017/ReferentiebestandenOngevallen/aardongevallen.txt")
  aflopen3_2017 <- read_csv("data/Bron/2017/ReferentiebestandenOngevallen/aflopen3.txt")
  objecttypes_2017 <- read_csv("data/Bron/2017/ReferentiebestandenOngevallen/objecttypes.txt")
  puntlocaties_2017 <- read_csv("data/Bron/2017/Netwerkgegevens/puntlocaties.txt")
  wegvakken_2017 <- read_csv("data/Bron/2017/Netwerkgegevens/wegvakken.txt")
  wegvakgeo_2017 <- st_read("data/Bron/2017/Netwerkgegevens/wegvakgeografie.shp")
  
  ongevallen_fixed_2017 <- ongevallen_2017 %>%
    left_join(aardongevallen_2017) %>%
    left_join(aflopen3_2017) %>%
    left_join(puntlocaties_2017) %>%
    left_join(partijen_2017) %>%
    left_join(objecttypes_2017) %>%
    left_join(wegvakken_2017) %>%
    left_join(wegvakgeo_2017) %>%
    dplyr::select(VKL_NUMMER, JAAR_VKL, X_COORD, Y_COORD, geometry, ANTL_PTJ, OTE_OMS, AP3_OMS, AOL_OMS, NIVEAUKOP)

ongevallen_2017_2020 <- union_all(ongevallen_fixed_2017, ongevallen_fixed_2018, ongevallen_fixed_2019, ongevallen_fixed_2020)

# # Filter Einsteindreef of Utrrecht?
# # Niet gelukt, werkt nog niet
# # st_filter
# ## Load shapefile of all roads in NL (Nationaal Wegen Bestand (NWB))
# roads_nl <- st_read("data/nwbwegen/geogegevens/shapefile/nederland_totaal/wegvakken/wegvakken.shp")
# 
# ## Filter for municipality (gme_naam) and road (stt_name) we are interested in
# street <- 
#   roads_nl %>% 
#   filter(
#     gme_naam == "Utrecht", 
#     stt_naam == "Einsteindreef"
#   )
# 
# ## Create 300m region of interest
# street_roi <- 
#   street %>% 
#   st_geometry() %>% 
#   st_combine() %>% 
#   st_buffer(300)
# 
# #ongevallen_2017_2020_roi
# ongevallen_fixed_2020_roi <- 
#   ongevallen_fixed_2020 %>%
#     st_filter(street_roi) %>% 
#     st_crop(street_roi)  
    
# Write new file
st_write(ongevallen_2017_2020, "data/output/ongevallen_parsed_2017_2020.shp")
st_write(ongevallen_2017_2020, "data/output/ongevallen_parsed_2017_2020.geojson")
save(ongevallen_2017_2020, file = "data/output/ongevallen_parsed_2017_2020.RData")

# Load data from 2011 to 2020 ------------------------------------------------------

#2011 tot 2020
#https://data.overheid.nl/dataset/9841-verkeersongevallen---bestand-geregistreerde-ongevallen-nederland#panel-resources

ongevallen_2011_2020 <- read_csv("data/Bron/01-01-2011_31-12-2020/Ongevallengegevens/ongevallen.txt")

partijen_2011_2020 <- read_csv("data/Bron/01-01-2011_31-12-2020/Ongevallengegevens/partijen.txt")
aardongevallen_2011_2020 <- read_csv("data/Bron/01-01-2011_31-12-2020/ReferentiebestandenOngevallen/aardongevallen.txt")
aflopen3_2011_2020 <- read_csv("data/Bron/01-01-2011_31-12-2020/ReferentiebestandenOngevallen/aflopen3.txt")
objecttypes_2011_2020 <- read_csv("data/Bron/01-01-2011_31-12-2020/ReferentiebestandenOngevallen/objecttypes.txt")
puntlocaties_2011_2020 <- read_csv("data/Bron/01-01-2011_31-12-2020/Netwerkgegevens/puntlocaties.txt")
wegvakken_2011_2020 <- read_csv("data/Bron/01-01-2011_31-12-2020/Netwerkgegevens/wegvakken.txt")
wegvakgeo_2011_2020 <- st_read("data/Bron/01-01-2011_31-12-2020/Netwerkgegevens/wegvakgeografie.shp")

ongevallen_2011_2020 <- ongevallen_2011_2020 %>%
  left_join(aardongevallen_2011_2020) %>%
  left_join(aflopen3_2011_2020) %>%
  left_join(puntlocaties_2011_2020) %>%
  left_join(partijen_2011_2020) %>%
  left_join(objecttypes_2011_2020) %>%
  left_join(wegvakken_2011_2020) %>%
  left_join(wegvakgeo_2011_2020) %>%
  dplyr::select(VKL_NUMMER, JAAR_VKL, X_COORD, Y_COORD, geometry, ANTL_PTJ, OTE_OMS, AP3_OMS, AOL_OMS, NIVEAUKOP)

# Write new file 2011-2020
st_write(ongevallen_2011_2020, "data/output/ongevallen_parsed_2011_2020.shp")
st_write(ongevallen_2011_2020, "data/output/ongevallen_parsed_2011_2020.geojson")
save(ongevallen_2011_2020, file = "data/output/ongevallen_parsed_2011_2020.RData")

## Documentatie over inhoud in bestand
# VKL_NUMMER - Unieke identificatie van het verkeersongeval, waarbij de partij betrokken is
# JAAR_VKL - Jaar waarin het verkeersongeval heeft plaatsgevonden
# X_COORD - De x-coördinaat van een puntlocatie, in verschoven rijksdriehoekmeting 
# Y_COORD - De y-coördinaat van een puntlocatie, in verschoven rijksdriehoekmeting
# geometry - simple feature linestring
# ANTL_PTJ - Het aantal partijen betrokken bij het verkeersongeval
# OTE_OMS - Omschrijving van een objecttype (OTE_ID)
# AP3_OMS - Omschrijving van een afloop 3 categorieën (AP3_ID)
# AOL_OMS - Omschrijving van een aard ongeval
# NIVEAUKOP - Aanduiding op welk niveau het verkeersongeval gekoppeld is aan het NWB (voor uitleg zie bijlage A):
# E = Ongeval exact gekoppeld aan NWB
# K = Ongeval gekoppeld op kruispuntniveau
# S = Ongeval gekoppeld op straatniveau
# G = Ongeval gekoppeld op gemeente niveau


#############################################
#############################################
# function to join and select info
file_cleanup <- function(dataset, year) {
  ongevallen_fixed <- dataset %>%
    left_join(partijen_year) %>%
    left_join(aardongevallen_year) %>%
    left_join(aflopen3_year) %>%
    left_join(objecttypes_year) %>%
    left_join(puntlocaties_year) %>%
    left_join(wegvakken_year) %>%
    left_join(wegvakgeo_year) %>%
    dplyr::select(VKL_NUMMER, JAAR_VKL, X_COORD, Y_COORD, geometry, ANTL_PTJ, OTE_OMS, AP3_OMS, AOL_OMS, NIVEAUKOP)
  return(ongevallen_fixed)
}
