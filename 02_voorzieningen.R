## Voorzieningen - Bioscopen

# zou je met ggplot2 het volgende kunnen doen:
# een histogram (x-as: afstand tot bioscoop, y-as: count), 
# input data is per buurtcode. Dan vervolgens de 4 (?) buurten in overvecht 
# waar de einsteindreef aan grenst uitlichten (met een geom_vline ofzo) om te 
# kijken of de afstand van deze buurten tot deze voorziening onder/bovengemiddeld is.

# wijk overvecht
# buurten https://www.utrecht.nl/fileadmin/uploads/documenten/wonen-en-leven/wijken/wijk-overvecht/plattegrond-Overvecht-met-buurtindeling.jpg
# Neckardreef, Zambesidreef, Vechtzoom-zuid, Zamenhofdreef

library(httr)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(sf)
library(stars)
library(ggspatial)
library(raster)
library(rjson)

# import data set

bioscoop <- read.csv("data/Nabijheid_voorzieningen__buurt_2019_24092021_143836.csv", sep = ";")

bios_afs <- bioscoop %>%
  dplyr::select(Wijken.en.buurten, Vrije.tijd.en.cultuur.Bioscoop.Afstand.tot.bioscoop..km.) %>%
    rename(afstand.km = Vrije.tijd.en.cultuur.Bioscoop.Afstand.tot.bioscoop..km.) %>%
      mutate(afstand.km = as.numeric(afstand.km))

einsteindreef_buurten <- bios_afs %>%
  filter(
    Wijken.en.buurten == "Neckardreef en omgeving" |
      Wijken.en.buurten == "Zambesidreef en omgeving" |
      Wijken.en.buurten == "Vechtzoom-zuid" |
      Wijken.en.buurten == "Zamenhofdreef en omgeving")

einsteindreef_buurten_afs <- as.numeric(unlist(einsteindreef_buurten[2]))

bios_afs_plot <-
  ggplot(bios_afs, aes(x = afstand.km)) + 
  geom_histogram(binwidth = 0.5, color="darkblue", fill="lightblue") +
  #geom_point() +
  labs(x = "Afstand tot bioscoop (km)", y = "Aantal") +
  geom_vline(xintercept = einsteindreef_buurten_afs, linetype = "solid", color = "red", size = .5) +
  geom_label(aes(einsteindreef_buurten_afs[3], Inf), label = "Einsteindreef", vjust = 2) +
  #geom_label(aes(einsteindreef_buurten_afs[1], Inf), label = einsteindreef_buurten[1,1], vjust = 2) +
  #geom_label(aes(einsteindreef_buurten_afs[2], Inf), label = einsteindreef_buurten[2,1], vjust = 2) +
  #geom_label(aes(einsteindreef_buurten_afs[3], Inf), label = einsteindreef_buurten[3,1], vjust = 2) +
  #geom_label(aes(einsteindreef_buurten_afs[4], Inf), label = einsteindreef_buurten[4,1], vjust = 2) +
  ylim(0, 28) +
  theme_minimal()

bios_afs_plot

##### Map

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
  st_buffer(3000)

roads_in_roi <- 
  roads_nl %>%
  st_filter(street_roi) %>% 
  st_crop(street_roi)

## data

req <- parse_url("https://ckan.dataplatform.nl/api/3/action/datastore_search")

req$query <- list(
  resource_id = "6a5c90d7-7836-4602-a6f4-e47885c0e567",
  limit = 100,
  offset = 0,
  q='Utrecht'
)

bios <- build_url(req)

bios <- jsonlite::fromJSON(bios, flatten=TRUE)
bios <- bios$result$records
bios

bios <- st_as_sf(bios, coords = c("latitude", "longitude"), crs = 28992, agr = "constant")

## plot

ggplot() +
  annotation_map_tile("cartolight", zoom = 15) +
  geom_sf(data = street, colour = "orange", size = 2) +
  geom_sf(data = roads_in_roi, colour = "black", size = .5) +
  #geom_sf(data = bios, size = 2, shape = 23, fill = "light seagreen") +
  theme_void() +
  annotation_scale(location = "br")

#geom_point(data = bios, aes(x = longitude, y = latitude), size = 3, shape = 13, fill = "darkred") +


