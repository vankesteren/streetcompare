## NWA Project
## Distributieplot voorzieningen
## Bioscopen

# Wijk 04 Overvecht 
# https://www.utrecht.nl/fileadmin/uploads/documenten/wonen-en-leven/wijken/wijk-overvecht/plattegrond-Overvecht-met-buurtindeling.jpg

# Buurten Einstendreef
# CBS buurtcode   Buurtnaam
# BU03440321      Zamenhofdreef en omgeving
# BU03440322      Neckardreef en omgeving
# BU03440331      Vechtzoom-zuid
# BU03440341      Zambesidreef en omgeving

# Load packages
library(httr)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(sf)
library(stars)
#library(ggspatial)
#library(raster)
#library(rjson)
library(cbsodataR)  # CBS data package

# Nabijheid voorzieningen; afstand locatie, wijk- en buurtcijfers 2019
# Downloaden van selectie van data
# AfstandTotBioscoop_104

voorzieningen <- cbs_get_data("84463NED", select = c("Gemeentenaam_1","SoortRegio_2", "WijkenEnBuurten", "AfstandTotBioscoop_104")) 
head(voorzieningen)

# koppel CBS buurtcode met Buurtnaam

# filter voor Urecht en alleen buurten

voorzieningen_nl <- voorzieningen %>%
  filter(
    str_detect(SoortRegio_2, "Buurt")
  )

utrecht_bios <- voorzieningen %>%
  filter(
    str_detect(Gemeentenaam_1, "Utrecht") &
    str_detect(WijkenEnBuurten, "BU034") &
    str_detect(SoortRegio_2, "Buurt"))

einsteindreef_buurten <- utrecht_bios %>%
  filter(
    WijkenEnBuurten == "BU03440321" |
      WijkenEnBuurten == "BU03440322" |
      WijkenEnBuurten == "BU03440331" |
      WijkenEnBuurten == "BU03440341") 

einsteindreef_buurten$Gemeentenaam_1 <- c(einsteindreef_buurten$Gemeentenaam_1)
einsteindreef_buurten$WijkenEnBuurten <- c(einsteindreef_buurten$WijkenEnBuurten)
einsteindreef_buurten$SoortRegio_2 <- c(einsteindreef_buurten$SoortRegio_2)
einsteindreef_buurten$AfstandTotBioscoop_104 <- c(einsteindreef_buurten$AfstandTotBioscoop_104)

# # recode buurtcode voor naam
# einsteindreef_buurten <- einsteindreef_buurten %>%
#   mutate(WijkenEnBuurten == recode(WijkenEnBuurten,
#                                    "BU03440321" = "Zamenhofdreef",
#                                    "BU03440322" = "Neckardreef",
#                                    "BU03440331" = "Vechtzoom-zuid",
#                                    "BU03440341" = "Zambesidreef"))


utr_bios_afs_plot <-
  ggplot(utrecht_bios, aes(x = AfstandTotBioscoop_104)) + 
  geom_histogram(binwidth = 0.5, colour = "darkblue", fill="lightblue") +
  geom_vline(data = einsteindreef_buurten, aes(xintercept= AfstandTotBioscoop_104, colour = WijkenEnBuurten), linetype = "solid", show.legend = FALSE) +
  #geom_label(aes(3.3, 20), label = "Einsteindreef", vjust = 0) +
  #geom_text(data = einsteindreef_buurten, aes(label = c(WijkenEnBuurten), y = 23), angle = 45) +
  geom_text(aes(einsteindreef_buurten$AfstandTotBioscoop_104[1], 22), label = "Zamenhofdreef", colour = "orange", angle = 45) +
  geom_text(aes(einsteindreef_buurten$AfstandTotBioscoop_104[2], 15), label = "Neckardreef",  colour = "darkgreen", vjust = 1, angle = 45) +
  geom_text(aes(einsteindreef_buurten$AfstandTotBioscoop_104[3], 17), label = "Vechtzoom-zuid", colour = "darkgreen", vjust = 1, angle = 45) +
  geom_text(aes(einsteindreef_buurten$AfstandTotBioscoop_104[4], 23), label = "Zambesidreef", colour = "purple", vjust = 1, angle = 45) +
  ylim(0, 25) +
  labs(x = "Afstand tot bioscoop (km)", y = "Aantal") +
  theme_minimal()

utr_bios_afs_plot
ggsave(utr_bios_afs_plot, file = "fig/utrect_bios_plot.jpg")

## Nederland, alleen buurten
bios_afs_plot <-
  ggplot(voorzieningen_nl, aes(x = AfstandTotBioscoop_104)) + 
  geom_histogram(binwidth = 2, colour = "darkblue", fill="lightblue") +
  geom_vline(data = einsteindreef_buurten, aes(xintercept= AfstandTotBioscoop_104, colour = WijkenEnBuurten), linetype = "solid", show.legend = FALSE) +
  #geom_label(aes(3.3, 20), label = "Einsteindreef", vjust = 0) +
  #geom_text(data = einsteindreef_buurten, aes(label = c(WijkenEnBuurten), y = 23), angle = 45) +
  geom_text(aes(einsteindreef_buurten$AfstandTotBioscoop_104[1], 2000), label = "Zamenhofdreef", colour = "orange", angle = 45) +
  geom_text(aes(einsteindreef_buurten$AfstandTotBioscoop_104[2], 2200), label = "Neckardreef",  colour = "darkgreen", vjust = 1, angle = 45) +
  geom_text(aes(einsteindreef_buurten$AfstandTotBioscoop_104[3], 2400), label = "Vechtzoom-zuid", colour = "darkgreen", vjust = 1, angle = 45) +
  geom_text(aes(einsteindreef_buurten$AfstandTotBioscoop_104[4], 2600), label = "Zambesidreef", colour = "purple", vjust = 1, angle = 45) +
  #ylim(0, 25) +
  labs(x = "Afstand tot bioscoop (km)", y = "Aantal") +
  theme_minimal()

bios_afs_plot
ggsave(bios_afs_plot, file = "fig/nl_bios_plot.jpg")

## TODO: 
## Fix geom_text om buurtnaam in te laden
## fix recode/mutate code

# Erik-Jan doet dit

# stedelijkheid
buurtstats <- read_sf("data/WijkBuurtkaart_2020_v1/buurt_2020_v1.shp")

# connect stedelijkheid met buurtstatistieken
sted_df <- 
  voorzieningen %>% 
  dplyr::filter(SoortRegio_2 == "Buurt     ") %>% 
  mutate(BU_CODE = WijkenEnBuurten) %>% 
  left_join(buurtstats %>% dplyr::select(BU_CODE, STED)) %>% 
  dplyr::filter(STED == 1) # selecteer alleen stedelijke gebieden zoals overvecht

sted_df %>% 
  ggplot(aes(x = AfstandTotBioscoop_104)) + 
  geom_histogram() + 
  geom_vline(xintercept = median(sted_df$AfstandTotBioscoop_104, na.rm = TRUE))


########################################
# old code inladen van tabel
########################################

# inladen van csv van tabel
# https://www.cbs.nl/nl-nl/maatwerk/2020/24/nabijheid-voorzieningen-buurtcijfers-2019

voorzieningen_tbl <- read.csv("data/Nabijheid_voorzieningen__buurt_2019_28092021_140430.csv", sep = ";")
#bioscoop <- read.csv("data/Nabijheid_voorzieningen__buurt_2019_24092021_143836.csv", sep = ";")

voorzieningen_tbl <- voorzieningen_tbl %>%
  filter(str_detect(Regioaanduiding.Soort.regio..omschrijving., "Buurt"))

bios_afs <- voorzieningen_tbl %>%
  select(Wijken.en.buurten, Vrije.tijd.en.cultuur.Bioscoop.Afstand.tot.bioscoop..km.) %>%
    rename(afstandTotBiosKm = Vrije.tijd.en.cultuur.Bioscoop.Afstand.tot.bioscoop..km.)

bios_afs$afstandTotBiosKm <- bios_afs$afstandTotBiosKm %>% 
  factor %>% str_replace(',', '.') %>% as.numeric

einsteindreef_buurten_afs <- bios_afs %>%
  filter(
    Wijken.en.buurten == "Neckardreef en omgeving" |
      Wijken.en.buurten == "Zambesidreef en omgeving" |
      Wijken.en.buurten == "Vechtzoom-zuid" |
      Wijken.en.buurten == "Zamenhofdreef en omgeving")

bios_afs_plot2 <-
  ggplot(bios_afs, aes(x = afstandTotBiosKm)) + 
  geom_histogram(binwidth = 0.5, color="darkblue", fill="lightblue") +
  labs(x = "Afstand tot bioscoop (km)", y = "Aantal") +
  geom_vline(data = einsteindreef_buurten_afs, aes(xintercept = afstandTotBiosKm, colour = Wijken.en.buurten), linetype = "solid", size = .5, show.legend = FALSE) +
  #geom_label(aes(einsteindreef_buurten_afs$afstandTotBiosKm[3], 25), label = "Einsteindreef", vjust = 2) +
  geom_text(aes(einsteindreef_buurten_afs$afstandTotBiosKm[1], 22), label = einsteindreef_buurten_afs$Wijken.en.buurten[1], vjust = 1, angle = 45) +
  geom_text(aes(einsteindreef_buurten_afs$afstandTotBiosKm[2], 15), label = einsteindreef_buurten_afs$Wijken.en.buurten[2], vjust = 1, angle = 45) +
  geom_text(aes(einsteindreef_buurten_afs$afstandTotBiosKm[3], 17), label = einsteindreef_buurten_afs$Wijken.en.buurten[3], vjust = 1, angle = 45) +
  geom_text(aes(einsteindreef_buurten_afs$afstandTotBiosKm[4], 22), label = einsteindreef_buurten_afs$Wijken.en.buurten[4], vjust = 1, angle = 45) +
  ylim(0, 25) +
  theme_minimal()

bios_afs_plot2
ggsave(bios_afs_plot2, file = "fig/bios_plot_vantabel.jpg")









########################################
##### Map
########################################


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
# street_roi <- 
#   street %>% 
#   st_geometry() %>% 
#   st_combine() %>% 
#   st_buffer(3000)

# roads_in_roi <- 
#   roads_nl %>%
#   st_filter(street_roi) %>% 
#   st_crop(street_roi)

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
  #geom_sf(data = roads_in_roi, colour = "black", size = .5) +
  #geom_sf(data = bios, size = 2, shape = 23, fill = "light seagreen") +
  theme_void() +
  annotation_scale(location = "br")

#geom_point(data = bios, aes(x = longitude, y = latitude), size = 3, shape = 13, fill = "darkred") +


