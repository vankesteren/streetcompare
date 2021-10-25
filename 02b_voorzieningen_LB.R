library(httr)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(sf)
library(stars)
library(cbsodataR) 

metadata <- cbs_get_meta("84463NED") # handig voor de variabelenamen
print(metadata$DataProperties$Key)

# AfstandTotAttractie_110
# AfstandTotBioscoop_104
# AfstandTotPoppodium_103
# AfstandTotPodiumkunstenTotaal_99
#AfstandTotZwembad_93
#AfstandTotBibliotheek_92
#AfstandTotMuseum_95
#AfstandTotHotelED_48
#AfstandTotRestaurant_44
#AfstandTotCafetariaED_40
#AfstandTotCafeED_36
#AfstandTotWarenhuis_32

# AfstandTotBelangrijkOverstapstation_91
# AfstandTotTreinstationsTotaal_90
# AfstandTotOpritHoofdverkeersweg_89

data <- cbs_get_data("84463NED", select = c("Gemeentenaam_1",
                                            "SoortRegio_2", 
                                            "WijkenEnBuurten",
                                            "AfstandTotWarenhuis_32",
                                            "AfstandTotCafeED_36",
                                            "AfstandTotCafetariaED_40",
                                            "AfstandTotRestaurant_44",
                                            "AfstandTotHotelED_48",
                                            "AfstandTotOpritHoofdverkeersweg_89",
                                            "AfstandTotTreinstationsTotaal_90",
                                            "AfstandTotBelangrijkOverstapstation_91",
                                            "AfstandTotMuseum_95",
                                            "AfstandTotBibliotheek_92",
                                            "AfstandTotZwembad_93",
                                            "AfstandTotPodiumkunstenTotaal_99",
                                            "AfstandTotPoppodium_103",
                                            "AfstandTotBioscoop_104",
                                            "AfstandTotAttractie_110")) %>%
  mutate(WijkenEnBuurten = str_trim(WijkenEnBuurten)) %>%
  filter(str_detect(SoortRegio_2, "Buurt"))

buurtstats <- read_sf("data/WijkBuurtkaart_2020_v1/buurt_2020_v1.shp")

data_sted <- 
  data %>%
  left_join(buurtstats, by = c(WijkenEnBuurten = "BU_CODE"))  %>%
  filter(STED == 1)

data_utrecht <- 
  data %>%
  mutate(WijkenEnBuurten = str_trim(WijkenEnBuurten),
         Gemeentenaam_1 = str_trim(Gemeentenaam_1)) %>%
  filter(Gemeentenaam_1 == "Utrecht")

buurten <- data %>%
  filter(
      WijkenEnBuurten == "BU03440321" |
      WijkenEnBuurten == "BU03440322" |
      WijkenEnBuurten == "BU03440331" |
      WijkenEnBuurten == "BU03440341")

#### Voorzieningen grafieken

### AfstandTotBioscoop_104  
         
## AfstandTotBioscoop_104 buurten met zelfde stedelijkheid
bios_sted <-
  ggplot(data_sted, aes(x = AfstandTotBioscoop_104)) + 
  geom_histogram(binwidth = 0.5, colour = "black", fill = "navajowhite") +
  geom_vline(data = buurten, 
             aes(xintercept = AfstandTotBioscoop_104, 
                 colour = WijkenEnBuurten), color = c("orangered4", "orangered", "orange2", "seagreen4"),
             linetype = "solid", show.legend = FALSE, size = 2) +
  #geom_text(data = buurten, aes(label = c(WijkenEnBuurten), y = 23), angle = 45) +
  geom_text(aes(buurten$AfstandTotBioscoop_104[1], 260), label = "Zamenhofdreef", colour = "orangered4", angle = 45) +
  geom_text(aes(buurten$AfstandTotBioscoop_104[2], 240), label = "Neckardreef",  colour = "orangered", vjust = 0, angle = 45) +
  geom_text(aes(buurten$AfstandTotBioscoop_104[3], 220), label = "Vechtzoom-zuid", colour = "orange2", vjust = 1, angle = 45) +
  geom_text(aes(buurten$AfstandTotBioscoop_104[4], 200), label = "Zambesidreef", colour = "seagreen4", vjust = 1, angle = 45) +
  labs(x = "Afstand tot bioscoop (km)", y = "Aantal") +
  theme_minimal()

bios_sted
ggsave(bios_sted, file = "fig/6_bios_sted.jpg")

## AfstandTotBioscoop_104 buurten Utrecht
bios_utrecht <-
  ggplot(data_utrecht, aes(x = AfstandTotBioscoop_104)) + 
  geom_histogram(binwidth = 0.5, colour = "black", fill = "navajowhite") +
  geom_vline(data = buurten, 
             aes(xintercept = AfstandTotBioscoop_104, 
                 colour = WijkenEnBuurten), color=c("orangered4", "orangered", "orange2", "seagreen4"),
             linetype = "solid", show.legend = FALSE, size = 2) +
  #geom_text(data = buurten, aes(label = c(WijkenEnBuurten), y = 23), angle = 45) +
  geom_text(aes(buurten$AfstandTotBioscoop_104[1], 17), label = "Zamenhofdreef", colour = "orangered4", angle = 45) +
  geom_text(aes(buurten$AfstandTotBioscoop_104[2], 16), label = "Neckardreef",  colour = "orangered", vjust = 0, angle = 45) +
  geom_text(aes(buurten$AfstandTotBioscoop_104[3], 15), label = "Vechtzoom-zuid", colour = "orange2", vjust = 1, angle = 45) +
  geom_text(aes(buurten$AfstandTotBioscoop_104[4], 14), label = "Zambesidreef", colour = "seagreen4", vjust = 1, angle = 45) +
  labs(x = "Afstand tot bioscoop (km)", y = "Aantal") +
  theme_minimal()

bios_utrecht
ggsave(bios_utrecht, file = "fig/7_bios_utrecht.jpg")

#### Mobiliteit grafieken

### AfstandTotBelangrijkOverstapstation_91

## AfstandTotBelangrijkOverstapstation_91 buurten met zelfde stedelijkheid
overstapStation_sted <-
  ggplot(data_sted, aes(x = AfstandTotBelangrijkOverstapstation_91)) + 
  geom_histogram(binwidth = 0.5, colour = "black", fill="navajowhite") +
  geom_vline(data = buurten, 
             aes(xintercept = AfstandTotBelangrijkOverstapstation_91, 
                 colour = WijkenEnBuurten), color = c("orangered4", "orangered", "orange2", "seagreen4"),
             linetype = "solid", show.legend = FALSE, size = 2) +
  #geom_text(data = buurten, aes(label = c(WijkenEnBuurten), y = 23), angle = 45) +
  geom_text(aes(buurten$AfstandTotBelangrijkOverstapstation_91[1], 180), label = "Zamenhofdreef", colour = "orangered4", angle = 45) +
  geom_text(aes(buurten$AfstandTotBelangrijkOverstapstation_91[2], 160), label = "Neckardreef",  colour = "orangered", vjust = 1, angle = 45) +
  geom_text(aes(buurten$AfstandTotBelangrijkOverstapstation_91[3], 170), label = "Vechtzoom-zuid", colour = "orange2", vjust = 1, angle = 45) +
  geom_text(aes(buurten$AfstandTotBelangrijkOverstapstation_91[4], 150), label = "Zambesidreef", colour = "seagreen4", vjust = 1, angle = 45) +
  labs(x = "Afstand tot (belangerijk) overstapstation (km)", y = "Aantal") +
  theme_minimal()

overstapStation_sted
ggsave(overstapStation_sted, file = "fig/8_overstapStation_sted.jpg")

## AfstandTotBelangrijkOverstapstation_91 buurten Utrecht
overstapStation_utrecht <-
  ggplot(data_utrecht, aes(x = AfstandTotBelangrijkOverstapstation_91)) + 
  geom_histogram(binwidth = 0.5, colour = "black", fill = "navajowhite") +
  geom_vline(data = buurten, 
             aes(xintercept= AfstandTotBelangrijkOverstapstation_91, 
                 colour = WijkenEnBuurten), color = c("orangered4", "orangered", "orange2", "seagreen4"),
             linetype = "solid", show.legend = FALSE, size = 2) +
  #geom_text(data = buurten, aes(label = c(WijkenEnBuurten), y = 23), angle = 45) +
  geom_text(aes(buurten$AfstandTotBelangrijkOverstapstation_91[1], 14), label = "Zamenhofdreef", colour = "orangered4", angle = 45) +
  geom_text(aes(buurten$AfstandTotBelangrijkOverstapstation_91[2], 12.5), label = "Neckardreef",  colour = "orangered", vjust = 0, angle = 45) +
  geom_text(aes(buurten$AfstandTotBelangrijkOverstapstation_91[3], 13.5), label = "Vechtzoom-zuid", colour = "orange2", vjust = 1, angle = 45) +
  geom_text(aes(buurten$AfstandTotBelangrijkOverstapstation_91[4], 12), label = "Zambesidreef", colour = "seagreen4", vjust = 1, angle = 45) +
  labs(x = "Afstand tot (belangerijk) overstapstation (km)", y = "Aantal") +
  theme_minimal()

overstapStation_utrecht
ggsave(overstapStation_utrecht, file = "fig/9_overstapStation_utrecht.jpg")


### AfstandTotTreinstationsTotaal_90

## AfstandTotTreinstationsTotaal_90 buurten met zelfde stedelijkheid
treinstation_sted <-
  ggplot(data_sted, aes(x = AfstandTotTreinstationsTotaal_90)) + 
  geom_histogram(binwidth = 0.5, colour = "black", fill = "navajowhite") +
  geom_vline(data = buurten, 
             aes(xintercept= AfstandTotTreinstationsTotaal_90, 
                 colour = WijkenEnBuurten), color = c("orangered4", "orangered", "orange2", "seagreen4"),
             linetype = "solid", show.legend = FALSE, size = 2) +
  #geom_text(data = buurten, aes(label = c(WijkenEnBuurten), y = 23), angle = 45) +
  geom_text(aes(buurten$AfstandTotTreinstationsTotaal_90[1], 280), label = "Zamenhofdreef", colour = "orangered4", angle = 45) +
  geom_text(aes(buurten$AfstandTotTreinstationsTotaal_90[2], 250), label = "Neckardreef",  colour = "orangered", vjust = 0, angle = 45) +
  geom_text(aes(buurten$AfstandTotTreinstationsTotaal_90[3], 240), label = "Vechtzoom-zuid", colour = "orange2", vjust = 1, angle = 45) +
  geom_text(aes(buurten$AfstandTotTreinstationsTotaal_90[4], 220), label = "Zambesidreef", colour = "seagreen4", vjust = 1, angle = 45) +
  labs(x = "Afstand tot treinstation (km)", y = "Aantal") +
  theme_minimal()

treinstation_sted
ggsave(treinstation_sted, file = "fig/10_treinstation_sted.jpg")

## AfstandTotTreinstationsTotaal_90 buurten Utrecht
treinstation_utrecht <-
  ggplot(data_utrecht, aes(x = AfstandTotTreinstationsTotaal_90)) + 
  geom_histogram(binwidth = 0.5, colour = "black", fill = "navajowhite") +
  geom_vline(data = buurten, 
             aes(xintercept= AfstandTotTreinstationsTotaal_90, 
                 colour = WijkenEnBuurten), color = c("orangered4", "orangered", "orange2", "seagreen4"),
             linetype = "solid", show.legend = FALSE, size = 2) +
  #geom_text(data = buurten, aes(label = c(WijkenEnBuurten), y = 23), angle = 45) +
  geom_text(aes(buurten$AfstandTotTreinstationsTotaal_90[1], 19), label = "Zamenhofdreef", colour = "orangered4", angle = 45) +
  geom_text(aes(buurten$AfstandTotTreinstationsTotaal_90[2], 18), label = "Neckardreef",  colour = "orangered", vjust = 1, angle = 45) +
  geom_text(aes(buurten$AfstandTotTreinstationsTotaal_90[3], 17), label = "Vechtzoom-zuid", colour = "orange2", vjust = 1, angle = 45) +
  geom_text(aes(buurten$AfstandTotTreinstationsTotaal_90[4], 16), label = "Zambesidreef", colour = "seagreen4", vjust = 1, angle = 45) +
  labs(x = "Afstand tot treinstation (km)", y = "Aantal") +
  theme_minimal()

treinstation_utrecht
ggsave(treinstation_utrecht, file = "fig/11_treinstation_utrecht.jpg")

### AfstandTotOpritHoofdverkeersweg_89

## AfstandTotOpritHoofdverkeersweg_89 buurten met zelfde stedelijkheid
oprithoofdverkeersweg_sted <-
  ggplot(data_sted, aes(x = AfstandTotOpritHoofdverkeersweg_89)) + 
  geom_histogram(binwidth = 0.5, colour = "black", fill = "navajowhite") +
  geom_vline(data = buurten, 
             aes(xintercept= AfstandTotOpritHoofdverkeersweg_89, 
                 colour = WijkenEnBuurten), color = c("orangered4", "orangered", "orange2", "seagreen4"),
             linetype = "solid", show.legend = FALSE, size = 2) +
  #geom_text(data = buurten, aes(label = c(WijkenEnBuurten), y = 23), angle = 45) +
  geom_text(aes(buurten$AfstandTotOpritHoofdverkeersweg_89[1], 250), label = "Zamenhofdreef", colour = "orangered4", angle = 45) +
  geom_text(aes(buurten$AfstandTotOpritHoofdverkeersweg_89[2], 250), label = "Neckardreef",  colour = "orangered", vjust = 0, angle = 45) +
  geom_text(aes(buurten$AfstandTotOpritHoofdverkeersweg_89[3], 200), label = "Vechtzoom-zuid", colour = "orange2", vjust = 1, angle = 45) +
  geom_text(aes(buurten$AfstandTotOpritHoofdverkeersweg_89[4], 230), label = "Zambesidreef", colour = "seagreen4", vjust = 1, angle = 45) +
  labs(x = "Afstand tot oprit hoofdverkeersweg (km)", y = "Aantal") +
  theme_minimal()

oprithoofdverkeersweg_sted
ggsave(oprithoofdverkeersweg_sted, file = "fig/12_oprithoofdverkeersweg_sted.jpg")

## AfstandTotOpritHoofdverkeersweg_89 buurten Utrecht
oprithoofdverkeersweg_utrecht <-
  ggplot(data_utrecht, aes(x = AfstandTotOpritHoofdverkeersweg_89)) + 
  geom_histogram(binwidth = 0.5, colour = "black", fill = "navajowhite") +
  geom_vline(data = buurten, 
             aes(xintercept= AfstandTotOpritHoofdverkeersweg_89, 
                 colour = WijkenEnBuurten), color = c("orangered4", "orangered", "orange2", "seagreen4"),
             linetype = "solid", show.legend = FALSE, size = 2) +
  #geom_text(data = buurten, aes(label = c(WijkenEnBuurten), y = 23), angle = 45) +
  geom_text(aes(buurten$AfstandTotOpritHoofdverkeersweg_89[1], 25), label = "Zamenhofdreef", colour = "orangered4", angle = 45) +
  geom_text(aes(buurten$AfstandTotOpritHoofdverkeersweg_89[2], 28), label = "Neckardreef",  colour = "orangered", vjust = 0, angle = 45) +
  geom_text(aes(buurten$AfstandTotOpritHoofdverkeersweg_89[3], 17), label = "Vechtzoom-zuid", colour = "orange2", vjust = 1, angle = 45) +
  geom_text(aes(buurten$AfstandTotOpritHoofdverkeersweg_89[4], 22), label = "Zambesidreef", colour = "seagreen4", vjust = 1, angle = 45) +
  labs(x = "Afstand tot oprit hoofdverkeersweg (km)", y = "Aantal") +
  theme_minimal()

oprithoofdverkeersweg_utrecht
ggsave(oprithoofdverkeersweg_utrecht, file = "fig/13_overstapStation_sted.jpg")

