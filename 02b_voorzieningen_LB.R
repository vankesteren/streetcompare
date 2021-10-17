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


data <- cbs_get_data("84463NED", select = c("Gemeentenaam_1",
                                            "SoortRegio_2", 
                                            "WijkenEnBuurten",
                                            "AfstandTotWarenhuis_32",
                                            "AfstandTotCafeED_36",
                                            "AfstandTotCafetariaED_40",
                                            "AfstandTotRestaurant_44",
                                            "AfstandTotHotelED_48",
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
  
         
bios_sted <-
  ggplot(data_sted, aes(x = AfstandTotBioscoop_104)) + 
  geom_histogram(binwidth = 0.5, colour = "black", fill="navajowhite") +
  geom_vline(data = buurten, 
             aes(xintercept= AfstandTotBioscoop_104, 
                 colour = WijkenEnBuurten), color=c("orangered4","orangered","orange2","seagreen4"),
             linetype = "solid", show.legend = FALSE, size=2) +
  #geom_text(data = buurten, aes(label = c(WijkenEnBuurten), y = 23), angle = 45) +
  geom_text(aes(buurten$AfstandTotBioscoop_104[1], 22), label = "Zamenhofdreef", colour = "orangered4", angle = 45) +
  geom_text(aes(buurten$AfstandTotBioscoop_104[2], 15), label = "Neckardreef",  colour = "orangered", vjust = 0, angle = 45) +
  geom_text(aes(buurten$AfstandTotBioscoop_104[3], 17), label = "Vechtzoom-zuid", colour = "orange2", vjust = 1, angle = 45) +
  geom_text(aes(buurten$AfstandTotBioscoop_104[4], 23), label = "Zambesidreef", colour = "seagreen4", vjust = 1, angle = 45) +
  labs(x = "Afstand tot bioscoop (km)", y = "Aantal") +
  theme_minimal()

bios_sted
ggsave(bios_sted, file = "fig/4_bios_sted.jpg")

bios_utrecht <-
  ggplot(data_utrecht, aes(x = AfstandTotBioscoop_104)) + 
  geom_histogram(binwidth = 0.5, colour = "black", fill="navajowhite") +
  geom_vline(data = buurten, 
             aes(xintercept= AfstandTotBioscoop_104, 
                 colour = WijkenEnBuurten), color=c("orangered4","orangered","orange2","seagreen4"),
             linetype = "solid", show.legend = FALSE, size=2) +
  #geom_text(data = buurten, aes(label = c(WijkenEnBuurten), y = 23), angle = 45) +
  geom_text(aes(buurten$AfstandTotBioscoop_104[1], 22), label = "Zamenhofdreef", colour = "orangered4", angle = 45) +
  geom_text(aes(buurten$AfstandTotBioscoop_104[2], 15), label = "Neckardreef",  colour = "orangered", vjust = 0, angle = 45) +
  geom_text(aes(buurten$AfstandTotBioscoop_104[3], 17), label = "Vechtzoom-zuid", colour = "orange2", vjust = 1, angle = 45) +
  geom_text(aes(buurten$AfstandTotBioscoop_104[4], 23), label = "Zambesidreef", colour = "seagreen4", vjust = 1, angle = 45) +
  labs(x = "Afstand tot bioscoop (km)", y = "Aantal") +
  theme_minimal()

bios_utrecht
ggsave(bios_utrecht, file = "fig/5_bios_utrecht.jpg")
