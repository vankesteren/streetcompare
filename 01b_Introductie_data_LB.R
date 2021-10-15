library(cbsodataR)
library(tidyverse)
library(sf)

################################################################################
#1. Introductie data
# Veelal adhv open CBS data

# Kerncijfers wijken en buurten
# https://www.cbs.nl/nl-nl/onze-diensten/methoden/onderzoeksomschrijvingen/korte-onderzoeksbeschrijvingen/kerncijfers-wijken-en-buurten 
# Maar ook andere databronnen, soms administratieve bronnen, zoals van politie
# maar zouden ook surveys kunnen zijn 
# of bijvoorbeeld sensoren die langs wegen staan, of in het water

metadata <- cbs_get_meta("84463NED") # handig voor de variabelenamen
print(metadata$DataProperties$Key)

# https://www.utrecht.nl/fileadmin/uploads/documenten/wonen-en-leven/wijken/wijk-overvecht/plattegrond-Overvecht-met-buurtindeling.jpg
# kijken naar plaatje: soms zijn statistieken op wijkniveau (wijk=overvecht)
# soms op buurtniveau = meer detail --> dan kiezen we de 4 wijken die om de einsteindreef liggen

# Buurten Einstendreef
# CBS buurtcode   Buurtnaam
# BU03440321      Zamenhofdreef en omgeving
# BU03440322      Neckardreef en omgeving
# BU03440331      Vechtzoom-zuid
# BU03440341      Zambesidreef en omgeving

## Hierbij laten zien: 
# 1. Kaartje Utrecht met daarin Wijk Overvecht

data <- cbs_get_data("84463NED", 
                     select=c("Gemeentenaam_1","SoortRegio_2", "WijkenEnBuurten", "AfstandTotBioscoop_104")) %>%
  mutate(WijkenEnBuurten = str_trim(WijkenEnBuurten),
         Gemeentenaam_1 = str_trim(Gemeentenaam_1)) 


grenzen <- st_read("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_buurt_2017_gegeneraliseerd&outputFormat=json")


data2 <- 
  data %>%
  left_join(grenzen, by = c(WijkenEnBuurten = "statcode"))

data2 = data2%>%
  filter(Gemeentenaam_1 == "Utrecht")


# Utrecht 
utrecht = data2 %>%
  ggplot + 
  geom_sf(aes(geometry = geometry), fill = c("seashell1")) +
  scale_fill_viridis_c() +
  labs(title = "Buurten in Utrecht", fill = "") +
  theme_void()

utrecht

ggsave(utrecht, file = "fig/1_Utrecht.jpg")

# Overvecht in Utrecht

#1: Bedrijventerrein en omgeving,  BU03440333
#2: Tigrisdreef en omgeving, BU03440342
#3: Poldergebied Overvecht, BU03440343
#4: Vechtzoom Noord Klopvaart, BU03440332
#5: Zambesidreef en omgeving, BU03440341
#6: Neckardreef en omgeving, BU03440322
#7: Vechtzoom Zuid, BU03440331
#8: Wolga en Donaudreef en omgeving, BU03440312
#9: Zamenhofdreef en omgeving, BU03440321
#10: Taag en Rubicondreef en omgeving. BU03440311

buurtenovervecht = c("BU03440333",
                     "BU03440342",
                     "BU03440343",
                     "BU03440332",
                     "BU03440341",
                     "BU03440322",
                     "BU03440331",
                     "BU03440312",
                     "BU03440321",
                     "BU03440311")

overvecht = data2 %>%
  ggplot + 
  geom_sf(aes(geometry = geometry),
              fill = ifelse(data2$WijkenEnBuurten %in% buurtenovervecht, 'navajowhite3', 'seashell1')) +
  scale_fill_viridis_c() +
  labs(title = "De buurten in Overvecht", fill = "") +
  theme_void()

overvecht
ggsave(overvecht, file = "fig/2_Overvecht.jpg")

# BU03440321      Zamenhofdreef en omgeving
# BU03440322      Neckardreef en omgeving
# BU03440331      Vechtzoom-zuid
# BU03440341      Zambesidreef en omgeving

# 2. Kaartje Utrecht, lijn om Overvecht en daarin de wijken, en Einsteindreef zichtbaar

buurtenoverig = c("BU03440333",
                  "BU03440342",
                  "BU03440343",
                  "BU03440332",
                  "BU03440312",
                  "BU03440311")

buurteneinstein = c("BU03440341",
                    "BU03440322",
                    "BU03440331",
                    "BU03440321")

einstein =data2 %>%
  ggplot + 
  geom_sf(aes(geometry = geometry),
          fill = ifelse(data2$WijkenEnBuurten %in% buurtenoverig, 
                        'navajowhite3', 
                        ifelse(data2$WijkenEnBuurten %in% buurteneinstein, 
                               'orangered', 
                               'seashell1'))) +
  scale_fill_viridis_c() +
  labs(title = "De buurten bij de Einsteindreef", fill = "") +
  theme_void()

einstein
ggsave(einstein, file = "fig/3_Einstein.jpg")

# We gebruiken open data om bijvoorbeeld in kaart te brengen wat de situatie
# bij de einsteindreef tov vergelijkbare situaties elders in Nederland

# neem voorbeeld voorzieningen: bioscoop
# we kunnen bekijken wat de afstand is tot een voorziening voor alle wijken in Nederland
# Niet zo nuttig, want een stedelijke wijk zoals hier (waar je als inwoner bepaalde
# voorzieningen mag verwachten) kun je niet vergelijken met een dorp op het 
# platteland waar mensen juist voor de rust gaan wonen en niet voor voorzieningen
# zoals een bioscoop. 

# Proberen daarom altijd 2 vergelijkingen te maken:
# overvecht/einsteindreef tov andere wijken in Utrecht
# Overvecht/einstendreef tov andere wijken in Nederland van zelfde niveau stedelijkheid
## Wat is stedelijkheid? https://www.cbs.nl/nl-nl/nieuws/2019/44/meeste-afval-per-inwoner-in-minst-stedelijke-gemeenten/stedelijkheid
# Einsteindreef is meest stedelijke type
# Kaartje laten zien met alle andere wijken in Nederland met zelfde niveau 
# Evt ook kaartje met alles? 

# Kaartje maken van alle buurten met stedelijk=1

# heel NL inladen
data <- cbs_get_data("84463NED", 
                     select=c("Gemeentenaam_1","SoortRegio_2", "WijkenEnBuurten", "AfstandTotBioscoop_104")) %>%
  mutate(WijkenEnBuurten = str_trim(WijkenEnBuurten),
         Gemeentenaam_1 = str_trim(Gemeentenaam_1))

# buurtstats aan koppelen
buurtstats <- read_sf("data/WijkBuurtkaart_2020_v1/buurt_2020_v1.shp")

# combinen
data3 <- 
  data %>%
  left_join(buurtstats, by = c(WijkenEnBuurten = "BU_CODE")) # %>%
  #filter(SoortRegio_2 == "Buurt     ")

# plot alle stedelijkheid = 1 
stedelijk = data3 %>%
  ggplot + 
  geom_sf(aes(geometry = geometry),  
          fill = ifelse(data3$STED == 1, "orangered", "navajowhite3"), colour = NA) +
  scale_fill_viridis_c() +
  labs(title = "Stedelijke wijken in Nederland", fill = "") +
  theme_void()

stedelijk
ggsave(stedelijk, file = "fig/4_Stedelijk.jpg")

# plot de verdeling van stedelijkheid over Nederland


data4 = data3 %>%
  dplyr::select(STED, AfstandTotBioscoop_104, geometry) %>%
  mutate(STED = na_if(STED, -99999999))

data4$STED = factor(data4$STED)
my_palette = c("orangered4","orangered","navajowhite4","navajowhite3","navajowhite")  


stedelijkheid = data4 %>%
  drop_na(STED) %>%
  ggplot +
  geom_sf(aes(fill=STED, geometry = geometry), color=NA) +
  scale_fill_manual(values=my_palette) +
  labs(title = "Stedelijkheid per wijk in Nederland", fill = "") +
  theme_void()

stedelijkheid
ggsave(stedelijkheid, file = "fig/5_Stedelijkheid.jpg")
                                 
                                 