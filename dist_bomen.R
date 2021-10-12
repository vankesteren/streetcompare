# Generate map of trees per km² per buurt
library(tidyverse)
library(sf)

# data prep ----
boom <- read_csv("data/bomenperbuurt.csv") %>% rename(bomen = totaal)

buurt <- st_read("data/WijkBuurtkaart_2020_v1/buurt_2020_v1.shp") %>% 
  mutate(oppervlakte_km2 = OPP_LAND / 100)

boom_buurt <- left_join(buurt, boom, by = c("BU_CODE" = "buurtcode"))

sted_boom <- boom_buurt %>% 
  filter(STED == 1)

einst_boom <- sted_boom %>% 
  filter(BU_CODE %in% c("BU03440321",  "BU03440322", "BU03440331", "BU03440341")) %>% 
  mutate(BU_NAAM = str_remove(BU_NAAM, "\\ .+"))

# plotting ----
sted_boom %>% 
  ggplot(aes(x = bomen/oppervlakte_km2)) +
  geom_histogram(bins = 50) +
  geom_vline(data = einst_boom, mapping = aes(xintercept = bomen / oppervlakte_km2, colour = BU_NAAM)) +
  labs(x = "Bomen per km²", y = "Aantal stedelijke buurten", colour = "Buurt", 
       title = "Aantal bomen per km²", subtitle = "In stedelijke wijken in Nederland") +
  theme_minimal() +
  scale_colour_viridis_d() +
  theme(legend.position = "top")

ggsave("fig/dist/bomen_km2.png", width = 7, height = 5, bg = "white")
