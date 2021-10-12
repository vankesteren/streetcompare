# generate map of einsteindreef buurten
library(tidyverse)
library(sf)

buurt <- st_read("data/WijkBuurtkaart_2020_v1/buurt_2020_v1.shp")
einst <- buurt %>% 
  filter(BU_CODE %in% c("BU03440321",  "BU03440322", "BU03440331", "BU03440341")) %>% 
  mutate(BU_NAAM = str_remove(BU_NAAM, "\\ .+"))

buurt %>%
  filter(GM_NAAM == "Utrecht") %>% 
  ggplot() +
  geom_sf(fill = NA) + 
  geom_sf(data = einst, aes(fill = BU_NAAM)) +
  scale_fill_viridis_d() + 
  theme_void() +
  labs(fill = "Buurt", title = "Einsteindreef buurten") 

ggsave("fig/buurten_overvecht.png", width = 6, height = 6, bg = "white")
