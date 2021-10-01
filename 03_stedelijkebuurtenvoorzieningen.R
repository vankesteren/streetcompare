library(tidyverse)
library(sf)
library(cbsodataR)
library(ggrepel)
library(firatheme)

# voorzieningen totaal
voorzieningen_nabijheid <- cbs_get_data("84463NED")

# stedelijkheid
buurtstats <- read_sf("data/WijkBuurtkaart_2020_v1/buurt_2020_v1.shp")

# connect stedelijkheid met buurtstatistieken
buurt_df <- 
  voorzieningen_nabijheid %>% 
  filter(SoortRegio_2 == "Buurt     ") %>% 
  mutate(BU_CODE = WijkenEnBuurten) %>% 
  left_join(buurtstats) %>% 
  select(BU_CODE, BU_NAAM, GM_NAAM, STED, starts_with("Afstand"))

# long-format
buurt_df_long <- 
  buurt_df %>% 
  pivot_longer(
    cols = starts_with("Afstand"), 
    names_to = c("Voorziening", "ColNum"), 
    values_to = "Afstand", 
    names_prefix = "AfstandTot", 
    names_sep = "_"
  )



einst_df_long <- 
  buurt_df_long %>% 
  filter(BU_CODE %in% c("BU03440321",  "BU03440322", "BU03440331", "BU03440341")) %>% 
  mutate(BU_NAAM = str_remove(BU_NAAM, "\\ .+"))




# nu kunnen we plotten 
vz_list <- c("Bioscoop", "Museum", "PodiumkunstenTotaal", 
             "Restaurant", "Warenhuis", "Ziekenhuis", 
             "Apotheek", "Bibliotheek", "Kinderdagverblijf")

bdf <- buurt_df_long %>% filter(STED == 1, Voorziening %in% vz_list, ColNum != 11) 
edf <- einst_df_long %>% filter(STED == 1, Voorziening %in% vz_list, ColNum != 11) 

bdf %>% 
  ggplot(aes(x = Afstand)) +
  geom_histogram() +
  geom_vline(data = edf, aes(xintercept = Afstand, colour = BU_NAAM)) +
  facet_wrap(~Voorziening, scales = "free") +
  labs(x = "Afstand (km)", y = "Aantal stedelijke buurten", colour = "Buurt") +
  theme_minimal() +
  scale_colour_viridis_d() +
  theme(legend.position = "top")

ggsave("fig/einstein_profiel.png", width = 9, height = 6)