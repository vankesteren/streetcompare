library(tidyverse)
library(sf)
library(cbsodataR)

# voorzieningen totaal
voorzieningen_nabijheid <- cbs_get_data("84463NED")

# stedelijkheid
buurtstats <- read_sf("data/WijkBuurtkaart_2020_v1/buurt_2020_v1.shp")

# connect stedelijkheid met buurtstatistieken
sted_df <- 
  voorzieningen_nabijheid %>% 
  filter(SoortRegio_2 == "Buurt     ") %>% 
  mutate(BU_CODE = WijkenEnBuurten) %>% 
  left_join(buurtstats %>% select(BU_CODE, STED)) %>% 
  filter(STED == 1) # selecteer alleen stedelijke gebieden zoals overvecht

einst_df <- 
  sted_df %>% 
  filter(BU_CODE %in% c("BU03440321",  "BU03440322", "BU03440331", "BU03440341")) %>% 
  mutate(buurnaam = c("Zamenhofdreef", "Neckardreef", "Vechtzoom-zuid", "Zambesidreef"))


# nu kunnen we plotten 
library(ggrepel)


sted_df %>% 
  ggplot(aes(x = AfstandTotBelangrijkOverstapstation_91)) + 
  geom_histogram(bins = 100) + 
  geom_vline(data = einst_df, aes(xintercept = AfstandTotBelangrijkOverstapstation_91, colour = buurnaam)) +
  geom_label_repel(data = einst_df, aes(y = 100, label = buurnaam), vjust = 0, angle = 45)
