# analyse van economisch draagvlak
library(tidyverse)
library(sf)

# einsteindreef buurten
roi <- c("BU03440321",  "BU03440322", "BU03440331", "BU03440341")

buurt_utrecht <- 
  st_read("data/WijkBuurtkaart_2020_v1/buurt_2020_v1.shp") |> 
  filter(OPP_LAND > 0, GM_NAAM == "Utrecht") |> 
  mutate(across(where(is_numeric), na_if, -99999999))

einstein_highlight <- 
  buurt_utrecht |> 
  filter(BU_CODE %in% roi)

# plot einstein in utrecht
buurt_utrecht |> 
  ggplot() +
  geom_sf() +
  geom_sf(data = einstein_highlight |> st_union(), fill = "#FA343488") +
  theme_minimal()

# Leeftijdssamenstelling
pop_summary <- 
  buurt_utrecht |> 
  select(BU_CODE, BU_NAAM, ends_with("JR")) |> 
  pivot_longer(ends_with("JR"), names_to = "Leeftijdsgroep", names_pattern = "([0-9]{2}\\_[0-9\\w]{2})",
               values_to = "Percentage") |> 
  as_tibble() |> 
  select(-geometry) |> 
  mutate(einstein = BU_CODE %in% roi,
         regio = ifelse(einstein, "Einstein", "Utrecht")) |> 
  group_by(regio, Leeftijdsgroep) |> 
  summarise(Percentage = mean(Percentage, na.rm = TRUE)) |> 
  mutate(Leeftijdsgroep = str_replace_all(Leeftijdsgroep, "_", "-"), 
         Leeftijdsgroep = str_replace_all(Leeftijdsgroep, "-EO", "+"), 
         Leeftijdsgroep = as_factor(Leeftijdsgroep), 
         cs = (cumsum(Percentage) + c(0, cumsum(Percentage)[-n()])) / 2) 

pop_summary |> 
  ggplot() +
  geom_col(aes(x = Percentage, fill = Leeftijdsgroep, y = regio), 
           position = position_stack(reverse = TRUE), width = 0.7) +
  geom_text(aes(x = cs, label = Leeftijdsgroep, y = regio), 
            col = rep(c("white","white", "white", "black", "black"), 2)) +
  scale_fill_viridis_d(guide = "none") +
  theme_minimal() +
  labs(title = "Leeftijdssamenstelling Einstein", y = "Regio", 
       subtitle = "In vergelijking met de rest van Utrecht")

  
ggsave("table_session/img/leeftijdssamenstelling.png", width = 8, height = 4, bg = "white")

#