# Figures for paper
library(tidyverse)
library(sf)

facilities <- read_rds("data/84463NED.rds")
neighbourhoods <- read_sf("data/WijkBuurtkaart_2020_v1/buurt_2020_v1.shp")

df_joined <- left_join(neighbourhoods, facilities, by = c(BU_CODE = "WijkenEnBuurten"))

focus_neighbourhoods <- c("BU03440321", "BU03440322", "BU03440331", "BU03440341")

df_city <- df_joined |> filter(STED == 1)
df_focus <- df_city |> filter(BU_CODE %in% focus_neighbourhoods)


# Histogram of distance to cinemas in city neighbourhoods
p_cinema <- 
  ggplot(df_city, aes(x = AfstandTotBioscoop_119)) + 
  geom_histogram(binwidth = 0.5, colour = "black", fill = "navajowhite") +
  geom_vline(
    data = df_focus, 
    aes(xintercept = AfstandTotBioscoop_119), 
    color = c("orangered4", "orangered", "orange2", "seagreen4"),
    linetype = "solid", show.legend = FALSE, lwd = 1.5
  ) +
  geom_label_repel(
    data = df_focus, 
    aes(x = AfstandTotBioscoop_119, y = (1:4)*10 + 240, label = str_extract(BU_NAAM, "\\w+")),
    color = c("orangered4", "orangered", "orange2", "seagreen4")
  ) +
  labs(x = "Distance to cinema (km)", y = "Count", title = "Distance to nearest cinema", 
       subtitle = "For city neighbourhoods in NL") +
  theme_minimal()

ggsave(filename = "fig/cinema.pdf", plot = p_cinema, width = 8, height = 6, bg = "white")


# Number of people close to cinemas
cinemas <- st_read("data/bioscopen_totaal/bioscopen_totaal.shp") %>% st_transform(st_crs(neighbourhoods))

# Creating a new cinema
new_cinema <- 
  st_sf(
    id = 0, 
    name = "Cinema Overvecht",
    bevolking  = NA,
    oad = NA, 
    sted = 1,
    geometry = st_sfc(st_point(x = c( 5.110539482611714, 52.11620171434056)), crs = 4326) %>% 
      st_transform(st_crs(neighbourhoods))
  )

cinemas_utrecht <- 
  cinemas %>% 
  st_filter(neighbourhoods %>% filter(GM_NAAM == "Utrecht"))

# plot the location
lwd <- .5
p_new_cinema <- 
  ggplot() +
  geom_sf(data = neighbourhoods %>% filter(GM_NAAM == "Utrecht"), linewidth = lwd) +
  geom_sf(data = cinemas_utrecht) +
  geom_sf(data = cinemas_utrecht %>% st_buffer(1500), fill = "orange", alpha = .2, linewidth = lwd) +
  geom_sf(data = new_cinema) +
  geom_sf(data = new_cinema %>% st_buffer(1500), fill = "seagreen", alpha = .5, linewidth = lwd) +
  annotate("segment", x = 138050.2, y = 460700.2, xend = 136050.2, yend = 458700.2, linewidth = lwd) +
  annotate("label", x = 138050.2, y = 460700.2, label = "New Cinema") +
  theme_minimal() +
  labs(x = "", y = "", title = "Location of new cinema", subtitle = "With potential service area of 1.5 km")


ggsave(filename = "fig/new_cinema.pdf", plot = p_new_cinema, width = 8, height = 8, bg = "white")


# voeg overvecht bios toe aan data
cinemas_bound <- bind_rows(new_cinema, cinemas |> filter(sted == 1))
cinemas_bound_buf <- st_buffer(cinemas_bound, 1500)

# compute number of inhabitants in 1.5 km radius
num_inhabitants <- numeric(nrow(cinemas_bound))
for (b in 1:nrow(cinemas_bound)) {
  print(b)
  nbh <- neighbourhoods %>% st_filter(cinemas_bound_buf[b,])
  inw <- nbh %>% pull(AANT_INW)
  num_inhabitants[b] <- sum(inw[inw > 0])
}
cinemas_bound$service <- num_inhabitants

# histogram of service amount for city-based cinemas
p_new_cinema_hist <- 
  cinemas_bound %>% 
  filter(id != 0) %>% 
  ggplot(aes(x = service)) + 
  geom_histogram(colour = "black", fill = "navajowhite") +
  geom_vline(data = cinemas_bound %>% filter(id == 0), aes(xintercept = service),
             linetype = "solid", show.legend = FALSE, size = 1.5) +
  annotate("label", x = cinemas_bound[1,]$service + 1000, y = 10, label = "New cinema", hjust = 0) +
  labs(x = "Number of inhabitants within 1.5km", y = "Number of cinemas", 
       title = "Number of inhabitants within 1.5 km of city cinemas") +
  theme_minimal()

ggsave(filename = "fig/new_cinema_hist.pdf", plot = p_new_cinema_hist, width = 8, height = 6, bg = "white")



p_new_cinema + (p_cinema / p_new_cinema_hist) %+% theme(text = element_text(family = "arial"))

ggsave("fig/cinema_plot_combined.pdf", width = 14, height = 8)
