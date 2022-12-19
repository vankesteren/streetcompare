# bioscopen: inwoners binnen 1.5km
library(tidyverse)
library(sf)


# load data
buurt <- st_read("data/WijkBuurtkaart_2020_v1/buurt_2020_v1.shp")
bios  <- st_read("data/bioscopen_totaal/bioscopen_totaal.shp") %>% st_transform(st_crs(buurt))

# we maken een hypothetische nieuwe bioscoop
nieuwe_bios <- 
  st_sf(
    id = 0, 
    name = "Cinema Overvecht",
    bevolking  = NA,
    oad = NA, 
    sted = 1,
    geometry = st_sfc(st_point(x = c( 5.110539482611714, 52.11620171434056)), crs = 4326) %>% 
      st_transform(st_crs(buurt))
  )

bios_utr <- 
  bios %>% 
  st_filter(buurt %>% filter(GM_NAAM == "Utrecht"))



# plot de locatie
oude_bios_plot <- 
  ggplot() +
  geom_sf(data = buurt %>% filter(GM_NAAM == "Utrecht")) +
  geom_sf(data = bios_utr) +
  geom_sf(data = bios_utr %>% st_buffer(1500), fill = "orange", alpha = .2) +
  theme_minimal()

nieuwe_bios_plot <- 
  ggplot() +
  geom_sf(data = buurt %>% filter(GM_NAAM == "Utrecht")) +
  geom_sf(data = bios_utr) +
  geom_sf(data = bios_utr %>% st_buffer(1500), fill = "orange", alpha = .2) +
  geom_sf(data = nieuwe_bios) +
  geom_sf(data = nieuwe_bios %>% st_buffer(1500), fill = "seagreen", alpha = .5) +
  theme_minimal()

ggsave(oude_bios_plot, filename = "fig/oude_bios_locatie.png", width = 9, height = 9, bg = "white")
ggsave(nieuwe_bios_plot, filename = "fig/nieuwe_bios_locatie.png", width = 9, height = 9, bg = "white")

# voeg overvecht bios toe aan data
bios_2 <- bind_rows(nieuwe_bios, bios)
bios_2_buf <- st_buffer(bios_2, 1500)

# bereken bevolking cijfer & oad cijfer
bev <- oad <- numeric(nrow(bios_2))
for (b in 1:nrow(bios_2)) {
  print(b)
  brt <- buurt %>% st_filter(bios_2_buf[b,])
  inw <- brt %>% pull(AANT_INW)
  oad <- brt %>% pull(OAD)
  bev[b] <- sum(inw[inw > 0])
  oad[b] <- mean(oad[oad > 0])
}
bios_2$bevolking <- bev
bios_2$oad       <- oad

write_rds(bios_2, "data/output/bios.rds")

# plot
bios_hist <- 
  bios_2 %>% 
  filter(id != 0) %>% 
  ggplot(aes(x = bevolking)) + 
  geom_histogram(colour = "black", fill = "navajowhite") +
  geom_vline(data = bios_2 %>% filter(id == 0), aes(xintercept = bevolking),
             linetype = "solid", show.legend = FALSE, size = 1.5) +
  annotate("label", x = bios_2[1,]$bevolking, y = 10, label = "Cinema Overvecht", hjust = 0) +
  labs(x = "Aantal inwoners in 1.5km", y = "Aantal bioscopen") +
  theme_minimal()

ggsave(bios_hist, filename = "fig/cinema_overvecht_bevolking.png", width = 9, height = 7, bg = "white")

