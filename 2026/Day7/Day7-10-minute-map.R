###############################################################################
## Load libraries                                                            ##
###############################################################################

library(tidyverse)
library(rinat)
library(sf)
library(geodata)
library(sbBrand)
library(ggtext)
###############################################################################
## Load data                                                                  ##
###############################################################################

korth_raw <- get_inat_obs(
  query = "Korthalsella salicornioides",
  geo = TRUE,
  maxresults = 2000,
  place_id = 6803
) |>
  filter_out(license == "") |>
  select(
    scientific_name,
    datetime,
    place_guess,
    latitude,
    longitude,
    common_name,
    taxon_id,
    quality_grade,
    license
  )

write_csv(korth_raw, "2026/Day7/data/korthalsella.inat.csv")
korth_raw <- read_csv("2026/Day7/data/korthalsella.inat.csv")
nz1 <- geodata::gadm(
  country = "NZ",
  level = 1,
  path = "2026/Day7/data"
)
nz1_sf <- st_as_sf(nz1)
bg_col <- "#F5F5F5"
social_caption <- sb_social_caption(data_source = "iNaturalist")

p <- ggplot() +
  geom_sf(data = nz1_sf, colour = "grey80", linewidth = 0.2) +
  coord_sf(xlim = c(166, 179), ylim = c(-47.8, -34.2), expand = FALSE) +
  geom_point(
    data = korth_raw,
    mapping = aes(longitude, latitude),
    fill = "#7b9324",
    color = "#596621",
    shape = 21,
    size = 2
  ) +
  labs(caption = social_caption) +
  theme_void() +
  theme(
    plot.caption = element_markdown(hjust = 0, margin = margin(t = 10)),
    legend.position = "none",
    plot.background = element_rect(fill = "#F5F5F5", colour = NA)
  )
p


ggsave(
  "2026/Day7/output/korthalsella.png",
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)
