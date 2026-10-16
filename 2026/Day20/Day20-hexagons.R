library(tidyverse)
library(rinat)
library(sf)
library(geodata)

pterostylis_species <- c(
  "Pterostylis banksii",
  "Pterostylis trullifolia",
  "Pterostylis alobula",
  "Pterostylis brumalis",
  "Pterostylis australis",
  "Pterostylis tasmanica",
  "Pterostylis micromega",
  "Pterostylis silvicultrix",
  "Pterostylis areolata",
  "Pterostylis graminea",
  "Pterostylis auriculata",
  "Pterostylis alveata",
  "Pterostylis irsoniana",
  "Pterostylis tanypoda",
  "Pterostylis patens",
  "Pterostylis montana",
  "Pterostylis oliveri",
  "Pterostylis humilis",
  "Pterostylis tristis",
  "Pterostylis cardiostigma",
  "Pterostylis agathicola",
  "Pterostylis paludosa",
  "Pterostylis venosa",
  "Pterostylis cernua",
  "Pterostylis porrecta",
  "Pterostylis nutans",
  "Pterostylis irwinii",
  "Pterostylis foliata",
  "Pterostylis puberula"
)

#possibly is used to wrap a function to return a value instead of an error
get_species <- possibly(
  \(sp) {
    get_inat_obs(
      taxon_name = sp,
      geo = TRUE,
      maxresults = 10000,
      place_id = 6803
    )
  },
  otherwise = NULL
)

greenhood_nz_nd <- pterostylis_species |>
  set_names() |>
  map(get_species, .progress = TRUE) |>
  list_rbind(names_to = "queried_species")

greenhood_nz <- pterostylis_species |>
  set_names() |>
  map(get_species, .progress = TRUE) |>
  list_rbind(names_to = "queried_species")

#gadm Returns a SpatVect, which is in vector format and is a polygon layer with one row per region
#It is more detailed than rnatural earth - it has more admin levels so it is better for regional maps and small

nz1 <- geodata::gadm(
  country = "NZ",
  level = 1,
  path = "2026/Day20/data/nz"
)
nz1_sf <- st_as_sf(nz1)

greenhood_coord <- greenhood_nz |>
  filter(
    quality_grade == "research"
  ) |>
  filter_out(
    longitude > 166 & longitude < 179,
    latitude > -47.8 & latitude < -34.2
  )

#Initial df was 11427 rows
#Keeping quality grade research 10067 rows
#Filtering longitude and latitude 10002 rows this filters out entries from Chatham islands, Auckland islands and Antipodes island
#Filtering out observations with no CC license 9451
greenhood_nz1 <- greenhood_nz |>
  filter(
    quality_grade == "research",
    longitude > 166 & longitude < 179,
    latitude > -47.8 & latitude < -34.2
  ) |>
  filter_out(license == "") |>
  select(
    scientific_name,
    datetime,
    time_zone,
    latitude,
    longitude,
    observed_on,
    user_login,
    quality_grade
  )


bg_col <- "#F5F5F5"

hex_size <- 20000

nz_proj <- nz1_sf |>
  st_crop(xmin = 166, xmax = 179, ymin = -47.8, ymax = -34.2) |>
  st_transform(2193) |>
  st_union()

#st_make_grid covers the bounding box of nz_proj with a regular grid of cells
#cellsize is in the units of the crs, so 20000 is 20 km in NZTM (metres)
#square = FALSE gives hexagons instead of squares; for hexagons cellsize is the distance between opposite flat sides
#it returns an sfc of polygons, so st_sf makes it a data frame, and st_filter drops hexes that miss the land
hex_grid <- nz_proj |>
  st_make_grid(cellsize = hex_size, square = FALSE) |>
  st_sf() |>
  mutate(hex_id = row_number()) |>
  st_filter(nz_proj)

obs_sf <- greenhood_nz1 |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(2193)

hex_counts <- hex_grid |>
  mutate(n_obs = lengths(st_intersects(hex_grid, obs_sf)))

hx <- ggplot() +
  geom_sf(data = nz_proj, fill = bg_col, color = "grey") +
  geom_sf(
    data = filter(hex_counts, n_obs > 0),
    mapping = aes(fill = n_obs),
    color = NA
  ) +
  scale_fill_viridis_c(trans = "log10") +
  theme_void() +
  theme(legend.position = "bottom")

hx

ggsave(
  "output/hexbin_map.png",
  plot = hx,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

mp <- ggplot() +
  geom_sf(data = nz1_sf, fill = "#F5F5F5", color = "grey") +
  coord_sf(xlim = c(166, 179), ylim = c(-47.8, -34.2), expand = FALSE) +
  geom_point(
    data = filter(greenhood_nz2, scientific_name == "Other"),
    mapping = aes(x = longitude, y = latitude),
    fill = "#bababa",
    color = "#878787",
    shape = 21,
    size = 2
  ) +
  geom_point(
    data = filter(greenhood_nz2, scientific_name == "Pterostylis banksii"),
    mapping = aes(x = longitude, y = latitude),
    fill = "#7b9324",
    color = "#596621",
    shape = 21,
    size = 1,
    alpha = 0.8
  ) +
  theme_void() +
  theme(
    legend.position = "none"
    #plot.background = element_rect(fill = "#cfecf7", color = "#cfecf7")
  )

mp

ggsave(
  "output/tutukiwi_map.png",
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)
