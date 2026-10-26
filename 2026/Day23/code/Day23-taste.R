library(tidyverse)
library(sf)
library(rnaturalearth)
library(tidygeocoder)
library(janitor)

lower_ni_regions <- c("Wellington", "Manawatu", "Hawke", "Taranaki")

regions <- ne_states(country = "New Zealand", returnclass = "sf") |>
  filter(str_detect(name, str_c(lower_ni_regions, collapse = "|"))) |>
  select(region = name) |>
  st_transform(2193)

wineries_lower_ni <- read_csv(
  "2026/Day23/data/Risk_Measure_Data_Extract_2026-10-26.csv"
) |>
  clean_names() |>
  filter(str_detect(primary, "Receipt of Grapes")) |>
  mutate(
    name = coalesce(na_if(operator_trading_name, ""), operator_business_name),
    full_address = str_c(
      physical_address,
      physical_town_city,
      "New Zealand",
      sep = ", "
    )
  ) |>
  geocode(address = full_address, method = "arcgis") |>
  filter(!is.na(lat)) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  st_transform(2193) |>
  select(name, physical_address, physical_town_city) |>
  mutate(source = "MPI WSMP register") |>
  st_join(regions, join = st_is_within_distance, dist = 2000, left = FALSE) |>
  distinct(name, geometry, .keep_all = TRUE)

# Save the geocoded wineries as a GeoPackage so the geocoding doesn't need rerunning; delete_dsn = TRUE replaces any existing file
st_write(
  wineries_lower_ni,
  "2026/Day23/data/wineries_lower_ni.gpkg",
  delete_dsn = TRUE
)

wineries_lower_ni |>
  st_transform(4326) |>
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) |>
  st_drop_geometry() |>
  write_csv("2026/Day23/data/wineries_lower_ni.csv")

wineries_lower_ni |>
  st_drop_geometry() |>
  count(region, source)

region_labels <- regions |>
  st_point_on_surface()

ggplot() +
  geom_sf(data = regions, fill = "grey92", color = "white", linewidth = 0.4) +
  geom_sf(
    data = wineries_lower_ni,
    color = "#722F37",
    alpha = 0.7,
    size = 1.6
  ) +
  geom_sf_text(
    data = region_labels,
    aes(label = region),
    color = "grey35",
    size = 2.8224,
    fontface = "bold"
  ) +
  coord_sf(datum = NA) +
  theme_minimal(base_size = 10) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    panel.grid = element_blank()
  )

ggsave(
  "2026/Day23/output/wineries_lower_north_island.png",
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)
