# Data: Stats NZ, Travel - Number of flights by direction and overseas port, Year ended December 1998-2025
# File: 2026/Day24/data/Travel_Number_of_flights_by_direction_and_overseas_port_Year_ended_December_19982025.csv
# Overseas airport coordinates are approximate and typed by hand below

library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(patchwork)
library(janitor)
library(here)

day_dir <- here("2026", "Day24")
#Selects robinson projection
robinson <- "+proj=robin +lon_0=150"
years_shown <- c(2019, 2022, 2025)

ports <- tribble(
  ~port                 , ~lon    , ~lat   ,
  "Adelaide"            ,  138.53 , -34.95 ,
  "Apia"                , -171.99 , -13.83 ,
  "Bandar Seri Begawan" ,  114.93 ,   4.94 ,
  "Bangkok"             ,  100.75 ,  13.69 ,
  "Beijing"             ,  116.59 ,  40.08 ,
  "Brisbane"            ,  153.12 , -27.38 ,
  "Buenos Aires"        ,  -58.54 , -34.82 ,
  "Cairns"              ,  145.76 , -16.89 ,
  "Coolangatta"         ,  153.51 , -28.17 ,
  "Denpasar"            ,  115.17 ,  -8.75 ,
  "Dubai"               ,   55.36 ,  25.25 ,
  "Guangzhou"           ,  113.30 ,  23.39 ,
  "Hong Kong"           ,  113.91 ,  22.31 ,
  "Honolulu"            , -157.92 ,  21.32 ,
  "Kuala Lumpur"        ,  101.71 ,   2.74 ,
  "London"              ,   -0.45 ,  51.47 ,
  "Los Angeles"         , -118.41 ,  33.94 ,
  "Melbourne"           ,  144.84 , -37.67 ,
  "Nadi"                ,  177.44 , -17.76 ,
  "Nagoya"              ,  136.80 ,  34.86 ,
  "Noumea"              ,  166.21 , -22.02 ,
  "Osaka"               ,  135.24 ,  34.43 ,
  "Papeete"             , -149.61 , -17.56 ,
  "Perth"               ,  115.97 , -31.94 ,
  "Port Vila"           ,  168.32 , -17.70 ,
  "Rarotonga"           , -159.81 , -21.20 ,
  "San Francisco"       , -122.38 ,  37.62 ,
  "Santiago"            ,  -70.79 , -33.39 ,
  "Seoul"               ,  126.45 ,  37.46 ,
  "Shanghai"            ,  121.81 ,  31.14 ,
  "Singapore"           ,  103.99 ,   1.36 ,
  "Sydney"              ,  151.18 , -33.94 ,
  "Taipei"              ,  121.23 ,  25.08 ,
  "Tokyo"               ,  140.39 ,  35.77 ,
  "Tonga"               , -175.14 , -21.24 ,
  "Vancouver"           , -123.18 ,  49.19
)

hub <- tibble(port = "Auckland", lon = 174.79, lat = -37.01)

to_xy <- function(df, crs = robinson) {
  xy <- df |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    st_transform(crs) |>
    st_coordinates()

  df |> mutate(x = xy[, 1], y = xy[, 2])
}

ports_xy <- to_xy(ports)
hub_xy <- to_xy(hub)

flights <- read_csv(
  here(
    "2026",
    "Day24",
    "data",
    "Travel_Number_of_flights_by_direction_and_overseas_port_Year_ended_December_19982025.csv"
  )
) |>
  clean_names() |>
  mutate(value = as.numeric(value)) |>
  filter(
    direction_of_flight == "Total",
    year %in% years_shown,
    overseas_port != "Total all overseas ports",
    value > 0
  ) |>
  select(year, port = overseas_port, flights = value)

flights |>
  anti_join(ports, by = "port") |>
  distinct(port)

make_arc <- function(x1, y1, x2, y2, bend = 0.2, n = 50) {
  mx <- (x1 + x2) / 2 - bend * (y2 - y1)
  my <- (y1 + y2) / 2 + bend * (x2 - x1)
  t <- seq(0, 1, length.out = n)

  st_linestring(cbind(
    (1 - t)^2 * x1 + 2 * (1 - t) * t * mx + t^2 * x2,
    (1 - t)^2 * y1 + 2 * (1 - t) * t * my + t^2 * y2
  ))
}

edges <- flights |>
  inner_join(ports_xy, by = "port") |>
  mutate(
    x_hub = hub_xy$x,
    y_hub = hub_xy$y,
    year_label = factor(year, labels = str_c("Year to December ", years_shown))
  ) |>
  arrange(desc(flights))

edges_sf <- edges |>
  mutate(
    geometry = st_sfc(
      pmap(list(x_hub, y_hub, x, y), make_arc),
      crs = robinson
    )
  ) |>
  st_as_sf()

edge_labels <- edges |>
  slice_max(flights, n = 6, by = year)

world_raw <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(name != "Antarctica") |>
  st_make_valid()

use_s2 <- sf_use_s2(FALSE)

greenland_split <- world_raw |>
  filter(name == "Greenland") |>
  (\(g) {
    bind_rows(
      st_crop(g, xmin = -180, xmax = -30.01, ymin = -90, ymax = 90),
      st_crop(g, xmin = -29.99, xmax = 180, ymin = -90, ymax = 90)
    )
  })()

sf_use_s2(use_s2)

world <- world_raw |>
  filter(name != "Greenland") |>
  bind_rows(greenland_split) |>
  st_transform(robinson)

flight_range <- range(edges$flights)

plot_year <- function(yr) {
  edges_yr <- filter(edges, year == yr)
  edges_sf_yr <- filter(edges_sf, year == yr)

  ggplot() +
    geom_sf(data = world, fill = "grey92", colour = "white", linewidth = 0.15) +
    geom_sf(
      data = edges_sf_yr,
      aes(linewidth = flights),
      colour = "#0072B2",
      alpha = 0.55,
      lineend = "round"
    ) +
    geom_point(
      data = edges_yr,
      aes(x, y),
      colour = "#0072B2",
      size = 0.9
    ) +
    geom_point(
      data = hub_xy,
      aes(x, y),
      size = 3,
      shape = 21,
      fill = "white",
      colour = "grey20",
      stroke = 1.1
    ) +
    geom_text_repel(
      data = slice_max(edges_yr, flights, n = 6),
      aes(x, y, label = port),
      size = 3,
      colour = "grey20",
      min.segment.length = Inf,
      seed = 24
    ) +
    scale_linewidth(
      range = c(0.1, 3),
      limits = flight_range,
      transform = "sqrt",
      breaks = c(1000, 5000, 10000, 20000),
      labels = scales::label_comma(),
      name = "Flights"
    ) +
    coord_sf(crs = robinson, expand = FALSE) +
    labs(tag = str_c("Year to Dec ", yr)) +
    theme_void(base_size = 12) +
    theme(
      legend.position = "top",
      plot.tag = element_text(face = "bold", size = 13),
      plot.tag.position = c(0.01, 0.97),
      plot.margin = margin(5, 5, 5, 5)
    )
}

combined <- wrap_plots(
  map(years_shown, plot_year),
  ncol = 1,
  guides = "collect"
) +
  plot_annotation(
    caption = "Total flights (arrivals and departures) between New Zealand and each overseas port. Routes are drawn from Auckland, but counts cover all New Zealand airports.\nSource: Stats NZ, Number of flights by direction and overseas port."
  ) &
  theme(
    legend.position = "top",
    plot.caption = element_text(hjust = 0, colour = "grey30", size = 9)
  )

combined


map_2025 <- plot_year(2025) +
  labs(
    tag = NULL,
    caption = "Year to December 2025. Total flights (arrivals and departures) between New Zealand and each overseas port. Routes are drawn from Auckland, but counts cover all New Zealand airports.\nSource: Stats NZ, Number of flights by direction and overseas port."
  ) +
  theme(plot.caption = element_text(hjust = 0, colour = "grey30", size = 9))

map_2025

ggsave(
  file.path(day_dir, "output", "nz_flight_network_2025.png"),
  map_2025,
  width = 12,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

projections <- c(
  "Robinson" = robinson,
  "Equal Earth" = "+proj=eqearth +lon_0=150",
  "Mercator" = "+proj=merc +lon_0=150",
  "Azimuthal equidistant" = "+proj=aeqd +lat_0=-37.01 +lon_0=174.79"
)

world_for <- function(crs, split_greenland) {
  base <- if (split_greenland) {
    bind_rows(filter(world_raw, name != "Greenland"), greenland_split)
  } else {
    world_raw
  }

  st_transform(base, crs)
}

plot_projection <- function(label, crs, yr = 2025) {
  ports_p <- to_xy(ports, crs)
  hub_p <- to_xy(hub, crs)

  y_limits <- if (str_detect(crs, "merc")) {
    to_xy(tibble(lon = c(0, 0), lat = c(-60, 75)), crs)$y
  } else {
    NULL
  }

  edges_p <- flights |>
    filter(year == yr) |>
    inner_join(ports_p, by = "port") |>
    mutate(x_hub = hub_p$x, y_hub = hub_p$y) |>
    arrange(desc(flights))

  edges_sf_p <- edges_p |>
    mutate(
      geometry = st_sfc(
        pmap(list(x_hub, y_hub, x, y), make_arc),
        crs = crs
      )
    ) |>
    st_as_sf()

  ggplot() +
    geom_sf(
      data = world_for(crs, !str_detect(crs, "aeqd")),
      fill = "grey92",
      colour = "white",
      linewidth = 0.15
    ) +
    geom_sf(
      data = edges_sf_p,
      aes(linewidth = flights),
      colour = "#0072B2",
      alpha = 0.55,
      lineend = "round"
    ) +
    geom_point(data = edges_p, aes(x, y), colour = "#0072B2", size = 0.9) +
    geom_point(
      data = hub_p,
      aes(x, y),
      size = 3,
      shape = 21,
      fill = "white",
      colour = "grey20",
      stroke = 1.1
    ) +
    scale_linewidth(
      range = c(0.1, 3),
      limits = flight_range,
      transform = "sqrt",
      breaks = c(1000, 5000, 10000, 20000),
      labels = scales::label_comma(),
      name = "Flights"
    ) +
    coord_sf(crs = crs, ylim = y_limits, expand = FALSE) +
    labs(tag = label) +
    theme_void(base_size = 12) +
    theme(
      plot.tag = element_text(face = "bold", size = 12),
      plot.tag.position = c(0.01, 0.97),
      plot.margin = margin(5, 5, 5, 5)
    )
}

projection_maps <- imap(projections, \(crs, label) plot_projection(label, crs))

projection_comparison <- wrap_plots(
  projection_maps,
  ncol = 2,
  guides = "collect"
) +
  plot_annotation(
    caption = "Year to December 2025. Total flights (arrivals and departures) between New Zealand and each overseas port, drawn from Auckland. Counts cover all New Zealand airports.\nSource: Stats NZ, Number of flights by direction and overseas port."
  ) &
  theme(
    legend.position = "top",
    plot.caption = element_text(hjust = 0, colour = "grey30", size = 9)
  )

projection_comparison

ggsave(
  file.path(day_dir, "output", "nz_flight_network_2025_projections.png"),
  projection_comparison,
  width = 16,
  height = 9,
  dpi = 300,
  bg = "white"
)
