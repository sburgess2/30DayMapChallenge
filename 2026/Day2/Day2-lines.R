library(tidyverse)
library(sf)
library(osrm)
library(maptiles)
library(tidyterra)
library(shadowtext)
library(ggtext)
library(ragg)
library(sbBrand)

stops_geocoded <- read_csv("2026/Day2/data/stops_geocoded.csv") |>
  filter(travel_day <= 7)

all_stops_unique <- stops_geocoded |>
  distinct()

stops_sf <- stops_geocoded |>
  st_as_sf(coords = c("long", "lat"), crs = st_crs("EPSG:4326"))

hotel_towns <- tribble(
  ~hotel                   , ~town     ,
  "Tashi Namgay Resort"    , "Paro"    ,
  "Sonam Zhidey Resort"    , "Haa"     ,
  "Norkhil Boutique Hotel" , "Thimphu" ,
  "Dumra Farm Resort"      , "Punakha"
)

all_stops_cat <- all_stops_unique |>
  mutate(
    category = case_when(
      grepl("Airport", place, ignore.case = TRUE) ~ "airport",
      grepl("Resort|Hotel|Ecolodge", place, ignore.case = TRUE) ~ "hotel",
      grepl("Camp", place, ignore.case = TRUE) ~ "camping",
      grepl("River", place, ignore.case = TRUE) ~ "rafting",
      grepl(
        "Dzong|Temple|Monastery|Nunnery|Goenpa|Nest|Chorten",
        place,
        ignore.case = TRUE
      ) ~ "temple",
      grepl("View|Black|Preserve", place, ignore.case = TRUE) ~ "wildlife",
      grepl("Heritage|Festival", place, ignore.case = TRUE) ~ "culture",
      grepl("Pass", place, ignore.case = TRUE) ~ "pass",
      TRUE ~ "other"
    )
  )

get_route <- function(src_coords, dst_coords, route_type) {
  if (route_type == "walking") {
    line <- sf::st_sfc(
      sf::st_linestring(rbind(src_coords, dst_coords)),
      crs = 4326
    )
    dist_km <- as.numeric(sf::st_length(line)) / 1000

    sf::st_sf(
      src = paste(src_coords, collapse = ","),
      dst = paste(dst_coords, collapse = ","),
      duration = NA_real_,
      distance = dist_km,
      geometry = line
    )
  } else {
    osrmRoute(
      src = src_coords,
      dst = dst_coords,
      osrm.profile = "car"
    )
  }
}

routes_geocoded <- stops_sf |>
  mutate(
    origin_place = place,
    destination_place = lead(place),
    dest_travel_day = lead(travel_day),
    src_long = st_coordinates(geometry)[, 1],
    src_lat = st_coordinates(geometry)[, 2],
    dest_long = lead(st_coordinates(geometry)[, 1]),
    dest_lat = lead(st_coordinates(geometry)[, 2])
  ) |>
  filter(!is.na(destination_place)) |>
  rowwise() |>
  mutate(
    route = list(get_route(
      src_coords = c(src_long, src_lat),
      dst_coords = c(dest_long, dest_lat),
      route_type = route_type
    ))
  )

routes_sf <- routes_geocoded |>
  ungroup() |>
  sf::st_drop_geometry() |>
  dplyr::select(-route) |>
  dplyr::bind_cols(
    do.call(rbind, routes_geocoded$route) |>
      sf::st_as_sf() |>
      dplyr::select(geometry)
  ) |>
  sf::st_as_sf()

day3_routes <- routes_sf |>
  filter(dest_travel_day == 3)

day3_stops <- all_stops_cat |>
  filter(travel_day == 3, category != "hotel")

day3_hotel_places <- unique(c(
  day3_routes$origin_place,
  day3_routes$destination_place
))

day3_towns <- all_stops_cat |>
  filter(category == "hotel", place %in% day3_hotel_places) |>
  left_join(hotel_towns, by = c("place" = "hotel")) |>
  rename(hotel = place)

day3_bbox <- day3_routes |>
  st_union() |>
  st_buffer(3000) |>
  st_bbox()

day3_tiles <- get_tiles(
  x = st_as_sfc(day3_bbox),
  provider = "Esri.WorldImagery",
  crop = TRUE,
  zoom = 14,
  verbose = TRUE
)

day3_static_map <- ggplot() +
  geom_spatraster_rgb(data = day3_tiles) +
  geom_sf(
    data = day3_routes |> filter(route_type == "driving"),
    color = "#AB6AD5FF",
    linewidth = 1.4
  ) +
  geom_sf(
    data = day3_routes |> filter(route_type == "walking"),
    color = "#DC5FBDFF",
    linewidth = 1.2,
    linetype = "dotted"
  ) +
  geom_point(
    data = day3_stops,
    mapping = aes(x = long, y = lat),
    color = "white",
    size = 1.8
  ) +
  geom_shadowtext(
    data = day3_stops,
    mapping = aes(x = long, y = lat, label = place),
    color = "white",
    bg.color = "black",
    size = 3,
    vjust = 1.8
  ) +
  geom_shadowtext(
    data = day3_towns,
    mapping = aes(x = long, y = lat, label = town),
    color = "white",
    bg.color = "black",
    fontface = "bold",
    size = 4.5,
    vjust = -0.6
  ) +
  annotate(
    geom = "shadowtext",
    x = day3_bbox["xmax"],
    y = day3_bbox["ymin"],
    label = "Basemap: Esri World Imagery (Esri, Maxar, Earthstar Geographics)",
    color = "white",
    bg.color = "black",
    size = 2.5,
    hjust = 1.02,
    vjust = -0.8
  ) +
  coord_sf(
    default_crs = st_crs(4326),
    xlim = c(day3_bbox["xmin"], day3_bbox["xmax"]),
    ylim = c(day3_bbox["ymin"], day3_bbox["ymax"]),
    expand = FALSE
  ) +
  labs(
    title = "Day 3 in Bhutan: from Paro over the 3,988 m Chelela pass to the Haa Valley",
    subtitle = paste(
      "In early April 2026 I was very lucky to spend two weeks on holiday in Bhutan,",
      "a special country with a rich culture and beautiful forests for hiking.",
      "On Day 3 we travelled from Paro to the Haa Valley.",
      "On the way we hiked from Kila Goemba, a cliffside nunnery,",
      "to the top of Chelela Pass (3,988 m).",
      "We then visited the Black Temple, built in the 7th century, and the White Temple."
    ),
    caption = sb_social_caption()
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      face = "bold",
      size = rel(1.4),
      margin = margin(b = 6)
    ),
    plot.subtitle = element_textbox_simple(margin = margin(b = 10)),
    plot.caption = element_markdown(hjust = 0, margin = margin(t = 10)),
    plot.margin = margin(10, 10, 10, 10)
  )

day3_static_map
