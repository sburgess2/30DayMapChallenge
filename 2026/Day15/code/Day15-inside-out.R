library(tidyverse)
library(sf)

library(rnaturalearth)
library(ggtext)
library(sbBrand)
library(systemfonts)

if (requireNamespace("showtext", quietly = TRUE)) {
  showtext::showtext_auto(FALSE)
}

require_font("IM Fell English")

social_caption <- sb_social_caption()

# Germany boundary from Natural Earth, projected to UTM zone 32N (metres)
# so the shape is not stretched, keeping only the geometry column
germany <- ne_countries(
  scale = "medium",
  country = "Germany",
  returnclass = "sf"
) |>
  st_transform(25832) |>
  st_geometry()

# The point everything is rotated around
centre <- st_centroid(germany)

# Rotate 180 degrees: move the centre to (0, 0), flip the sign of every
# coordinate, move back, then restore the CRS that the arithmetic drops
rotate_180 <- function(geometry, centre) {
  ((geometry - centre) * -1 + centre) |>
    st_set_crs(25832)
}

germany_flipped <- rotate_180(germany, centre) |>
  st_sf(geometry = _)


# Schwerin (lon/lat), projected then rotated around Germany's centre

easthome <- st_sfc(st_point(c(11.41, 53.63)), crs = 4326) |>
  st_transform(25832) |>
  rotate_180(centre) |>
  st_sf(label = "Easthome", geometry = _)

# Vaduz, Liechtenstein (lon/lat), projected then rotated the same way

martensbridge <- st_sfc(st_point(c(9.52, 47.14)), crs = 4326) |>
  st_transform(25832) |>
  rotate_180(centre) |>
  st_sf(label = "Martensbridge", geometry = _)

cantons_scale <- 1.8

switzerland_flipped <- ne_countries(
  scale = "medium",
  country = "Switzerland",
  returnclass = "sf"
) |>
  st_transform(25832) |>
  st_geometry() |>
  rotate_180(centre)

cantons_shift <- c(-90000, 0)

place_cantons <- function(geometry) {
  ((geometry - st_geometry(martensbridge)) *
    cantons_scale +
    st_geometry(martensbridge) +
    cantons_shift) |>
    st_set_crs(25832)
}

switzerland_flipped <- switzerland_flipped |>
  place_cantons() |>
  st_sf(geometry = _)

greenwell <- st_sfc(st_point(c(8.41, 46.82)), crs = 4326) |>
  st_transform(25832) |>
  rotate_180(centre)

greenwell <- greenwell |>
  place_cantons() |>
  st_sf(label = "Greenwell", geometry = _)

start <- st_coordinates(martensbridge)[1, 1:2]
greenwell_xy <- st_coordinates(greenwell)[1, 1:2]
road_vector <- greenwell_xy - start
road_length <- sqrt(sum(road_vector^2))
road_direction <- road_vector / road_length
road_normal <- c(-road_direction[2], road_direction[1])
road_middle <- (start + greenwell_xy) / 2 + road_normal * road_length * 0.1

greenwell_road <- cbind(
  spline(1:3, c(start[1], road_middle[1], greenwell_xy[1]), n = 100)$y,
  spline(1:3, c(start[2], road_middle[2], greenwell_xy[2]), n = 100)$y
) |>
  st_linestring() |>
  st_sfc(crs = 25832) |>
  st_sf(geometry = _)

raven_centre <- (start + greenwell_xy) / 2

raven_range <- expand_grid(row = 0:1, t = seq(-50000, 50000, by = 9000)) |>
  mutate(
    x = raven_centre[1] + t + row * 4500,
    y = raven_centre[2] + row * 8000
  ) |>
  st_as_sf(coords = c("x", "y"), crs = 25832) |>
  filter(as.numeric(st_distance(geometry, st_geometry(greenwell_road))) > 9000)

raven_label <- st_sfc(
  st_point(raven_centre + c(65000, 4000)),
  crs = 25832
) |>
  st_sf(label = "Raven Range", geometry = _)

cantons_label <- st_sfc(
  st_point(start + c(270000, -25000)),
  crs = 25832
) |>
  st_sf(label = "THE CANTONS", geometry = _)

home_angle <- 100 * pi / 180
home_xy <- greenwell_xy + 35000 * c(cos(home_angle), sin(home_angle))

home_road <- rbind(greenwell_xy, home_xy) |>
  st_linestring() |>
  st_sfc(crs = 25832) |>
  st_sf(geometry = _)

home <- st_sfc(st_point(home_xy), crs = 25832) |>
  st_sf(geometry = _)

end <- st_coordinates(easthome)[1, 1:2]
direction <- end - start
route_length <- sqrt(sum(direction^2))
normal <- c(-direction[2], direction[1]) / route_length


to_xy <- function(u, v) {
  tibble(
    x = start[1] + direction[1] * u + normal[1] * v * route_length,
    y = start[2] + direction[2] * u + normal[2] * v * route_length
  )
}

as_points <- function(df) {
  df |>
    mutate(to_xy(u, v)) |>
    st_as_sf(coords = c("x", "y"), crs = 25832)
}

smooth_line <- function(waypoints, n = 200) {
  i <- seq_len(nrow(waypoints))
  to_xy(
    spline(i, waypoints$u, n = n)$y,
    spline(i, waypoints$v, n = n)$y
  ) |>
    as.matrix() |>
    st_linestring() |>
    st_sfc(crs = 25832) |>
    st_sf(geometry = _)
}

earth <- "#7A5C3A"

ink <- "#3B2F22"

journey <- tribble(
  ~u    , ~v     ,
  0.000 ,  0.000 ,
  0.000 ,  0.070 ,
  0.050 ,  0.090 ,
  0.095 ,  0.090 ,
  0.200 ,  0.060 ,
  0.320 ,  0.040 ,
  0.450 ,  0.000 ,
  0.580 , -0.060 ,
  0.700 ,  0.000 ,
  0.800 ,  0.050 ,
  0.900 ,  0.030 ,
  1.000 ,  0.000
) |>
  smooth_line()

towns <- greenwell

rosehall <- tribble(
  ~u    , ~v    , ~label     ,
  0.380 , 0.060 , "Rosehall"
) |>
  as_points()

features <- tribble(
  ~u    , ~v    , ~label      , ~hjust , ~fontface , ~colour , ~size ,
  0.500 , 0.140 , "THE WEALD" , 0.5    , "plain"   , earth   ,    12
) |>
  as_points()

note_targets <- bind_rows(
  st_coordinates(easthome) |> as_tibble(),
  st_coordinates(greenwell_road)[75, 1:2] |> t() |> as_tibble(),
  st_coordinates(martensbridge) |> as_tibble(),
  st_coordinates(home) |> as_tibble(),
  st_coordinates(towns) |> as_tibble(),
  st_coordinates(rosehall) |> as_tibble()
) |>
  select(xend = X, yend = Y)

notes <- tribble(
  ~u     , ~v     , ~curve , ~label                                                                                  ,
   1.200 ,  0.300 , 0.2    , "In book three, Penric and Desdemona help investigate a murder in the Weald's capital." ,
  -0.300 , -0.500 , 0.2    , "Penric is allowed to keep Desdemona."                                                  ,
  -0.700 ,  0.500 , 0.2    , "Penric grows up at Jurald Court."                                                      ,
  -0.700 ,  0.080 , 0.2    , "On his way to his wedding, Penric inherits a dying sorcerer's demon."                  ,
   0.380 ,  0.600 , 0.2    , "Penric spends three years at the seminary training as a learned divine and sorcerer."
) |>
  mutate(
    to_xy(u, v),
    label = str_wrap(label, 22)
  ) |>
  bind_cols(note_targets) |>
  mutate(
    gap = sqrt((xend - x)^2 + (yend - y)^2),
    xend = xend - (xend - x) / gap * 10000,
    yend = yend - (yend - y) / gap * 10000
  )

library(camcorder)
gg_record(
  device = "png",
  width = 7,
  height = 9,
  unit = "in",
  dpi = 300
)

p <- ggplot(germany_flipped) +
  geom_sf(fill = "#D8BE97", colour = "#B89A6E", linewidth = 0.4) +
  geom_sf(
    data = switzerland_flipped,
    fill = "#D8BE97",
    colour = "#B89A6E",
    linewidth = 0.4
  ) +
  geom_sf_text(
    data = cantons_label,
    aes(label = label),
    family = "IM Fell English",
    colour = earth,
    size = 12 / .pt
  ) +
  geom_sf(data = raven_range, shape = 17, size = 2.2, colour = earth) +
  geom_sf(data = journey, linetype = "dotted", linewidth = 0.8, colour = ink) +
  geom_sf(
    data = greenwell_road,
    linetype = "dotted",
    linewidth = 0.8,
    colour = ink
  ) +
  geom_sf(
    data = home_road,
    linetype = "dotted",
    linewidth = 0.8,
    colour = ink
  ) +
  geom_sf(data = home, size = 1.5, colour = ink) +
  geom_blank(
    data = tibble(
      x = c(min(notes$x) - 140000, max(notes$x) + 140000),
      y = c(min(notes$y) - 100000, max(notes$y) + 100000)
    ),
    aes(x = x, y = y)
  ) +
  geom_sf_text(
    data = raven_label,
    aes(label = label),
    hjust = 0,
    family = "IM Fell English",
    fontface = "italic",
    colour = earth,
    size = 11 / .pt
  ) +
  geom_sf(data = towns, size = 2, colour = ink) +
  geom_sf_text(
    data = towns,
    aes(label = label),
    hjust = 0,
    nudge_x = 12000,
    nudge_y = 20000,
    family = "IM Fell English",
    colour = ink,
    size = 12 / .pt,
    lineheight = 0.9
  ) +
  geom_sf(data = rosehall, size = 2, colour = ink) +
  geom_sf_text(
    data = rosehall,
    aes(label = label),
    hjust = 0,
    nudge_x = 6000,
    nudge_y = 20000,
    family = "IM Fell English",
    colour = ink,
    size = 12 / .pt
  ) +
  geom_sf_text(
    data = features,
    aes(
      label = label,
      hjust = hjust,
      fontface = fontface,
      colour = colour,
      size = size / .pt
    ),
    family = "IM Fell English"
  ) +
  scale_colour_identity() +
  scale_size_identity() +
  geom_curve(
    data = filter(notes, curve > 0),
    aes(x = x, y = y, xend = xend, yend = yend),
    curvature = 0.2,
    linewidth = 0.3,
    colour = ink,
    arrow = arrow(length = unit(0.15, "cm"))
  ) +
  geom_curve(
    data = filter(notes, curve < 0),
    aes(x = x, y = y, xend = xend, yend = yend),
    curvature = -0.2,
    linewidth = 0.3,
    colour = ink,
    arrow = arrow(length = unit(0.15, "cm"))
  ) +
  coord_sf(clip = "off") +
  geom_label(
    data = notes,
    aes(x = x, y = y, label = label),
    family = "IM Fell English",
    colour = ink,
    fill = "#FBF8F2",
    border.colour = "#E6D5B8",
    linewidth = 0.6,
    size = 8 / .pt,
    lineheight = 0.9
  ) +
  geom_sf(data = easthome, size = 3, colour = ink) +
  geom_sf_text(
    data = easthome,
    aes(label = label),
    nudge_x = -45000,
    nudge_y = 15000,
    family = "IM Fell English",
    colour = ink,
    size = 12 / .pt
  ) +
  geom_sf(data = martensbridge, size = 3, colour = ink) +
  geom_sf_text(
    data = martensbridge,
    aes(label = label),
    hjust = 1,
    nudge_x = -15000,
    nudge_y = 15000,
    family = "IM Fell English",
    colour = ink,
    size = 12 / .pt
  ) +
  labs(
    title = "Penric's journey",
    caption = social_caption
  ) +
  theme_void(base_family = "IM Fell English") +
  theme(
    plot.background = element_rect(fill = "#FBF8F2", colour = NA),
    plot.margin = margin(20, 20, 20, 20),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      colour = ink,
      size = 18,
      margin = margin(b = 10)
    ),
    plot.caption = element_markdown(
      family = "Baloo 2",
      hjust = 0,
      margin = margin(t = 10)
    )
  )

p
record_polaroid()


ggsave(
  "2026/Day15/output/day15-upside-down-germany.png",
  p,
  width = 7,
  height = 9,
  dpi = 300,
  device = ragg::agg_png
)
