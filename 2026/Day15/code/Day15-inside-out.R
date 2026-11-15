library(tidyverse)
library(sf)

library(rnaturalearth)
library(showtext)

font_add_google("IM Fell English", "fell")
showtext_auto()
showtext_opts(dpi = 300)

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

start <- st_coordinates(martensbridge)[1, 1:2]
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

raven_range <- expand_grid(row = 0:1, v = seq(-0.40, 0.30, by = 0.02)) |>
  mutate(
    v = v + row * 0.01,
    u = 0.085 + row * 0.015
  ) |>
  filter(abs(v - 0.09) > 0.03) |>
  as_points() |>
  st_filter(st_buffer(germany_flipped, -5000))

towns <- tribble(
  ~u    , ~v    , ~label                              ,
  0.900 , 0.030 , "Mother's hospice,\nGreenwell Town"
) |>
  as_points()

features <- tribble(
  ~u     , ~v     , ~label        , ~hjust , ~fontface , ~colour , ~size ,
   0.160 , -0.200 , "Raven Range" , 0.5    , "italic"  , earth   , 4     ,
  -0.070 , -0.250 , "THE CANTONS" , 0.5    , "plain"   , earth   , 4.4   ,
   0.720 ,  0.250 , "THE WEALD"   , 0.5    , "plain"   , earth   , 4.4
) |>
  as_points()

note_targets <- bind_rows(
  st_coordinates(easthome) |> as_tibble(),
  st_line_sample(st_geometry(journey), sample = 0.96) |>
    st_cast("POINT") |>
    st_coordinates() |>
    as_tibble(),
  st_coordinates(martensbridge) |> as_tibble()
) |>
  select(xend = X, yend = Y)

notes <- tribble(
  ~u     , ~v     , ~label                                   ,
   1.080 ,  0.330 , "Easthome: add story text here"          ,
   0.800 , -0.220 , "Road to Greenwell: add story text here" ,
  -0.080 ,  0.350 , "Martensbridge: add story text here"
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
  height = 8,
  unit = "in",
  dpi = 300
)

p <- ggplot(germany_flipped) +
  geom_sf(fill = "#D8BE97", colour = "#B89A6E", linewidth = 0.4) +
  geom_sf(data = raven_range, shape = 17, size = 2.2, colour = earth) +
  geom_sf(data = journey, linetype = "dotted", linewidth = 0.8, colour = ink) +
  geom_sf(data = towns, size = 2, colour = ink) +
  geom_sf_text(
    data = towns,
    aes(label = label),
    hjust = 0,
    nudge_x = 12000,
    nudge_y = 20000,
    family = "fell",
    colour = ink,
    size = 3.6,
    lineheight = 0.9
  ) +
  geom_sf_text(
    data = features,
    aes(
      label = label,
      hjust = hjust,
      fontface = fontface,
      colour = colour,
      size = size
    ),
    family = "fell"
  ) +
  scale_colour_identity() +
  scale_size_identity() +
  geom_curve(
    data = notes,
    aes(x = x, y = y, xend = xend, yend = yend),
    curvature = 0.2,
    linewidth = 0.3,
    colour = ink,
    arrow = arrow(length = unit(0.15, "cm"))
  ) +
  geom_label(
    data = notes,
    aes(x = x, y = y, label = label),
    family = "fell",
    colour = ink,
    fill = "#FBF8F2",
    label.size = 0,
    size = 3.2,
    lineheight = 0.9
  ) +
  geom_sf(data = easthome, size = 3, colour = ink) +
  geom_sf_text(
    data = easthome,
    aes(label = label),
    nudge_x = -45000,
    nudge_y = 15000,
    family = "fell",
    colour = ink,
    size = 4.6
  ) +
  geom_sf(data = martensbridge, size = 3, colour = ink) +
  geom_sf_text(
    data = martensbridge,
    aes(label = label),
    hjust = 1,
    nudge_x = -15000,
    nudge_y = 15000,
    family = "fell",
    colour = ink,
    size = 4.6
  ) +
  theme_void(base_family = "fell") +
  theme(
    plot.background = element_rect(fill = "#FBF8F2", colour = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

p
record_polaroid()


ggsave(
  "2026/Day15/output/day15-upside-down-germany.png",
  p,
  width = 7,
  height = 8,
  dpi = 300
)
