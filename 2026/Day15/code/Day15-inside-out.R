library(tidyverse)
library(sf)
library(rnaturalearth)

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

# Start and end coordinates of the journey, the straight-line direction
# between them, its length, and a unit vector at right angles to it
start <- st_coordinates(martensbridge)[1, 1:2]
end <- st_coordinates(easthome)[1, 1:2]
direction <- end - start
route_length <- sqrt(sum(direction^2))
normal <- c(-direction[2], direction[1]) / route_length

# t runs from 0 (start) to 1 (end) in 100 steps. Each step sits on the
# straight line, then is pushed sideways by two sine waves of different
# lengths added together (uneven bends). Multiplying by sin(pi * t) makes
# the push zero at both ends so the route still meets the two towns.
journey <- tibble(t = seq(0, 1, length.out = 100)) |>
  mutate(
    offset = route_length *
      (0.05 * sin(2 * pi * 1.5 * t) + 0.025 * sin(2 * pi * 4.2 * t + 1)) *
      sin(pi * t),
    x = start[1] + direction[1] * t + normal[1] * offset,
    y = start[2] + direction[2] * t + normal[2] * offset
  ) |>
  select(x, y) |>
  as.matrix() |>
  st_linestring() |>
  st_sfc(crs = 25832) |>
  st_sf(geometry = _)

# Layers are drawn in order: country, journey, then towns on top.
# nudge_x and nudge_y shift the labels away from their dots (in metres).
p <- ggplot(germany_flipped) +
  geom_sf(fill = "#D8BE97", colour = "#B89A6E", linewidth = 0.4) +
  geom_sf(data = journey, linetype = "dotted", linewidth = 0.8) +
  geom_sf(data = easthome, size = 3) +
  geom_sf_text(
    data = easthome,
    aes(label = label),
    nudge_x = -45000,
    nudge_y = 15000
  ) +
  geom_sf(data = martensbridge, size = 3) +
  geom_sf_text(
    data = martensbridge,
    aes(label = label),
    nudge_x = 40000,
    nudge_y = 15000
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#FBF8F2", colour = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

p

dir.create("../output", showWarnings = FALSE)

ggsave(
  "../output/day15-upside-down-germany.png",
  p,
  width = 7,
  height = 8,
  dpi = 300
)
