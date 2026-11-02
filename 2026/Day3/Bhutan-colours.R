library(magick)
library(purrr)

image_read("2026/Day3/IMG_6417.heic") |>
  image_write("2026/Day3/three_friends.jpg", format = "jpeg", quality = 100)

photos <- list.files(
  "2026/Day3",
  pattern = "\\.heic$",
  full.names = TRUE,
  ignore.case = TRUE
)

walk(photos, \(f) {
  image_read(f) |>
    image_write(
      sub("\\.heic$", ".jpg", f, ignore.case = TRUE),
      format = "jpeg",
      quality = 100
    )
})


bhutan_palette <- c(
  "#D6D7B8",
  "#A7A68F",
  "#5e6d3f",
  "#9D6639",
  "#C9844C",
  "#928B8F",
  "#C9C8CD",
  "#757971",
  "#56494F",
  "#AEB7C5",
  "#2F5372",
  "#4D6B8D",
  "#3e5e7c",
  "#6A2613",
  "#AD4A34",
  "#E39C81",
  "#DBB6AE",
  "#E2C26B"
)

library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)

swatches <- tibble(
  id = seq_along(bhutan_palette),
  colour = bhutan_palette
)

ggplot(swatches, aes(x = id, y = 1, fill = colour)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = colour), angle = 90, size = 3, colour = "white") +
  scale_fill_identity() +
  theme_void()

set.seed(1)

bars <- tibble(
  group = factor(seq_along(bhutan_palette)),
  value = runif(length(bhutan_palette), 20, 100)
)

ggplot(bars, aes(x = group, y = value, fill = group)) +
  geom_col() +
  scale_fill_manual(values = bhutan_palette, guide = "none") +
  theme_minimal()

stacked <- expand.grid(
  x = 1:10,
  group = factor(seq_along(bhutan_palette))
) |>
  as_tibble() |>
  mutate(value = runif(n(), 1, 5))

ggplot(stacked, aes(x = x, y = value, fill = group)) +
  geom_area(colour = "white", linewidth = 0.2) +
  scale_fill_manual(values = bhutan_palette, guide = "none") +
  theme_minimal()

asia <- ne_countries(
  scale = "medium",
  continent = "Asia",
  returnclass = "sf"
) |>
  arrange(name) |>
  mutate(fill = rep_len(bhutan_palette, n()))

ggplot(asia) +
  geom_sf(aes(fill = fill), colour = "white", linewidth = 0.2) +
  scale_fill_identity() +
  coord_sf(xlim = c(60, 105), ylim = c(5, 42)) +
  theme_void()
