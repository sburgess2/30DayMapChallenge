library(tidyverse)
library(sf)
library(ggtext)
library(glue)
library(systemfonts)
library(ragg)
library(magick)
library(patchwork)
library(geomtextpath)
library(sbBrand)

patches <- st_read("2026/Day4/data/karka_patches2.gpkg") |>
  st_transform(2193)

map_area <- patches |>
  st_bbox() |>
  st_as_sfc() |>
  st_buffer(100)

map_box <- st_bbox(map_area)

osm_file <- "2026/Day4/data/map.osm"

main_road_width <- 4
minor_road_width <- 1.5

park_area <- st_read(osm_file, layer = "multipolygons", quiet = TRUE) |>
  filter(
    landuse == "forest" |
      natural == "wood" |
      leisure %in% c("park", "nature_reserve")
  ) |>
  st_transform(2193)

osm_lines <- st_read(osm_file, layer = "lines", quiet = TRUE) |>
  st_transform(2193)

roads <- osm_lines |>
  filter(
    !is.na(highway),
    !highway %in% c("path", "footway", "track", "bridleway", "steps")
  ) |>
  mutate(
    road_width = if_else(
      highway %in% c("secondary", "tertiary"),
      main_road_width,
      minor_road_width
    )
  )

streams <- osm_lines |>
  filter(waterway %in% c("stream", "river", "drain"))


road_label <- roads |>
  filter(name == "Kawakawa Road") |>
  st_union() |>
  st_cast("MULTILINESTRING") |>
  st_line_merge() |>
  st_as_sf() |>
  mutate(label = "Kawakawa Rd")

spread_graphic <- image_read("2026/Day4/images/p_karka_plant_only.png") |>
  image_ggplot()

#systemfonts::require_font("Avenir")
systemfonts::require_font("Baloo 2")
systemfonts::require_font("Nunito Sans")
font <- "Nunito Sans"

#font <- "Avenir"
brand_font <- "Baloo 2"

bg_col <- "#F5F5F5"
text_col <- "#3b3b3b"
patch_col <- "#3F6B2A"
land_col <- "#F0EFEC"
park_col <- "#DDE6D8"
road_col <- "#FFFFFF"
stream_col <- "#8FBBE0"
stream_label_col <- "#4A86B5"
label_col <- "#545454"
label_pt <- 10
park_pt <- 13

plot_title <- "Where tall reed clusters in Awahuri Forest Park"

subtitle <- paste(
  "<i>Phragmites karka</i> grows in dense clusters along Makino Stream",
  "and has spread into Awahuri Forest Park. It spreads by surface runners,",
  "so a single plant becomes a thick stand up to 4 m tall."
)

subtitle_pt <- 10

social_caption <- sb_social_caption(
  data_source = "Awahuri Forest Kitchener Park Trust. Basemap: OpenStreetMap contributors"
)

library(camcorder)

gg_record(
  device = "png",
  width = 8,
  height = 9,
  units = "in",
  dpi = 300
)

karka_map <- ggplot() +
  geom_sf(data = park_area, fill = park_col, colour = NA) +
  geom_sf(
    data = roads,
    mapping = aes(linewidth = road_width),
    colour = road_col,
    lineend = "round"
  ) +
  scale_linewidth_identity() +
  geom_sf(data = streams, colour = stream_col, linewidth = 0.8) +
  geom_sf(
    data = patches,
    fill = patch_col,
    colour = NA
  ) +
  geom_textsf(
    data = road_label,
    mapping = aes(label = label),
    family = font,
    colour = label_col,
    size = label_pt / .pt,
    linecolour = NA
  ) +
  annotate(
    "text",
    x = I(0.75),
    y = I(0.75),
    label = "Mākino Stream",
    family = font,
    colour = stream_label_col,
    size = label_pt / .pt
  ) +
  annotate(
    "text",
    x = I(0.5),
    y = I(0.4),
    label = "Awahuri Forest Park",
    family = font,
    colour = label_col,
    size = park_pt / .pt
  ) +
  annotate(
    "segment",
    x = I(0.56),
    y = I(0.85),
    xend = I(0.64),
    yend = I(0.76),
    linewidth = 0.3,
    colour = label_col,
    arrow = arrow(length = unit(1.5, "mm"), type = "closed")
  ) +
  geom_richtext(
    data = tibble(label = "Tall reed<br>(<i>Phragmites karka</i>)"),
    mapping = aes(x = I(0.54), y = I(0.9), label = label),
    family = font,
    colour = label_col,
    size = label_pt / .pt,
    fill = NA,
    label.colour = NA
  ) +
  coord_sf(
    xlim = c(map_box["xmin"], map_box["xmax"]),
    ylim = c(map_box["ymin"], map_box["ymax"]),
    crs = 2193,
    default_crs = 2193,
    expand = FALSE
  ) +
  theme_void(base_family = font) +
  theme(panel.background = element_rect(fill = land_col, colour = NA))

karka_map

subtitle_panel <- ggplot() +
  geom_textbox(
    data = tibble(x = 0, y = 1, label = subtitle),
    mapping = aes(x = x, y = y, label = label),
    family = font,
    colour = text_col,
    size = subtitle_pt / .pt,
    width = unit(1, "npc"),
    box.colour = NA,
    fill = NA,
    hjust = 0,
    vjust = 1
  ) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme_void()

header <- wrap_plots(subtitle_panel, spread_graphic, widths = c(3, 1.7))

karka_composite <- wrap_plots(
  header,
  karka_map,
  ncol = 1,
  heights = c(0.65, 3)
) +
  plot_annotation(
    title = plot_title,
    caption = social_caption,
    theme = theme(
      plot.title = element_textbox_simple(
        family = font,
        face = "bold",
        size = rel(1.2),
        colour = text_col
      ),
      plot.caption = element_textbox_simple(
        colour = text_col,
        margin = margin(t = 10)
      ),
      plot.margin = margin(10, 10, 10, 10),
      plot.background = element_rect(fill = bg_col, colour = bg_col)
    )
  )

karka_composite
record_polaroid()
gg_stop_recording()

ragg::agg_png(
  filename = "2026/Day4/output/day04-karka-clusters.png",
  width = 8,
  height = 9,
  units = "in",
  res = 300,
  background = bg_col
)
print(karka_composite)
dev.off()
