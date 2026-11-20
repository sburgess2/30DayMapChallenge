library(tidyverse)
library(magick)
library(ggtext)
library(sbBrand)

systemfonts::require_font("Baloo 2")

file_map <- "2026/Day19/output/output_1.png"
file_out <- ""

font <- "Baloo 2"
text_col <- "#333333"

map_img <- image_read(file_map)
map_info <- image_info(map_img)
map_aspect <- map_info$width / map_info$height
map_raster <- as.raster(map_img)

plot_title <- "The smell of my parents' garden"

social_caption <- sb_social_caption(
  data_source = "Garden traced by the author from Esri World Imagery. Stream: OpenStreetMap contributors"
)

library(camcorder)

gg_record(
  width = 10,
  height = 8.5,
  dpi = 300,
  device = png
)
annotated <- ggplot() +
  annotation_raster(map_raster, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
  geom_text(
    aes(x = 0.62, y = 0.16),
    label = "In spring, as you walk around the\n Michaela arch of Michaela, you catch a\nwaft of sweet, lemony fragrance.",
    family = font,
    hjust = 0,
    vjust = 1,
    lineheight = 1.05,
    size = 8 / .pt,
    colour = text_col
  ) +
  geom_curve(
    aes(x = 0.62, y = 0.16, xend = 0.55, yend = 0.33),
    curvature = 0.25,
    colour = text_col,
    linewidth = 0.4,
    arrow = arrow(length = unit(2, "mm"), type = "closed")
  ) +
  geom_text(
    aes(x = 0.04, y = 0.80),
    label = "In late summer and early autumn,\nthe NZ cranberries smell like\napple pie. I love eating them in\nsalads.",
    family = font,
    hjust = 0,
    vjust = 1,
    lineheight = 1.05,
    size = 8 / .pt,
    colour = text_col
  ) +
  geom_curve(
    aes(x = 0.12, y = 0.75, xend = 0.2, yend = 0.5),
    curvature = 0.25,
    colour = text_col,
    linewidth = 0.4,
    arrow = arrow(length = unit(2, "mm"), type = "closed")
  ) +
  coord_fixed(
    ratio = 1 / map_aspect,
    xlim = c(0, 1),
    ylim = c(0, 1),
    expand = FALSE
  ) +
  labs(title = plot_title, caption = social_caption) +
  theme_void(base_family = font) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(
      family = font,
      colour = text_col,
      face = "bold",
      size = 20,
      margin = margin(b = 8)
    ),
    plot.caption = element_textbox_simple(
      family = font,
      colour = text_col,
      hjust = 0,
      size = 8,
      margin = margin(t = 10)
    ),
    plot.margin = margin(15, 15, 15, 15),
    plot.background = element_rect(fill = "white", colour = "white")
  )
annotated
record_polaroid()
ggsave(
  "2026/Day19/output/garden_scent_annotated.png",
  annotated,
  width = 10,
  height = 8.5,
  dpi = 300,
  device = ragg::agg_png
)
