###############################################################################
## Load libraries                                                            ##
###############################################################################

library(tidyverse)
library(ggtext)
library(glue)
library(systemfonts)
library(ragg)
library(rinat)
library(sf)
library(maptiles)
library(tidyterra)
library(sbBrand)

###############################################################################
## Load data                                                                  ##
###############################################################################

phrag_raw <- get_inat_obs(
  query = "Phragmites karka",
  geo = TRUE,
  maxresults = 2000,
  place_id = 6803
)

write_csv(phrag_raw, "data/20260612_phrag.csv")
phrag_raw <- read_csv("data/20260612_phrag.csv")

phrag <- phrag_raw |>
  select(
    scientific_name,
    place_guess,
    datetime,
    time_zone,
    latitude,
    longitude,
    observed_on,
    user_login,
    quality_grade
  ) |>
  filter(scientific_name == "Phragmites karka", quality_grade == "research") |>
  filter(
    longitude > 174.6 & longitude < 176.1,
    latitude < -39.8 & latitude > -40.9
  )

phrag_sf <- st_as_sf(phrag, coords = c("longitude", "latitude"), crs = 4326)

cities <- tibble(
  name = c("Palmerston North", "Whanganui"),
  longitude = c(175.6082, 175.0470),
  latitude = c(-40.3523, -39.9301)
) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

regions <- tibble(
  name = c("Manawatū", "Whanganui"),
  longitude = c(175.61, 175.05),
  latitude = c(-40.35, -39.93)
) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

#bbox_sf <- st_as_sfc(
# st_bbox(
#  c(xmin = 174.6, ymin = -40.9, xmax = 176.1, ymax = -39.8),
#  crs = st_crs(4326)
#)
#)

#bbox_sf <- st_as_sfc(
# st_bbox(
#  c(xmin = 174.6, ymin = -40.9, xmax = 175.7, ymax = -39.8),
# crs = st_crs(4326)))

#For square map
bbox_sf <- st_as_sfc(
  st_bbox(
    c(xmin = 174.6, ymin = -40.9, xmax = 176.044, ymax = -39.8),
    crs = st_crs(4326)
  )
)

#osm_tiles <- get_tiles(bbox_sf, provider = "OpenStreetMap", zoom = 9)

# MapTiler Aquarelle Map
maptiler_aquarelle <- list(
  src = "MapTiler.Aquarelle",
  q = "https://api.maptiler.com/maps/aquarelle-v4/{z}/{x}/{y}.png?key={apikey}",
  sub = NA,
  cit = "© MapTiler © OpenStreetMap contributors"
)

osm_tiles <- get_tiles(
  bbox_sf,
  provider = maptiler_aquarelle,
  apikey = Sys.getenv("MAPTILER_KEY"),
  zoom = 11, # 9 was too pale/coarse for this extent
  crop = TRUE, # square it to your bbox
  forceDownload = TRUE # bypass any stale cached tiles from earlier attempts
)

#CartoDB.Voyager Map
osm_tiles <- get_tiles(
  bbox_sf,
  provider = "CartoDB.VoyagerNoLabels",
  zoom = 11,
  crop = TRUE
)


#To view different tiles see
#https://www.maptiler.com/
#https://leaflet-extras.github.io/leaflet-providers/preview/index.html

###############################################################################
## Look at data                                                               ##
###############################################################################

str(phrag)
head(phrag)
glimpse(phrag)
unique(phrag$place_guess)
phrag[!complete.cases(phrag), ]

###############################################################################
## Load fonts                                                                 ##
###############################################################################

#Register google fonts using systemfonts (showtext and ggtext-gridtext weren't working together, it may also be because of the newer version of ggplot2 being used for this plot)
require_font("Lato")
require_font("Baloo 2")

#register_font(
# name = "Font Awesome 7 Brands",
#plain = "fonts/fontawesome-free-7.1.0/otfs/Font-Awesome-7-Brands-Regular-400.otf"
#)

font <- "Lato"
brand_font <- "Baloo 2"

###############################################################################
## Define colours                                                             ##
###############################################################################

bg_col <- "#F5F5F5"
text_col <- "#3b3b3b"
brand_color <- "#55692f"
title_col <- "#22513DFF"
highlight_fill <- "#6a3d9a"
point_fill <- "#cab2d6"
point_col <- "black"
line_col <- "grey60"
label_col <- "#545454"


###############################################################################
## Titles, subtitles, captions                                               ##
###############################################################################

#plot_title <- "New Zealand <i>Phragmites karka</i> observations are<br>concentrated in the Manawatū-Whanganui region"

plot_title <- "The invasive weed, <i>Phragmites karka</i>, has been located across the North Island<br> of New Zealand with heavy infestations in the Manawatū-Whanganui region"
social_caption <- sb_social_caption(data_source = "iNaturalist")
###############################################################################
## Plot                                                                       ##
###############################################################################

#Note to self have a go at replacing circle with plant symbol. Put title on map

title_x <- mean(c(174.6, 176.044)) # 175.322

library(camcorder)

gg_record(
  device = "png",
  width = 6,
  height = 6,
  unit = "in",
  dpi = 300
)


ggplot() +
  geom_spatraster_rgb(data = osm_tiles) +
  geom_sf(
    data = phrag_sf,
    shape = 21,
    fill = point_fill,
    color = point_col,
    size = 3,
    alpha = 0.6
  ) +
  # highlight the special point
  geom_point(
    data = phrag |>
      filter(grepl("Tangimoana", place_guess)) %>%
      filter(
        longitude > 175.223 & longitude < 175.225,
        latitude < -40.28 & latitude > -40.32
      ),
    aes(x = longitude, y = latitude),
    shape = 21,
    fill = highlight_fill, # dark purple fill
    color = point_col, # keep same outline
    size = 4
  ) +

  #geom_sf(
  # data = cities,
  #shape = 21,
  #fill = "white",
  #color = "black",
  #size = 0.5
  #) +
  #geom_sf_text(
  # data = cities,
  #aes(label = name),
  #family = font,
  #color = label_col,
  #size = 8, #
  #fontface = "bold",
  #size.unit = "pt",
  #vjust = c(0.5, -0.4) # Palmerston North centred, Whanganui lifted above its point) +
  geom_sf_text(
    data = regions,
    aes(label = name),
    family = font,
    color = label_col,
    size = 12, #
    fontface = "bold",
    size.unit = "pt",
    vjust = c(0.5, -0.5) # Palmerston North centred, Whanganui lifted above its point
  ) +
  labs(
    # title = plot_title) +
    caption = social_caption
  ) +
  #x = NULL,
  #y = NULL)

  #annotate(
  # "curve",
  # x = 175.02,
  # y = -40.303,
  #xend = 175.21,
  #yend = -40.303,
  #color = line_col,
  #linewidth = 0.4,
  #curvature = -0.2,
  #arrow = grid::arrow(length = grid::unit(0.02, "npc"), type = "closed")) +
  #geom_richtext(
  # data = tibble(
  #  x = 175.02,
  # y = -40.27,
  #label = "<i>P. karka</i> was first<br>identified in 2006<br> on the banks of the<br>Rangitāiki River"),
  #aes(x = x, y = y, label = label),
  #family = font,
  #color = text_col,
  #size = 2.8, #in mm, 1 pt is 0.35mm
  #hjust = 1,
  #vjust = 1,
  #fill = NA,
  #label.color = NA) +
  #geom_richtext(
  # data = tibble(
  #  x = title_x,
  # y = -39.81,
  #label = plot_title),
  #aes(x = x, y = y, label = label),
  #family = font,
  #fontface = "bold",
  #color = title_col,
  #size = 3.5,
  #hjust = 0.5,
  #vjust = 1,
  #lineheight = 1.2,
  #fill = scales::alpha(bg_col, 0.7), # translucent box so text reads over the map
  #label.color = NA,
  #label.padding = grid::unit(c(4, 6, 4, 6), "pt")) +

  # annotate(
  #  "text",
  # x = 174.63,
  #y = -40.87,
  #label = "data: iNaturalist June 2026",
  #family = font,
  #color = label_col,
  #size = 2.5,
  #hjust = 0,
  #vjust = 0) +
  coord_sf(default_crs = 4326) +

  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    #plot.margin = margin(10, 10, 10, 10),
    plot.caption = element_textbox_simple(
      color = text_col,
      hjust = 0,
      margin = margin(t = 10)
    ),
    #plot.title = element_markdown(
    # color = text_col,
    #face = "bold",
    #family = font,
    #lineheight = 1.2,
    #margin = margin(b = 8)),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
record_polaroid()

gg_stop_recording()

###############################################################################
## Save png                                                                   ##
###############################################################################

ggsave(
  filename = "phrag_osm_nolabel.png",
  width = 6,
  height = 6,
  dpi = 300,
  bg = "transparent",
  device = ragg::agg_png
)
