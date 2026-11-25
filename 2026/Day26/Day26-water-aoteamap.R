###############################################################################
####################
## Load libraries ##
####################

library(tidyverse)
library(ggtext)
library(glue)
library(systemfonts)
library(ragg)
library(sbBrand)
library(camcorder)
library(sf)
library(ggrepel)
library(patchwork)
library(fontawesome)
library(png)

###############################################################################
###############
## Load data ##
###############

raw <- read_csv("data/raw/2026_07_27_SSdata_GBI.csv")

water <- raw |>
  select(
    site = `Safeswim Site`,
    site_code = `Site Code`,
    date_time = Date,
    enterococci = `Enterococci (MPN/100mL)`,
    e_coli = `E. coli (MPN/100mL)`,
    human_flag = `PCR Human HumM2 Flag`,
    human_result = `PCR Human HumM2 Numeric Result`,
    location_type = `Location Type`,
    lon = Easting,
    lat = Northing
  ) |>
  mutate(
    date = parse_date_time(date_time, orders = c("dmy HM", "dmy")),
    site = str_replace(site, " @ ", " "),
    human_detected = !is.na(human_result) &
      (is.na(human_flag) | human_flag != "<")
  ) |>
  filter(!is.na(date))

nztm <- 2193

aotea_window <- st_bbox(
  c(xmin = 175.28, xmax = 175.60, ymin = -36.40, ymax = -36.05),
  crs = 4326
) |>
  st_as_sfc() |>
  st_transform(nztm)

#coast_path <- list.files(
# "data/linz",
# pattern = "\.shp$",
# recursive = TRUE,
# full.names = TRUE
#)[1]

aotea <- st_read(
  "data/linz/lds-nz-coastlines-and-islands-polygons-topo-150k-SHP/nz-coastlines-and-islands-polygons-topo-150k.shp"
) |>
  st_transform(nztm) |>
  st_make_valid() |>
  st_intersection(aotea_window)

###############################################################################
#############################
## Look at data (EDA)      ##
#############################

str(water)
glimpse(water)
unique(water$location_type)
water[!complete.cases(water), ]

st_crs(aotea)
st_bbox(aotea)
nrow(aotea)

water |>
  summarise(
    n = n(),
    n_ent = sum(!is.na(enterococci)),
    n_ecoli = sum(!is.na(e_coli)),
    lon = first(lon),
    lat = first(lat),
    .by = c(site, location_type)
  )

###############################################################################
################
## Load fonts ##
################

require_font("Lato")
require_font("Baloo 2")

font <- "Lato"
brand_font <- "Baloo 2"

###############################################################################
####################
## Define colours ##
####################

title_col <- "#131313"
bg_col <- "#F5F5F5"
text_col <- "#3b3b3b"

label_col <- "#636363"
brand_color <- "#55692f"
land_col <- "#D9D9D9"
sea_col <- "#FCFCFC"
coast_col <- "#BFBFBF"
stroke_col <- "#4D4D4D"
stroke_col <- "black"
ecoli_col <- brand_color
ent_col <- "#2C6E8F"
icon_col <- "black"

###############################################################################
###############################
## Social media variables    ##
###############################

github_icon <- "&#xf09b"
github_username <- "sburgess2"

linkedin_icon <- "&#xf08c"
linkedin_username <- "sa-burgess2"

###############################################################################
###################################
## Titles, subtitles, captions   ##
###################################

plot_title <- "tbc"

ent_limit <- 280
ecoli_limit <- 550

ecoli_subtitle <- glue(
  "<span style='color:{ecoli_col}'>*E. coli* at freshwater and estuary ",
  "sites</span> <span style='color:{label_col}'>. The size of each bubble indicates the percentage of samples",
  " (collected Jan 2023 - Jul 2026) above ",
  "{ecoli_limit} MPN/100mL</span>"
)

ent_subtitle <- glue(
  "<span style='color:{ent_col}'>Enterococci at marine sites</span> ",
  "<span style='color:{label_col}'>. The size of each bubble indicates the percentage of samples",
  " (collected Jan 2023 - Jul 2026) above ",
  "{ent_limit} MPN/100mL</span>"
)

social_caption <- sb_social_caption(
  data_source = "Safeswim, Auckland Council. Coastline: LINZ Topo 1:50k"
)

###############################################################################
##########
## Plot ##
##########

gg_record(
  device = agg_png,
  width = 7,
  height = 8,
  unit = "in",
  dpi = 300
)

site_to_remove <- c("Mulberry Grove", "Medlands North", "Gooseberry Flat Beach")
site_bubbles <- water |>
  mutate(
    indicator = if_else(location_type == "Marine", "Enterococci", "E. coli"),
    value = if_else(location_type == "Marine", enterococci, e_coli),
    limit = if_else(location_type == "Marine", ent_limit, ecoli_limit)
  ) |>
  filter(!is.na(value)) |>
  filter_out(site %in% site_to_remove) |>
  summarise(
    samples = n(),
    pct_over = mean(value > limit),
    human_detected = any(human_detected),
    lon = first(lon),
    lat = first(lat),
    .by = c(site, location_type, indicator)
  ) |>
  mutate(label = site) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(nztm)

human_threshold <- 0.10

human_bubbles <- site_bubbles |>
  filter(human_detected, pct_over > human_threshold)

human_icon_file <- tempfile(fileext = ".png")

fa_png("person", file = human_icon_file, fill = icon_col, width = 256)

human_icon <- readPNG(human_icon_file)

icon_height <- 900

icon_layers <- function(bubbles) {
  marks <- human_bubbles |>
    filter(indicator %in% unique(bubbles$indicator))

  if (nrow(marks) == 0) {
    return(NULL)
  }

  xy <- st_coordinates(marks)
  half_height <- icon_height / 2
  half_width <- half_height * (dim(human_icon)[2] / dim(human_icon)[1])

  pmap(
    list(xy[, 1], xy[, 2]),
    \(x, y) {
      annotation_raster(
        human_icon,
        xmin = x - half_width,
        xmax = x + half_width,
        ymin = y - half_height,
        ymax = y + half_height,
        interpolate = TRUE
      )
    }
  )
}

ecoli_bubbles <- site_bubbles |>
  filter(indicator == "E. coli")

ent_bubbles <- site_bubbles |>
  filter(indicator == "Enterococci")

min_pct <- min(site_bubbles$pct_over)
max_pct <- max(site_bubbles$pct_over)

label_nudges <- c("Awana Stream Campground" = 1900)

map_sites <- function(bubbles, bubble_col, subtitle) {
  nudged <- bubbles |>
    mutate(label_nudge = coalesce(unname(label_nudges[site]), 0))

  ggplot() +
    geom_sf(
      data = aotea,
      fill = land_col,
      colour = coast_col,
      linewidth = 0.2
    ) +
    geom_sf(
      data = bubbles,
      mapping = aes(size = pct_over),
      shape = 21,
      fill = bubble_col,
      colour = stroke_col,
      #stroke = 0.3,
      alpha = 0.5
    ) +
    icon_layers(bubbles) +
    geom_text_repel(
      data = nudged,
      mapping = aes(geometry = geometry, label = label),
      stat = "sf_coordinates",
      family = font,
      size = 2.4696,
      colour = text_col,
      lineheight = 1.1,
      seed = 1,
      nudge_y = nudged$label_nudge,
      min.segment.length = 0,
      segment.colour = label_col,
      segment.size = 0.3,
      box.padding = 0.7,
      point.padding = 0.4
    ) +
    scale_size_area(
      max_size = 14,
      limits = c(0, max_pct),
      breaks = c(min_pct, max_pct),
      labels = scales::label_percent(accuracy = 1),
      guide = guide_legend(
        title = "Proportion of samples above the limit",
        title.position = "top",
        direction = "horizontal",
        override.aes = list(
          shape = 21,
          fill = "grey70",
          colour = stroke_col,
          alpha = 0.5
        )
      )
    ) +
    coord_sf(datum = NA, expand = FALSE) +
    labs(
      subtitle = subtitle,
      x = NULL,
      y = NULL
    ) +
    theme_void(base_family = font, base_size = 10) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(color = label_col, size = 8),
      legend.text = element_text(color = label_col, size = 8),
      legend.key = element_blank(),
      legend.background = element_blank(),
      plot.title.position = "plot",
      plot.margin = margin(10, 10, 10, 10),
      plot.caption = element_textbox_simple(
        color = text_col,
        hjust = 0,
        margin = margin(t = 10)
      ),
      #plot.title = element_textbox_simple(
      #  color = title_col,
      # face = "bold",
      #size = rel(1.2)
      #),
      plot.subtitle = element_textbox_simple(
        color = text_col,
        margin = margin(t = 5, b = 14)
      ),
      panel.background = element_rect(fill = sea_col, color = NA),
      plot.background = element_rect(fill = bg_col, color = bg_col)
    )
}

ecoli_map <- map_sites(ecoli_bubbles, ecoli_col, ecoli_subtitle)

ent_map <- map_sites(ent_bubbles, ent_col, ent_subtitle)

aotea_maps <- ecoli_map +
  ent_map +
  plot_layout(guides = "collect") +
  plot_annotation(
    #title = plot_title,
    caption = social_caption,
    theme = theme(
      legend.position = "bottom",
      legend.title = element_text(family = font, color = label_col, size = 8),
      legend.text = element_text(family = font, color = label_col, size = 8),
      legend.key = element_blank(),
      legend.background = element_blank(),
      plot.title.position = "plot",
      plot.margin = margin(10, 10, 10, 10),
      plot.title = element_textbox_simple(
        family = font,
        color = title_col,
        face = "bold",
        size = rel(1.2),
        margin = margin(b = 10)
      ),
      plot.caption = element_textbox_simple(
        family = font,
        color = text_col,
        size = rel(0.8),
        hjust = 0,
        margin = margin(t = 10)
      ),
      plot.background = element_rect(fill = bg_col, color = bg_col)
    )
  )

aotea_maps

record_polaroid()

###############################################################################
##############1
## Save png ##
##############

ggsave(
  plot = aotea_maps,
  filename = "output/20260814-aotea_maps.png",
  width = 11,
  height = 7,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)
