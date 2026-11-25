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

raw <- read_csv("2026/Day26/data/2026_07_27_SSdata_GBI.csv")

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
  "2026/Day26/data/lds-nz-coastlines-and-islands-polygons-topo-150k-SHP/nz-coastlines-and-islands-polygons-topo-150k.shp"
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
bg_col <- "#fbfaf7"
text_col <- "#3b3b3b"

label_col <- "#636363"
brand_color <- "#55692f"
land_col <- "#e6e3dc"
sea_col <- "#fbfaf7"
coast_col <- "#e6e3dc"
ecoli_col <- brand_color
ent_col <- "#2C6E8F"


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

plot_title <- "Two Aotea swimming spots were above the water quality limits more than half the time"
plot_subtitle <- "Proportion of samples above the safe swimming limit at each monitored site for the Oct-Apr 2023/24, 2024/25, 2025/26 swimming seasons. Check safeswim.org.nz before swimming."

ent_limit <- 280
ecoli_limit <- 550

ecoli_subtitle <- glue(
  "<span style='color:{ecoli_col}'>*E. coli* at freshwater and estuary ",
  "sites</span> <span style='color:{label_col}'>. The size of each bubble indicates the percentage of samples",
  " (collected Oct-Apr, 2023/24 to 2025/26) above ",
  "{ecoli_limit} MPN/100mL</span>"
)

ent_subtitle <- glue(
  "<span style='color:{ent_col}'>Enterococci at marine sites</span> ",
  "<span style='color:{label_col}'>. The size of each bubble indicates the percentage of samples",
  " (collected Oct-Apr, 2023/24 to 2025/26) above ",
  "{ent_limit} MPN/100mL</span>"
)

social_caption <- sb_social_caption(
  data_source = glue(
    "Safeswim, Auckland Council. Basemap: LINZ Topo 1:50k <br>",
    "Limits: Enterococci {ent_limit} and *E. coli* {ecoli_limit} MPN/100mL"
  )
)

###############################################################################
##########
## Plot ##
##########

gg_record(
  device = png,
  width = 7,
  height = 8,
  unit = "in",
  dpi = 300
)

bubble_pal <- c(
  "30% or more" = "#D72000FF",
  "10-29%" = "#FFAD0AFF",
  "Under 10%" = "#9093A2FF"
)

site_to_remove <- c("Mulberry Grove", "Gooseberry Flat Beach")
site_bubbles <- water |>
  mutate(
    indicator = if_else(location_type == "Marine", "Enterococci", "E. coli"),
    value = if_else(location_type == "Marine", enterococci, e_coli),
    limit = if_else(location_type == "Marine", ent_limit, ecoli_limit)
  ) |>
  filter(!is.na(value)) |>
  filter(
    month(date) %in% c(10:12, 1:4),
    date >= ymd("2023-10-01"),
    date < ymd("2026-05-01")
  ) |>
  filter_out(site %in% site_to_remove) |>
  summarise(
    samples = n(),
    pct_over = mean(value > limit),
    human_detected = any(human_detected),
    lon = median(lon),
    lat = median(lat),
    .by = c(site, location_type, indicator)
  ) |>
  mutate(
    bubble_col = case_when(
      pct_over >= 0.3 ~ "30% or more",
      pct_over >= 0.1 ~ "10-29%",
      .default = "Under 10%"
    ) |>
      factor(levels = c("30% or more", "10-29%", "Under 10%"))
  ) |>
  #mutate(label = site) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(nztm)


ecoli_bubbles <- site_bubbles |>
  filter(indicator == "E. coli")

ent_bubbles <- site_bubbles |>
  filter(indicator == "Enterococci")

min_pct <- min(site_bubbles$pct_over)
max_pct <- max(site_bubbles$pct_over)

medlands_nudge_x <- 400

medlands_beach <- site_bubbles |>
  filter(site == "Medlands Beach") |>
  mutate(
    geometry = st_set_crs(
      st_geometry(geometry) + c(medlands_nudge_x, 0),
      st_crs(site_bubbles)
    )
  )

other_bubbles <- site_bubbles |>
  filter(site != "Medlands Beach")

label_sites <- bind_rows(other_bubbles, medlands_beach) |>
  mutate(
    site_label = str_squish(str_remove_all(site, "\\s*\\([^)]*\\)")),
    label = glue("{site_label}\n{scales::percent(pct_over, accuracy = 1)}")
  )

label_pal <- c(
  "30% or more" = "#D72000FF",
  "10-29%" = text_col,
  "Under 10%" = text_col
)

label_nudges <- c("Awana Stream Campground" = 1900)

record_polaroid()

gg_resize_film(width = 11, height = 8, units = "in", dpi = 300)

map_sites <- function(bubbles, panel_title) {
  panel_labels <- bubbles |>
    mutate(
      site_label = str_squish(str_remove_all(site, "\\s*\\([^)]*\\)")),
      label = glue("{site_label}\n{scales::percent(pct_over, accuracy = 1)}")
    )

  ggplot() +
    geom_sf(
      data = aotea,
      fill = land_col,
      colour = coast_col,
      linewidth = 0.2
    ) +
    geom_sf(
      data = bubbles,
      mapping = aes(size = pct_over, fill = bubble_col),
      shape = 21,
      colour = "white",
      stroke = 0.8
    ) +
    geom_text_repel(
      data = filter(panel_labels, bubble_col == "30% or more"),
      mapping = aes(label = label, geometry = geometry),
      stat = "sf_coordinates",
      family = font,
      fontface = "bold",
      colour = "#D72000FF",
      size = 2.5,
      lineheight = 0.9,
      min.segment.length = 0,
      segment.colour = label_col,
      segment.size = 0.2,
      box.padding = 0.5,
      point.padding = 0.3,
      max.overlaps = Inf,
      seed = 1
    ) +
    geom_text_repel(
      data = filter(panel_labels, bubble_col != "30% or more"),
      mapping = aes(label = label, geometry = geometry),
      stat = "sf_coordinates",
      family = font,
      colour = text_col,
      size = 2.5,
      lineheight = 0.9,
      min.segment.length = 0,
      segment.colour = label_col,
      segment.size = 0.2,
      box.padding = 0.5,
      point.padding = 0.3,
      max.overlaps = Inf,
      seed = 1
    ) +
    scale_fill_manual(
      values = bubble_pal,
      drop = FALSE,
      name = "Samples above the limit"
    ) +
    scale_size_area(
      max_size = 14,
      limits = c(0, max_pct),
      guide = "none"
    ) +
    guides(fill = guide_legend(override.aes = list(size = 5))) +
    coord_sf(datum = NA, expand = FALSE) +
    labs(title = panel_title, x = NULL, y = NULL) +
    theme_void(base_family = font, base_size = 10) +
    theme(
      plot.title = element_markdown(
        family = font,
        color = title_col,
        face = "bold",
        size = rel(1.2),
        hjust = 0,
        lineheight = 1.1,
        margin = margin(b = 8)
      ),
      plot.title.position = "plot",
      panel.background = element_rect(fill = sea_col, color = NA),
      plot.background = element_rect(fill = bg_col, color = bg_col)
    )
}

ecoli_map <- map_sites(
  ecoli_bubbles,
  glue("*E. coli*: streams and estuary")
)

ent_map <- map_sites(
  ent_bubbles,
  glue("Enterococci: beaches")
)

aotea_maps <- ecoli_map +
  ent_map +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = plot_title,
    subtitle = str_wrap(plot_subtitle, 130),
    caption = social_caption,
    theme = theme(
      legend.position = "bottom",
      legend.title = element_text(family = font, color = label_col, size = 8),
      legend.text = element_text(family = font, color = label_col, size = 9),
      legend.key = element_blank(),
      legend.background = element_blank(),
      plot.title.position = "plot",
      plot.margin = margin(10, 10, 10, 10),
      plot.title = element_text(
        family = font,
        color = title_col,
        face = "bold",
        size = 18,
        hjust = 0,
        margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        family = font,
        color = text_col,
        size = 10,
        hjust = 0,
        lineheight = 1,
        margin = margin(b = 12)
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

gg_stop_recording()

###############################################################################
##############1
## Save png ##
##############

ggsave(
  plot = aotea_maps,
  filename = "2026/Day26/output/20260814-aotea_maps.png",
  width = 11,
  height = 8,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)

###############################################################################
######################
## Season panel map ##
######################

season_bubbles <- water |>
  mutate(
    indicator = if_else(location_type == "Marine", "Enterococci", "E. coli"),
    value = if_else(location_type == "Marine", enterococci, e_coli),
    limit = if_else(location_type == "Marine", ent_limit, ecoli_limit),
    season = case_when(
      date >= ymd("2023-10-01") & date < ymd("2024-05-01") ~ "2023/24",
      date >= ymd("2024-10-01") & date < ymd("2025-05-01") ~ "2024/25",
      date >= ymd("2025-10-01") & date < ymd("2026-05-01") ~ "2025/26"
    )
  ) |>
  filter(!is.na(value), !is.na(season)) |>
  filter_out(site %in% site_to_remove) |>
  mutate(
    lon = median(lon),
    lat = median(lat),
    .by = site
  ) |>
  summarise(
    samples = n(),
    pct_over = mean(value > limit),
    lon = first(lon),
    lat = first(lat),
    .by = c(site, season, location_type, indicator)
  ) |>
  mutate(
    bubble_col = case_when(
      pct_over >= 0.3 ~ "30% or more",
      pct_over >= 0.1 ~ "10-29%",
      .default = "Under 10%"
    ) |>
      factor(levels = c("30% or more", "10-29%", "Under 10%")),
    site_label = str_squish(str_remove_all(site, "\\s*\\([^)]*\\)")),
    label = glue("{site_label}\n{scales::percent(pct_over, accuracy = 1)}")
  ) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(nztm)

season_xy <- st_coordinates(season_bubbles)

season_bubbles <- season_bubbles |>
  st_drop_geometry() |>
  mutate(
    x = season_xy[, 1] + if_else(site == "Medlands Beach", medlands_nudge_x, 0),
    y = season_xy[, 2]
  ) |>
  st_as_sf(coords = c("x", "y"), crs = nztm)

season_max_pct <- max(season_bubbles$pct_over)

season_map <- ggplot() +
  geom_sf(
    data = aotea,
    fill = land_col,
    colour = coast_col,
    linewidth = 0.2
  ) +
  geom_sf(
    data = season_bubbles,
    mapping = aes(size = pct_over, fill = bubble_col),
    shape = 21,
    colour = "white"
  ) +
  geom_text_repel(
    data = season_bubbles,
    mapping = aes(label = label, geometry = geometry, colour = bubble_col),
    stat = "sf_coordinates",
    family = font,
    size = 2,
    lineheight = 0.9,
    min.segment.length = 0,
    segment.colour = label_col,
    segment.size = 0.2,
    box.padding = 0.4,
    point.padding = 0.2,
    max.overlaps = Inf,
    seed = 1
  ) +
  facet_wrap(vars(season), nrow = 1) +
  scale_fill_manual(values = bubble_pal, name = "Samples above the limit") +
  scale_colour_manual(values = label_pal, guide = "none") +
  scale_size_area(
    max_size = 12,
    limits = c(0, season_max_pct),
    labels = scales::label_percent(accuracy = 1),
    guide = "none"
  ) +
  guides(fill = guide_legend(override.aes = list(size = 5, colour = "white"))) +
  coord_sf(datum = NA, expand = FALSE) +
  labs(caption = social_caption, x = NULL, y = NULL) +
  theme_void(base_family = font, base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(color = label_col, size = 8),
    legend.text = element_text(color = label_col, size = 8),
    legend.key = element_blank(),
    legend.background = element_blank(),
    strip.text = element_text(
      family = font,
      color = text_col,
      face = "bold",
      size = 11,
      margin = margin(b = 6)
    ),
    plot.margin = margin(10, 10, 10, 10),
    plot.caption = element_textbox_simple(
      color = text_col,
      hjust = 0,
      margin = margin(t = 10)
    ),
    panel.background = element_rect(fill = sea_col, color = NA),
    panel.spacing = unit(0.5, "lines"),
    plot.background = element_rect(fill = bg_col, color = bg_col)
  )

season_map

ggsave(
  plot = season_map,
  filename = "2026/Day26/output/20260814-aotea_season_maps.png",
  width = 14,
  height = 7,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)

###############################################################################
##########################
## Season comparison    ##
##########################

season_df <- season_bubbles |>
  st_drop_geometry() |>
  mutate(
    site_label = fct_reorder(site_label, pct_over, .fun = mean, .na_rm = TRUE)
  )

season_pal <- c(
  "2023/24" = "#B8BCCB",
  "2024/25" = "#7A7F96",
  "2025/26" = "#1F2233"
)

compare_theme <- theme_minimal(base_family = font, base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(color = label_col, size = 8),
    legend.text = element_text(color = label_col, size = 8),
    axis.text = element_text(color = text_col),
    axis.title = element_text(color = label_col, size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.2),
    panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.2),
    plot.margin = margin(10, 10, 10, 10),
    plot.caption = element_textbox_simple(
      color = text_col,
      hjust = 0,
      margin = margin(t = 10)
    ),
    plot.background = element_rect(fill = bg_col, color = bg_col)
  )

season_range <- season_df |>
  summarise(
    xmin = min(pct_over),
    xmax = max(pct_over),
    .by = site_label
  )

dot_chart <- ggplot(season_df, aes(x = pct_over, y = site_label)) +
  geom_vline(
    xintercept = c(0.1, 0.3),
    colour = "grey70",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_linerange(
    data = season_range,
    mapping = aes(xmin = xmin, xmax = xmax, y = site_label),
    inherit.aes = FALSE,
    colour = "grey75",
    linewidth = 0.6
  ) +
  geom_point(
    mapping = aes(colour = season, size = samples),
    alpha = 0.9
  ) +
  scale_colour_manual(values = season_pal, name = "Season") +
  scale_size_continuous(range = c(1.5, 5), name = "Samples") +
  scale_x_continuous(
    labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  labs(
    x = "Samples above the limit",
    y = NULL,
    caption = social_caption
  ) +
  compare_theme +
  theme(panel.grid.major.y = element_line(colour = "grey93", linewidth = 0.2))

dot_chart

ggsave(
  plot = dot_chart,
  filename = "2026/Day26/output/20260814-aotea_season_dotchart.png",
  width = 8,
  height = 9,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)
