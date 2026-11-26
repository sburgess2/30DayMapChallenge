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

aotea <- st_read(
  "2026/Day26/data/lds-nz-coastlines-and-islands-polygons-topo-150k-SHP/nz-coastlines-and-islands-polygons-topo-150k.shp"
) |>
  st_transform(nztm) |>
  st_make_valid() |>
  st_intersection(aotea_window)

###############################################################################
################
## Load fonts ##
################

require_font("Lato")
require_font("Baloo 2")

font <- "Lato"

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


###############################################################################
###################################
## Titles, subtitles, captions   ##
###################################

plot_title <- "Two Aotea swimming spots were above the safe swimming limits in more than half of the samples"
plot_subtitle <- "Proportion of samples above the safe swimming limit at each monitored site for the Oct-Apr 2023/24, 2024/25, 2025/26 swimming seasons. Check safeswim.org.nz before swimming."

ent_limit <- 280
ecoli_limit <- 550

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
  width = 11,
  height = 9.5,
  unit = "in",
  dpi = 300
)

bubble_pal <- c(
  "30% or more" = "#D72000FF",
  "10-29.9%" = "#FFAD0AFF",
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
      round(pct_over * 100, 1) >= 30 ~ "30% or more",
      round(pct_over * 100, 1) >= 10 ~ "10-29.9%",
      .default = "Under 10%"
    ) |>
      factor(levels = names(bubble_pal))
  ) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(nztm)


ecoli_bubbles <- site_bubbles |>
  filter(indicator == "E. coli")

ent_bubbles <- site_bubbles |>
  filter(indicator == "Enterococci")

max_pct <- max(site_bubbles$pct_over)

medlands_nudge_x <- 400

label_pal <- c(
  "30% or more" = "#D72000FF",
  "10-29.9%" = text_col,
  "Under 10%" = text_col
)

bubble_radius_m <- 1500
label_gap_m <- 350

label_adj <- tribble(
  ~site_label               ,
  ~side                     ,
  ~dy                       ,
  "Awana Stream Campground" ,
  "left"                    ,
                          0 ,
  "Kaitoke Creek"           ,
  "left"                    ,
                          0 ,
  "Medlands Stream"         ,
  "left"                    ,
                          0 ,
  "Garden Road Stream"      ,
  "left"                    ,
                          0 ,
  "Whangapoua Estuary"      ,
  "right"                   ,
                          0 ,
  "Karaka Bay"              ,
  "left"                    ,
                          0 ,
  "Port Fitzroy"            ,
  "right"                   ,
                          0 ,
  "Okupu Beach"             ,
  "left"                    ,
                          0 ,
  "Medlands North"          ,
  "right"                   ,
                       1000 ,
  "Medlands Beach"          ,
  "right"                   ,
                       -300 ,
  "Pah Beach"               ,
  "right"                   ,
                        800 ,
  "Puriri Bay"              ,
  "left"                    ,
                       -300 ,
  "Mulberry Grove South"    ,
  "below"                   ,
                          0
)

map_sites <- function(bubbles, panel_title) {
  site_xy <- st_coordinates(bubbles)

  panel_labels <- bubbles |>
    st_drop_geometry() |>
    mutate(
      x = site_xy[, 1],
      y = site_xy[, 2],
      site_label = str_squish(str_remove_all(site, "\\s*\\([^)]*\\)")),
      label = glue("{site_label}\n{scales::percent(pct_over, accuracy = 0.1)}"),
      radius = bubble_radius_m * sqrt(pct_over / max_pct) + label_gap_m
    ) |>
    left_join(label_adj, by = "site_label") |>
    mutate(
      side = replace_na(side, "right"),
      dy = replace_na(dy, 0),
      label_x = case_when(
        side == "right" ~ x + radius,
        side == "left" ~ x - radius,
        .default = x
      ),
      label_y = case_when(
        side == "below" ~ y - radius - 600,
        .default = y + dy
      ),
      label_hjust = case_when(
        side == "right" ~ 0,
        side == "left" ~ 1,
        .default = 0.5
      )
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
    geom_text(
      data = filter(panel_labels, bubble_col == "30% or more"),
      mapping = aes(
        x = label_x,
        y = label_y,
        label = label,
        hjust = label_hjust
      ),
      family = font,
      fontface = "bold",
      colour = "#D72000FF",
      size = 2.5,
      lineheight = 0.9
    ) +
    geom_text(
      data = filter(panel_labels, bubble_col != "30% or more"),
      mapping = aes(
        x = label_x,
        y = label_y,
        label = label,
        hjust = label_hjust
      ),
      family = font,
      colour = text_col,
      size = 2.5,
      lineheight = 0.9
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
    coord_sf(datum = NA, expand = FALSE, clip = "off") +
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
      plot.margin = margin(5, 30, 5, 5),
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
    subtitle = str_wrap(plot_subtitle, 135),
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
        size = 16,
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

record_polaroid()

gg_stop_recording()

###############################################################################
##############
## Save png ##
##############

ggsave(
  plot = aotea_maps,
  filename = "2026/Day26/output/20260814-aotea_maps.png",
  width = 11,
  height = 9.5,
  unit = "in",
  bg = bg_col,
  dpi = 300,
  device = agg_png
)

###############################################################################
#######################
## Instagram version ##
#######################

aotea_maps_instagram <- aotea_maps +
  plot_annotation(
    title = str_wrap(plot_title, 55),
    subtitle = str_wrap(plot_subtitle, 90)
  )

aotea_maps_instagram

ggsave(
  plot = aotea_maps_instagram,
  filename = "2026/Day26/output/20260814-aotea_maps_instagram.png",
  width = 7.5,
  height = 9.375,
  unit = "in",
  bg = bg_col,
  dpi = 144,
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
      round(pct_over * 100, 1) >= 30 ~ "30% or more",
      round(pct_over * 100, 1) >= 10 ~ "10-29.9%",
      .default = "Under 10%"
    ) |>
      factor(levels = names(bubble_pal)),
    site_label = str_squish(str_remove_all(site, "\\s*\\([^)]*\\)")),
    label = glue("{site_label}\n{scales::percent(pct_over, accuracy = 0.1)}")
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
