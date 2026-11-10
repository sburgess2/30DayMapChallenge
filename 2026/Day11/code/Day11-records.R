#Data downloaded using exportify

library(tidyverse)
library(tidygeocoder)
#library(packcircles)

#function to clean artist names
clean_name <- function(x) {
  x |>
    str_to_lower() |> #convert to lower case
    str_squish() #trim spaces at start and end
}

song_files <- c(
  "2026/Day11/data/Sara.csv",
  "2026/Day11/data/sara80s.csv",
  "2026/Day11/data/saraeighties.csv"
)

songs <- map(song_files, read_csv) |>
  list_rbind() |>
  distinct(`Track URI`, .keep_all = TRUE) |>
  filter(
    `Track Name` != "Faith", #This has now been deleted from my playlist.
    !`Artist Name(s)` %in% c("T3NZU", "Snoblack") #They sing Beggin, just kept original of Beggin
  ) |>
  mutate(
    `Artist Name(s)` = if_else(
      `Artist Name(s)` == "Disney;Shakira",
      "Shakira",
      `Artist Name(s)`
    )
  ) |>
  mutate(
    `Artist Name(s)` = str_remove(`Artist Name(s)`, "^Ziv Zaifman;")
  ) |>
  mutate(
    `Artist Name(s)` = if_else(
      str_detect(
        `Album Name`,
        regex(
          "soundtrack|motion picture|\\bcast\\b|\\(from",
          ignore_case = TRUE
        )
      ),
      str_extract(`Artist Name(s)`, "^[^;]+"),
      `Artist Name(s)`
    )
  ) |>
  distinct(`Track Name`, .keep_all = TRUE)

write_csv(songs, "2026/Day11/data/songs_clean.csv")

songs_wide <- songs |>
  select(`Track Name`, `Track URI`, artist = `Artist Name(s)`) |>
  separate_wider_delim(
    artist,
    delim = ";",
    names_sep = "_",
    too_few = "align_start"
  )

song_long <- songs_wide |>
  pivot_longer(
    starts_with("artist_"),
    names_to = NULL,
    values_to = "artist",
    values_drop_na = TRUE
  )

song_count <- song_long |>
  mutate(artist_key = clean_name(artist)) |>
  count(artist_key, name = "songs") |>
  summarise(songs = sum(songs), .by = artist_key)

artists <- read_csv("2026/Day11/data/artists_all.csv") |>
  filter_out(is.na(Country))

#arcgis uses Esri's commercial adress and place data. It's more forgving than  OSM and the number of requests aren't limited.
locations <- artists |>
  distinct(Town, Region, Country) |>
  unite(
    "full_address",
    Town,
    Region,
    Country,
    sep = ", ",
    na.rm = TRUE,
    remove = FALSE
  ) |>
  geocode(address = full_address, method = "arcgis")

records <- artists |>
  distinct(Artist, artist_key, Town, Region, Country) |>
  inner_join(song_counts, by = "artist_key") |>
  left_join(locations, by = c("Town", "Region", "Country")) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  st_transform(equal_earth) |>
  arrange(desc(songs)) |>
  mutate(rank = row_number(), radius = sqrt(songs) * size_scale)


# Circle packing layout, no map (Visuals 3 and 4)
pack <- circleProgressiveLayout(records$songs, sizetype = "area")

packed <- records |>
  select(Artist, songs, label_fill) |>
  bind_cols(pack |> transmute(cx = x, cy = y, r = radius * 0.96))

ellipse_poly <- function(cx, cy, a, b, angle, n = 120) {
  t <- seq(0, 2 * pi, length.out = n + 1)
  ex <- a * cos(t)
  ey <- b * sin(t)
  ring <- cbind(
    cx + ex * cos(angle) - ey * sin(angle),
    cy + ex * sin(angle) + ey * cos(angle)
  )
  ring[n + 1, ] <- ring[1, ]
  st_polygon(list(ring))
}

semibreve_shape <- function(cx, cy, r) {
  st_difference(
    ellipse_poly(cx, cy, 0.95 * r, 0.66 * r, 0),
    ellipse_poly(cx, cy, 0.55 * r, 0.3 * r, 0.9)
  )
}

crotchet_shape <- function(cx, cy, r) {
  head_x <- cx - 0.12 * r
  head_y <- cy - 0.4 * r
  stem_x <- head_x + c(0.4, 0.48) * r
  head <- ellipse_poly(head_x, head_y, 0.5 * r, 0.34 * r, 0.35)
  stem <- st_polygon(list(cbind(
    c(stem_x[1], stem_x[2], stem_x[2], stem_x[1], stem_x[1]),
    c(head_y, head_y, cy + 0.9 * r, cy + 0.9 * r, head_y)
  )))
  st_union(head, stem)
}

stave <- packed |>
  crossing(offset = -2:2) |>
  mutate(
    d = offset * 0.4 * r,
    half = sqrt(pmax(r^2 - d^2, 0)),
    x0 = cx - half,
    x1 = cx + half,
    y0 = cy + d
  )

label_box <- packed |>
  mutate(
    xmin = cx - 0.75 * r,
    xmax = cx + 0.75 * r,
    ymin = cy - 0.99 * r,
    ymax = cy - 0.82 * r
  )

plot_packed <- function(shape_fn) {
  notes <- packed |>
    mutate(geometry = st_sfc(pmap(list(cx, cy, r), shape_fn))) |>
    st_as_sf()

  ggplot() +
    geom_segment(
      data = stave,
      aes(x = x0, xend = x1, y = y0, yend = y0),
      colour = "grey65",
      linewidth = 0.25
    ) +
    geom_sf(data = notes, aes(fill = label_fill), colour = NA) +
    geom_fit_text(
      data = label_box,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = Artist),
      colour = "grey25",
      grow = TRUE,
      reflow = TRUE,
      min.size = 0
    ) +
    scale_fill_identity(
      guide = "legend",
      breaks = unname(genre_colours),
      labels = names(genre_colours),
      name = NULL
    ) +
    labs(title = plot_title) +
    theme_void() +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 20,
        hjust = 0.5,
        margin = margin(b = 10)
      ),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

# Visual 3: circle packing, semibreves (day11_semibreve_packing.png)
semibreve_plot <- plot_packed(semibreve_shape)
semibreve_plot

ggsave(
  "2026/Day11/output/day11_semibreve_packing.png",
  semibreve_plot,
  width = 10,
  height = 10,
  dpi = 300,
  bg = "white",
  create.dir = TRUE
)

# Visual 4: circle packing, crotchets (day11_crotchet_packing.png)
crotchet_plot <- plot_packed(crotchet_shape)
crotchet_plot

ggsave(
  "2026/Day11/output/day11_crotchet_packing.png",
  crotchet_plot,
  width = 10,
  height = 10,
  dpi = 300,
  bg = "white",
  create.dir = TRUE
)

# Treble and bass staves, notes in order of song count (Visuals 5 and 6)
stave_gap <- 9

staff <- records |>
  select(Artist, songs) |>
  mutate(
    r = 2 * sqrt(songs / max(songs)),
    stave = if_else(row_number() %% 2 == 1, "Treble", "Bass"),
    stave_y = if_else(stave == "Treble", 0, -stave_gap),
    width = 2.3 * r + 0.6
  ) |>
  group_by(stave) |>
  mutate(
    cx = 4 + cumsum(width) - width / 2,
    label_row = row_number() %% 2
  ) |>
  ungroup()

stave_end <- max(staff$cx + staff$width / 2) + 1

stave_tbl <- tibble(stave = c("Treble", "Bass"), stave_y = c(0, -stave_gap))

staff_lines <- stave_tbl |>
  crossing(offset = -2:2) |>
  mutate(y = stave_y + offset, x0 = 0, x1 = stave_end)

bar_lines <- tibble(
  x = c(0, stave_end),
  y0 = -stave_gap - 2,
  y1 = 2
)

plot_staff <- function(shape_fn, lift) {
  notes <- staff |>
    mutate(
      cy = stave_y + lift(r),
      geometry = st_sfc(pmap(list(cx, cy, r), shape_fn))
    ) |>
    st_as_sf()

  ggplot() +
    geom_segment(
      data = staff_lines,
      aes(x = x0, xend = x1, y = y, yend = y),
      colour = "grey55",
      linewidth = 0.3
    ) +
    geom_segment(
      data = bar_lines,
      aes(x = x, xend = x, y = y0, yend = y1),
      colour = "grey35",
      linewidth = 0.4
    ) +
    geom_text(
      data = stave_tbl,
      aes(x = 2, y = stave_y, label = stave),
      fontface = "italic",
      colour = "grey40",
      size = 4
    ) +
    geom_sf(data = notes, fill = "#1b1b1b", colour = NA) +
    geom_text(
      data = staff,
      aes(x = cx, y = stave_y - 2.5 - 0.8 * label_row, label = Artist),
      vjust = 1,
      colour = "grey25",
      size = 2.8
    ) +
    labs(title = plot_title) +
    theme_void() +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 20,
        hjust = 0.5,
        margin = margin(b = 10)
      ),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

# Visual 5: two staves, semibreves (day11_semibreve_staff.png)
semibreve_staff_plot <- plot_staff(semibreve_shape, \(r) 0 * r)
semibreve_staff_plot

ggsave(
  "2026/Day11/output/day11_semibreve_staff.png",
  semibreve_staff_plot,
  width = 14,
  height = 7,
  dpi = 300,
  bg = "white",
  create.dir = TRUE
)

# Visual 6: two staves, crotchets (day11_crotchet_staff.png)
crotchet_staff_plot <- plot_staff(crotchet_shape, \(r) 0.4 * r)
crotchet_staff_plot

ggsave(
  "2026/Day11/output/day11_crotchet_staff.png",
  crotchet_staff_plot,
  width = 14,
  height = 7,
  dpi = 300,
  bg = "white",
  create.dir = TRUE
)

# Longitude as time, latitude as pitch (Visuals 7 and 8)
nudge_x <- function(df, gap = 1.04, iterations = 500) {
  x <- df$x
  y <- df$y
  r <- df$radius * gap
  n <- length(x)

  for (k in seq_len(iterations)) {
    for (i in seq_len(n - 1)) {
      for (j in (i + 1):n) {
        dx <- x[j] - x[i]
        dy <- y[j] - y[i]
        need <- sqrt(max((r[i] + r[j])^2 - dy^2, 0))

        if (abs(dx) < need) {
          dir <- if (dx == 0) sign(cos(i + j)) else sign(dx)
          shift <- need - abs(dx)
          wi <- r[j] / (r[i] + r[j])
          x[i] <- x[i] - dir * shift * wi
          x[j] <- x[j] + dir * shift * (1 - wi)
        }
      }
    }
  }

  df |> mutate(x = x)
}

pitch_notes <- records |>
  select(Artist, songs, Town, Region, Country) |>
  left_join(
    locations |> select(Town, Region, Country, long, lat),
    by = c("Town", "Region", "Country")
  ) |>
  mutate(
    x = long * 0.25,
    y = lat * 7 / 60,
    radius = 1.3 * sqrt(songs / max(songs))
  ) |>
  nudge_x() |>
  arrange(x) |>
  mutate(label_row = row_number() %% 2)

pitch_left <- min(pitch_notes$x - pitch_notes$radius) - 4
pitch_right <- max(pitch_notes$x + pitch_notes$radius) + 2

pitch_lines <- crossing(
  stave = c("Treble", "Bass"),
  offset = 0:4
) |>
  mutate(
    y = if_else(stave == "Treble", 2 + offset, -6 + offset),
    x0 = pitch_left,
    x1 = pitch_right
  )

pitch_bars <- tibble(
  x = c(pitch_left, pitch_right),
  y0 = -6,
  y1 = 6
)

lat_ticks <- tibble(
  lat = c(60, 30, 0, -30),
  label = c("60°N", "30°N", "Equator", "30°S")
) |>
  mutate(y = lat * 7 / 60, x = pitch_left - 0.6)

lon_ticks <- tibble(
  lon = seq(-120, 180, by = 60),
  label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°")
) |>
  mutate(x = lon * 0.25)

plot_pitch <- function(shape_fn, lift) {
  notes <- pitch_notes |>
    mutate(
      cy = y + lift(radius),
      geometry = st_sfc(pmap(list(x, cy, radius), shape_fn))
    ) |>
    st_as_sf()

  ggplot() +
    geom_hline(
      yintercept = 0,
      colour = "grey80",
      linewidth = 0.3,
      linetype = "dashed"
    ) +
    geom_segment(
      data = pitch_lines,
      aes(x = x0, xend = x1, y = y, yend = y),
      colour = "grey55",
      linewidth = 0.3
    ) +
    geom_segment(
      data = pitch_bars,
      aes(x = x, xend = x, y = y0, yend = y1),
      colour = "grey35",
      linewidth = 0.4
    ) +
    geom_text(
      data = lat_ticks,
      aes(x = x, y = y, label = label),
      hjust = 1,
      colour = "grey40",
      size = 3
    ) +
    geom_text(
      data = lon_ticks,
      aes(x = x, y = -10.5, label = label),
      colour = "grey40",
      size = 3
    ) +
    geom_sf(data = notes, fill = "#1b1b1b", colour = NA) +
    geom_text(
      data = pitch_notes,
      aes(x = x, y = y - 0.8 * radius - 0.4 - 0.8 * label_row, label = Artist),
      vjust = 1,
      colour = "grey25",
      size = 2.6
    ) +
    labs(title = plot_title) +
    theme_void() +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 20,
        hjust = 0.5,
        margin = margin(b = 10)
      ),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

# Visual 7: longitude/latitude staff, semibreves (day11_semibreve_pitch.png)
semibreve_pitch_plot <- plot_pitch(semibreve_shape, \(r) 0 * r)
semibreve_pitch_plot

ggsave(
  "2026/Day11/output/day11_semibreve_pitch.png",
  semibreve_pitch_plot,
  width = 16,
  height = 6,
  dpi = 300,
  bg = "white",
  create.dir = TRUE
)

# Visual 8: longitude/latitude staff, crotchets (day11_crotchet_pitch.png)
crotchet_pitch_plot <- plot_pitch(crotchet_shape, \(r) 0.4 * r)
crotchet_pitch_plot

ggsave(
  "2026/Day11/output/day11_crotchet_pitch.png",
  crotchet_pitch_plot,
  width = 16,
  height = 6,
  dpi = 300,
  bg = "white",
  create.dir = TRUE
)


##Multi panel map
#This is the one I like the least
# Visual 9: three-panel map, Europe / US and Canada / NZ and Australia (day11_records_three_panel.png)
panel_scale <- 140000

region_limits <- function(box) {
  st_bbox(
    c(xmin = box[1], xmax = box[2], ymin = box[3], ymax = box[4]),
    crs = 4326
  ) |>
    st_as_sfc() |>
    st_transform(equal_earth) |>
    st_bbox()
}

region_panel <- function(countries, box, label) {
  lims <- region_limits(box)

  rec <- records |>
    filter(Country %in% countries, Artist != "HUNTR/X") |>
    mutate(
      x = x_true,
      y = y_true,
      radius = sqrt(songs) * panel_scale
    ) |>
    arrange(desc(songs)) |>
    mutate(rank = row_number()) |>
    nudge_apart()

  rec_moved <- rec |>
    filter(sqrt((x - x_true)^2 + (y - y_true)^2) > 1000)

  rec_layers <- rec |>
    group_split(rank) |>
    map(record_layers) |>
    list_flatten()

  ggplot() +
    geom_sf(data = world, fill = "grey93", colour = "white", linewidth = 0.2) +
    geom_segment(
      data = rec_moved,
      aes(x = x_true, y = y_true, xend = x, yend = y),
      colour = "grey45",
      linewidth = 0.3
    ) +
    geom_point(
      data = rec_moved,
      aes(x = x_true, y = y_true),
      colour = "grey45",
      size = 0.8
    ) +
    rec_layers +
    scale_fill_identity(
      guide = "legend",
      breaks = unname(genre_colours),
      labels = names(genre_colours),
      name = NULL
    ) +
    coord_sf(
      xlim = unname(lims[c("xmin", "xmax")]),
      ylim = unname(lims[c("ymin", "ymax")]),
      expand = FALSE
    ) +
    labs(title = label) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0),
      panel.border = element_rect(colour = "grey70", fill = NA),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

boxes <- list(
  europe = c(-10, 22, 48, 61),
  us_canada = c(-128, -62, 23, 52),
  oceania = c(110, 180, -48, -8)
)

panel_widths <- map_dbl(boxes, \(b) {
  lims <- region_limits(b)
  unname(lims["xmax"] - lims["xmin"])
})

europe_panel <- region_panel(
  c("United Kingdom", "Sweden"),
  boxes$europe,
  "Europe"
)
us_panel <- region_panel(c("USA", "Canada"), boxes$us_canada, "US and Canada")
oceania_panel <- region_panel(
  c("New Zealand", "Australia"),
  boxes$oceania,
  "New Zealand and Australia"
)

three_panel <- patchwork::wrap_plots(
  europe_panel,
  us_panel,
  oceania_panel,
  nrow = 1,
  widths = unname(panel_widths),
  guides = "collect"
) +
  patchwork::plot_annotation(
    title = plot_title,
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  ) &
  theme(legend.position = "bottom")

three_panel

ggsave(
  "2026/Day11/output/day11_records_three_panel.png",
  three_panel,
  width = 18,
  height = 7,
  dpi = 300,
  bg = "white",
  create.dir = TRUE
)
