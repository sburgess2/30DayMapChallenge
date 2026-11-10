#Data downloaded using exportify

library(tidyverse)
library(tidygeocoder)
library(sf)
library(camcorder)
library(ggrepel)
library(ggtext)
library(sbBrand)
#library(packcircles)

systemfonts::require_font("Dancing Script")
systemfonts::require_font("Baloo 2")
systemfonts::require_font("Cabin")
systemfonts::require_font("Noto Music")

font <- "Dancing Script"
label_font <- "Cabin"
caption_font <- "Baloo 2"
text_col <- "grey25"
bg_col <- "white"

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

#write_csv(songs, "2026/Day11/data/songs_clean.csv")

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
  drop_na(Country) |>
  mutate(artist_key = clean_name(Artist))

#arcgis uses Esri's commercial adress and place data. It's more forgving than  OSM and the number of requests aren't limited.
#locations <- artists |>
# distinct(Town, Region, Country) |>
#unite( "full_address",Town,Region,Country,sep = ", ", na.rm = TRUE,remove = FALSE) |>
#geocode(address = full_address, method = "arcgis")

#write_csv(locations, "2026/Day11/data/locations.csv")
locations <- read_csv("2026/Day11/data/locations.csv")

missing_coords <- locations |>
  filter(is.na(lat) | is.na(long))

stopifnot(
  "Some locations have no lat or long, see missing_coords" = nrow(
    missing_coords
  ) ==
    0
)

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

minim_shape <- function(cx, cy, r) {
  head_x <- cx - 0.12 * r
  head_y <- cy - 0.4 * r
  stem_x <- head_x + c(0.4, 0.48) * r
  head <- st_difference(
    ellipse_poly(head_x, head_y, 0.5 * r, 0.34 * r, 0.35),
    ellipse_poly(head_x, head_y, 0.28 * r, 0.12 * r, 0.9)
  )
  stem <- st_polygon(list(cbind(
    c(stem_x[1], stem_x[2], stem_x[2], stem_x[1], stem_x[1]),
    c(head_y, head_y, cy + 0.9 * r, cy + 0.9 * r, head_y)
  )))
  st_union(head, stem)
}

dotted_minim_shape <- function(cx, cy, r) {
  dot <- ellipse_poly(cx + 0.75 * r, cy - 0.4 * r, 0.14 * r, 0.14 * r, 0)
  st_union(minim_shape(cx, cy, r), dot)
}

note_shape <- function(cx, cy, r, songs) {
  case_when(
    songs == 1 ~ list(crotchet_shape(cx, cy, r)),
    songs == 2 ~ list(minim_shape(cx, cy, r)),
    songs == 3 ~ list(dotted_minim_shape(cx, cy, r)),
    .default = list(semibreve_shape(cx - 0.12 * r, cy, 0.62 * r))
  )[[1]]
}

space_x <- function(x, half_width, pad, region, bar_space) {
  n <- length(x)
  need <- half_width[-n] +
    half_width[-1] +
    pad +
    if_else(region[-n] != region[-1], bar_space, 0)
  offset <- c(0, cumsum(need))
  isoreg(x - offset)$yf + offset
}

note_radius <- 1.15
note_pad <- 0.1
line_gap <- 28
bar_space <- 1.2

region_levels <- c(
  "USA",
  "Canada",
  "Latin America and Caribbean",
  "UK and Ireland",
  "Europe",
  "Africa",
  "Asia",
  "Australia and NZ"
)

all_notes <- artists |>
  distinct(artist_key, .keep_all = TRUE) |>
  inner_join(song_count, by = "artist_key") |>
  left_join(
    locations |> select(Town, Region, Country, long, lat),
    by = c("Town", "Region", "Country")
  ) |>
  drop_na(long, lat) |>
  mutate(
    x = long * 0.25,
    y = lat * 7 / 60,
    radius = note_radius,
    half_width = case_when(
      songs <= 2 ~ 0.55,
      songs == 3 ~ 0.75,
      .default = 0.6
    ) *
      radius
  ) |>
  mutate(
    region = case_when(
      Country == "USA" ~ "USA",
      Country == "Canada" ~ "Canada",
      Country %in%
        c("Puerto Rico", "Trinidad and Tobago", "Colombia", "Mexico") ~
        "Latin America and Caribbean",
      Country %in% c("United Kingdom", "Ireland") ~ "UK and Ireland",
      Country == "Tanzania" ~ "Africa",
      Country %in%
        c("India", "South Korea", "Singapore", "Philippines", "Armenia") ~
        "Asia",
      Country %in% c("Australia", "New Zealand") ~ "Australia and NZ",
      .default = "Europe"
    ),
    region = factor(region, levels = region_levels),
    line = if_else(
      region %in% region_levels[1:3],
      1,
      2
    )
  ) |>
  arrange(line, region, x) |>
  group_by(line) |>
  mutate(
    x = space_x(
      x,
      half_width,
      note_pad * note_radius,
      region,
      bar_space
    ),
    x = x - min(x - half_width) + 5
  ) |>
  ungroup() |>
  mutate(
    y = y - (line - 1) * line_gap,
    note_y = if_else(songs >= 4, y, y + 0.4 * radius),
    note_col = if_else(songs >= 4, "#000000", "#9a9a9a")
  )

region_bars <- all_notes |>
  arrange(line, x) |>
  group_by(line) |>
  mutate(
    next_region = lead(region),
    bar_x = (x + half_width + lead(x - half_width)) / 2
  ) |>
  ungroup() |>
  filter(!is.na(next_region), region != next_region) |>
  transmute(
    line,
    x = bar_x,
    y0 = -6 - (line - 1) * line_gap,
    y1 = 6 - (line - 1) * line_gap
  )

#Clefs are text characters from the Unicode "Musical Symbols" block, drawn with the Noto Music font.
#"\U0001D11E" is the Unicode code point U+1D11E, the treble (G) clef.
#"\U0001D122" is U+1D122, the bass (F) clef. "\U" followed by eight hex digits is how R writes a character above U+FFFF.
#y = 4 is the middle of the treble staff (lines at y = 2 to 6) and y = -4 is the middle of the bass staff (lines at y = -6 to -2).
#Subtracting (line - 1) * line_gap moves the second line of music down, and x = 2.4 sits the clefs just right of the opening bar line.
clefs <- crossing(
  line = 1:2,
  tibble(clef = c("\U0001D11E", "\U0001D122"), y = c(4, -4))
) |>
  mutate(y = y - (line - 1) * line_gap, x = 2.4)

all_labels <- all_notes |>
  filter(songs >= 4) |>
  mutate(
    system_y = -(line - 1) * line_gap,
    above = y - system_y >= 0
  ) |>
  arrange(line, x) |>
  group_by(line, above) |>
  mutate(stagger = (row_number() %% 2) * 1) |>
  ungroup() |>
  mutate(
    label_y = system_y + if_else(above, 9 + stagger, -9 - stagger),
    label_nudge = label_y - y
  )

all_right <- max(all_notes$x + all_notes$half_width) + 2

all_lines <- crossing(
  line = 1:2,
  stave = c("Treble", "Bass"),
  offset = 0:4
) |>
  mutate(
    y = if_else(stave == "Treble", 2 + offset, -6 + offset) -
      (line - 1) * line_gap,
    x0 = 0,
    x1 = all_right
  )

all_bars <- crossing(line = 1:2, x = c(0, all_right)) |>
  mutate(
    y0 = -6 - (line - 1) * line_gap,
    y1 = 6 - (line - 1) * line_gap
  )

all_lat_ticks <- crossing(
  line = 1:2,
  tibble(lat = 0, label = "Equator")
) |>
  mutate(y = lat * 7 / 60 - (line - 1) * line_gap, x = -0.6)

all_note_sf <- all_notes |>
  mutate(
    geometry = st_sfc(pmap(
      list(x, note_y, radius, songs),
      note_shape
    ))
  ) |>
  st_as_sf()

note_key <- tibble(
  songs = 1:4,
  label = c("1 song", "2 songs", "3 songs", "4 plus songs"),
  x = 3 + (0:3) * 10,
  y = -(line_gap + 14)
) |>
  mutate(
    radius = note_radius,
    note_y = if_else(songs >= 4, y, y + 0.4 * radius),
    note_col = if_else(songs >= 4, "#000000", "#9a9a9a")
  )

note_key_sf <- note_key |>
  mutate(
    geometry = st_sfc(pmap(
      list(x, note_y, radius, songs),
      note_shape
    ))
  ) |>
  st_as_sf()

plot_title <- "My Spotify playlist visualised as sheet music"
subtitle <- "Every note represents an artist, placed by their birthplace (or the lead singer's birthplace for a band). Notes run west to east, and height shows latitude. The equator is middle C. The longer notes mean there are more songs from that artist on my playlist. Whole notes are the artists with the most songs on my playlist. Bar lines separate the regions: USA, Canada, Latin America and the Caribbean, UK and Ireland, Europe, Africa, Asia, then Australia and NZ."
social_caption <- sb_social_caption(
  data_source = "Spotify playlists via Exportify"
)

gg_record(
  device = "png",
  width = 15,
  height = 10,
  unit = "in",
  dpi = 300
)

all_pitch_plot <- ggplot() +
  geom_segment(
    data = all_lines,
    aes(x = x0, xend = x1, y = y, yend = y),
    colour = "grey55",
    linewidth = 0.3
  ) +
  geom_segment(
    data = all_bars,
    aes(x = x, xend = x, y = y0, yend = y1),
    colour = "grey35",
    linewidth = 0.4
  ) +
  geom_text(
    data = all_lat_ticks,
    aes(x = x, y = y, label = label),
    family = label_font,
    hjust = 1,
    colour = "grey40",
    size = 10 / .pt
  ) +
  geom_segment(
    data = region_bars,
    aes(x = x, xend = x, y = y0, yend = y1),
    colour = "grey35",
    linewidth = 0.4
  ) +
  geom_text(
    data = clefs,
    aes(x = x, y = y, label = clef),
    family = "Noto Music",
    colour = text_col,
    size = 24 / .pt
  ) +
  geom_sf(data = all_note_sf, aes(fill = note_col), colour = NA) +
  geom_text_repel(
    data = all_labels,
    aes(x = x, y = y, label = Artist),
    nudge_y = all_labels$label_nudge,
    direction = "x",
    family = label_font,
    colour = text_col,
    size = 10 / .pt,
    segment.colour = "grey60",
    segment.size = 0.2,
    min.segment.length = 0,
    max.overlaps = Inf,
    seed = 1
  ) +
  geom_sf(data = note_key_sf, aes(fill = note_col), colour = NA) +
  scale_fill_identity() +
  geom_text(
    data = note_key,
    aes(x = x + 1.4, y = y, label = label),
    family = label_font,
    hjust = 0,
    colour = text_col,
    size = 10 / .pt
  ) +
  labs(title = plot_title, subtitle = subtitle, caption = social_caption) +
  theme_void(base_family = font) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_textbox_simple(
      colour = text_col,
      face = "bold",
      size = 24,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_textbox_simple(
      family = label_font,
      colour = text_col,
      size = 14,
      margin = margin(b = 10)
    ),
    plot.caption = element_textbox_simple(
      family = caption_font,
      colour = text_col,
      hjust = 0,
      margin = margin(t = 10)
    ),
    plot.background = element_rect(fill = bg_col, colour = bg_col)
  )

all_pitch_plot
record_polaroid()
ggsave(
  "2026/Day11/output/day11_all_artists_pitch.png",
  all_pitch_plot,
  width = 15,
  height = 10,
  dpi = 300,
  bg = bg_col,
  create.dir = TRUE
)

ig_n_lines <- 3
ig_line_gap <- 28
ig_lines <- 1:ig_n_lines

ig_notes <- all_notes |>
  mutate(
    x = long * 0.25,
    y = lat * 7 / 60
  ) |>
  arrange(region, x) |>
  mutate(line = ceiling(row_number() / ceiling(n() / ig_n_lines))) |>
  group_by(line) |>
  mutate(
    x = space_x(
      x,
      half_width,
      note_pad * note_radius,
      region,
      bar_space
    ),
    x = x - min(x - half_width) + 5
  ) |>
  ungroup() |>
  mutate(
    y = y - (line - 1) * ig_line_gap,
    note_y = if_else(songs >= 4, y, y + 0.4 * radius)
  )

ig_region_bars <- ig_notes |>
  arrange(line, x) |>
  group_by(line) |>
  mutate(
    next_region = lead(region),
    bar_x = (x + half_width + lead(x - half_width)) / 2
  ) |>
  ungroup() |>
  filter(!is.na(next_region), region != next_region) |>
  transmute(
    line,
    x = bar_x,
    y0 = -6 - (line - 1) * ig_line_gap,
    y1 = 6 - (line - 1) * ig_line_gap
  )

ig_clefs <- crossing(
  line = ig_lines,
  tibble(clef = c("\U0001D11E", "\U0001D122"), y = c(4, -4))
) |>
  mutate(y = y - (line - 1) * ig_line_gap, x = 2.4)

ig_labels <- ig_notes |>
  filter(songs >= 4) |>
  mutate(
    system_y = -(line - 1) * ig_line_gap,
    above = y - system_y >= 0
  ) |>
  arrange(line, x) |>
  group_by(line, above) |>
  mutate(stagger = (row_number() %% 2) * 1) |>
  ungroup() |>
  mutate(
    label_y = system_y + if_else(above, 9 + stagger, -9 - stagger),
    label_nudge = label_y - y
  )

ig_right <- max(ig_notes$x + ig_notes$half_width) + 2

ig_staff_lines <- crossing(
  line = ig_lines,
  stave = c("Treble", "Bass"),
  offset = 0:4
) |>
  mutate(
    y = if_else(stave == "Treble", 2 + offset, -6 + offset) -
      (line - 1) * ig_line_gap,
    x0 = 0,
    x1 = ig_right
  )

ig_bars <- crossing(line = ig_lines, x = c(0, ig_right)) |>
  mutate(
    y0 = -6 - (line - 1) * ig_line_gap,
    y1 = 6 - (line - 1) * ig_line_gap
  )

ig_lat_ticks <- crossing(
  line = ig_lines,
  tibble(lat = 0, label = "Equator")
) |>
  mutate(y = lat * 7 / 60 - (line - 1) * ig_line_gap, x = -0.6)

ig_note_sf <- ig_notes |>
  mutate(
    geometry = st_sfc(pmap(
      list(x, note_y, radius, songs),
      note_shape
    ))
  ) |>
  st_as_sf()

ig_key <- note_key |>
  mutate(
    y = -(ig_line_gap * (ig_n_lines - 1) + 14),
    note_y = if_else(songs >= 4, y, y + 0.4 * radius)
  )

ig_key_sf <- ig_key |>
  mutate(
    geometry = st_sfc(pmap(
      list(x, note_y, radius, songs),
      note_shape
    ))
  ) |>
  st_as_sf()

gg_record(
  device = "png",
  width = 12,
  height = 15,
  unit = "in",
  dpi = 300
)

ig_pitch_plot <- ggplot() +
  geom_segment(
    data = ig_staff_lines,
    aes(x = x0, xend = x1, y = y, yend = y),
    colour = "grey55",
    linewidth = 0.3
  ) +
  geom_segment(
    data = ig_bars,
    aes(x = x, xend = x, y = y0, yend = y1),
    colour = "grey35",
    linewidth = 0.4
  ) +
  geom_text(
    data = ig_lat_ticks,
    aes(x = x, y = y, label = label),
    family = label_font,
    hjust = 1,
    colour = "grey40",
    size = 10 / .pt
  ) +
  geom_segment(
    data = ig_region_bars,
    aes(x = x, xend = x, y = y0, yend = y1),
    colour = "grey35",
    linewidth = 0.4
  ) +
  geom_text(
    data = ig_clefs,
    aes(x = x, y = y, label = clef),
    family = "Noto Music",
    colour = text_col,
    size = 24 / .pt
  ) +
  geom_sf(data = ig_note_sf, aes(fill = note_col), colour = NA) +
  geom_text_repel(
    data = ig_labels,
    aes(x = x, y = y, label = Artist),
    nudge_y = ig_labels$label_nudge,
    direction = "x",
    family = label_font,
    colour = text_col,
    size = 10 / .pt,
    segment.colour = "grey60",
    segment.size = 0.2,
    min.segment.length = 0,
    max.overlaps = Inf,
    seed = 1
  ) +
  geom_sf(data = ig_key_sf, aes(fill = note_col), colour = NA) +
  scale_fill_identity() +
  geom_text(
    data = ig_key,
    aes(x = x + 1.4, y = y, label = label),
    family = label_font,
    hjust = 0,
    colour = text_col,
    size = 10 / .pt
  ) +
  labs(title = plot_title, subtitle = subtitle, caption = social_caption) +
  theme_void(base_family = font) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_textbox_simple(
      colour = text_col,
      face = "bold",
      size = 24,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_textbox_simple(
      family = label_font,
      colour = text_col,
      size = 14,
      margin = margin(b = 10)
    ),
    plot.caption = element_textbox_simple(
      family = caption_font,
      colour = text_col,
      hjust = 0,
      margin = margin(t = 10)
    ),
    plot.background = element_rect(fill = bg_col, colour = bg_col)
  )

ig_pitch_plot
record_polaroid()

ggsave(
  "2026/Day11/output/day11_all_artists_pitch_instagram.png",
  ig_pitch_plot,
  width = 12,
  height = 15,
  dpi = 300,
  bg = bg_col,
  create.dir = TRUE
)
