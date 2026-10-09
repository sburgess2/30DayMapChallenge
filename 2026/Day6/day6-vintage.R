###############################################################################
## Load libraries                                                            ##
###############################################################################

library(tidyverse)

###############################################################################
## Load data                                                                  ##
###############################################################################

hagelloch_raw <- read_csv("2026/Day6/data/hagelloch.csv")

set.seed(6)

# How far (in metres) the road sits outside a house wall
door_gap <- 3

# One row per unique location, i.e. one row per household.
# distinct() drops repeated x/y pairs so children in the same house collapse
# into a single row, and hh is a simple 1..n id for each household.
households <- hagelloch_raw |>
  distinct(x.loc, y.loc) |>
  mutate(hh = row_number())

# dist() gives the straight-line distance between every pair of households.
# as.matrix() turns it into a square table: row i, column j is the distance
# from household i to household j (and the diagonal is 0).
dmat <- as.matrix(dist(households[, c("x.loc", "y.loc")]))

# For each row of dmat, sort the distances smallest to largest. The smallest
# is always 0 (the household to itself), so [2] is the nearest *other* house.
nn_dist <- apply(dmat, 1, \(r) sort(r)[2])

# Building size depends on how crowded the area is:
# - nn_dist * 0.6 makes houses smaller where neighbours are close
# - pmax(..., 5) and pmin(..., 14) stop them getting smaller than 5 m
#   or bigger than 14 m
# - runif(n(), 0.8, 1.1) multiplies by a random number so no two are identical
# - h (height) is a random 55-80% of w (width), giving a rectangle not a square
households <- households |>
  mutate(
    nn_dist = nn_dist,
    w = pmin(pmax(nn_dist * 0.6, 5), 14) * runif(n(), 0.8, 1.1),
    h = w * runif(n(), 0.55, 0.8)
  )

###############################################################################
## Build roads                                                                ##
###############################################################################

# Treat the households as nodes in a network, with the distance between each
# pair as the weight of the link between them
g <- igraph::graph_from_adjacency_matrix(
  dmat,
  mode = "undirected",
  weighted = TRUE
)

# A minimum spanning tree keeps the shortest set of links that still connects
# every household with no loops. These links become the roads.
# as_edgelist() returns a two-column matrix: the household at each end of
# every road.
el <- igraph::as_edgelist(igraph::mst(g), names = FALSE)

# For every household, find which way its roads head. We:
# 1. find its neighbours on the road network (nb) by looking in both columns
#    of el, since a household can be at either end of a road
# 2. work out the x and y distance to each neighbour (dx, dy)
# 3. divide by the straight-line distance (d) so each neighbour counts equally,
#    near or far
# 4. add them up. mx and my point in the average direction of the roads.
mean_dirs <- map(
  seq_len(nrow(households)),
  \(i) {
    nb <- c(el[el[, 1] == i, 2], el[el[, 2] == i, 1])
    dx <- households$x.loc[nb] - households$x.loc[i]
    dy <- households$y.loc[nb] - households$y.loc[i]
    d <- sqrt(dx^2 + dy^2)
    tibble(mx = sum(dx / d), my = sum(dy / d))
  }
) |>
  list_rbind()

# Give each household a "door" point just outside one wall:
# - door_on_x is TRUE if the roads head mostly left/right, FALSE if mostly
#   up/down
# - if left/right, move the door along x by half the building width plus the
#   gap, to the left (-1) or right (+1) depending on the sign of mx
# - if up/down, do the same along y using half the building height and my
# - the other coordinate is left alone, so the door stays centred on that wall
households <- households |>
  bind_cols(mean_dirs) |>
  mutate(
    door_on_x = abs(mx) >= abs(my),
    door_x = x.loc +
      if_else(door_on_x, if_else(mx >= 0, 1, -1) * (w / 2 + door_gap), 0),
    door_y = y.loc +
      if_else(door_on_x, 0, if_else(my >= 0, 1, -1) * (h / 2 + door_gap))
  )

# Attach the household size and door info to every child, then jitter children
# who share a house so their circles don't sit exactly on top of each other.
# Within each household, children are nudged by a random amount of up to a
# third of the building width/height either way. Single children stay put.
hagelloch <- hagelloch_raw |>
  select(PN, HN, x.loc, y.loc) |>
  left_join(households, by = c("x.loc", "y.loc")) |>
  group_by(hh) |>
  mutate(
    household_size = n(),
    x_plot = if_else(
      household_size > 1,
      x.loc + runif(n(), -w / 3, w / 3),
      x.loc
    ),
    y_plot = if_else(
      household_size > 1,
      y.loc + runif(n(), -h / 3, h / 3),
      y.loc
    )
  ) |>
  ungroup()

# One row per road, running from the door of one household to the door of
# another:
# - x, y are the start and xend, yend the end of the road
# - len is the straight-line length (Pythagoras)
# - ang is the direction the road points, in radians
# - bend is a random sideways wobble of up to 12% of the road's length
# - cx, cy is the "control point": the road's midpoint pushed sideways by
#   bend. sin and cos of ang give the direction at right angles to the road.
edges <- tibble(from = el[, 1], to = el[, 2]) |>
  mutate(
    edge_id = row_number(),
    x = households$door_x[from],
    y = households$door_y[from],
    xend = households$door_x[to],
    yend = households$door_y[to],
    len = sqrt((xend - x)^2 + (yend - y)^2),
    ang = atan2(yend - y, xend - x),
    bend = runif(n(), -0.12, 0.12) * len,
    cx = (x + xend) / 2 - bend * sin(ang),
    cy = (y + yend) / 2 + bend * cos(ang)
  )

# Turn each straight road into a gentle curve.
# cross_join() pairs every road with every value in the second table, so each
# road is repeated 12 times, once for each t from 0 (start) to 1 (end).
# The px and py formulas are a quadratic Bezier curve: at each t they blend
# the start point, the control point (cx, cy) and the end point, so the path
# bows towards the control point instead of running dead straight.
# geom_path() then joins the 12 points of each road into a smooth line.
road_points <- edges |>
  select(edge_id, x, y, xend, yend, cx, cy) |>
  cross_join(tibble(t = seq(0, 1, length.out = 12))) |>
  mutate(
    px = (1 - t)^2 * x + 2 * (1 - t) * t * cx + t^2 * xend,
    py = (1 - t)^2 * y + 2 * (1 - t) * t * cy + t^2 * yend
  )

###############################################################################
## Build buildings                                                            ##
###############################################################################

# Draws one building as the four corners of a rectangle.
# id          which building this is (so ggplot knows which corners belong
#             together)
# cx, cy      centre of the building
# w, h        width and height
# theta       how far to rotate it, in radians (0 = no rotation)
#
# dx and dy are the corners measured from the centre before rotating:
# (-1,-1), (1,-1), (1,1), (-1,1) going around the box, scaled by half the
# width and half the height.
# The x and y lines rotate each corner by theta (standard 2D rotation) and then
# shift it to the building's centre.
make_rect <- function(id, cx, cy, w, h, theta) {
  dx <- c(-1, 1, 1, -1) * w / 2
  dy <- c(-1, -1, 1, 1) * h / 2
  tibble(
    id = id,
    x = cx + dx * cos(theta) - dy * sin(theta),
    y = cy + dx * sin(theta) + dy * cos(theta)
  )
}

# The household buildings. pmap() runs make_rect once per row, using the
# columns as its arguments (they must be named to match the function).
# list_rbind() then stacks the results into one tibble of corners.
# theta = 0 keeps houses square to the page.
house_polys <- households |>
  transmute(id = hh, cx = x.loc, cy = y.loc, w, h, theta = 0) |>
  pmap(make_rect) |>
  list_rbind()

# Stops filler buildings overlapping each other. It goes through the candidate
# buildings one at a time, in order. A building is kept only if it is at least
# min_d away from every building already kept.
# keep    TRUE/FALSE flag for each row, filled in as we go
# kx, ky  positions of the buildings kept so far
# The first building is always kept, since there is nothing to clash with.
# all(... >= min_d) is TRUE only when every kept building is far enough away.
thin <- function(df, min_d) {
  keep <- logical(nrow(df))
  kx <- numeric()
  ky <- numeric()
  for (i in seq_len(nrow(df))) {
    if (
      length(kx) == 0 ||
        all(sqrt((kx - df$px[i])^2 + (ky - df$py[i])^2) >= min_d)
    ) {
      keep[i] <- TRUE
      kx <- c(kx, df$px[i])
      ky <- c(ky, df$py[i])
    }
  }
  df[keep, ]
}

# Candidate spots for extra buildings along each road:
# - filter(len >= 12): ignore roads too short to fit a building
# - n = floor(len / 12): about one building slot every 12 m of road
# - uncount() repeats each road n times and numbers the repeats in k, so a
#   road with n = 3 becomes three rows with k = 1, 2, 3
# - t = (k - 0.5) / n: how far along the road each slot is (0 = start,
#   1 = end), placed in the middle of equal segments
# - cross_join() with side = -1 and 1 doubles every slot, one for each side of
#   the road
# - px, py: start at the road's start, move t * len along its direction
#   (cos/sin of ang), then step 9 m sideways (the perpendicular direction is
#   -sin and cos of ang) to the left or right depending on side
# - w, h: random building size
# - theta = ang: rotate the building to line up with the road
# - nearest_house: distance from this spot to the closest real household.
#   map2_dbl() runs the small function for each px/py pair; sqrt(...) is the
#   distance to every household and min() picks the closest.
# - the final filter drops spots within 11 m of a real household, then
#   randomly drops about 35% (runif(n()) > 0.35) so the rows aren't uniform
filler_candidates <- edges |>
  filter(len >= 12) |>
  mutate(n = floor(len / 12)) |>
  uncount(n, .id = "k", .remove = FALSE) |>
  mutate(t = (k - 0.5) / n) |>
  cross_join(tibble(side = c(-1, 1))) |>
  mutate(
    px = x + t * len * cos(ang) - side * 9 * sin(ang),
    py = y + t * len * sin(ang) + side * 9 * cos(ang),
    w = runif(n(), 7, 12),
    h = w * runif(n(), 0.55, 0.8),
    theta = ang,
    nearest_house = map2_dbl(
      px,
      py,
      \(a, b) {
        min(sqrt((households$x.loc - a)^2 + (households$y.loc - b)^2))
      }
    )
  ) |>
  filter(nearest_house >= 11, runif(n()) > 0.35)

# Remove candidates closer than 9 m to another, then number the survivors
fillers <- thin(filler_candidates, 9) |>
  mutate(id = row_number())

# Convert the kept filler spots into rectangle corners, the same way as the
# household buildings but rotated to match their road
filler_polys <- fillers |>
  transmute(id, cx = px, cy = py, w, h, theta) |>
  pmap(make_rect) |>
  list_rbind()

###############################################################################
## Define colours                                                             ##
###############################################################################

paper_col <- "#EFE3C6"
ink_col <- "#5a4a32"
road_fill <- "#F6EDD6"
filler_fill <- "#D8C9A3"
house_fill <- "#B8745A"
point_fill <- "#2F5D62"
point_col <- "black"
label_col <- "white"

###############################################################################
## Build scale bar                                                            ##
###############################################################################

# The coordinates are in metres, so a bar 50 units long is 50 m on the map.
# It sits in the bottom-left corner, 15 m below the lowest thing drawn
# (building corners, filler buildings or road points).
bar_len <- 50
bar_height <- 3
bar_x0 <- min(c(house_polys$x, filler_polys$x, road_points$px))
bar_y0 <- min(c(house_polys$y, filler_polys$y, road_points$py)) - 15

# Two blocks side by side, one dark and one pale, each half the bar length
scale_bar <- tibble(
  xmin = bar_x0 + c(0, bar_len / 2),
  xmax = bar_x0 + c(bar_len / 2, bar_len),
  ymin = bar_y0,
  ymax = bar_y0 + bar_height,
  fill = c(ink_col, paper_col)
)

# Labels at the start, middle and end of the bar, just above it
scale_labels <- tibble(
  x = bar_x0 + c(0, bar_len / 2, bar_len),
  y = bar_y0 + bar_height + 3,
  label = c("0", "25", "50 m")
)

###############################################################################
## Plot                                                                     ##
###############################################################################

# Layers are drawn in order, so later ones sit on top.
# The road is drawn twice: a wider dark line, then a narrower pale line on top,
# which leaves a dark edge on each side of the road.
ggplot() +
  geom_path(
    data = road_points,
    aes(x = px, y = py, group = edge_id),
    color = ink_col,
    linewidth = 3.2,
    lineend = "round"
  ) +
  geom_path(
    data = road_points,
    aes(x = px, y = py, group = edge_id),
    color = road_fill,
    linewidth = 2.4,
    lineend = "round"
  ) +
  geom_polygon(
    data = filler_polys,
    aes(x = x, y = y, group = id),
    fill = filler_fill,
    color = ink_col,
    linewidth = 0.3
  ) +
  geom_polygon(
    data = house_polys,
    aes(x = x, y = y, group = id),
    fill = house_fill,
    color = ink_col,
    linewidth = 0.4
  ) +
  geom_point(
    data = hagelloch,
    aes(x = x_plot, y = y_plot),
    shape = 21,
    fill = point_fill,
    color = point_col,
    size = 6,
    alpha = 0.5
  ) +
  geom_text(
    data = hagelloch,
    aes(x = x_plot, y = y_plot, label = HN),
    color = label_col,
    size = 2.2,
    fontface = "bold"
  ) +
  geom_rect(
    data = scale_bar,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
    color = ink_col,
    linewidth = 0.4
  ) +
  geom_text(
    data = scale_labels,
    aes(x = x, y = y, label = label),
    color = ink_col,
    size = 3
  ) +
  scale_fill_identity() +
  coord_equal() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = paper_col, color = NA),
    plot.margin = margin(15, 15, 15, 15)
  )

###############################################################################
## Save png                                                                   ##
###############################################################################

ggsave(
  filename = "2026/Day6/output/hagelloch_vintage.png",
  width = 8,
  height = 8,
  dpi = 300,
  device = ragg::agg_png
)
