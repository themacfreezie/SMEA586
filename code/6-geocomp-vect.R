#' ---
#' title: "Geocomputation with Vector Data"
#' author: "S.L. Jardine"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output:
#'    html_document:
#'       number_sections: true
#'       toc: true
#'       toc_float:
#'          collapsed: true
#' ---
#' 
#+ include=F
library(here) #setup
library(gridExtra) #multiple figures
library(rmapshaper) #simplifying polygons
library(sf) #vector data  
library(smoothr) #simplifying polygons
library(spData) #datasets
library(tidyverse) #data manipulation
library(units)

# +
#' # Overview
#' Geocomputation is the art and science of solving complex 
#' spatial problems with computers. The [`sf` github website](https://r-spatial.github.io/sf/index.html) 
#' is a nice resource for functions that aid geocomputation when working with 
#' vector data as is the [sf cheatsheet](https://github.com/rstudio/cheatsheets/blob/master/sf.pdf). 
#' This section of the class also closely follows 
#' [Chapter 4](https://geocompr.robinlovelace.net/spatial-operations.html) 
#' and [Chapter 5](https://r.geocompx.org/geometry-operations.html) of Lovelace et al.'s textbook.

# +
#' # Spatial subsetting
#' We will use the New Zealand datasets from [`spData`](https://nowosad.github.io/spData/reference/index.html) 
#' to demonstrate spatial subsetting, which as the name implies is when you select observations
#' based on their location. Here we want to select elevations that are in the 
#' Canterbury region of New Zealand.

nz <- nz
nz_height <- nz_height
canterbury <- nz %>% filter(Name == "Canterbury")

ggplot() +
  geom_sf(data = nz, fill = "white") +
  geom_sf(data = canterbury, fill = "lemonchiffon") +
  geom_sf(data = nz_height, color = "red", shape = 2) +
  theme_void() +
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.border = element_rect(colour = "black", fill = NA))

#' ## Method 1
#' Method 1 is direct. You use the square brackets to perform subsetting in a way
#' that is familiar, because it is similar to subsetting in base R. In base R
#' you can subset a dataset called `dat` with the command `dat[X, Y]` where `X`
#' is a logical vector that selects the observations (rows) to include in the subset
#' and `Y` is a vector with the numbers or names of the columns to be included in 
#' the subset. Here instead of passing a logical vector to the square brackets, 
#' to filter the rows you want to include, you can simply pass another spatial dataset.

canterbury_height1 <- nz_height[canterbury, ]
  # grab those peaks just contained in 'canterbury'
  # not listen in nz_height, being drawn from object 'canterbury'

ggplot() +
  geom_sf(data = nz, fill = "white") +
  geom_sf(data = canterbury, fill = "lemonchiffon") +
  geom_sf(data = canterbury_height1, color = "red", shape = 2) +
  theme_void() +
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.border = element_rect(colour = "black", fill = NA))

#' Note that `st_intersects()` is the default operation and picks out
#' features that touch, cross, and lie within the dataset to be subsetted. 
#' (Being called in line 61)
#' However, you don't have to go with the default you can provide
#' a third input argument to the square brackets when subsetting a `sf` object
#'  to specify the operation or `op` that you want to perform. 
#'  For example, `st_disjoint()`will select all observations that are disjoint from Canterbury, 
#'  i.e. is the opposite of `st_intersects()`. 

exclude_canterbury_height1 <- nz_height[canterbury, , op=st_disjoint]

ggplot() +
  geom_sf(data = nz, fill = "white") +
  geom_sf(data = canterbury, fill = "lemonchiffon") +
  geom_sf(data = exclude_canterbury_height1, color = "red", shape = 2) +
  theme_void() +
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.border = element_rect(colour = "black", fill = NA))

#' ## Method 2
#' You can also just use st_filter(), which is what I do.

canterbury_height2 <- nz_height %>% st_filter(., y= canterbury)
exclude_canterbury_height2 <- nz_height %>% 
  st_filter(., y= canterbury, .predicate = st_disjoint)

ggplot() +
  geom_sf(data = nz, fill = "white") +
  geom_sf(data = canterbury, fill = "lemonchiffon") +
  geom_sf(data = canterbury_height2, color = "red", shape = 2) +
  theme_void() +
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.border = element_rect(colour = "black", fill = NA))

# +
#' # Topological relations
#' 
# create a polygon
a_poly <- st_polygon(list(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, -1))))
a <- st_sfc(a_poly)
# create a line
l_line <- st_linestring(x = matrix(c(-1, -1, -0.5, 1), ncol = 2))
l <- st_sfc(l_line)
# create points
p_df <- data.frame(matrix(c(0.5, 1, -1, 0, 0, 1, 0.5, 1), ncol = 2))
p <- st_as_sf(x = p_df, coords = 1:2)
p$id <- c("1", "2", "3", "4")

ggplot() +
  geom_sf(data = a, color = "red") +
  geom_sf(data = l) +
  geom_sf(data = p, aes(fill = id, color = id)) +
  theme_classic()

st_intersects(p, a)
st_intersects(p, a, sparse = FALSE)
  # crosses, touches, lies within
st_disjoint(p, a, sparse = FALSE)
  # opposite of st_intersects
st_within(p, a, sparse = FALSE)
  # completely witin
st_contains(a,p, sparse = FALSE)
  # the area contains the point but not vice versa
  # why is this good? 
st_touches(p, a, sparse = FALSE)
  # what's touching the boundary
st_is_within_distance(p, a, dist = 0.9, sparse = FALSE)
  # this is done by drawing a little buffer around a under the hood
  # look at the buffer below!

a2 <- st_buffer(a, 0.9)
ggplot() +
  geom_sf(data = a2, color = "blue") +
  geom_sf(data = a, color = "red") +
  geom_sf(data = l) +
  geom_sf(data = p, aes(fill = id, color = id)) +
  theme_classic()

# +
#' # Spatial joining
#' 
#' ## Overlapping joins
world <- world
set.seed(2018) # set seed for reproducibility
bb_world <- st_bbox(world)

random_df <- tibble(
  x = runif(n = 10, min = bb_world[1], max = bb_world[3]),
  y = runif(n = 10, min = bb_world[2], max = bb_world[4])
)
  # 10 random draws of points on the map

random_points <- random_df %>% 
  st_as_sf(coords = c("x", "y")) %>% # set coordinates
  st_set_crs(4326) # set geographic CRS

ggplot() +
  geom_sf(data = world, color = "gray", fill = "white") +
  geom_sf(data = random_points, shape = 4, size = 3, stroke = 2) +
  theme_void()

#' **Question: how do I filter out countries in world that share space with
#' the random points?**

world_random <- world %>%  st_filter(random_points)

ggplot() +
  geom_sf(data = world, color = "gray", fill = "white") +
  geom_sf(data = world_random,
          aes(color = name_long, fill = name_long)) +
  theme_void() +
  theme(legend.position = "none")

random_points
random_joined <- st_join(random_points, world["name_long"])
  #spatial join
random_joined

ggplot() +
  geom_sf(data = world, color = "gray", fill = "white") +
  geom_sf(
    data = random_joined, shape = 4,
    size = 3, stroke = 2, aes(color = name_long, fill = name_long)
  ) +
  theme_void()

#' ## Non-overlapping joins
#' 
#' Sometimes two geographic datasets do not touch but still have a strong geographic 
#' relationship enabling joins. The [`cycle_hire`](https://nowosad.github.io/spData/reference/cycle_hire.html) 
#' and [`cycle_hire_osm`](https://nowosad.github.io/spData/reference/cycle_hire_osm.html) datasets provide
#' a nice example. Imagine that we need to join the `capacity` variable in 
#' `cycle_hire_osm` onto the official target data contained in `cycle_hire`. We can take
#' each target station in the `cycle_hire` dataset and join it to observations in the
#' `cycle_hire_osm` dataset that are within 20 meters (~65 feet). 

cycle_hire <- read_sf(system.file("shapes/cycle_hire.geojson", package = "spData"))
cycle_hire_osm <- read_sf(system.file("shapes/cycle_hire_osm.geojson", package = "spData"))

ggplot() +
  geom_sf(data = cycle_hire, color = "blue") +
  geom_sf(data = cycle_hire_osm, color = "red") +
  theme_void()

any(st_touches(cycle_hire, cycle_hire_osm, sparse = FALSE))

st_crs(cycle_hire)
st_crs(cycle_hire_osm)

#' Both datasets are in a geographic CRS, so they need to be transformed to a 
#' projected CRS in order to perform distance calculations. We'll use ESPG 27700.

cycle_hire <- st_transform(cycle_hire, crs = 27700) 
cycle_hire_osm <- st_transform(cycle_hire_osm, crs = 27700)

sel <- st_is_within_distance(cycle_hire, cycle_hire_osm, dist = 20)
summary(lengths(sel) > 0)

#' This shows that there are 436 points in the target object (cylce_hire) that are within
#' 20 meters of a point in cycle_hire_osm. We can join these data to `cycle_hire` using `st_join()`.

z <- st_join(cycle_hire, cycle_hire_osm, st_is_within_distance, dist = 20)
  # there are some obs in cycle_hire with more than one point within their buffer

#' There are more rows in `z` than in `cycle_hire`, because some stations have
#' more than one match, so we can calculate the mean capacity for each station
#' as long as capacity is a numeric variable.

nrow(cycle_hire)
nrow(z)
# these must be aggregated up to the level of each cycle_hire obs
z <- z %>% 
  mutate(capacity = as.numeric(capacity)) %>% 
  group_by(id) %>% 
  summarize(capacity = sum(capacity, na.rm = TRUE))

ggplot() +
  geom_sf(data = z, aes(color = capacity)) +
  scale_color_continuous(low = "blueviolet", high = "red") +
  theme_void()

# +
#' # Spatial data aggregation
#' 
#' We have performed spatial data aggregation in the `2-preliminaries.R` using `group_by()`
#' with `summarize()`. We can also use `aggregate()`.

# +
#' ## Congruent
#' Congruent aggregation is when you want to aggregate smaller spatial units up to larger spatial
#' units and the smaller spatial units are entirely contained by the larger spatial units. For example,
#' you may want to aggregate data from all 50 US states to the entire US. The states are
#' entirely contained by the country borders. Here we'll look at the example of
#' aggregating the New Zealand elevation observations by provinces of New Zealand. 

ptm <- proc.time()
nz_avheight <- nz %>% 
  st_join(., nz_height) %>% 
  group_by(Name) %>% 
  summarize(av_elev = mean(elevation, na.rm = TRUE))
proc.time() - ptm

??sf::aggregate
  # default join is st_intersects

ptm <- proc.time()
nz_avheight2 <- aggregate(
  x = nz_height,
  by = nz,
  FUN = mean
)
proc.time() - ptm
  # faster than st_join method

ggplot() +
  geom_sf(data = nz_avheight, aes(fill = av_elev), col = "gray60", lwd = 0.25) +
  scale_fill_distiller(palette = "YlOrRd", na.value = "grey80", direction = 1) +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill = NA))

ggplot() +
  geom_sf(data = nz_avheight2, aes(fill = elevation), col = "gray60", lwd = 0.25) +
  scale_fill_distiller(palette = "YlOrRd", na.value = "grey80", direction = 1) +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill = NA))
  # output is the same

#' ## Incongruent
#' Inongruent aggregation is when you want to aggregate smaller spatial units up to larger spatial
#' units and the smaller spatial units are *not* entirely contained by the larger spatial units.
#' Here we'll look at an example with the `incongruent` and `aggregating_zones` datasets.
#' Suppose you want to know the mean value from `incongruent` for each of the 2 zones in
#' `aggregating_zones`. 

incongruent <- incongruent
aggregating_zones <- aggregating_zones

ggplot() +
  geom_sf(data = incongruent, aes(fill = value)) +
  geom_sf(
    data = aggregating_zones, 
    fill = "transparent", color = "blue", lwd = 1) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  theme_void() +
  theme(legend.position = "none")

#' When using `st_interpolate_aw()` you can set `extensive = T` or 
#' `extensive = F`. With `extensive = T` you get the sum and with
#' `extensive = F` you get the mean.

??sf::st_interpolate_aw

agg_aw <- st_interpolate_aw(
  incongruent[, "value"],
  aggregating_zones,
  extensive = TRUE
)

#' Here we specified, `extensive = T` meaning the sum should be preserved, which is something
#' we can check.

agg_aw$value
sum(agg_aw$value)
sum(incongruent$value)

# +
#' # Distance relations
#' 

nz_highest <- nz_height %>% 
  top_n(n = 1, wt = elevation)

canterbury_cent <- st_centroid(canterbury)

st_distance(nz_highest, canterbury_cent)

dist_mat <- st_distance(nz_height, nz_height)
class(dist_mat)
min(dist_mat)
max(dist_mat)
  
# +
#' # Simplification
#' ## Lines
seine <- seine
seine_simp <- st_simplify(seine, dTolerance = 2000)

ggplot() +
  geom_sf(data = seine, color = "blue") +
  geom_sf(data = seine_simp, color = "red") +
  theme_void()

object.size(seine)
object.size(seine_simp)

#' ## Polygons

us_states <- us_states %>% st_transform(., crs = 2163)  
us_states_simp1 <- st_simplify(us_states, dTolerance = 1e5)
us_states_simp2 <- rmapshaper::ms_simplify(
  us_states,
  keep = 0.01,
  keep_shapes = TRUE
)
us_states_simp2F <- rmapshaper::ms_simplify(
  us_states,
  keep = 0.01,
  keep_shapes = TRUE
)
us_states_simp3 <- smoothr::smooth(
  us_states,
  method = "ksmooth",
  smoothness = 6
)

orig <- ggplot() + geom_sf(data = us_states) + labs(title = "original data")
simp1 <- ggplot() + geom_sf(data = us_states_simp1) + labs(title = "st_simplify")
simp2 <- ggplot() + geom_sf(data = us_states_simp2) + labs(title = "ms_simplify")
simp2F <- ggplot() + geom_sf(data = us_states_simp2) + labs(title = "ms_simplify - false")
simp3 <- ggplot() + geom_sf(data = us_states_simp3) + labs(title = "smooth")


grid.arrange(orig, simp1, simp2, simp3)
# grid.arrange(simp2, simp2F)

#' # Centroids
#' 

world_cents <- cbind(
  world,
  st_coordinates(st_centroid(world %>% select(geom)))
)

sea_box <- world %>% 
  filter(subregion == "South-Eastern Asia") %>% 
  st_bbox()

centroid_fig <- ggplot() +
  geom_sf(data = world) +
  geom_point(data = world_cents, aes(x = X, y = Y)) +
  coord_sf(xlim = c(sea_box$xmin , sea_box$xmax), ylim = c(sea_box$ymin, sea_box$ymax)) +
  labs(y = "Latitude", x = "Longitude", title = "Examine centroids") 
centroid_fig 

world_pos <- st_point_on_surface(world)

centroid_fig +
  geom_sf(data = world_pos, color = "red") +
  coord_sf(
    xlim = c(sea_box$xmin , sea_box$xmax),
    ylim = c(sea_box$ymin, sea_box$ymax)
    )

#' # Clipping and combining
#' 

# create circles
p_df <- data.frame(matrix(c(0, 1, 1, 1.5), ncol = 2))
p <- st_as_sf(x = p_df, coords = 1:2)
p$id <- c("x", "y")
x <- st_buffer(p %>% slice(1), dist = 1)
y <- st_buffer(p %>% slice(2), dist = 0.75)

circles_xy <- ggplot() +
  geom_sf(data = x, fill = "transparent") +
  geom_sf(data = y, fill = "transparent") +
  theme_classic()
circles_xy

int_xy <- st_intersection(x, y)
diff_xy <- st_difference(x, y)
diff_yx <- st_difference(y, x)
sym_diff_xy <- st_sym_difference(x, y)
union_xy <- st_union(x, y)

circles_xy +
  geom_sf(data = int_xy, fill = alpha("red", 0.5))

circles_xy +
  geom_sf(data = diff_xy, fill = alpha("red", 0.5))

circles_xy +
  geom_sf(data = diff_yx, fill = alpha("red", 0.5))

circles_xy +
  geom_sf(data = sym_diff_xy, fill = alpha("red", 0.5))

circles_xy +
  geom_sf(data = union_xy, fill = alpha("red", 0.5))

#' # Nearest features
#' 

??sf::st_nearest_feature

st_nearest_feature(random_points, world)
random_labs <- world[st_nearest_feature(random_points, world), "name_long"] %>% 
  st_drop_geometry()
random_points <- random_points %>% 
  cbind(., random_labs) %>% 
  cbind(., random_df)

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = random_points, shape = 4, size = 3, stroke = 2) +
  geom_text(data = random_points, aes(x = x, y = y, label = name_long), nudge_y = 10)
