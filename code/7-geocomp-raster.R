#' ---
#' title: "Geocomputation with Raster Data"
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
library(ggspatial) #plotting raster data
library(geodata) #example rasters
library(sf) #working with vector data   
library(spData) #datasets
library(terra) #working with raster data
library(tidyverse) #data manipulation
library(gridExtra) #arranging figures
here::i_am("code/7-geocomp-raster.R")

# +
#' # Overview
#' Here we continue learning about geocomputation, but this time for raster data.
#' This section of the class also closely follows parts of 
#' [Chapter 4](https://geocompr.robinlovelace.net/spatial-operations.html) 
#' [Chapter 5](https://r.geocompx.org/geometry-operations.html), and 
#' [Chapter 6](https://r.geocompx.org/raster-vector.html?q=rasteriz#rasterization) 
#' of Lovelace et al.'s textbook.

# +
#' # Spatial Subsetting

elev <- rast(system.file("raster/elev.tif", package = "spData"))
plot(elev)
  # very simple raster

elev
  # min = 1, max = 36
  # values marching up from 1 to 36 like words on a page

terra::extract(elev, matrix(c(0.1, 0.1), ncol = 2))
  # 16 - returns value of grid cell located @ (0.1, 0.1)

terra::extract(elev, matrix(c(0, 0), ncol = 2))
  # what about a point that would be overlapped?
  # returns 22 - bottom right cell

clip <- rast(
  xmin = 0.9, xmax = 1.8,
  ymin = -0.45, ymax = 0.45,
  resolution = 0.3, vals = rep(1, 9)
)

plot(elev)
plot(clip, add = TRUE, legend = FALSE)

elev[clip]
  # returns values for cells of which 'clip' covers the majority
jerp <- elev[clip, drop = FALSE]
  # returns spat raster
plot(jerp)

# something else
rmask1 <- elev
rmask2 <- elev

set.seed(50)
values(rmask1) <- sample(c(TRUE, FALSE), 36, replace = TRUE)
set.seed(50)
values(rmask2) <- sample(c(TRUE, NA), 36, replace = TRUE)

plot(rmask1)
plot(rmask2)

plot(elev[rmask1, drop = FALSE]) # with the square bracket [] operator
plot(terra::mask(elev, rmask2)) # mit der mask function in terra

# +
#' # Map algebra

#' ## Local operations
#' Local operations comprise all cell-by-cell operations in one or several layers.

#' First we will look at reclassification of a raster, e.g. if you want to group multiple
#' values of a variable into a category. 

??terra::classify
  # designed to classify raster data into bins

rcl <- matrix(
  c(0, 12, 1, 
    12, 24, 2, 
    24, 36, 3), 
  ncol = 3, 
  byrow = TRUE
)
rcl
  # reclassifying matrix - assigns groups and values for each group
  # values must be doubles

recl <- terra::classify(elev, rcl = rcl, right = TRUE, include.lowest = TRUE)
plot(recl)

#' Next, we'll look at raster algebra following a example from 
#' [Earth Data Science](https://www.earthdatascience.org/courses/earth-analytics/lidar-raster-data-r/lidar-chm-dem-dsm/). 
#' Here we have one raster dataset that is a dsm, or a digital surface model and the another raster dataset that is a dtm, 
#' or a digital terrain model. To calculate the height of the tree canopy we can subtract the dtm from the dsm.

dsm <- rast(here("data", "s7-dat", "NEON-DS-Airborne-Remote-Sensing", "HARV", "DSM", "HARV_dsmCrop.tif"))
dtm <- rast(here("data", "s7-dat", "NEON-DS-Airborne-Remote-Sensing", "HARV", "DTM", "HARV_dtmCrop.tif"))

st_crs(dsm)
st_crs(dtm)

fig_dsm <- ggplot() +
  layer_spatial(data = dsm) +
  scale_fill_viridis_c() +
  labs(fill = "DSM") +
  theme_void()
fig_dsm

fig_dtm <- ggplot() +
  layer_spatial(data = dtm) +
  scale_fill_viridis_c() +
  labs(fill = "DTM") +
  theme_void()
fig_dtm

grid.arrange(fig_dsm, fig_dtm)

#' ### Method 1
#' 
#' **Method 1** is intuitive, because we substract one object (dtm) from another (dsm)
#' using the common `-` operator.

ptm <- proc.time()
chm <- dsm - dtm
proc.time()[3] - ptm[3]
plot(chm)

ggplot() +
  layer_spatial(data = chm) +
  scale_fill_gradientn(
    name = "Canopy Height",
    colors = terrain.colors(10)
  ) +
  theme_void()

#' ### Method 2
#' 
#' **Method 2** uses the lapply or map functions and can be more efficient (take less computation time)
#' when working with large datasets.

subtract <- function(r1, r2){r1 - r2}

ptm <- proc.time()
chm2 <- lapply(dsm, dtm, FUN = subtract)
proc.time()[3] - ptm[3]

ptm <- proc.time()
chm3 <- map(dsm, dtm, .f = subtract)
proc.time()[3] - ptm[3]

summary(chm)
summary(chm2[[1]])
summary(chm3[[1]])

#' ## Focal operations
#' Focal operations take into account a central cell and its neighbors. Here we'll look at
#' finding the minimum value in the neighborhood of each cell, where the neighborhood is defined
#' as a 3 x 3 matrix centered on the cell.

elev <- rast(system.file("raster/elev.tif", package = "spData"))

neighborhood <- matrix(1, nrow = 3, ncol = 3)
neighborhood 
  # neighborhood defined as all the cells bordering the cell of interest

r_focal <- focal(elev, w = neighborhood, fun = min)
  # ignore missing neighbors with na.rm = TRUE
plot(r_focal)

ggplot() +
  layer_spatial(data = r_focal) +
  scale_fill_viridis_c() +
  theme_void()


#' ## Zonal operations
#' In a zonal operation, you can define a zone even if the zones aren't contiguous
#' cells as in the focal operations above. Here the raster `grain_df` defines the 
#' zones and we want to know the mean elevation for each of these soil types.

grain <- rast(system.file("raster/grain.tif", package = "spData"))

ggplot() +
  layer_spatial(data = grain) +
  theme_void()
  # got that graain

z <- zonal(elev, grain, fun = "mean") %>% 
  as.data.frame()

#' ## Merging rasters
#' Useful when corresponding imagery is divided into scenes covering a specific 
#' spatial extent and a study area covers more than one scene. 
#' Then, you need to merge the scenes covered by our study area.
#' terra::merge() merges rasters, and in areas of overlap, takes values from the 
#' first raster. Also see terra::mosaic(), which allows smoothing overlapping values.

aut <- geodata::elevation_30s(country = "AUT", path = tempdir())
ch <- geodata::elevation_30s(country = "CHE", path = tempdir())
  # add france!
fr <- geodata::elevation_30s(country = "FRA", path = tempdir())

aut_ch <- terra::merge(aut, ch)

rast_left <- ggplot() +
  layer_spatial(data = aut) +
  theme_void()

rast_right <- ggplot() +
  layer_spatial(data = ch) +
  theme_void()

rast_merge <- ggplot() +
  layer_spatial(data = aut_ch) +
  theme_void()

grid.arrange(rast_left, rast_right, rast_merge, nrow = 2)
rast_merge

aut_ch_fr <- terra::merge(aut_ch, fr)

rast_merge2 <- ggplot() +
  layer_spatial(data = aut_ch_fr) +
  theme_void()
rast_merge2

# +
#' # Extent and origin
#' When merging or performing map algebra on rasters, their resolution,  projection, origin and/or 
#' extent have to match. Therefore, we may need to change these features. 
#' 
# +
#' ## Extent
#' To determine the extent of a raster you can use the `extent()` function. If you need to extend
#' a raster you can use the `extend()` function.

ext(elev) # 'extent' function
e <- ext(-2.5, 2.5, -2, 2)
ext(e)
elev2 <- extend(elev, e)
  # this is a terra function
ext(elev2)
plot(elev2, colNA = "blue")

# elev + elev2
  # cannot be added, extents do not match
  # sometimes you add extent to one raster so it will match another

elev3 <- extend(elev, elev2)
  # extends the first raster (elev) to the extent of 2nd raster (elev2)
plot(elev3, colNA = "gray")

elev4 <- elev2 + elev3
plot(elev4, colNA = "purple")

# +
#' ## Origin
# Use the `origin()` function to identify the origin of a raster and you can modify the origin
#' Note that changing the resolution frequently also changes the origin.

origin(elev4)
origin(elev4) <- c(0.25, 0.25)

# elev_5 <- elev2 + elev4 # error message
  # now this addition won't work because extent of elev4 is shifted over

plot(elev4)
plot(elev, add = TRUE)

# +
#' # Aggregation and disaggregation (changing the resolution)

res(dsm)

# +
#' ## Reduce resolution

plot(dsm)
dsm_agg <- aggregate(dsm, fact = 10, fun = mean)
plot(dsm_agg)

fig_res1 <- fig_dsm  +
  labs(title = "DSM (original res)") +
  theme(legend.position = "none")

fig_res10 <- ggplot() +
  layer_spatial(data = dsm_agg) +
  scale_fill_viridis_c() +
  theme_void()  +
  labs(title = "DSM (reduced res)") +
  theme(legend.position = "none")

grid.arrange(fig_res1, fig_res10, ncol = 2)

# +
#' ## Increase resolution

# interesting. this does not generate information (obviously)

dsm_disagg <- disagg(
  dsm_agg, 
  fact = 10,
  method = "bilinear" # nearest neighbor is default
)

res(dsm_disagg)
plot(dsm_disagg)

summary(dsm)
summary(dsm_disagg)

# +
#' ## Resampling
#' 

origin(dsm)
target_rast <- rast(
  xmin = 731453, xmax = 733150, 
  ymin = 4712471, ymax = 4713838,
  resolution = 100, crs = "EPSG:32618")
origin(target_rast)

??terra::resample #note all methods

dsm_resamp <- resample(dsm, y = target_rast, method = "bilinear")
  # fills in data from one raster into another? I guess? 

ggplot() +
  layer_spatial(data = dsm_resamp) +
  scale_fill_viridis_c() +
  labs(fill = "DSM resampled") +
  theme_void()

# +
#' # Raster-vector interactions

# +
#' ## Raster cropping

site <- read_sf(here("data", "s7-dat", "NEON-DS-Site-Layout-Files", "HARV", "HarClip_UTMZ18.shp"))
plot(dsm)
plot(site, add = T)

dsm_site <- crop(dsm, site)
  
ggplot() +
  geom_sf(
    data = st_as_sfc(st_bbox(dsm)),
    fill = "blue",
    color = "blue", alpha = .2
  ) +
  layer_spatial(data = dsm_site ) +
  scale_fill_gradientn(name = "DSM crop",
    colors = terrain.colors(10))

site_buff <- st_buffer(site, dist = 100)

plot(dsm)
plot(site_buff, add = T)

dsm_site_buff <- crop(dsm, site_buff)

ggplot() +
  layer_spatial(data = dsm_site_buff ) +
  geom_sf(
    data = site_buff,
    fill = "blue",
    color = "blue", alpha = .2
  ) +
  scale_fill_gradientn(name = "DSM buffed",
                       colors = terrain.colors(10))
  # this is a rectangular crop, how do we fix it?

dsm_site_buff2 <- crop(dsm, site_buff, mask = TRUE)

ggplot() +
  layer_spatial(data = dsm_site_buff2) +
  geom_sf(
    data = site_buff,
    fill = "blue",
    color = "blue", alpha = .2
  ) +
  scale_fill_gradientn(name = "mask = TRUE",
                       colors = terrain.colors(10))


# +
#' ## Raster extraction

# +
#' ### At a point

tower <- read_sf(here("data", "s7-dat", "NEON-DS-Site-Layout-Files", "HARV", "HARVtower_UTM18N.shp"))
terra::extract(dsm, vect(tower)) # value at a single point

tower <- tower %>% 
  mutate(dsm = terra::extract(dsm, vect(geometry))) # value at a point as a variable
tower$dsm

ggplot() +
  layer_spatial(data = dsm_site) +
  geom_sf(data = tower) +
  scale_fill_gradientn(name = "DSM",
    colors = terrain.colors(10)) +
  theme(legend.position = "none")

# +
#' ### Within a polygon

terra::extract(dsm, vect(site), fun = "mean") # mean surface measurement within polygon (site)

# +
#' ## Rasterization (of vectors)
#'

# +
#' ### Points
#'  

cycle_hire_osm <- spData::cycle_hire_osm
cycle_hire_osm_projected <- st_transform(cycle_hire_osm, "EPSG:27700")
raster_template <- rast(
  ext(cycle_hire_osm_projected), 
  resolution = 1000,
  crs = st_crs(cycle_hire_osm_projected)$wkt
)
  # project vector data and raster to fit

ch_rast1 <- rasterize(
  vect(cycle_hire_osm_projected),
  raster_template,
  field = 1)

ch_rast2 <- rasterize(
  vect(cycle_hire_osm_projected),
  raster_template,
  fun = "length")

ch_rast3 <- rasterize(
  vect(cycle_hire_osm_projected),
  raster_template,
  field = "capacity",
  fun = "sum",
  na.rm = TRUE)


fig_pts <- ggplot() +
  geom_sf(
    data = cycle_hire_osm_projected,
    aes(color = capacity)
  ) +
  scale_color_continuous(name = "Capacity") +
  labs(title = "A. Points") +
  theme_void()

fig_rast1 <- ggplot() +
  layer_spatial(data = ch_rast1) +
  scale_fill_continuous(name = "Values") +
  labs(title = "B. Presence/Absense") +
  theme_void()

fig_rast2 <- ggplot() +
  layer_spatial(data = ch_rast2) +
  scale_fill_continuous(name = "Values") +
  labs(title = "C. Count") +
  theme_void()

fig_rast3 <- ggplot() +
  layer_spatial(data = ch_rast3) +
  scale_fill_continuous(name = "Values") +
  labs(title = "D. Sum") +
  theme_void()

grid.arrange(fig_pts, fig_rast1, fig_rast2, fig_rast3, ncol = 2)

# +
#' ### Polygons
#' 
ca <- us_states %>% filter(., NAME == "California")
ca_borders <- st_cast(ca, "MULTILINESTRING")
raster_template2 <- rast(ext(ca), resolution = 0.5,
                         crs = st_crs(ca)$wkt)

ca_rast1 <- rasterize(
  vect(ca_borders),
  raster_template2,
  touches = TRUE
)

ca_rast2 <- rasterize(
  vect(ca),
  raster_template2
)

fig_line_rast <- ggplot() +
  layer_spatial(data = ca_rast1) +
  geom_sf(data = ca, fill = "transparent", color = "black") +
  theme_void()

fig_poly_rast <- ggplot() +
  layer_spatial(data = ca_rast2) +
  geom_sf(data = ca, fill = "transparent", color = "black") +
  theme_void()

grid.arrange(fig_line_rast, fig_poly_rast, ncol = 2)

# +
#' ## Vectorization (of rasters)
#' 

# +
#' ### Points
#' 
elev <- rast(system.file("raster/elev.tif", package = "spData"))

elev_point <- as.points(elev) %>% 
  st_as_sf()

ggplot() +
  geom_sf(data = elev_point, aes(color = elev)) +
  scale_color_gradientn(
    name = "dem",
    colors = terrain.colors(36)
  )

# +
#' ### Contours
#' 
dem <- rast(system.file("raster/dem.tif", package = "spDataLarge"))

cl <- as.contour(dem) %>% 
  st_as_sf()

cl_acf <- as.contour(aut_ch_fr) %>% 
  st_as_sf()

ggplot() +
  layer_spatial(dem) +
  geom_sf(data = cl, aes(color = level)) +
  scale_fill_gradientn(
    name = "dem",
    colors = terrain.colors(10)
  ) +
  scale_color_viridis_c(name = "dem level")

ggplot() +
  layer_spatial(aut_ch_fr) +
  geom_sf(data = cl_acf, aes(color = level)) +
  scale_fill_gradientn(
    name = "dem",
    colors = terrain.colors(10)
  ) +
  scale_color_viridis_c(name = "dem level")

# +
#' ### Polygons
#' 
grain <- terra::rast(system.file("raster/grain.tif", package = "spData"))

grain_poly_1 <- as.polygons(grain, dissolve = FALSE) %>%  st_as_sf()
grain_poly_2 <- as.polygons(grain, dissolve = TRUE) %>%  st_as_sf()

fig_vect_cell <- ggplot() +
  geom_sf(data = grain_poly_1, aes(fill = grain))

fig_vect_value <- ggplot() +
  geom_sf(data = grain_poly_2, aes(fill = grain))

grid.arrange(fig_vect_cell, fig_vect_value, ncol = 2)
