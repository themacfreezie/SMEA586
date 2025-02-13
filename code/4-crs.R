#' ---
#' title: "Coordinate Reference Systems"
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
library(ggspatial) #plotting rasters
library(here) #setup
library(sf) #vector data   
library(spData) #datasets
library(terra) #raster data     
library(tidyverse) #data manipulation
here::i_am("code/4-crs.R")

# +
#' # Vector Data

#' To demonstrate how to identify and change the CRS in a vector data set, we'll use
#' some data sets from the package 
#' [`spData`](https://cran.r-project.org/web/packages/spData/spData.pdf).
#' In this section we will:
#' - Identify the CRS of a vector data set
#' - Change the CRS within a figure 
#' - Transform the CRS


#' ## Identifying the CRS
#' 

us <- us_states
st_crs(us)

# epsg code: 4269

#' ## Change the CRS in a figure
#' 

ggplot() + 
  geom_sf(data = us) +
  labs(title = "EPSG: 4269 (NAD83)") +
  theme_bw()

ggplot() + 
  geom_sf(data = us) +
  labs(title = "Alaska Zone 1") +
  coord_sf(crs = 26931) +
  theme_bw()

ggplot() + 
  geom_sf(data = us) +
  labs(title = "US Nat'l Atlas Equal Area") +
  coord_sf(crs = 2163) +
  theme_bw()


#' Note you haven't changed the CRS of the sf data set.
#' 

st_crs(us)

# you don't need to transform data from one crs to another if you're only
# mapping. geospatial data analysis however requires that crs match

#' ## Change the CRS of a sf data set using an ESPG code entered by hand
#' 

us_proj1 <- us %>% st_transform(., crs = 2163)
  # transforms to US Nat'l Atlas Equal Area crs

st_bbox(us)
st_bbox(us_proj1)

#' Note this doesn't matter for plotting purposes due the sf package automatically
#' applying the `coord_sf()` function to get layers to line up. It does matter
#' when you start to manipulate the spatial data sets, e.g. to find areas of 
#' intersection or overlap.
#'

ggplot() + 
  geom_sf( data = us, color = alpha("red", 0.2), fill = "transparent") +
  geom_sf( data = us_proj1, color = alpha("blue", 0.2), fill = "transparent") +
  labs(title = "Second sf object projected to crs of us") + 
  theme_bw()

ggplot() + 
  geom_sf( data = us_proj1, color = alpha("red", 0.2), fill = "transparent") +
  geom_sf( data = us, color = alpha("blue", 0.2), fill = "transparent") +
  labs(title = "Second sf object projected to crs of us") + 
  theme_bw()
  # crs is always drawn from the first geom_sf

#' ## Change the CRS of a sf dataset using an ESPG code extracted from 
#' another sf data set
#' 

ak <- alaska
st_crs(ak)
  #epsg = 3467

us_proj2 <- us %>% st_transform(., crs = st_crs(ak))

ggplot() + 
  geom_sf(data = us_proj2) +
  labs(title = "AK Coordinates (ESPG: 3467)") +
  theme_bw()

#' # Raster Data
#' 
#' To demonstrate CRS identification and manipulation for raster datasets,
#' we'll again use the dataset of the world's ocean bottom from 
#' [Natural Earth](https://www.naturalearthdata.com/downloads/10m-ocean-bottom/ocean-bottom-base/).
#' Because reprojecting a raster dataset can take a long time, we'll crop the data to
#' the state of Alaska

ocean <- rast(here("data", "s2_dat", "OB_LR.tif"))
ak_ocean <- crop(ocean, ext(-180, -127, 51, 72))

#' ## Identifying the CRS
#'

st_crs(ak_ocean)

#' ## Change the CRS of a raster dataset
#'

#' From [Lovelace et al.](https://geocompr.robinlovelace.net/reproj-geo-data.html?q=project#reproj-ras) that with a raster, 
#' the reprojection process is done with `project()` from the `terra` package. 
#' The `project()` function has a `x` argument, e.g. a raster
#' dataset, and a `y` argument, either another raster data set or an ESPG code,
#' **and** a method argument. [Lovelace et al.](https://geocompr.robinlovelace.net/reproj-geo-data.html?q=project#reproj-ras) 
#' describe how "when reprojecting categorical rasters, the estimated 
#' values must be the same as those of the original. This could be done 
#' using the nearest neighbor method (near), which sets each new cell value 
#' to the value of the nearest cell (center) of the input raster."  

ak_ocean_proj <- ak_ocean %>% project(., "EPSG:3467", method = "bilinear")

#' Let's check it out.
#'

ggplot() +
  layer_spatial(data = ak_ocean) + 
  geom_sf(data = ak, fill = "forestgreen") + 
  scale_fill_continuous(na.value = "white") +
  labs(title = "Unprojected raster") +
  theme_void()

ggplot() +
  layer_spatial(data = ak_ocean_proj) + 
  geom_sf(data = ak, fill = "forestgreen") + 
  scale_fill_continuous(na.value = "white") +
  labs(title = "Prrojected raster - EPSJ: 3467") +
  theme_void()
  # let's project it butthead
