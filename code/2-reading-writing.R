#' ---
#' title: "Reading and Writing Spatial Data"
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
here::i_am("code/2-reading-writing-temp.R")
library(here) #setup
library(sf) #vector data         
library(ncdf4) #net cdf rasters  
library(terra) #raster data 
library(tidyverse) #data manipulation
library(ggspatial) #expand spatial capability of ggplot

# +
#' # Overview
#' 

#' Spatial data (both vector and raster data) come in many different forms and there are trends
#' in preferred formats for storing spatial data. This section will give an overview of reading and 
#' writing the following types of spatial data:
#' - point data stored as longitude/latitude coordinates (e.g. a .csv),
#' - vector data stored as a shape file (.shp),
#' - raster data stored as a tiff file (.tif),
#' - raster data stored as a net cdf file (.nc).
#' - geodatabases which can store both vector and raster data (.gdb),
#' Due to time constraints we will not cover all of the various formats that spatial data can 
#' take, but I am happy to work with you on an individual to get whatever spatial data you 
#' have loaded into R.

# +
#' # Point data stored as longitude/latitude coordinates
#' 

mammals <- read_csv(here("data", "s2_dat", "obis_seamap_dataset_820_points.csv"))
head(mammals)
  # not spatial data but contains lat & long

#' To demonstrate how to work with point data, we will use data from a 2003 Gulf of Alaska
#' marine mammal survey from the [OBIS-SEAMAP project](http://seamap.env.duke.edu). The data
#' are stored as a .csv file. Note that you can always create your own .csv file with information
#' on the longitude/latitude coordinates of points of interest (e.g. a field study site). First,
#' we import the .csv file using `read_csv` and explore the data.

mammals <- st_as_sf(mammals, coords = c("longitude", "latitude"), crs = 4326)
class(mammals)
  # this is now a sf tibble dataframe - spatial dataset!

#' The data are stored as a tibble dataframe and R does not recognize these to be
#' spatial data. The variable `longitude` could represent any numerical value for
#' the observations in the dataset (e.g. age or price). To make these data spatial, 
#' we use the `st_as_sf()` function, which requires you to provide the name of the
#' longitude and latitude coordinates. Further, the epsg code for long/lat data 
#' is 4326. We will learn more about epsg codes, and coordinate reference systems (crs)
#' in subsequent sections of this course. For now, we will define the crs in the
#' `st_as_sf()` function.


#' Note that the new `mammals` dataset has a new variable called `geometry`, meaning it
#' is explicitly spatial data, R reads it as such, and we can plot it in `ggplot`. 

ggplot() +
  geom_sf(data = mammals, aes(color = species_name)) +
  scale_color_discrete( name = "species") +
  theme_bw()

# +
#' # Vector data stored as a shape file
#'

#' Here we will import a .shp of the world coastline from 
#' [Natural Earth](https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-coastline/), 
#' map the coastline, crop the coastline data based on a bounding box of [Alaska](http://bboxfinder.com/#0.000000,0.000000,0.000000,0.000000),
#' map the cropped coastline, and save the crop as a new .shp file.   

#' ## Reading a .shp file

coast <- read_sf(
    here("data", "s2_dat", "ne_10m_coastline", "ne_10m_coastline.shp")
    )
class(coast)
  # simple features dataframe

ggplot(data = coast) +
  geom_sf() +
  theme_void() + 
  theme(legend.position = "none")

#' ## Cropping a vector dataset
#' 
#' The bounding box of the coast object is based on decimal longitude/latitude
#' where the range of longitude is -180 to 180. Positive values are east of the Prime Meridian (runs through Greenwich)
#' and the range of latitude is -90 to 90 where positive values are north of the equator.
#' However latitude and longitude are not points on a plane, because the earth is round. You will
#' get an error message stating as much, but we can ignore it for now. The error message will
#' also note that attribute variables are assumed to be spatially constant throughout the geometries. 
#' This means that if you had spatial dataset with geometries for all countries in the world and a variable
#' giving the population for each country and you then crop the United States in half, the function
#' assumes is that half of the population will be in the cropped feature.

st_bbox(coast)
  # shows 'bounding box'
st_bbox(mammals)

coast_crop <- st_crop(
  coast,
  xmin = -179.2, ymin = 51.2,
  xmax = -126.9, ymax = 72.1
  )

ggplot(data = coast_crop) +
  geom_sf() +
  theme_void() + 
  theme(legend.position = "none")

#' ## Writing a .shp file

st_write(
  obj = coast_crop,
  dsn = here("data", "s2_dat", "coast_crop", "coast_crop.shp"),
  delete_layer = TRUE
)

# +
#' # raster data stored as a tiff file (.tif)
#'

#' Here we will import a .tif raster dataset of the world's ocean bottom from 
#' [Natural Earth](https://www.naturalearthdata.com/downloads/10m-ocean-bottom/ocean-bottom-base/),
#' plot the ocean bottom data (using base R), crop the ocean bottom data, map the cropped ocean bottom (using base R), 
#' save the crop as a new .tif file, and map the cropped ocean bottom together with the cropped coastline, and 
#' marine mammal sitings (using `ggplot`). 

#' ## Reading a .tif file
ocean <- rast(here("data", "s2_dat", "OB_LR.tif"))
plot(ocean)

#' The `crop()` function from the `terra` package requires the user to define the extend
#' defined as (xmin, xmax, ymin, ymax).

#' ## Cropping a raster dataset
ocean_crop <- crop(ocean, ext(-179.2, -126.9, 51.2, 72.1))
plot(ocean_crop)
  # different package than coast crop (vector v. raster)
  # therefore different language
    # xmin, xmax, ymin, ymax

#' ## Writing a .tif file 
terra::writeRaster(
  x = ocean_crop,
  filename = here("data", "s2_dat", "ocean_crop.tif"),
  overwrite = TRUE
)

#' ## Plotting raster and vector data in `ggplot`

#with geom_raster() function from ggplot
ptm <- proc.time()
ocean_crop_df <- as.data.frame(ocean_crop, xy=TRUE)

ggplot()+
  geom_raster(
    data = ocean_crop_df,
    aes(x, y, fill = OB_LR_1),
    show.legend = FALSE
    ) +
  geom_sf( data = coast_crop) +
  geom_sf(
    data=mammals,
    aes(color = scientific_name),
    alpha = 0.25
    ) +
  scale_color_discrete(name = "species") +
  theme_void()

proc.time()[3] - ptm

# with layer_spatial() function from ggspatial
ptm <- proc.time()

ggplot()+
  layer_spatial(
    data = ocean_crop
  ) +
  geom_sf( data = coast_crop) +
  geom_sf(
    data=mammals,
    aes(color = scientific_name),
    alpha = 0.25
  ) +
  scale_color_discrete(name = "species") +
  theme_void()

proc.time()[3] - ptm

# +
#' # raster data stored as a net cdf file (.nc)
#'

#' It is increasingly common to find large raster datasets stored as net cdf files.
#' Here we will use the `ncdf4` package to get information about a .nc file that 
#' contains [NOAA mean daily sea surface temperature](https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html)
#' from 1971-2000. We will use the `terra` package to load the raster data.



#' Let's try adding the world coastline. 



#' Note there is a misalignment between the net cdf file and the
#' coastline data due to whether the xmin value captures the eastern or western hemisphere. 
#' To fix this issue you essentially have to crop raster dataset so that you have 
#' one dataset for the west hemisphere and another for the east hemisphere. 
#' Then you can merge them back again, putting the western hemisphere to the left 
#' of the eastern hemisphere and redefining the extent of the merged raster. 
#' We will cover the method to do this in our section on geocomputation with raster
#' data.


# +
#' # Geodatabases
#'

#' Geodatabases (.gdb files) are a common way to store mutiple spatial datasets 
#' using ArcGIS and can handle both vector and raster data. Here we explore the 
#' [environmental sensitivity index data for Puget Sound](https://catalog.data.gov/dataset/environmental-sensitivity-index-esi-atlas-puget-sound-and-strait-of-juan-de-fuca-maps-and-geogr),
#' which stores multiple vector datasets and non-spatial data. 
#' First, we'll look at all of the layers in the geodatabase with the `st_layers()` function,
#' then we will read in one of the layers and plot it.
#' **Note** Historically, it was not possible to read raster layers from a .gdb file in
#' R, but [that may be changing](https://github.com/r-spatial/sf/issues/1931).

