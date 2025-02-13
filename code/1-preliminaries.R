#' ---
#' title: "Preliminaries"
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
#install.packages("devtools")
#devtools::install_github("Nowosad/spDataLarge")
library(here) #setup
library(htmlwidgets) #save maps as .html files
library(sf) #vector data 
library(leaflet) #interactive 
library(spData) #spatial datasets 
library(spDataLarge) #more spatial datasets
library(terra) #raster data
library(tidyverse) #data manipulation/visualization      

here::i_am("code/1-preliminaries-temp.R")

# +
#' # Setup
#' 

#' Using the `here` package instead of `setwd()` increases the ease of collaboration 
#' on R files, both between two individuals and between a single individual in 
#' two time periods (e.g. now and in the future). For example, if the first
#' line of my script is `setwd("C:Users\sunny\path\that\only\I\have")`, anyone 
#' else who wants to run my script will have to change the first line. I will 
#' also have to change the first line if I rename any folders on the path. After 
#' attaching the `here` package, we can use the `here()` function to determine where
#' `here` thinks the top-level of current project is.

here()

#' Now that my directories are correctly set, I can use the `here` package to 
#' read in a shapefile called fish.shp from the "data" folder. I would use:


#' together with the `read_sf()` function. If I create a map from the fish.shp 
#' data and want to save it as a .pdf to my "output" folder I would use:


#'together with the `pdf()`  or `ggsave()` functions.

#+
#' # Data

world <- read_sf(system.file("shapes/world.gpkg", package = "spData"))
class(world)
names(world)
head(world)
valid <- st_is_valid(world)
sum(valid)


#' # Basic operations 

mean(world$pop)
mean(world$pop, na.rm = TRUE)

unique(world$continent)
length(unique(world$continent))


#' ## Converting 

valid2 <- as.factor(valid)
valid3 <- as.numeric(valid)


#' ## Logical operations

world$continent == "Africa"
!world$continent == "Africa"
  # picks out those obs for which continent is not africa

world$continent %in% c("Africa", "Asia")
  # concatinate function, either africa or asia
!world$continent %in% c("Africa", "Asia")

world$continent == "Africa" | world$continent == "Asia"
  # same as above
world$continent == "Africa" & world$continent == "Asia"
  # all false, logic


#' ## Subsetting 
#' ### Base R

world[1,]
  # row 1, all columns
world[ , 1]
  # note: simple features keeps geometry attached
world[1, 1]
  # fiji w/ geometry

world[ , "iso_a2"]
  # same as line 99 above but reads better
world[world$name_long == "Fiji", "iso_a2"]
  # replicats line 101
world[world$name_long %in% c("Fiji", "Finland"), c("name_long", "continent")]


#' ### dplyr - tidyverse syntax

world %>%
  slice(1) %>%
  select(c("name_long", "continent"))
  # %>% this is called a pipe
  # slice grabs the first row in this case

world %>%
  slice(2:n()) %>%
  select(c("name_long", "continent"))

world %>%
  filter(continent == "Oceania") %>%
  dplyr::select(name_long, continent)
    # tidyverse syntax is cleaner - i find it a little harder to read atm

world %>%
  filter(continent == "Oceania") %>%
  dplyr::select(name_long, continent) %>% 
  st_drop_geometry()
    # drops that 'sticky' geometry


#' ## Assigning values 

head(world)
world$pop[world$name_long == "Western Sahara"] <- 578754
head(world)
  # base r method

world <- world %>%
  mutate(lifeExp = ifelse(name_long == "Western Sahara", 62.27, lifeExp))
head(world)
  # tidyverse syntax


#' ## Creating variables 

#' New variables can be created in base R or using the `mutate()` verb from the 
#' tidyverse. Note that the `mutate()` verb allows you to create a list of new 
#' variables, e.g. `pop_dens` and `GDP` when separating that list by commas.
#' In addition to using / to divide, you can use other operators including: 
#' -, +, ^2, ^0.5, sqrt(), log(), exp(), abs(). 

world$pop_dens <- world$pop/world$area_km2
  # base r, creates population density

world <- world %>%
  mutate(pop_dens = pop/area_km2, 
         gdp = gdpPercap*pop
         )
  # tidyverse, creates pop density and gdp at the same time


#' ## Joins

#' [Joins](https://dplyr.tidyverse.org/reference/mutate-joins.html) are important 
#' when working with spatial data, because you often have one 
#' dataset with geometries for enumeration units and another dataset with 
#' attributes of the enumeration units (but no geometry). To map these attributes, 
#' you can join the data without geometries to the data with geometries. 
#' #' Note that the first argument in `full_join()` (x) has to be the dataset 
#' with geometries. To join by variables with different names in x and y use a 
#' named vector. For example, by = c("a" = "b") will match x.a to y.b

coffee <- coffee_data
head(coffee)

world_coffee <- full_join(world, coffee, by = "name_long")
  # spatial data must be listed first in full_join arguement

class(world_coffee)

coffee_world <- full_join(coffee, world, by = "name_long")
class(coffee_world)
  # not a spatial dataset! this is why the spatial data must be the first argument

class(coffee_world$geom)

#' note there are alternatives to `full_join()` such as `left_join()`, etc. for 
#' you to explore.


#' ## Binds

africa <- world %>% filter(continent == "Africa")
not_africa <- world %>% filter(continent != "Africa")
world2 <- rbind(africa, not_africa)

#' Note you can use `cbind()` to bind datasets by columns.


#' ## Aggregate

wcs <- world_coffee %>% 
  group_by(subregion) %>% 
  summarize(
    mean = mean(coffee_production_2016, na.rm = TRUE),
    total = sum(coffee_production_2016, na.rm = TRUE)
  )

class(wcs)
plot(wcs)

#' The `summarize()` verb in dplyr together with `group_by()` allows you to 
#' calculate summary statistics for unique values of the grouping variable, both 
#' for spatial vector and non-spatial datasets. For spatial vector data, using 
#' `summarize()` with `group_by()` returns geometries for the grouping variable 
#' along with summary statistics. How cool is that?!?


# +
#' # Colors

#' Mapping is an art form (or at least it can be) and color is important. There 
#' many options for color in R including:
#' [built-in colors](http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf),
#' as well as 
#' [custom colors](https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf). 


#' # Plotting spatial vector data
#' ## Base R
plot(world)
plot(world["pop_dens"])
plot(world["pop_dens"], main = "Population Density")

#' ## ggplot
ggplot(data = world) +
  geom_sf()
  # add features to ggplot using + symbol

ggplot() +
  geom_sf(data = world)
  # same as above but allows for multiple layers from different data sets

ggplot() +
  geom_sf(data = world) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "World Map",
    tag = "a)"
  )
  # adds labels

ggplot() +
  geom_sf(data = world) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "World Map: theme_bw"
  ) +
  theme_bw()
  # adds theming

ggplot() +
  geom_sf(data = world, color = "black", fill = "ivory") +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "World Map: theme_bw"
  ) +
  theme_bw()
  # color options

ggplot() +
  geom_sf(data = world, color = "transparent",
          aes(fill = pop_dens)) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Population Density"
  ) +
  theme_bw()
  # aes function allows fill function to be performed by variable

ggplot() +
  geom_sf(data = world, color = "transparent",
          aes(fill = gdpPercap)) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "GDP per Capita"
  ) +
  theme_bw()
# aes function allows fill function to be performed by variable

ggplot() +
  geom_sf(data = world, color = "transparent",
          aes(fill = pop_dens)) +
  scale_fill_continuous("pop/km^2") +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Population Density"
  ) +
  theme_bw()
  # scale fill continuous

  ggplot() +
    geom_sf(data = world, color = "transparent",
            aes(fill = pop_dens)) +
    scale_fill_continuous(
      "pop/km^2",
      low = "green", high = "red") +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Population Density"
  ) +
    theme_bw()
  # scale fill continuous
  
  ggplot() +
    geom_sf(data = world, color = "transparent",
            aes(fill = pop_dens)) +
    scale_fill_gradient2(
      "pop/km^2",
      low = "blue", mid = "white", high = "red",
      midpoint = 600) +
    labs(
      x = "Longitude",
      y = "Latitude",
      title = "Population Density"
    ) +
    theme_bw()
  # scale fill gradient2
  
  ggplot() +
    geom_sf(data = world, color = "transparent",
            aes(fill = pop_dens)) +
    scale_fill_viridis_c(
      option = "plasma",
      name = "pop/km^2") +
    labs(
      x = "Longitude",
      y = "Latitude",
      title = "Population Density"
    ) +
    theme_bw()
  # scale fill viridis c
  
  ggplot() +
    geom_sf(data = world, color = "transparent",
            aes(fill = pop_dens)) +
    scale_fill_viridis_c(
      option = "plasma",
      trans = "log",
      name = "ln(pop/km^2)") +
    labs(
      x = "Longitude",
      y = "Latitude",
      title = "Population Density"
    ) +
    theme_bw()
  # scale fill viridis c
    # log transformation can be done in this function!
  
  ggplot() +
    geom_sf(data = world, color = "transparent",
            aes(fill = pop_dens)) +
    scale_fill_viridis_c(
      option = "plasma",
      trans = "log",
      name = "ln(pop/km^2)",
      direction = -1
    ) +
    labs(
      x = "Longitude",
      y = "Latitude",
      title = "Population Density"
    ) +
    theme_bw()
  # scale fill viridis c
  # direction of gradient is reversible 

  ggplot() +
    geom_sf(
      data = world, color = "transparent",
      aes(fill = continent)) + 
    scale_fill_discrete(name = "Continent")
  # scale fill discrete - for categorical
    
  
#' ## leaflet - for intereactive maps

leaflet(world) %>% 
  addTiles() %>% 
  addPolygons()

pal <- colorBin(
  palette = "plasma",
  domain = world$pop_dens,
  bins = 5
)
  # defines color palette

leaflet(world) %>% 
  addTiles() %>% 
  addPolygons(
    fillColor = ~pal(pop_dens),
    fillOpacity =1,
    weight = 0.9
  )
  # " poorly thought out cloropleth"

leaflet(world) %>% 
  addTiles() %>% 
  addPolygons(
    fillColor = ~pal(pop_dens),
    fillOpacity =1,
    weight = 0.9,
    popup = ~name_long
  )
  # popup lets you click on a feature and get the name to appear. neat!


# +
#' # Raster data
#' ## Base R

dem_filepath <- system.file(
  "raster/srtm.tif", 
  package = "spDataLarge")

dem <- rast(dem_filepath)
dem
plot(dem)

#' ## ggplot
dem_df <- as.data.frame(dem, xy = TRUE)

ggplot() + 
  geom_raster(
    data = dem_df,
    aes(x = x, y = y, fill = srtm)
  ) +
  scale_fill_gradient(
    low = "dodgerblue3",
    high = "white",
    name = "elevation"
  ) +
  theme_void()


#' # Saving Maps

#' Spatial datasets can get very large and will take a long time to appear in 
#' the plot window. Best practice is to save your maps as a .pdf file 
#' (or some alternative) to the output subdirectory in your working directory
#' using the `here` package described above. You can either wrap the `ggplot()` 
#' code in `pdf()` and `dev.off()` directly, or save the plot as an object to 
#' later be wrapped in `pdf()` and `dev.off()` as shown below.

# static map example
static_ex <- ggplot() + 
  geom_raster(
    data = dem_df,
    aes(x = x, y = y, fill = srtm)
  ) +
  scale_fill_gradient(
    low = "dodgerblue3",
    high = "white",
    name = "elevation"
  ) +
  theme_void()

ggsave(
  filename = here("output", "raster_example.pdf"),
  plot = static_ex,
  width = 12, height = 8, units = "in"
)

# interactive map example
interact_ex <- leaflet(world) %>% 
  addTiles() %>% 
  addPolygons(
    fillColor = ~pal(pop_dens),
    fillOpacity =1,
    weight = 0.9,
    popup = ~name_long
  )

saveWidget(
  interact_ex,
  file = here("output", "i_world.html"))
