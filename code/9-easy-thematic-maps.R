#' ---
#' title: "Easy Thematic Maps"
#' author: "Sunny L. Jardine"
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
library(sf) #vector data
library(spData) #datasets
library(terra) #raster data
library(tidyverse) #data manipulation
library(tmap) #thematic mapping
library(tmaptools) #for basemaps in plot mode
library(leaflet) #for basemaps in view mode
here::i_am("code/9-easy-thematic-maps.R")

#+
#' # Overview
#' Thus far, we've used `ggplot` to visualize our spatial data. There are a lot 
#' of advantages to using `ggplot` including, the figures are easy to customize,
#' many people use the `ggplot` infrastructure to develop other spatial data
#' visualization packages (e.g. the pie chart and bivariate choropleth packages
#' we've explored). However, `ggplot` was not developed specifically for
#' spatial data, which can make things hard. Thus, an alternative
#' package to consider is [`tmap`](https://r-tmap.github.io/tmap/),
#' which *was* developed specifically for spatial data.

#+
#' # Load & Prepare Data

us_geom <- us_states
us_data <- us_states_df

us <- inner_join(us_geom, us_data, by = c("NAME" = "state")) 
# class(us)

dsm <- rast(here("data",  "s7-dat", "NEON-DS-Airborne-Remote-Sensing", "HARV", "DSM", "HARV_dsmCrop.tif"))
# class(dsm)

data(World, land, metro)
# class(World)
# class(land)
  # stars object, another way to store raster data
# summary(land$cover_cls)
# class(metro)

#+
#' # Choropleth maps
#'

tm_shape(us) +
  tm_fill( col = "median_income_15")

tm_shape(us) +
  tm_polygons( col = "median_income_15")
  # adds borders

tm_shape(us) +
  tm_polygons( col = "median_income_15", title = "Median Income (2015)") +
  tm_compass(type = "4star", position = c("left", "top"), size = 2) +
  tm_scale_bar(breaks = c(0, 500, 1000), text.size = 0.75) +
  tm_layout(
    legend.title.size = 0.75,
    legend.text.size = 0.5,
    inner.margins = c(0.2, 0.3, 0.1, 0.01) # bottom, left, top, right margins
  )

tmap_options(
  title.size = 0.5, 
  legend.position = c("LEFT", "TOP"),
  legend.title.size = 2,
  legend.text.size = 0.5,
  inner.margins = c(0.2, 0.3, 0.1, 0.01)
)
  # can save same set of preferences to be applied across multiple maps

m_equal <- tm_shape(us) +
  tm_polygons(col = "median_income_15", style = "equal") +
  tm_layout(title = "equal breaks (default style)")
m_pretty <- tm_shape(us) +
  tm_polygons(col = "median_income_15", style = "pretty") +
  tm_layout(title = "pretty breaks")
m_quantile <- tm_shape(us) +
  tm_polygons(col = "median_income_15", style = "quantile") +
  tm_layout(title = "quantile breaks")
m_jenks <- tm_shape(us) +
  tm_polygons(col = "median_income_15", style = "jenks") +
  tm_layout(title = "jenks breaks")
m_cat <- tm_shape(us) +
  tm_polygons(col = "REGION", style = "cat") +
  tm_layout(title = "cat breaks")

tmap_arrange(m_equal, m_pretty, m_quantile, m_jenks, m_cat)
  # maps all them maps with options asserted above

tmap_options_reset()
  # clears out established options

#+
#' # Proportional symbols maps
#'

tm_shape(us) +
  tm_polygons(col = "white") +
  tm_bubbles(
    border.col = "black",
    col = "black",
    size = "poverty_level_15",
    title.size = "number in poverty"
  ) +
  tm_layout(
    legend.position = c("center", "bottom"),
    inner.margins = c(0.25, 0.01, 0.01, 0.01)
  )

#+
#' # Proportional symbols/ choropleth maps
#'

tm_shape(us) +
  tm_polygons(col = "white") +
  tm_bubbles(
    border.col = "black",
    col = "total_pop_15",
    size = "poverty_level_15",
    title.size = "number in poverty",
    title.col = "population"
  ) +
  tm_layout(
    legend.position = c("left", "bottom"),
    legend.stack = "horizontal",
    inner.margins = c(0.25, 0.01, 0.01, 0.01)
  )

#+
#' # Raster data
#'

tm_shape(dsm) + 
  tm_raster() +
  tm_layout(legend.outside = TRUE)

tm_shape(dsm) + 
  tm_raster(style = "quantile") +
  tm_layout(legend.outside = TRUE)

tm_shape(dsm) + 
  tm_raster(style = "quantile",
            n = 6,
            palette = get_brewer_pal("Greens", n = 6, plot = FALSE)) +
  tm_layout(legend.outside = TRUE)

tm_shape(dsm) + 
  tm_raster(style = "cont",
            palette = get_brewer_pal("Greens", n = 6, plot = FALSE)) +
  tm_layout(legend.outside = TRUE)

pal8 <- c(
  "#33A02C", "#B2DF8A", "#FDBF6F", "#1F78B4", 
  "#999999", "#E31A1C", "#E6E6E6", "#A6CEE3"
)

tm_shape(land, ylim = c(-88, 88)) +
  tm_raster("cover_cls", 
            palette = pal8,
            title = "Global Land Cover") +
  tm_shape(metro) +
  tm_dots(col = "#e31a1c") +
  tm_shape(World) +
  tm_borders(col = "black") +
  tm_layout( scale = 0.75,
             legend.position = c("left", "bottom"),
             legend.bg.color = "white", 
             legend.bg.alpha = 0.9,
             legend.frame = "gray50")

tm_shape(land, xlim = c(-170, 180)) +
  tm_raster("cover_cls", 
            palette = pal8,
            title = "Global Land Cover") +
  tm_shape(metro) +
  tm_dots(col = "#e31a1c") +
  tm_shape(World) +
  tm_borders(col = "black") +
  tm_layout( scale = 0.75,
             legend.outside = TRUE)

# save it up
tmap_save(filename = here("output", "tmap-raster.pdf"))

# interactivity
tm_shape(land, xlim = c(-170, 180)) +
  tm_raster("cover_cls", 
            palette = pal8,
            title = "Global Land Cover") +
  tm_shape(metro) +
  tm_dots(col = "#e31a1c") +
  tm_shape(World) +
  tm_borders(col = "black") +
  tm_layout( scale = 0.75,
             legend.outside = TRUE) +
  tmap_mode("view")

# save it up
tmap_save(filename = here("output", "tmap-raster.html"))
