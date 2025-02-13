#' ---
#' title: "Interactive Maps"
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
library(terra) #try to read a raster file
library(tidyverse) #data manipulation
library(raster) #reading a raster file
library(spData) #datasets
library(leaflet) #interactive maps
library(leaflet.extras) #heatmaps
library(htmlwidgets) #save maps as .html files
library(pals) #for bivariate choropleth maps
here::i_am("code/10-interactive-maps.R")

source(here("code", "11-helper-funs.R")) # this sources functions so she doesnt have to have too much code 

#+
#' # Overview
#' In this section we will explore one a packages that
#' enables the design of interactive maps
#' [`leaflet`](https://rstudio.github.io/leaflet/)
#' package.

#' Here we look at the most basic call to leaflet, where we add a default base map, set the center
#' of the view, specify the zoom level, and add a single labeled marker to our map.
#' 

leaflet() %>%
  addTiles() #adds base map, default base map, open street maps product 

leaflet() %>%
  addTiles() %>% 
  setView(lng= -122.31492, lat= 47.6525, zoom= 10) %>% 
  addMarkers(lng= -122.31492, lat= 47.6525, popup= "SMEA") #popup is different than a label where you just hover, popups you have to click 

leaflet() %>%
  addTiles() %>% 
  setView(lng= -122.31492, lat= 47.6525, zoom= 10) %>% 
  addMarkers(lng= -122.31492, lat= 47.6525, popup= ' <a href= "https://smea.uw.edu"> SMEA </a>'
  ) #sunny used javascript code to add a clickable webpage 

leaflet(
  options= leafletOptions(minZoom= 7) #this limits how much you can and cannot zoom, focuses your viewer 
) %>%
  addTiles() %>% 
  setView(lng= -122.31492, lat= 47.6525, zoom= 10) %>% 
  addMarkers(lng= -122.31492, lat= 47.6525, popup= ' <a href= "https://smea.uw.edu"> SMEA </a>'
  )


#' There are many icons to choose from: and you can put this library into leaflet 
#' For example, you can use the [font awesome](https://fontawesome.com/icons) 
#' library or make [custom icons with .png files](https://rstudio.github.io/leaflet/markers.html).


file_text <- readr::read_file(
  paste0(.libPaths()[1], 
         "/leaflet/htmlwidgets/plugins/Leaflet.awesome-markers/font-awesome.min.css")
)
icon_names <- stringr::str_extract_all(file_text, "(fa-)([^:]+)")[[1]]

icon_names <- icon_names[-(1 : 36)] %>% 
  stringr::str_sub(4, -1)

sort(icon_names)

smea_icon<- awesomeIcons(
  icon= "university",
  iconColor = "black",
  library= "fa", #for font awesome
  markerColor= "purple"
  ) #create the marker in the environment so we can call it below

leaflet() %>% 
  addTiles() %>% 
  addAwesomeMarkers(lng= -122.31492, lat= 47.6525, icon= smea_icon)

#' There are many [basemaps](http://leaflet-extras.github.io/leaflet-providers/preview/index.html) 
#' to choose from although not all of them are made for detailed maps.

names_providers <- names(providers)

leaflet(
  options= leafletOptions(minZoom= 7)
) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>% #changed from default base map 
  setView(lng= -122.31492, lat= 47.6525, zoom= 10) %>% 
  addMarkers(lng= -122.31492, lat= 47.6525, popup= ' <a href= "https://smea.uw.edu"> SMEA </a>'
  )

leaflet(
  options= leafletOptions(minZoom= 7) 
) %>%
  addProviderTiles(providers$Esri.WorldPhysical) %>% #some base maps are not high enough res and do not let you zoom in, so be cautious 
  setView(lng= -122.31492, lat= 47.6525, zoom= 10) %>% 
  addMarkers(lng= -122.31492, lat= 47.6525, popup= ' <a href= "https://smea.uw.edu"> SMEA </a>'
  ) 

#' We can also specify two basemaps and assign a group name to let the viewer
#' choose using the `addLayersControl()` function. 
#' 

leaflet(
  options= leafletOptions(minZoom= 7) 
) %>%
  addTiles(group= "Street") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "World") %>% #some base maps are not high enough res and do not let you zoom in, so be cautious 
  setView(lng= -122.31492, lat= 47.6525, zoom= 10) %>% 
  addMarkers(lng= -122.31492, lat= 47.6525, popup= ' <a href= "https://smea.uw.edu"> SMEA </a>', group= "SMEA"
  ) %>% 
  addLayersControl(
    baseGroups = c("Street", "World"),
    overlayGroups = "SMEA", 
    options= layersControlOptions(collapsed= FALSE)
  )

#' You can also use base maps from one of many 
#' [web mapping services (WMS)](https://leafletjs.com/examples/wms/wms.html) 
#' such as the USGS topographic maps from the [National Map](https://apps.nationalmap.gov/help/).

nhd_wms_url <- "https://basemap.nationalmap.gov/arcgis/services/USGSTopo/MapServer/WmsServer"

leaflet() %>% 
  addWMSTiles(nhd_wms_url, layers= 0) %>% 
  setView(lng= -121.7603, lat= 46.8523, zoom= 10) 

#' Finally, you can create an inset map with a call to `addMiniMap()`, add a scale
#' bar with a call to `addScaleBar()`, add a daylight layer with `addTerminator()`,
#' and add the lat/long graticule (grid) with `addGraticule()`.

leaflet() %>% 
  addWMSTiles(nhd_wms_url, layers= 0) %>% 
  setView(lng= -121.7603, lat= 46.8523, zoom= 10) %>% 
  addMiniMap(zoomLevelOffset = -4) %>% 
  addScaleBar() %>% 
  addTerminator() %>% #this makes a shadow when you zoom out, not always needed 
  addGraticule() #this makes grids when you zoom out 

#+
#' # Choropleth maps
#' We'll look at creating a choropleth map of world population density and 
#' categorizing countries based on their decile bin.

world <- world %>% 
  filter(!is.na(pop), !is.na(lifeExp)) %>% 
  mutate(
    pop_dens = pop/area_km2,
    popups = paste0(
      "<strong> Country: </strong> ", #strong= bold
      "<br>", #break
      name_long, #pulling from data 
      "<br/> ",
      "<strong> Population Density: </strong> ",
      "<br>",
      round(pop_dens, 1), 
      "<br/> "
    )
  )

#you have to make popups and labels differently 

world$labels <- paste0(
  "<strong> Country: </strong> ",
  "<br>",
  world$name_long, 
  "<br/> ",
  "<strong> Population Density: </strong> ",
  "<br>", #break
  round(world$pop_dens, 1), 
  "<br/> "
) %>%
  lapply(htmltools::HTML)

quantile(world$pop_dens, prob = seq(0, 1, 0.1))

pal<- colorBin(
  palette= "viridis", domain= world$pop_dens,
  bins= quantile(world$pop_dens, prob = seq(0, 1, 0.1))
) #need to make color palette here 

leaflet(world) %>% 
  addTiles() %>% 
  addPolygons(
    fillColor= ~pal(pop_dens), #need tilde to tell it that pop_dens is a varible from our data set 
    color= "transparent", #border color
    fillOpacity= 0.7,
    popup= ~popups,
    highlight = highlightOptions(
      color= "white",
      bringToFront = TRUE
    ) )%>% 
      addLegend(
        pal= pal,
        values= ~pop_dens,
        opacity= 0.7,
        title= "Population Denisty",
        position= "bottomright"
      ) #HAVE SUNNY HELP 

#+
#' # Bivariate choropleth maps
#' We'll look at creating a choropleth map of world population density and 
#' categorizing countries based on their decile bin.

us_sf <- us_states 
us_df <- us_states_df 

us <- left_join(us_sf, us_df, by = c("NAME" = "state"))

us <- us %>% 
  mutate(
    med_inc_th = median_income_15/1000,
    pov_rate = poverty_level_15/total_pop_15,
    non_pov_rate = 1 - pov_rate,
    tot_pov = poverty_level_10 + poverty_level_15
  )

#THE FUNCTION TO MAKE THIS IS OUTSOURCED FROM 11-HELPER-FUNS

map<- leaflet(us) %>%  addTiles()

addBivariateChoropleth(
  map, #first you pass it a map
  us, #then the data
  med_inc_th,#now pass it the two variables: income and poverty
  pov_rate,
  ntiles=3, #3x3 biv choropleth
  var1_label = "median income",
  var2_label = "poverty rate",
  label_arrows = TRUE,
  region_names = NA,
  add_legend = TRUE,
  paletteFunction = pals::stevens.pinkblue
  )

#+
#' # Proportional symbols map
  
world_geom <- world %>% dplyr::select(geom)
world_coords <- st_centroid(world_geom, of_largest_polygon = TRUE) %>% 
  st_coordinates()
world_cents <- cbind(world, world_coords)

world_cents$labels <- paste0(
  "<strong> Country: </strong> ",
  world$name_long, "<br/> ",
  "<strong> Population (Millions): </strong> ",
  round(world$pop/1000000, 1), "<br/> "
) %>%
  lapply(htmltools::HTML)

#+
#' # Proportional symbols choropleth (with custom legend)
  
# world_cents <- world_cents %>% 
#   mutate(color = case_when(
#     pop_dens > 267 ~ pal(270),
#     pop_dens > 70 ~ pal(80),
#     .default = pal(2)
#   )
#   )

#+
#' # Heatmaps
#' 

nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)

set.seed(1)
dots <- st_sample(x = nc, size = round(nc$BIR79/10, 0), type = "random")
coords <- dots %>% st_coordinates() %>% as.data.frame()
dots_hm <- cbind(dots, coords)

#+
#' # Thematic maps with variable layers
  
#' You may want to create a thematic map where the viewer is able to 
#' select the variable that they want to view. This can be done with group names
#' and the `addLayersControl()` function, which we used above to allow users
#' to select a basemap. In this case, you will want variable legends for the different thematic
#' layers to show only when the viewer selects that variable. It turns out this is
#' tricky to implement using `leaflet`, but the issue with the legends should be resolved in the 
#' next release, but for now the solution can be found on the 
#' [issues section](https://github.com/rstudio/leaflet/issues/477) of the github.
  

  # htmlwidgets::onRender("
  #     function(el, x) {
  #        var updateLegend = function () {
  #           var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);
  #           var selectedClass = selectedGroup.replace(' ', '');
  #           document.querySelectorAll('.legend').forEach(a => a.hidden=true);
  #           document.querySelectorAll('.legend').forEach(l => {
  #              if (l.classList.contains(selectedClass)) l.hidden=false;
  #           });
  #        };
  #        updateLegend();
  #        this.on('baselayerchange', el => updateLegend());
  #     }"
  # )


#+
#' # Raster data
  
#' Finally, the leaflet package can accomodate raster data with the
#' `addRasterImage()` function. Note: [this issue](https://github.com/rstudio/leaflet/issues/728)
#' with SpatRaster.
#' 
  
dsm <- raster(here("data", "s7-dat", "NEON-DS-Airborne-Remote-Sensing", "HARV", "DSM", "HARV_dsmCrop.tif"))

#+
#' # Saving Leaflet maps
  
