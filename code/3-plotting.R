#' ---
#' title: "Plotting Spatial Data"
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
here::i_am("code/3-plotting-temp.R")
library(here) #setup
library(ggspatial)  #north arrows, scale bars, and other stuff
library(ggrepel)
library(sf) #vector data   
library(spData) #datasets
library(tidyverse) #data manipulation


# +
#' # Overview
#'
#' In this section we will look how to add common aesthetic features to plots of spatial
#' data using the `ggplot` and [`ggspatial`](https://paleolimbot.github.io/ggspatial/index.html) packages. 
#' All demonstrations will make use of the `world` dataset in the `spData` package. 

world <- read_sf(system.file("shapes/world.gpkg", package="spData"))

#' Specifically, we will cover:
#' 
#' - Background colors
#' - Highlighting geometries
#' - Putting a box around a specific region of a dataset
#' - Zooming in on a specific region of a dataset
#' - Adding north arrows and scale bars using the [`ggsn` package](https://github.com/oswaldosantos/ggsn)
#' - Adding text labels to a plot


#' # Background color

world_map <- ggplot(data = world) +
  geom_sf(color = "#d4d4d4", fill = "ivory") +
  labs(title = "Default background color") +
  theme_void()
world_map

world_map<- world_map  + 
  labs(title= "Custom background color") +
  theme(panel.background= element_rect(fill= "lightcyan"))
world_map

#' # Highlighting geometries

indonesia<- world %>%  filter(name_long== "Indonesia") #this subsets just indonesia from the world data z
world_ind<- world_map +
  geom_sf(data= indonesia, color= "red", fill="pink") +
  labs(title= "Highlight Indonesia")

world_ind


#' # Putting a box around a specific region of a dataset
#' ## Method 1: Use a bounding box 
ind_box <- indonesia %>%  st_bbox()
class(ind_box) #not something you can map so need to convert 
ind_box_sf<- ind_box %>%  st_as_sfc()

world_ind +
  geom_sf(data= ind_box_sf,
          fill= "transparent",
          color= "black",
          lwd= 0.5) +
  labs(title= "Add bounding box from st_bbox()")



#' ## Method 2: Create a custom box 
#' 
#' You can create your own box. This is especially useful if you want to
#' put a box around a region for which is different from the bounding box
#' of a geometry, but here, I'll demonstrate the code using the bounding box
#' of Indonesia.
#' 
ind_box

ind_box[1]
ind_box[[1]] #the double box gives us a number we can actually work with as opposed to just 1 

cust_box<- 
  c(
    xmin = ind_box[[1]]*0.5,
    ymin = ind_box[[2]]*0.5,
    xmax = ind_box[[3]]*1.5,
    ymax = ind_box[[4]]*2
  ) %>% 
  st_bbox(., crs= st_crs(4326)) %>% 
  st_as_sfc()

world_ind +
  geom_sf(data= cust_box,
          fill= "transparent",
          color= "black") +
          labs(title= "Custom box")


#' # Zooming in on a specific region of a dataset
#' 
unique(world$subregion)
st_bbox(world)
sea_box<- world %>%  
  filter(subregion== "South-Eastern Asia") %>% 
  st_bbox()

ind_det<- world_ind +
  coord_sf(
    xlim= c(sea_box$xmin,sea_box$xmax),
    ylim=c(sea_box$ymin,sea_box$ymax),
    expand= TRUE
  ) +
  labs(title= "Zooming in")

ind_det #use this method in PS1 to focus on contiguous US 

#' # Adding north arrows and scale bars 
#' 

ind_det +
  annotation_scale(location = "tr") + 
  annotation_north_arrow(location= "tr", which_north= TRUE) #customs are overlapping

ind_det +
  annotation_scale(location = "tr", style= "ticks") + 
  annotation_north_arrow(location= "tr", which_north= TRUE, style= north_arrow_nautical(), 
                         pad_y = unit(1, "cm"))  + #the padding keeps a border on things 
  labs(title= "Custom decorations")
#the error message is because the data is expressed in lat and long coordinates, which gives us issues. 
#this is a common error message, it is saying the distance on the maps may not be equal 


#' # Adding text labels to a plot
#' 

#' ## Method 1: Use centroids to position labels and visualize with `geom_text()`

st_centroid(world %>% select(geom))

st_coordinates(st_centroid(world %>% select(geom)))

world_cents<- cbind(
  world,
  st_coordinates(st_centroid(world %>% select(geom)))
)

ind_det +
  geom_point(data= world_cents, aes(x= X, y= Y)) +
               labs(title= "Plot centroids")
             
ind_det +
  geom_text(data= world_cents, aes(x= X, y= Y, label= name_long)) +
labs(title= "Label centroids")


#' ## Method 2: Define customize location label and visualize with `geom_text()`

# https://www.latlong.net

jakarta <- data.frame(
  long= 106.865036,
  lat= -6.175110,
  city= "Jakarta"
)

ind_det + 
  geom_text(
    data= jakarta, aes(x= long, y= lat, label= city),
    color= "black", size= 5, nudge_y=5) # you can concatenate long and lat if you have multuple sites you want to include 


#' ## Method 3: Use `stat_spatial_identity()` from `ggspatial`

ind_det + 
  stat_spatial_identity(
    data= world_cents,
    aes(x= X, y= Y, label= name_long),
    geom= "label_repel"
  )


ind_det + 
  stat_spatial_identity(
    data= world_cents %>%  filter(subregion== "South-Eastern Asia"),
    aes(x= X, y= Y, label= name_long),
    geom= "label_repel",
    crm= 4326
  )

ind_det + 
geom_spatial_text_repel(
    data= world_cents %>%  filter(subregion== "South-Eastern Asia"),
    aes(x= X, y= Y, label= name_long),
    crs = 4326, color = "black", bg.color = "white", bg.r = 0.2,
    size = 3, box.padding = 0.30, segment.color = NA) + 
    labs(title = "Text labels with geom_spatial_text_repel())")
