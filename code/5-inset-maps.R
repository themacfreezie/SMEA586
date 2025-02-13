#' ---
#' title: "Inset Maps"
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
library(sf) #vector data   
library(spData) #datasets
library(tidyverse) #data manipulation
# +
#' # Overview
#' 
#' We will use datasets from the `spData` package to explore inset maps. I will give an example
#' of inset maps using the same projection and an example of an inset map using different projections.
#' The basic steps are the same, but when your inset map has components with different projections,
#' you'll need to think more carefully about the scaling.
#'



# +
#' # Inset map using the same projection

#' * Step 1: read in the datasets and make the main plot and subplot separately,
#' saving each to it's own object. Here' I'll pull some code from previous sessions
#' where we have mapped Indonesia with a marker and text label on Jakarta. The main plot
#' will be a **small-scale** plot of the world, while the subplot will be a **large-scale**
#' plot of Indonesia, where **small-scale** maps show a larger geographic area with few details 
#' on them and large-scale maps show a smaller amount of area with a greater amount of detail. 
#' Note the design of these maps can be improved, but the point of this exercise is to demonstrate
#' the tools needed to make an inset map not best practices for designing an inset map.


world <- read_sf(system.file("shapes/world.gpkg", package="spData"))
indonesia <- world %>%  filter(name_long == "Indonesia")
ind_box <- st_as_sfc(st_bbox(indonesia))
jakarta <- data.frame(long = 106.865036, lat = -6.175110,  city = "Jakarta")

mp_world <- ggplot() +
  geom_sf(data = world, color = "grey", fill = "ivory", size = 0.25) +
  geom_sf(data = world %>%  filter(name_long == "Indonesia"), 
          color = "red", fill = "pink", size = 0.25) +
  geom_sf(data = ind_box, fill = "transparent", size = 0.25) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightcyan"))
mp_world
  # world map with indonesia highlighted

sp_ind <- ggplot() +
  geom_sf(data = indonesia, color = "red", fill = "pink") +
  geom_point(data = jakarta,  aes(x = long, y = lat)) +
  geom_text(data = jakarta, 
            aes(x = long, y = lat, label = city),
            color = "black", size = 5, nudge_x = 10) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightcyan"))
sp_ind
  # detail map of indonesia including location of jakarta

#' * Step 2: get the bounding box of the main plot using `st_bbox()`
#' and the dimensions (width and length) of the subplot (also using `st_bbox()`).
#' Note that the width is always xmax - xmin and the height is always ymax - ymin. You
#' can get these x and y limits by subsetting the bounding box. 
#' The only unfortunate thing about subsetting the bounding box to use
#' it in an equation, i.e. xmax - xmin, is that it returns an incorrectly named number, i.e.
#' it calls the difference xmax.

world_bbox <- st_bbox(world)
world_bbox
  # xmin = -180, ymin = -89.9

indo_bbox <- st_bbox(indonesia)
indo_bbox
  # width = ~46, height = ~15
indo_w <- indo_bbox$xmax - indo_bbox$xmin
indo_w

indo_h <- indo_bbox$ymax - indo_bbox$ymin
indo_h


#' * Step 3: put everything together using the `annotation_custom()` function. To have
#' control over the position of the subplot, you will need to make sure to get the width and
#' length of the subplot right and scale it up by some factor. Here we'll scale the subplot
#' up by a factor of 4. 

mp_world +
  annotation_custom(
    grob = ggplotGrob(sp_ind)
  )

inset_map <- mp_world +
  annotation_custom(
    grob = ggplotGrob(sp_ind),
    xmin = world_bbox$xmin,
    xmax = world_bbox$xmin + 4*indo_w,
    ymin = world_bbox$ymin,
    ymax = world_bbox$ymin + 4*indo_h
  )

#' * Bonus step: You can add lines to your figure using `geom_segment()`. 

line1 <- data.frame(
  x1 = indo_bbox$xmax,
  x2 = world_bbox$xmin + 4*indo_w,
  y1 = indo_bbox$ymin,
  y2 = world_bbox$ymin + 4*indo_h
)

line2 <- data.frame(
  x1 = indo_bbox$xmin,
  x2 = world_bbox$xmin,
  y1 = indo_bbox$ymax,
  y2 = world_bbox$ymin + 4*indo_h
)

inset_map +
  geom_segment(
    data = line1,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    linewidth = 0.25
  ) +
  geom_segment(
    data = line2,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    linewidth = 0.25
  )

# +
#' # Inset map using different projections
#' Inset maps using different projections follow the same basic steps, but I recommend that
#' you transform the data to have the CRS you desire before creating the inset maps 
#' (instead of just imposing the CRS you desire using `coord_sf()`). This will make it easier
#' to get the dimensions of the subplots for positioning. 

#' * Step 1: read in the datasets, transform them to the CRS you desire,
#'  and make the main plot and subplot(s) separately, saving each to it's own object. 
#'  Here I'll use data on the mainland US, Alaska, and Hawaii. To reduce distortion, each will
#'  have their own projected CRS. To make the main plot, or the map of the mainland US, I 
#'  first inspect the bounding box of the data and use the `coord_sf()` function to make a
#'  little more room on the left side of the plot so that the subplots can fit.

mainland <- us_states %>% st_transform(., crs = 2163)
ml_bbox <- st_bbox(mainland)
ml_bbox

alaska <- alaska %>% st_transform(., crs = 3338)
hawaii <- hawaii %>% st_transform(., crs = 4135)

#create a main plot with extra room to place AK and HI
mp <- ggplot() +
  geom_sf(data = mainland, fill = "ivory") +
  coord_sf(xlim = c(-2500000, ml_bbox$xmax), 
           ylim = c(-2300000, ml_bbox$ymax)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "lightcyan"))

ap <- ggplot() +
  geom_sf(data = alaska, fill = "ivory") +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightcyan"))

hp <- ggplot() +
  geom_sf(data = hawaii, fill = "ivory") +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightcyan"))

#' * Step 2: get the bounding box of the main plot using `st_bbox()`
#' and the dimensions (width and length) of the subplots (also using `st_bbox()`). 

ak_bbox <- st_bbox(alaska)
ak_w <- ak_bbox$xmax - ak_bbox$xmin
ak_h <- ak_bbox$ymax - ak_bbox$ymin

hi_bbox <- st_bbox(hawaii)
hi_w <- hi_bbox$xmax - hi_bbox$xmin
hi_h <- hi_bbox$ymax - hi_bbox$ymin

#' * Step 3: put everything together using the `annotation_custom()` function. To have
#' control over the position of the subplot, you will need to make sure to get the width and
#' length of the subplots right and scale it up **or down** by some factor. 
#' Here we'll scale the Alaska subplot down by a factor of 2.5 and the Hawaii subplot up by 
#' a factor of 120000. When working with different projections, it's hard to figure out the
#' right scaling factors, but you need to think of the relative dimensions of the main plot to the
#' subplot. Here, the mainplot is 2514262.0--2031905.0=4546167 wide relative to Hawaii which is
#' 3.423812 wide. Without scaling Hawaii up by a large factor, we wouldn't be able to see it. After
#' scaling Hawaii up by 120000, the width is 410857.4 or about 9% of the width of the main plot,
#' which works.

mp +
  annotation_custom(
    grob = ggplotGrob(ap),
    xmin = -2500000,
    xmax = -2500000 + ak_w/2.5,
    ymin = -2300000,
    ymax = -2300000 + ak_h/2.5
  ) +
  annotation_custom(
    grob = ggplotGrob(hp),
    xmin = -2500000 + ak_w/2.5 + 1,
    xmax = -2500000 + ak_w/2.5 + 1 + 150000*hi_w,
    ymin = -2300000,
    ymax = -2300000 + 150000*hi_h
  )
