#' ---
#' title: "Thematic Maps"
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
#install.packages("biscale")
library(biscale) #bivariate choropleths
library(cowplot) #combine map and legend bivariate choropleth
library(scatterpie) #proportional symbols pie charts
library(here) #setup
library(sf) #vector data         
library(tidyverse) #data manipulation
library(spData) #datasets
library(ggdensity) #alternative heat map function

#+
#' # Load & Prepare Data
#' In this module, all of our code will use data included in R packages. Most of 
#' our thematic maps will make use of the [`us_states`](https://rdrr.io/cran/spData/man/us_states.html) and
#' [`us_states_df`](https://rdrr.io/cran/spData/man/us_states_df.html). Our heatmap
#' will make use of the [`nc`](https://cran.r-project.org/web/packages/spdep/vignettes/sids.html)
#' dataset.

us_sf <- us_states 
us_df <- us_states_df 

us <- left_join(us_sf, us_df, by = c("NAME" = "state"))

us <- us %>% 
  mutate(
    med_inc_th = median_income_15/1000,
    pov_rate = poverty_level_15/total_pop_15,
    non_pov_rate = 1 - pov_rate
  )

nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)

#+
#' # Choropleth maps

ggplot() +
  geom_sf(data = us, 
    aes(fill = med_inc_th), 
    color = "transparent") +
  labs(fill = "Median Income - 2015 (Th.)") +
  theme_bw() 

#' ## Quantile cut points

quantile(us$med_inc_th, prob = c(0.25, 0.5, 0.75))

ggplot() +
  geom_sf(
    data = us, 
    aes(fill = cut_number(med_inc_th, 4)), 
    color = "white") +
  labs(fill = "Median Income (Th.)") +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot() +
  geom_sf(
    data = us, 
    aes(fill = cut_number(med_inc_th, 4)), 
    color = "white") +
  labs(fill = "Median Income (Th.)") +
  scale_discrete_manual(
    aesthetics = "fill",
    values =c("#dad7cd", "#a3b18a", "#588157", "#344e41")
  ) +
  theme_bw() +
  theme(legend.position = "bottom")
  # look at 'coolers' website for colro scales?

#' ## Custom cut points

ggplot() +
  geom_histogram(data = us, aes(x = med_inc_th), bins = 30)

us <- us %>%
  mutate(cat = case_when(
    med_inc_th < 24.4 ~ "[21.3, 24.4)",
    med_inc_th >= 24.4 & med_inc_th < 29.4 ~ "[24.4, 29.4)",
    med_inc_th >= 29.4 ~ "[29.4, 40.9)"
  )
  )
    #case_when replaces nested if/else statements - "REALLY AWESOME"

ggplot() +
  geom_sf(
    data = us, 
    aes(fill = cat), 
    color = "white") +
  labs(fill = "Median Income (Th.)") +
  scale_discrete_manual(
    aesthetics = "fill",
    values =c("#a3b18a", "#588157", "#344e41")
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

#' ## Bivariate choropleths
#' 
#' Bivariate choropleths allow us to visualize the intersection
#' of two variables. In the olden days it used to be very hard to do
#' this in R, but the [`biscale`](https://cran.r-project.org/web/packages/biscale/vignettes/biscale.html) 
#' package has made life better.

bivch_dat <- bi_class(
  us,
  x = med_inc_th,
  y = pov_rate,
  style = "quantile",
  dim = 3
)

bivch_map <- ggplot() +
  geom_sf(
    data = bivch_dat,
    mapping = aes(fill = bi_class), color = "white",
    show.legend = FALSE
  ) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  bi_theme()
bivch_map

bivch_leg <- bi_legend(
  pal = "DkBlue",
  dim = 3,
  xlab = "Higher Median Income",
  ylab = "Higher Poverty",
  size = 8
)
bivch_leg

ggdraw() +
  draw_plot(bivch_map, 0, 0, 1, 1) +
  draw_plot(bivch_leg, 0, 0, 0.3, 0.3)

#+
#' # Proportional symbols maps
#' 
us <- cbind(us, st_coordinates(st_centroid(us)))

ggplot() + 
  geom_sf(data = us, fill = "transparent") +
  geom_point(
    data = us,
    aes(x=X, y=Y, size = poverty_level_15),
    alpha = 0.25, color = "red"
  ) + 
  scale_size_continuous(
    range = c(1, 20),
    breaks = c(325000, 650000, 1000000)
  ) +
  labs(size = "# in poverty", x = "", y = "") +
  theme_bw()
  # where'd the legend go? Sunny's going to come back to it...

ggplot() + 
  geom_sf(data = us, fill = "transparent") +
  geom_point(
    data = us,
    aes(x=X, y=Y, size = poverty_level_15, color = total_pop_15),
    alpha = 0.25,
    shape = "diamond" # u can change teh shape
  ) + 
  scale_size_continuous(
    range = c(1, 20),
    breaks = c(325000, 650000, 1000000)
  ) +
  labs(size = "# in poverty", x = "", y = "") +
  scale_color_viridis_c(
    option = "plasma",
    name = "population"
  )
  theme_bw()
  ## looks at two variables at once

#+
#' # Proportional symbols maps with pie charts
#' 
#' The [`scatterpie`](https://cran.r-project.org/web/packages/scatterpie/vignettes/scatterpie.html) 
#' package combines charts with proportional symbols. Note the input to the
#' `geom_scatterpie` needs to be a data frame and not a sf data frame. To 
#' accomplish this, when our pie chart variables are in a sf data frame,
#'  we can use the `st_drop_geometry()` function.

class(us)
us_scatter <- us %>% 
  select(NAME, total_pop_15, pov_rate, non_pov_rate, X, Y) %>% 
  st_drop_geometry()
class(us_scatter)

ggplot() + 
  geom_sf(data = us, color = "black", fill = "transparent") +
  geom_scatterpie(
    data = us_scatter,
    aes(x = X, y = Y, r = total_pop_15),
    cols = c("pov_rate", "non_pov_rate"),
    color = NA, alpha = 0.8
  ) 
  # oops! too big

st_bbox(us)

ggplot() + 
  geom_sf(
    data = us, 
    color = "black", 
    fill = "transparent"
  ) +
  geom_scatterpie(
    data = us_scatter,
    aes(x = X, y = Y, r = total_pop_15/8e6),
    cols = c("pov_rate", "non_pov_rate"),
    color = NA, alpha = 0.8
  ) +
  scale_fill_discrete(
    name = "", 
    labels = c("in poverty", "not in poverty")
  ) +
  geom_scatterpie_legend(
    us$total_pop_15/8e6,
    x = -124.7,
    y = 24.56
  ) +
  labs(x = "" , y = "") +
  theme_void()

#+
#' # Dot Density Maps
#' 
#' You can make dot density maps from count data if you
#' create one dot per count and place it randomly in the spatial 
#' unit of interest. This is possible with the `st_sample()` function, but
#' you have to make sure the variable that defines the sample size (number of dots)
#' is a numeric variable with no NA values. It's common to make dot density maps as such, 
#' e.g. election maps, but can be confusing to the viewer if not properly
#' explained. Here we'll demonstrate the concept using the North 
#' Carolina SIDS dataset. I will make some point data from county-level 
#' count data by generating the same number random points in each county as the 
#' number of births that county recorded in 1979.
#' 

ggplot() +
  geom_sf(data = nc, aes(fill = BIR79)) +
  labs(color = "Births (1979)") +
  theme_bw()

set.seed(1)

dots <- st_sample(
  x = nc, 
  size = round(nc$BIR79/10, 0),
  type = "random"
)

coords <- dots %>% st_coordinates() %>% as.data.frame()

dots_hm <- cbind(dots, coords)

ggplot() +
  geom_sf(
    data = nc, 
    fill = "transparent", 
    size = 0.1
  ) +
  geom_sf(
    data = dots, 
    size = 0.2, 
    alpha = 0.3
  ) +
  labs(caption = "1 dot represents 10 births") +
  theme_bw()

#' Note the most beautiful dot density maps are those where you have
#' subcounts from multiple populations of interest (e.g. voters for more than
#' one candidate or population counts from more than one demographic group).
 
#+
#' # Heatmaps
#' 
#' Heatmaps are another good way to display point data. For this demonstration,
#' I will use the point data created above from county-level counts of births
#' in North Carolina in 1979.

# old way
ggplot() + 
  geom_sf(
    data = nc, 
    fill = "transparent"
  ) +
  stat_density_2d(
    data = dots_hm,
    aes(
      x = X, 
      y = Y, 
      fill = ..level.., 
      alpha = ..level..
    ),
    size = 0.01, 
    bins = 24,
    geom = "polygon"
  ) +
  labs(
    x = "", 
    y = ""
  ) +
  scale_fill_gradient(
    low = "yellow", 
    high = "red"
  ) +
  scale_alpha(
    range = c(0, 0.4), 
    guide = FALSE
  ) +
  theme_void() +
  theme(legend.position = "none")

# using ggdensity ----
## subsection ----
ggplot() + 
  geom_sf(
    data = nc, 
    fill = "transparent"
  ) +
  geom_hdr(
    data = coords,
    aes(
      x = X,
      y = Y
    )
  ) +
  labs(fill = "probability") +
  theme_void() +
  theme(legend.position = "bottom")
  # what is this saying in terms of probability?