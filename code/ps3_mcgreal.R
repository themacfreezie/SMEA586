#' ---
#' title: "SMEA 586: Problem Set 3"
#' author: "Brian McGreal"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output:
#'    html_document:
#'       number_sections: true
#'       toc: false
#'       toc_float:
#'          collapsed: true
#' ---
#' 
#+ include=F
library(biscale) #bivariate choropleths
library(cowplot) #combine map and legend bivariate choropleth
library(here) #directories
library(sf) #vector data
library(tidyverse) #data processing
library(tigris) #access counties and roads
options(tigris_use_cache = TRUE)
here::i_am("code/ps3_mcgreal.R")

#' Over the next decade, massive investments in fish passage improvements will be 
#' made in the Pacific Northwest. Despite this, however, available financial resources 
#' fall extremely short of what would be needed to remove all blockages to fish passage. 
#' Thus, it is critical to ensure investment dollars are targeted to those projects 
#' that deliver the most high-quality salmon habitat.
#' 
#' However, it is often difficult to estimate habitat gains from barrier correction
#' projects due to unknown barriers in the system. In this problem set, we'll 
#' look at the problem of unknown or hidden barriers in Snohomish county. 
#' Specifically, we will define a hidden barrier to exist anytime there is not a 
#' recorded fish passage barrier within 50 feet of a road/stream intersection.
#' 
#' The goal of the problem set is to create a bivariate choropleth map 
#' demonstrating variability in the number of crossings without a documented fish 
#' passage barrier per square kilometer (AreaSqKm) and variability in the number 
#' fish passage barriers per square kilometer (AreaSqKm) across HUC 12 (watersheds) 
#' in Snohomish county. Make sure your figure does not show information for space 
#' outside of Snohomish county (and you can choose how you want to do this).
#' 
#' Note that although we don't cover bivariate choropleth maps until Section 8 of 
#' the class, you can do ~90% of the work on this problem set with tools learned
#' in Section 6.
#' 
#' The 5 datasets you will use are:
#' 
#' (1) A (https://rdrr.io/cran/tigris/man/roads.html)[roads dataset] for Snohomish County.
#' (2) The (https://wdfw.wa.gov/species-habitats/habitat-recovery/fish-passage/assessment)[WDFW Fish passage inventory] 
#' generated on the day you first download it (or subsequently update it by 
#' deleting from your data folder and downloading it again).
#' (3) The boundaries of Snohomish County.
#' (4) The (https://www.usgs.gov/national-hydrography/nhdplus-high-resolution)[USGS NHDPlus HR dataset]. 
#' (5) The boundaries of HUC12s within our study region. Note the HUC12s are uniquely 
#' identified by the NHDPlusID variable.

# +
#' # Load data

roads <- tigris::roads("Washington", "Snohomish", year = 2023) 

if(!file.exists(here("data", "WdfwFishPassage.zip"))){
  download.file(
    "https://fortress.wa.gov/dfw/public/PublicDownload/habitat/FishPassage/WdfwFishPassage.zip",
    here("data", "WdfwFishPassage.zip")
  )
  unzip(
    here("data", "WdfwFishPassage.zip"),
    exdir = here("data", "WdfwFishPassage")
  )
} else {
  message(paste0("WDFW Inventory already present. Last downloaded ", file.info(here("data", "fish_pass_invs", "WdfwFishPassage.zip"))$mtime, "."))
}

wdfw_inv <- 
  st_read(
    here("data", "WdfwFishPassage", "WdfwFishPassage.gdb"), 
    layer = "WDFW_FishPassageSite"
  ) 

snohomish <- tigris::counties("Washington", year = 2023) %>% 
  filter(NAME == "Snohomish")

streams <- 
  read_sf(
    here("data", "ps3_dat", "NHD1711", "NHDPLUS_H_1711_HU4_GDB.gdb"),
    # Include core flowline data, catchment data, and WDB HUC boundaries
    layer = "NHDFlowline"
  ) %>% 
  st_zm()

huc12s <- 
  read_sf(
    here("data", "ps3_dat", "NHD1711", "NHDPLUS_H_1711_HU4_GDB.gdb"),
    # Include core flowline data, catchment data, and WDB HUC boundaries
    layer = "WBDHU12"
  )

# subset data
# check classes
class(roads)
class(snohomish)
class(streams)
class(wdfw_inv)
class(huc12s)

# subset three of four
snohomish_roads <- roads %>% st_filter(., y = snohomish)
snohomish_streams <- streams %>% st_filter(., y = snohomish)
snohomish_huc12s <- huc12s %>% st_filter(., y = snohomish)

# crs don't match for these guys
st_crs(snohomish)
st_crs(wdfw_inv)

snohomish <- st_transform(snohomish, crs = 2856)
st_crs(snohomish)

# final subset
snohomish_wdfw_inv <- wdfw_inv %>% st_filter(., y = snohomish)

class(snohomish_roads)
class(snohomish_streams)
class(snohomish_wdfw_inv)
class(snohomish_huc12s)

snohomish_roads <- st_transform(snohomish_roads, crs = 2856)
snohomish_streams <- st_transform(snohomish_streams, crs = 2856)
snohomish_wdfw_inv <- st_transform(snohomish_wdfw_inv, crs = 2856)
snohomish_huc12s <- st_transform(snohomish_huc12s, crs = 2856)

# let's see if we can find intersecting roads and streams
snohomish_intersections <- st_intersection(snohomish_streams, snohomish_roads)
snohomish_intersections <- st_as_sf(x = snohomish_intersections)
class(snohomish_intersections)

# create ID for intersections
snohomish_intersections <- tibble::rowid_to_column(snohomish_intersections, "ID")

# define known barriers
z <- st_join(snohomish_intersections, snohomish_wdfw_inv, st_is_within_distance, dist = 15.24)
snohomish_unknown <- z %>% filter_at(vars(SiteRecordID), all_vars(is.na(.)))

# see how it goes together
# ptm <- proc.time()
# test <- ggplot() +
#   geom_sf(data = snohomish_huc12s) +
#   geom_sf(data = snohomish_streams, color = "blue", alpha = 0.2) +
#   geom_sf(data = snohomish_roads, color = "yellow", alpha = 0.2) +
#   geom_sf(data = snohomish_wdfw_inv, color = "green", alpha = 0.05) +
#   geom_sf(data = snohomish_unknown, color = "red", alpha = 0.05)
# test
# proc.time() - ptm

# counting points per huc12
CountPointsInPolygons <- function(pts, polygons){
  countPts = c()
  for (i in 1:nrow(polygons)) {
    polySelect <- polygons[i,]
    pts2 <- st_intersection(pts, polySelect)
    countPts[i] = nrow(pts2)
  
  }
  
  return(cbind(polygons,countPts))
}

huc12s_intersections <- CountPointsInPolygons(snohomish_intersections, snohomish_huc12s)
huc12s_intersections <- huc12s_intersections %>% 
  rename(intersectionPts = countPts)

huc12s_unknown <- CountPointsInPolygons(snohomish_unknown, snohomish_huc12s)
huc12s_unknown <- huc12s_unknown %>% 
  rename(hiddenPts = countPts)

huc12s_unknownJOIN <- as.data.frame(cbind(huc12s_unknown$TNMID, huc12s_unknown$hiddenPts))

huc12s_unknownJOIN <- huc12s_unknownJOIN %>% 
  rename(hiddenPts = V2)
huc12s_unknownJOIN <- huc12s_unknownJOIN %>% 
  rename(TNMID = V1)

class(huc12s_intersections)
class(huc12s_unknownJOIN)

# join them data sets
huc12s_barriers <- full_join(huc12s_intersections, huc12s_unknownJOIN, by = "TNMID")
huc12s_barriers <- transform(huc12s_barriers, hiddenPts = as.numeric(hiddenPts))

# bivariate chloropleth?
summary(huc12s_barriers$intersectionPts)
summary(huc12s_barriers$hiddenPts)
huc12s_barriers$intersectionPts_scaled <- (huc12s_barriers$intersectionPts/huc12s_barriers$AreaSqKm)
huc12s_barriers$hiddenPts_scaled <- (huc12s_barriers$hiddenPts/huc12s_barriers$AreaSqKm)

huc12s_barriers <- st_transform(huc12s_barriers, crs = 2856)
bivch_dat <- bi_class(
  huc12s_barriers,
  x = intersectionPts_scaled,
  y = hiddenPts_scaled,
  style = "quantile",
  dim = 4
)

bivch_dat <- st_intersection(snohomish, bivch_dat)

bivch_map <- ggplot() +
  geom_sf(
    data = bivch_dat,
    mapping = aes(fill = bi_class), 
    color = "white",
    show.legend = FALSE
  ) +
  bi_scale_fill(pal = "GrPink2", dim = 4) +
  labs(title = "Hidden Salmon Barriers v. Total Salmon Barriers, per sq. km", subtitle = "Snohomish County, WA") +
  theme_bw()
bivch_map

bivch_leg <- bi_legend(
  pal = "GrPink2",
  dim = 4,
  xlab = "More Total Barriers",
  ylab = "More Hidden Barriers",
  size = 8
)
bivch_leg

bi_chloropleth <- ggdraw() +
  draw_plot(bivch_map, 0, 0, 1, 1) +
  draw_plot(bivch_leg, 0.78, 0.1, 0.25, 0.25)
bi_chloropleth

# snohomish_huc12sISSUE <- st_transform(snohomish_huc12s, crs = 4269)
# snohomishISSUE <- st_transform(snohomish, crs = 4269)
# issue <- ggplot() +
#   geom_sf(data = snohomish_huc12sISSUE) +
#   geom_sf(data = snohomishISSUE, color = "red", alpha = 0.05)
# issue
