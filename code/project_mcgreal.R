library(ggspatial) #expand spatial capability of ggplot
library(here) #setup
library(readxl) #read in spreadsheet
library(sf) #vector data
library(spData) #datasets
library(tidyverse) #data manipulation

# set loc
here::i_am("code/project_mcgreal.R")

# read in and clean district data
comdists <- read_sf(here(
  "data", "proj_dat", "SEAK_ComDists", "Southeast_Salmon_Districts_GCS_WGS1984.shp")
)
class(comdists)
comdists$District <- as.numeric(comdists$DISTRICT_C)
  # rename district variable for merge
comdists <- comdists[-c(1:2, 9), ]
  # throw out enterance observation

# read in stream spatial data
streams_map <- read_sf(here(
  "data", "proj_dat", "SEAK_Streams", "SEAK_streams.shp")
)
class(streams_map)

# read in and clean stream data
streams <- read_excel(here(
  "data", "proj_dat", "awc_stream.xlsx"), col_names = TRUE)
streams <- streams[-c(1, 4:7)]
  # drop extraneous variables from streams
names(streams)[names(streams) == "SHAPE_Length"] <- "LENGTH"
  # rename vars in streams.df 
streams$STREAM_NO <- substr(streams$AWC_CODE, 1, 12)
  # create new var w/ streamID substring
streams <- streams[-c(1:2)]
  # drop extraneous variables from streams.df
streams$STREAM_NO <- substr(streams$STREAM_NO, 1, 11)
streams$STREAM_NOstart <- substr(streams$STREAM_NO, 1, 7)
streams$STREAM_NOend <- substr(streams$STREAM_NO, 9, 11)
streams$STREAM_NO <- paste(streams$STREAM_NOstart, streams$STREAM_NOend, sep="")
streams <- streams[-c(3:4)]
  # cut extraneous characters (8 and 12) out of strings for matching
streams <- streams %>%
  group_by(STREAM_NO) %>%
  summarize(LENGTH = sum(LENGTH))
  # collapse around stream_NO
streams$District <- substr(streams$STREAM_NO, 1, 3)
  # get district variable for streams
streams <- streams[streams$District < 116,]
  # drop if district > 115
streams$District <- as.numeric(streams$District)
  # Stream # and district as numeric
  
# crop little map of SEAK
alaska <- alaska %>% st_transform(., crs = 3338)
st_bbox(alaska)
st_bbox(streams_map)
seak <- st_crop(alaska, xmin = 910000, ymin = 736000, xmax = 1490000, ymax = 1220000)

# read in and clean SEAK pink counts
pinks <- read_excel(here("data", "proj_dat", "adfg_pink.xlsx"), col_names = TRUE)
pinks$ct <- pinks$PEAK_COUNT
pinks <- pinks[-c(4:10)]
class(pinks)
  # drop extraneous variables from pinks.df
pinksE <- pinks %>% filter(YEAR %% 2 == 0)
pinksO <- pinks %>% filter(YEAR %% 2 != 0)
  # data transform - split into odd/even runs

# merge stream and pink data
mergeE <- merge(pinksE, streams, by=c("STREAM_NO", "District"))
mergeO <- merge(pinksO, streams, by=c("STREAM_NO", "District"))

# group length and ct by district and year
groupedE <- mergeE %>%
  group_by(District, YEAR) %>%
  summarize(ct = sum(ct), length = sum(LENGTH))

groupedE$LENGTHkm <- (groupedE$length/1000)
groupedE$ESCbyKM <- (groupedE$ct/groupedE$LENGTHkm)
  # create escapement per km
groupedE <- groupedE[-c(3:5)]
  # drop extraneous variables from groupedE

groupedO <- mergeO %>%
  group_by(District, YEAR) %>%
  summarize(ct = sum(ct), length = sum(LENGTH))

groupedO$LENGTHkm <- (groupedO$length/1000)
groupedO$ESCbyKM <- (groupedO$ct/groupedO$LENGTHkm)
  # create escapement per km
groupedO <- groupedO[-c(3:5)]
  # drop extraneous variables from groupedO

# spatial joins
comdist_pinksE <- full_join(comdists, groupedE, by = "District")
comdist_pinksO <- full_join(comdists, groupedO, by = "District")

comdist_pinksE <- comdist_pinksE[-c(1:10)]
comdist_pinksO <- comdist_pinksO[-c(1:10)]
# drop extraneous variables from groupedO

# let's take stock with some visualizations
# ggplot() + 
#   geom_sf(data = seak, fill = "ivory") +
#   geom_sf(data = comdists, aes(color = DISTRICT_C, fill = DISTRICT_C)) +
#   theme_bw()
# 
# plot(groupedE$YEAR, groupedE$ESCbyKM, col = groupedE$District)
# plot(groupedO$YEAR, groupedO$ESCbyKM, col = groupedO$District)
# 
# summary(lm(groupedE$ESCbyKM ~ groupedE$YEAR, groupedE))
# summary(lm(groupedO$ESCbyKM ~ groupedO$YEAR, groupedO))

# plot esc/km by year
# ggplot() + 
#   geom_sf(data = seak, fill = "ivory") +
#   geom_sf(data = comdist_pinksE, aes(color = "transparent", fill = ESCbyKM)) +
#   theme_bw()
