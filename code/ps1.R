#' ---
#' title: "SMEA 586: Problem Set 1"
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
library(here)
library(readxl)
library(sf)
library(tidycensus)
library(tidyverse)
library(tigris)

here::i_am("code/ps1.R")

#' We will explore questions related to US citizen's opinions on climate change 
#' using data from the 
#' [Yale Program on Climate Change Communication](https://climatecommunication.yale.edu/visualizations-data/ycom-us/).
#' 
#' Specifically, we will create maps from the 2023 county-level estimates of:
#' (1) Estimated percentage who think global warming will harm people in developing 
#' countries a moderate amount/a great deal (`devharm`); and
#' (2) Estimated percentage who think global warming will harm people in the US a 
#' moderate amount/a great deal (`harmus`),
#' along with US Census Bureau's county boundaries that can be downloaded using the
#' [tigris package](https://github.com/walkerke/tigris).
#' 
#' The Yale data are provided on our course website and should be 
#' saved to your `data` subdirectory.
#'

#' 1) Start by reading in Yale data from 2023, which comes as a .xlsx file and
#' are stored in the tab `YCOM_2023` (I've done this part for you). 
#' Then select the county-level observations. Call this object `clim_dat`.

clim_dat <- read_excel(here("data", "YCOM7_publicdata.xlsx"), sheet = "YCOM_2023")
class(clim_dat)
clim_dat <- clim_dat %>% filter(geotype == "county")

#' 2) Now use the [`tigris` package](https://github.com/walkerke/tigris) to load in county borders 
#' from 2023, naming this spatial dataset `us_counties`. Note that the default is to 
#' download high-resolution data, best for visualizing a small number of counties, but
#' not necessary for our purposes.

us_counties <- counties(state = NULL, cb = FALSE, resolution = "500k", year = 2023, progress_bar = FALSE)

#' 3) Next load the dataset `us_states.csv` (I did this part for you), selecting the 
#' variables `STATEFP` and `NAME`, and join these data to `us_counties` by FIPs code.

us_states <- read_csv(here("data", "us_states.csv"))
class(us_states)
us_counties <- full_join(us_counties, us_states, by = "STATEFP")
class(us_counties)

#' 4) Note that `clim_dat` has a variable named `geoname` that can, in theory, be matched
#' with the variable `NAMELSAD` in `us_counties`, but the formatting isn't right. Here,
#' you will use the function [`separate_wider_delim()`](https://tidyr.tidyverse.org/reference/separate_wider_delim.html)
#' to reformat `geoname`. Note that you want to include any spaces in your definition of the
#' delimiter (I discovered this the hard way).

clim_dat <- separate_wider_delim(
   clim_dat, 
   geoname, 
   ", ",
   names = c("County", "State"),
   too_few = "debug",
   cols_remove = TRUE)

#' 5) Use the `class()` function on `clim_dat` and `us_counties` to discover what 
#' types of data are contained in these two objects and confirm that `clim_dat` 
#' is a spatial data frame. 

class(clim_dat)
  # clim_dat is not a spatial dataframe, nor was it to begin with
  # is there a way to convert it?
class(us_counties)

#' 6) Join the `clim_dat` and `us_counties` dataset to generate a spatial (sf) dataframe called
#' `clim_counties`. 
#' 
#' **Question: which counties were you unable to match to the Yale data?**
#' 
#' Answer! 

names(clim_dat)
names(us_counties)
names(us_counties)[names(us_counties) == "NAMELSAD"] <- "County"
names(us_counties)[names(us_counties) == "NAME.y"] <- "State"
names(us_counties)
clim_counties <- full_join(us_counties, clim_dat, by = c("County", "State"))
class(clim_counties)
clim_counties <- subset(clim_counties, select = c("County", "State", "geoname", "count", "harmus", "devharm"))
class(clim_counties)
names(clim_counties)

#' 7) Map the estimated percentage in a county that believe climate change will 
#' harm people in developing countries for US counties in the **contiguous US** using a 
#' [colorblind-friendly color palette](https://knightlab.northwestern.edu/2016/07/18/three-tools-to-help-you-make-colorblind-friendly-graphics/) diverging color scale centered at the mean for the country. Define the
#' fill for counties without a `devharm` estimate as "grey80" (lighter than the default).
#' Save it as a pdf to your output folder named "ps1_fig1_lastname.pdf" 
#' (where you replace lastname with your actual last name). Note that the lat/long
#' boundaries of the contiguous US are provided on this 
#' [Quora blog](https://www.quora.com/What-is-the-longitude-and-latitude-of-a-bounding-box-around-the-continental-United-States).
#' 
#' **Question: interpret this map.**
#' 
#' Answer! 

st_bbox(clim_counties)

clim_crop <- st_crop(
  clim_counties,
  xmin = -125, ymin = 20,
  xmax = -66.94, ymax = 49.39
)
st_bbox(clim_crop)

q7 <- ggplot() +
  geom_sf(data = clim_crop, color = "transparent", 
          aes(fill = devharm)) +
  scale_fill_continuous(
    "% county pop.",
    low = "cyan4", high = "orange", na.value="grey80") +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "% pop. by county believing climate change will harm developing countries"
  ) +
  theme_bw()
q7

ggsave(
  filename = here("output", "ps1_fig1_mcgreal.pdf"),
  plot = q7
)

#' 8) Now create a map showing the divergence between beliefs that climate change will
#' harm people in developing countries and beliefs that climate change will harm
#' people in the US. Use a colorblind-friendly diverging color scale centered around zero. 
#' Again, define the fill for counties without data metric as "grey80" 
#' (lighter than the default). 
#' 
#' **Question: interpret this map.**
#' 
#' Answer!

clim_crop$harmdiff <- clim_crop$devharm - clim_crop$harmus

ggplot() +
  geom_sf(data = clim_crop, color = "transparent", 
          aes(fill = harmdiff)) +
  scale_fill_continuous(
    "% county pop.",
    low = "blue", high = "gold", na.value="grey80") +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Divergence in belief climate change will harm developing countries v. harm US"
  ) +
  theme_bw()

#' 9) Aggregate the county-level data to the state level and calculate the fraction
#' of people in the state that believe climate change will harm people in developing
#' countries. Plot these data and name the figure "ps1_fig3_lastname.pdf" 
#' (where you replace lastname with your actual last name).

clim_crop$devharm_count = (clim_crop$devharm/100) * clim_crop$count

clim_cropSTATE <- clim_crop %>% 
  group_by(State) %>% 
  summarize(
    count = sum(count, na.rm = TRUE),
    devharm_count = sum(devharm_count, na.rm = TRUE)
  )

clim_cropSTATE$devharm <- clim_cropSTATE$devharm_count / clim_cropSTATE$count

q9 <- ggplot() +
  geom_sf(data = clim_cropSTATE,
          aes(fill = devharm)) +
  scale_fill_continuous(
    "% state pop.",
    low = "red", high = "yellow", na.value="grey80") +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "% pop. by state believing climate change will harm developing countries"
  ) +
  theme_bw()
q9

ggsave(
  filename = here("output", "ps1_fig3_mcgreal.pdf"),
  plot = q9
)

