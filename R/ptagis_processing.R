library(tidyverse)
library(sf)
library(leaflet)
library(arrow)
library(plotly)
library(dataRetrieval)
library(patchwork)

# want to update to do this by code, but for 
# now the first step is to query detections
# within the South Fork Clearwater during
# the spawn year of interest and preceding year,
# and save those results in the detections folder
# of this project. This code will filter
# to get only the most recent spawn year available
# and will drop any detections of fish tagged
# as juveniles that are detected in the 
# same year as tagging

# once that part is done, read in the output here

dat <- read_csv("data/SF Clearwater STHD.csv") %>% 
  mutate(observation_sitecode=word(`Site Name`,1,sep=" "),
         release_sitecode=word(`Release Site Name`,1,sep=" "),
         observation_datetime=as.POSIXct(`Obs Time Value`,
                                         format = "%m/%d/%Y %I:%M:%S %p", 
                                         tz = "America/Los_Angeles"),
         observation_month=month(observation_datetime),
         observation_year=year(observation_datetime),
         spawn_year=ifelse(observation_month>6,(observation_year+1),
                           observation_year),
         release_datetime=mdy(`Release Date MMDDYYYY`),
         release_year=year(release_datetime),
         yrs_at_large=observation_year-release_year) %>% 
  select(pit_id=`Tag Code`,rear_type=`Rear Type Code`,
         release_sitecode,release_lifestage=`Mark Life Stage Value`,
         release_datetime,release_year,
         observation_sitecode,
         observation_datetime,observation_month,
         observation_year,
         yrs_at_large,
         spawn_year,
         length_mm=`Mark Length mm`) %>% 
  mutate(most_recent=max(spawn_year,na.rm=T)) %>% 
  filter(spawn_year==most_recent) %>% 
  filter(yrs_at_large!=0 | release_lifestage != "Juvenile")
