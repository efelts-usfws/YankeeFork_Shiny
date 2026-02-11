library(vroom) 
library(dataRetrieval)
library(conflicted)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)

conflicts_prefer(vroom::locale,
                 dplyr::filter)

# set the timeout above default of 60 seconds
# because sometimes the API calls are slow

options(timeout=300)

# need to create a data directory in the processing
# code so it gets stored in github actions

dir.create("data", recursive=TRUE, showWarnings=FALSE)



# want to update to do this by code, but for 
# now the first step is to query detections
# within the Yankee Fork during
# the spawn year of interest and preceding year,
# and save those results in the detections folder
# of this project. This code will filter
# to get only the most recent spawn year available
# and will drop any detections of fish tagged
# as juveniles that are detected in the 
# same year as tagging

# assign spawn year based on species and the date
# the code runs

julian_today <- yday(today())


# read in data from web API where scheduled
# query of the PIT tag array is stored

use_detections.dat <- vroom(file = "https://api.ptagis.org/reporting/reports/efelts60/file/USE%20All.csv",
                            delim = ",",
                            locale = locale(encoding= "UTF-16LE")) %>% 
  mutate(observation_sitecode=word(`Site`,1,sep=" "),
         release_sitecode=word(`Release Site`,1,sep=" "),
         observation_datetime=as.POSIXct(`Obs Time`,
                                         format = "%m/%d/%Y %I:%M:%S %p", 
                                         tz = "America/Los_Angeles"),
         observation_month=month(observation_datetime),
         observation_year=year(observation_datetime),
         spawn_year=ifelse(observation_month>6,(observation_year+1),
                           observation_year),
         release_datetime=mdy(`Release Date`),
         release_year=year(release_datetime),
         yrs_at_large=observation_year-release_year,
         species=`Species Name`) %>% 
  select(pit_id=Tag,rear_type=`Rear Type Code`,
         release_sitecode,release_lifestage=`Mark Life Stage`,
         release_datetime,release_year,observation_sitecode,
         observation_datetime,observation_month,
         observation_year,yrs_at_large,spawn_year,
         species,
         length_mm=`Mark Length`) |> 
  mutate(spawn_year=case_when(
    yday(observation_datetime)>=183 & species=="Steelhead" ~ observation_year+1,
    TRUE ~ observation_year
  )) #|> 
  group_by(species) |> 
  mutate(most_recent=max(spawn_year,na.rm=T)) |> 
  filter(spawn_year==year(today()))

# where were juveniles marked?

juv_mark.sum <- use_detections.dat |> 
  group_by(species,release_sitecode,release_lifestage,yrs_at_large,spawn_year) |> 
  tally() |> 
  filter(release_lifestage=="Juvenile")

# bring in query from API that searches for USE
# fish detected downstream in the hydrosystem and
# get their latest detection by PIT id

others_downstream_detections.dat <-  vroom(file = "https://api.ptagis.org/reporting/reports/efelts60/file/USE%20All%20Downstream.csv",
                                    delim = ",",
                                    locale = locale(encoding= "UTF-16LE")) |>  
  mutate(observation_sitecode=word(`Site`,1,sep=" "),
         release_sitecode=word(`Release Site`,1,sep=" "),
         observation_datetime=as.POSIXct(`Obs Time`,
                                         format = "%m/%d/%Y %I:%M:%S %p", 
                                         tz = "America/Los_Angeles")) |> 
  group_by(Tag) |> 
  slice(which.max(observation_datetime)) |> 
  select(pit_id=Tag,
         latest_downstream=observation_datetime)

yfk_downstream_detections.dat <-  vroom(file = "https://api.ptagis.org/reporting/reports/efelts60/file/YFK%20All%20Downstream.csv",
                                           delim = ",",
                                           locale = locale(encoding= "UTF-16LE")) |>  
  mutate(observation_sitecode=word(`Site`,1,sep=" "),
         release_sitecode=word(`Release Site`,1,sep=" "),
         observation_datetime=as.POSIXct(`Obs Time`,
                                         format = "%m/%d/%Y %I:%M:%S %p", 
                                         tz = "America/Los_Angeles")) |> 
  group_by(Tag) |> 
  slice(which.max(observation_datetime)) |> 
  select(pit_id=Tag,
         latest_downstream=observation_datetime)

bind_downstream_detections.dat <-   bind_rows(others_downstream_detections.dat,
            yfk_downstream_detections.dat)


# pull out detections at USE that were marked as
# juveniles and drop
# any that don't appear in the downstream detections 
# prior to their latest detection at the USE array

#############
## in addition to the downstream search,
# there's other locations where these juveniles
# could've been marked that we already assume
# they were anadromous, such as lower granite
# dam; right now those are getting dropped and
# they shouldn't be

use_juvenile.filter <- use_detections.dat %>% 
  filter(release_lifestage=="Juvenile") |> 
  group_by(pit_id) %>% 
  slice(which.max(observation_datetime)) |> 
  inner_join(bind_downstream_detections.dat,by="pit_id") |> 
  filter(latest_downstream<observation_datetime)

# now summarize relevant values and pull
# in marking location as well

use_dat.mark <- use_detections.dat  |>  
  group_by(pit_id)  |> 
  summarize(species=first(species),release_sitecode=first(release_sitecode),
            release_datetime=first(release_datetime)) 


use_individuals.summary <- use_detections.dat |> 
  filter(pit_id %in% use_juvenile.filter$pit_id|
           release_lifestage=="Adult"|
           release_sitecode %in% c("LGRRBR","LGRRRR",
                                   "SALTRP","SNKTRP")) |>  
  group_by(pit_id)  |>  
  summarize(use_first=first(observation_datetime),
           use_final=last(observation_datetime),
           use_diff=as.numeric(use_final-use_first,units="days"),
            length_mm=mean(length_mm,na.rm=T),
            release_lifestage=first(release_lifestage)) |>  
  left_join(use_dat.mark,by="pit_id") 
