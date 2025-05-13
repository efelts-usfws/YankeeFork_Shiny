
# script to build data from previous years and use for
# comparison with current run year

library(stringr)
library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(plotly)
library(lubridate)
library(dataRetrieval)

# Read in all detections on the Yankee Fork array,
# drop any that were tagged as juveniles and have
# been at large for less than a year, also there
# were a few classified as resident rainbows that
# I'll drop out here as well


dat <- read_csv("data/YFK STHD_Comparisons.csv") |>  
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
         yrs_at_large=observation_year-release_year) |> 
  select(pit_id=`Tag Code`,rear_type=`Rear Type Code`,
         run_type=`Run Name`,
         release_sitecode,release_lifestage=`Mark Life Stage Value`,
         release_datetime,release_year,
         observation_sitecode,
         observation_datetime,observation_month,
         observation_year,yrs_at_large,
         spawn_year,
         length_mm=`Mark Length mm`) |> 

# some have NA for life stage; for those if length > 300 mm
  # go ahead and treat as adult marked, otherwise Juvenile which
  # means they'll be searched in a later query for downstream
  # detections, and if not found in that they'll be dropped
  # from the run timing analysis
  
  mutate(most_recent=max(spawn_year,na.rm=T),
         release_lifestage=case_when(
           
           is.na(release_lifestage) & is.na(length_mm) ~ "Juvenile",
           is.na(release_lifestage) & length_mm <= 300 ~ "Juvenile",
           is.na(release_lifestage) & length_mm > 300 ~ "Adult",
           TRUE ~ release_lifestage
           
         )) |> 
  filter(!spawn_year==most_recent,
         !run_type=="Resident") |> 
  filter(yrs_at_large!=0 | release_lifestage != "Juvenile")

# bring in detection data from downstream, basically
# all the Snake and Columbia dam infrastructure and find the last
# detection they had down there

yfk_downstream.filter <- read_csv("data/YFK Steelhead Downstream Comparisons.csv") |> 
  mutate(ds_datetime=as.POSIXct(`Obs Time Value`,
                             format = "%m/%d/%Y %I:%M:%S %p", 
                             tz = "America/Los_Angeles"),
         ds_site=word(`Site Name`,1,sep=" ")) |> 
  select(pit_id=`Tag Code`,
         ds_datetime,ds_site) |> 
  group_by(pit_id) |> 
  slice(which.max(ds_datetime))

# pull out detection at YFK that were marked as juveniles
#  at YFK, SAWT, PAHSIW, YANKWF, or SALR4 and find any that don't 
# appear in the downstream detections query; this is to make sure 
# we're tracking migratory fish as those detected further down
# in the hydrostysem were much more likey to have spent time
# in the ocean, and detections are very high  in the adult ladders
# so if they did that there should be a high probability they
# show up in this query; also, need to see if they showed up downstream
# and that they had detections at YFK after they showed
# up down there to consider them as part of these summaries



yfk_juvenile.filter <- dat |> 
  filter(release_lifestage=="Juvenile",
         release_sitecode %in% c("YANKFK","SAWT",
                                 "PAHSIW","YANKWF",
                                 "SALR4")) |> 
  distinct(pit_id) |> 
  filter(!pit_id %in% yfk_downstream.filter$pit_id)

# for the juveniles, 

yfk_juvenile.filter2 <- dat |> 
  filter(release_lifestage=="Juvenile",
         release_sitecode %in% c("YANKFK","SAWT",
                                 "PAHSIW","YANKWF",
                                 "SALR4")) |> 
  left_join(yfk_downstream.filter,by="pit_id") |> 
  filter(!is.na(ds_datetime)) |> 
  distinct(pit_id)



# now summarize relevant values and pull in marking locations as well

dat.mark <- dat |> 
  filter(!pit_id %in% yfk_juvenile.filter$pit_id) |> 
  group_by(pit_id) |> 
  summarize(release_sitecode=first(release_sitecode),
            release_datetime=first(release_datetime))

yfk_logical <- c("YFK","YANKFK")

yfkentry_logical <- c("YFK")

# one thing to consider is i think some of these are
# still likely resident fish, i.e. marked as juv 
# and only 1 yr at large, also found one that seems 
# to be an error that says life stage was adult but
# length was 150, so going to drop any here that claim
# life stage is adult and length is less than 450, which
# should be a pretty conservative filter 

yfkreturns_completedyrs <- dat |> 
  filter(!pit_id %in% yfk_juvenile.filter$pit_id) |> 
  group_by(pit_id) |>
  summarize(spawn_year=first(spawn_year),
            yfk_entry_first=first(observation_datetime),
            yfk_entry_last=last(observation_datetime),
            length_mm=mean(length_mm,na.rm=T),
            release_lifestage=first(release_lifestage),
            yrs_at_large=first(yrs_at_large))

