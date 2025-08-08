
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


dat <- read_csv("data/YFK All_Comparisons.csv") |>  
  mutate(observation_sitecode=word(`Site Name`,1,sep=" "),
         release_sitecode=word(`Release Site Name`,1,sep=" "),
         observation_datetime=as.POSIXct(`Obs Time Value`,
                                         format = "%m/%d/%Y %I:%M:%S %p", 
                                         tz = "America/Los_Angeles"),
         observation_month=month(observation_datetime),
         observation_year=year(observation_datetime),
         spawn_year=case_when(
           yday(observation_datetime)>=183 & `Species Name`=="Steelhead" ~ observation_year+1,
           TRUE ~ observation_year
         ),
         release_datetime=mdy(`Release Date MMDDYYYY`),
         release_year=year(release_datetime),
         yrs_at_large=observation_year-release_year) |> 
  select(pit_id=`Tag Code`,species=`Species Name`,
         rear_type=`Rear Type Code`,
         run_type=`Run Name`,
         release_sitecode,release_lifestage=`Mark Life Stage Value`,
         release_datetime,release_year,
         observation_sitecode,
         observation_datetime,observation_month,
         observation_year,yrs_at_large,
         spawn_year,
         length_mm=`Mark Length mm`) |> 

# some steelhead have NA for life stage; for those if length > 300 mm
  # go ahead and treat as adult marked, otherwise Juvenile which
  # means they'll be searched in a later query for downstream
  # detections, and if not found in that they'll be dropped
  # from the run timing analysis
  
  mutate(release_lifestage=case_when(
           
           is.na(release_lifestage) & length_mm <= 300 &
             species %in% c("Steelhead","Chinook") ~ "Juvenile",
           is.na(release_lifestage) & length_mm > 300 &
             species %in% c("Steelhead","Chinook") ~ "Adult",
           is.na(release_lifestage) & is.na(length_mm)&
             species %in% c("Chinook","Steelhead")& release_sitecode %in% c("COLR3","LGRLDR")
           ~ "Adult",
           is.na(release_lifestage) & is.na(length_mm)&
             species %in% c("Chinook","Steelhead")& !release_sitecode %in% c("COLR3","LGRLDR")
           ~ "Juvenile",
           is.na(release_lifestage) & species %in% c("Bull Trout") &
             length_mm >= 300 ~ "Adult",
           is.na(release_lifestage) & species %in% c("Bull Trout") &
             length_mm < 300 ~ "Juvenile",
           TRUE ~ release_lifestage
           
         )) |> 
  filter(!(yrs_at_large==0 & release_lifestage=="Juvenile"))

  
 
  
# bring in detection data from downstream, basically
# all the Snake and Columbia dam infrastructure and find the last
# detection they had down there; this is split into 2 bc it
# was exceeding the PTAGIS row limit

yfk_downstream.filter1 <- read_csv("data/YFK All Downstream Comparisons_15-25.csv") |> 
  mutate(ds_datetime=as.POSIXct(`Obs Time Value`,
                             format = "%m/%d/%Y %I:%M:%S %p", 
                             tz = "America/Los_Angeles"),
         ds_site=word(`Site Name`,1,sep=" ")) |> 
  select(pit_id=`Tag Code`,
         ds_datetime,ds_site) |> 
  group_by(pit_id) |> 
  slice(which.max(ds_datetime))



yfk_downstream.filter2 <- read_csv("data/YFK All Downstream_06-14.csv") |> 
  mutate(ds_datetime=as.POSIXct(`Obs Time Value`,
                                format = "%m/%d/%Y %I:%M:%S %p", 
                                tz = "America/Los_Angeles"),
         ds_site=word(`Site Name`,1,sep=" ")) |> 
  select(pit_id=`Tag Code`,
         ds_datetime,ds_site) |> 
  group_by(pit_id) |> 
  slice(which.max(ds_datetime))

yfk_downstream.filter <- bind_rows(yfk_downstream.filter1,
                                   yfk_downstream.filter2)


# pull out detection at YFK that were marked as juveniles
#  at YFK, SAWT, PAHSIW, YANKWF, or SALR4, join to downstream
# detections and drop any that don't 
# appear in the downstream detections query; this is to make sure 
# we're tracking migratory fish as those detected further down
# in the hydrosystem were much more likey to have spent time
# in the ocean, and detections are very high  in the adult ladders
# so if they did that there should be a high probability they
# show up in this query; also, need to see if they showed up downstream
# and be able to filter to detections at YFK after they showed
# up down there, because don't want to be considering detections from
# before they emigrated

yfk_juvenile.filter <- dat |> 
  filter(release_lifestage=="Juvenile",
         release_sitecode %in% c("YANKFK","SAWT",
                                 "PAHSIW","YANKWF",
                                 "SALR4")) |> 
  group_by(pit_id) |> 
  slice(which.max(observation_datetime)) |> 
  inner_join(yfk_downstream.filter,by="pit_id") |> 
  filter(observation_datetime>ds_datetime) |> 
  ungroup() |> 
  select(-c(ds_datetime,ds_site))

# because of the way that i set up downstream
# detection query, this will not find the downstream
# history of those marked anywhere except YANKF; to do
# so would require separate queries because there's a 
# lot of fish released from these various sites; there
# was a small enough number of according records that i 
# just looked manually to see which should be retained

manual_keep <- c("3D9.1BF26C0876","3DD.003BE21D8A",
                 "3DD.0077A773FD","3DD.0077B47D49",
                 "3DD.0077E4130B")


# filter out those marked as adults or marked as juveniles
# outside the Upper Salmon from original, or the ones
# i said to manually keep,
# then bind with the filtered detections of those
# marked as juvenile

summary.dat <- dat |> 
  filter(release_lifestage=="Adult"|
           (release_lifestage=="Juvenile"&
              !release_sitecode %in% c("YANKFK","SAWT",
                                       "PAHSIW","YANKWF",
                                       "SALR4"))|
         pit_id %in% manual_keep) |> 
  bind_rows(yfk_juvenile.filter)




# now summarize relevant values and pull in marking locations as well

dat.mark <- summary.dat |> 
  group_by(pit_id) |> 
  summarize(release_sitecode=first(release_sitecode),
            release_datetime=first(release_datetime))

# some of the bull trout didn't have a release date or loactation but 
# were definitely YFK and the release date would be same as mark
# date so making that correction here



yfk_logical <- c("YFK","YANKFK")

yfkentry_logical <- c("YFK")

# one thing to consider is i think some of these are
# still likely resident fish, i.e. marked as juv 
# and only 1 yr at large, also found one that seems 
# to be an error that says life stage was adult but
# length was 150, so going to drop any here that claim
# life stage is adult and length is less than 450, which
# should be a pretty conservative filter 

yfkindividuals_completedyrs <- summary.dat |> 
  group_by(pit_id,species,spawn_year) |>
  summarize(yfk_first=first(observation_datetime),
            yfk_entry_final=last(observation_datetime),
            yfk_diff=as.numeric(yfk_entry_final-yfk_first,units="days"),
            length_mm=mean(length_mm,na.rm=T),
            release_lifestage=first(release_lifestage)) |> 
  left_join(dat.mark,by="pit_id") 

# some of the bull trout didn't have a release date or loactation but 
# were definitely YFK and the release date would be same as mark
# date so making that correction here

blt.append <- yfkindividuals_completedyrs |> 
  filter(is.na(release_datetime),
         species=="Bull Trout")

blt_correction <-  read_csv("data/YFK All_Comparisons.csv") |>
  mutate(release_sitecode=word(`Release Site Name`,1,sep=" "),
         observation_datetime=as.POSIXct(`Obs Time Value`,
                                         format = "%m/%d/%Y %I:%M:%S %p", 
                                         tz = "America/Los_Angeles"),
         mark_date=mdy(`Mark Date MMDDYYYY`),
         species=`Species Name`,
         pit_id=`Tag Code`) |> 
  filter(pit_id %in% blt.append$pit_id) |> 
  group_by(pit_id) |> 
  summarize(release_datetime_correction=first(mark_date)) |> 
  mutate(release_sitecode_correction="YANKFK")

yfkindividuals_completedyrs <- yfkindividuals_completedyrs |> 
  left_join(blt_correction,by="pit_id") |> 
  mutate(release_datetime=case_when(
    is.na(release_datetime) ~ release_datetime_correction,
    TRUE ~ release_datetime
  ),
  release_sitecode=case_when(
    release_sitecode=="-" ~ release_sitecode_correction,
    TRUE ~ release_sitecode
  )) |> 
  select(-c(release_sitecode_correction,
            release_datetime_correction)) |> 
  filter(species %in% c("Steelhead","Chinook",
                        "Bull Trout"))

# now summarize by day so we can reference how much of 
# the run should be complete on a given day

# think about when running this which spawn years to 
# drop...i.e. those that are in progress, by species
# that will need to be updated in the final filter here
# depending on when this is being run

yfk_entry_daily <- yfkindividuals_completedyrs |> 
  mutate(yfk_entry_date=as_date(yfk_first)) |> 
  group_by(spawn_year,species) |> 
  mutate(sy_total=n()) |> 
  ungroup() |> 
  group_by(species,yfk_entry_date) |> 
  summarize(spawn_year=first(spawn_year),
            n=n(),
            sy_total=first(sy_total)) |>
  group_by(spawn_year,species) |> 
  mutate(daily_running_total=cumsum(n),
         daily_prop=n/sy_total,
         daily_cumulative=daily_running_total/sy_total) |> 
  filter(!(species %in% c("Bull Trout","Chinook")&
             spawn_year==2025))

# that's the data frame that will get used in constructing
# descriptions of where current year is relative to the
# previous runs


saveRDS(yfk_entry_daily,
        "data/daily_completed")
