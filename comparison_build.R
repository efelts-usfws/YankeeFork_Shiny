
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
  left_join(yfk_downstream.filter,by="pit_id") |> 
  filter(!is.na(ds_datetime)) |> 
  group_by(pit_id) |> 
  filter(observation_datetime>ds_datetime) |> 
  ungroup() |> 
  select(-c(ds_datetime,ds_site))

# because of the way that i set up downstream
# detection query, this will not find the downstream
# history of those marked anywhere except YANKWF; to do
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
  group_by(pit_id,spawn_year) |>
  summarize(yfk_first=first(observation_datetime),
            yfk_entry_final=last(observation_datetime),
            yfk_diff=as.numeric(yfk_entry_final-yfk_first,units="days"),
            length_mm=mean(length_mm,na.rm=T),
            release_lifestage=first(release_lifestage)) |> 
  left_join(dat.mark,by="pit_id") |> 
  mutate(species="Steelhead")




# now summarize by day so we can reference how much of 
# the run should be complete on a given day

yfk_entry_daily <- yfkindividuals_completedyrs |> 
  mutate(yfk_final_date=as_date(yfk_entry_final)) |> 
  group_by(spawn_year) |> 
  mutate(sy_total=n()) |> 
  ungroup() |> 
  group_by(yfk_final_date) |> 
  summarize(spawn_year=first(spawn_year),
            n=n(),
            sy_total=first(sy_total)) |>
  group_by(spawn_year) |> 
  mutate(daily_running_total=cumsum(n),
         daily_prop=n/sy_total,
         daily_cumulative=daily_running_total/sy_total)

# that's the data frame that will get used in constructing
# descriptions of where current year is relative to the
# previous runs

saveRDS(yfk_entry_daily,
        "data/daily_completed")


# add in Chinook

chn.dat <- read_csv("data/YFK CHN_Comparisons.csv") |>  
  mutate(observation_sitecode=word(`Site Name`,1,sep=" "),
         release_sitecode=word(`Release Site Name`,1,sep=" "),
         observation_datetime=as.POSIXct(`Obs Time Value`,
                                         format = "%m/%d/%Y %I:%M:%S %p", 
                                         tz = "America/Los_Angeles"),
         observation_month=month(observation_datetime),
         observation_year=year(observation_datetime),
         spawn_year=observation_year,
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
  mutate(most_recent=max(spawn_year,na.rm=T),
         release_lifestage=case_when(
           
           is.na(release_lifestage) & is.na(length_mm) ~ "Juvenile",
           is.na(release_lifestage) & length_mm <= 300 ~ "Juvenile",
           is.na(release_lifestage) & length_mm > 300 ~ "Adult",
           TRUE ~ release_lifestage
           
         )) |> 
  filter(!(yrs_at_large==0 & release_lifestage=="Juvenile"),
         !(yrs_at_large==1 & release_sitecode%in% c("YANKFK","YANKWF")))


chn_yfk_downstream.filter <- read_csv("data/YFK Chinook Downstream Comparisons.csv") |> 
  mutate(ds_datetime=as.POSIXct(`Obs Time Value`,
                                format = "%m/%d/%Y %I:%M:%S %p", 
                                tz = "America/Los_Angeles"),
         ds_site=word(`Site Name`,1,sep=" ")) |> 
  select(pit_id=`Tag Code`,
         ds_datetime,ds_site) |> 
  group_by(pit_id) |> 
  slice(which.max(ds_datetime))

chn_yfk_juvenile.filter <- chn.dat |> 
  filter(release_lifestage=="Juvenile") |> 
  left_join(chn_yfk_downstream.filter,by="pit_id") |> 
  filter(!is.na(ds_datetime)) |> 
  group_by(pit_id) |> 
  filter(observation_datetime>ds_datetime) |> 
  ungroup() |> 
  select(-c(ds_datetime,ds_site))

chn_summary.dat <- chn.dat |> 
  filter(release_lifestage=="Adult"|
           (release_lifestage=="Juvenile"&
              !release_sitecode %in% c("YANKFK","YANKWF"))) |> 
  bind_rows(chn_yfk_juvenile.filter)

chn_dat.mark <- chn_summary.dat |> 
  group_by(pit_id) |> 
  summarize(release_sitecode=first(release_sitecode),
            release_datetime=first(release_datetime))
  

chn_yfkindividuals_completedyrs <- chn_summary.dat |> 
  group_by(pit_id,spawn_year) |>
  summarize(yfk_first=first(observation_datetime),
            yfk_entry_final=last(observation_datetime),
            yfk_diff=as.numeric(yfk_entry_final-yfk_first,units="days"),
            length_mm=mean(length_mm,na.rm=T),
            release_lifestage=first(release_lifestage)) |> 
  left_join(chn_dat.mark,by="pit_id") |> 
  mutate(species="Chinook")

# individuals completed will be used in Shiny, 
# so want species and then bind and save 
# as an RDS

# that's the data frame that will be used to plot so save here


yfkindividuals_completedyrs_bind <- bind_rows(yfkindividuals_completedyrs,
                                              chn_yfkindividuals_completedyrs)

saveRDS(yfkindividuals_completedyrs_bind,
        "data/individuals_completed_bind")
