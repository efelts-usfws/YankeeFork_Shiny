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



ptagis.dat <- readRDS("data/ptagis_sites")



yfk_sites <- c("YFK","CEY","YANKWF","YANKFK",
               "EIGH3C")


yfk.dat <- ptagis.dat |>  
  filter(site_code %in% yfk_sites)

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

yfk_detections.dat <- vroom(file = "https://api.ptagis.org/reporting/reports/efelts60/file/YFK%20All.csv",
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
  )) |> 
  group_by(species) |> 
  mutate(most_recent=max(spawn_year,na.rm=T)) |> 
  filter(spawn_year==year(today()))


# where were juveniles marked?
  
juv_mark.sum <- yfk_detections.dat |> 
    group_by(species,release_sitecode,
             release_year,release_lifestage) |> 
    tally()
  
# bring in query from API that searches for YFK
# fish detected downstream in the hydrosystem and
  # get their latest detection by PIT id

downstream_detections.dat <-  vroom(file = "https://api.ptagis.org/reporting/reports/efelts60/file/YFK%20All%20Downstream.csv",
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


  
# # grab all distinct pit ids from downstream
# 
# yfk_downstream.filter <- downstream_detections.dat |> 
#   distinct(pit_id)


# pull out detections at YFK that were marked as
# juveniles and drop
# any that don't appear in the downstream detections 
# prior to their latest detection at the YFK array

yfk_juvenile.filter <- yfk_detections.dat %>% 
  filter(release_lifestage=="Juvenile") |> 
 group_by(pit_id) %>% 
 slice(which.max(observation_datetime)) |> 
 inner_join(downstream_detections.dat,by="pit_id") |> 
  filter(latest_downstream<observation_datetime)



# now summarize relevant values and pull
# in marking location as well

dat.mark <- yfk_detections.dat  |>  
  group_by(pit_id)  |> 
  summarize(species=first(species),release_sitecode=first(release_sitecode),
            release_datetime=first(release_datetime)) 

# took the granite part out, could definitely add
# it back, probably will once I work the other stuff out,
# would likely just add an additional PTAGIS API to
# get all mark lifestage detections at LGR adult ladder,
# or marking at LGR so can get the associated
# passage dates

yfk_logical <- c("YFK","YANKFK")

yfkentry_logical <- c("YFK")

yfk_individuals.summary <- yfk_detections.dat |> 
  filter(pit_id %in% yfk_juvenile.filter$pit_id|
           release_lifestage=="Adult") |>  
  mutate(yfk=ifelse(observation_sitecode %in% yfk_logical,TRUE,
                   FALSE),
         yfk_entry=ifelse(observation_sitecode %in% yfkentry_logical,
                         TRUE,FALSE)) %>% 
  group_by(pit_id)  |>  
  summarize(yfk_first=first(observation_datetime[yfk==TRUE]),
            yfk_entry_final=last(observation_datetime[yfk_entry==TRUE]),
            yfk_diff=as.numeric(yfk_entry_final-yfk_first,units="days"),
            length_mm=mean(length_mm,na.rm=T),
            release_lifestage=first(release_lifestage)) |>  
  left_join(dat.mark,by="pit_id") 

if(nrow(yfk_individuals.summary)>0){
  

yfk_entry.summary <- yfk_individuals.summary |>  
  mutate(yfk_entry_date=as_date(yfk_first))  |>  
  group_by(yfk_entry_date,species)  |>  
  summarise(n=n()) |>  
  group_by(species) |>  
  mutate(sy_total=sum(n),
         cumulative_total=cumsum(n),
         daily_prop=n/sy_total,
         daily_cumulative=cumsum(daily_prop)) 
}else{
yfk_entry.summary <-  tibble(yfk_entry_date=as_date(today()),
                             species=c("Bull Trout","Chinook","Steelhead"),
                             n=0,
                             sy_total=0,
                             cumulative_total=0,
                             daily_prop=0,
                             daily_cumulative=0) 

}

# get numbers by location as well

yfk_location.summary <- yfk_detections.dat |> 
  group_by(pit_id,species)  |>  
  summarize(release_sitecode=first(release_sitecode),
            release_lifestage=first(release_lifestage))  |>  
  left_join(ptagis.dat,by=c("release_sitecode"="site_code"))


# now also grab yfk water data from USGS gaging station

yfk.site <- "13296000"


# temp is coded as 00010, discharge as 00060,
# we'll go for both of those

parm.cd <- c("00010","00060")


# put today's date into text format
# to feed into the query of daily
# water data

today.text <- as.character(today(tz="America/Los_Angeles"))

yfk.daily <- readNWISdv(siteNumber=yfk.site,
                           parameterCd = parm.cd,
                           startDate="1990-01-01",
                           endDate=today.text) |> 
  select(date=Date,
         mean_temp=X_00010_00003,
         mean_discharge=X_00060_00003) |> 
  mutate(date=as_date(date))

yfk.dat <-yfk.daily |> 
  filter(date>=as_date("2026-01-01"),
         date<=today()) |>  
  mutate(group=1)



saveRDS(yfk.dat,"data/yfk_flow")

# for individuals bind together species

# save species separately

saveRDS(yfk_individuals.summary,
        "data/individuals")
# 
# saveRDS(chn_yfk_individuals.summary,
#               "data/individuals_chn")

# save entr summaries separately

# bind summaries together for species


saveRDS(yfk_entry.summary,
              "data/daily")
# 
# saveRDS(chn_yfk_entry.summary,
#         "data/daily_chn")


# right now the complete range won't update 
# programatically as new spawn years are added;
# I don't think it's a big deal but just need
# to remember when appending a new one

sy_current <- tibble(species=c("Steelhead","Chinook",
                               "Bull Trout")) |> 
  mutate(today.year=year(today()),
         today.jday=yday(today()),
         today_spawn_year=case_when(
           species == "Steelhead"& today.jday>=183 ~ today.year+1,
           TRUE ~ today.year))

# helper df for limits on max for dummy 

species_max_dates <- tibble(
  species=c("Bull Trout","Chinook","Steelhead"),
  max_date=as.Date(c("1976-12-31","1976-12-31",
                     "1976-12-31"))
)

# need to set minimum dates also so that each observed year 
# spans all the potential dates, i.e. earliest
# that fish have been observed

species_min_dates <- readRDS("data/daily_completed") |> 
  mutate(day_of_year=yday(yfk_entry_date),
         dummy_date=case_when(
           species=="Steelhead"&day_of_year<183 ~ as.Date(day_of_year,origin="1976-01-01"),
           TRUE ~ as.Date(day_of_year-1,origin="1976-01-01")
         )) |> 
  group_by(species) |> 
  summarize(min_dummy=min(dummy_date))

complete_daily <- readRDS("data/daily_completed")|>
  left_join(sy_current,by="species") |> 
  filter(spawn_year<today_spawn_year)|> 
  ungroup() |> 
  mutate(day_of_year=yday(yfk_entry_date),
         dummy_date=case_when(
           species=="Steelhead"&day_of_year<183 ~ as.Date(day_of_year,origin="1976-01-01"),
           TRUE ~ as.Date(day_of_year-1,origin="1976-01-01")
         )) |> 
  left_join(species_max_dates,by="species") |> 
  left_join(species_min_dates,by="species") |> 
   group_by(species,spawn_year) |> 
  # mutate(min_date=min(dummy_date,na.rm=T))# |> 
  complete(dummy_date=seq(min(min_dummy), max(max_date), by="day")) |> 
  ungroup() |> 
  select(-c(min_dummy,max_date)) |> 
  mutate(across(n,~replace_na(.x,0))) |> 
  mutate(across(daily_prop,~replace_na(.x,0)))|> 
  group_by(species,spawn_year) |> 
  fill(c("daily_running_total","sy_total",
         "daily_cumulative"),.direction="down")
  
  # complete(yfk_entry_date=seq(as_date("2012-07-01"),as_date("2024-06-30"),
  #                             by="day")) |> 
  # mutate(obs_year=year(yfk_entry_date),
  #        obs_month=month(yfk_entry_date),
  #        spawn_year=ifelse(obs_month>6,(obs_year+1),
  #                                     obs_year)) |> 
  # group_by(spawn_year,species) |> 
  # fill(sy_total,.direction="updown") |> 
  # mutate(n=ifelse(is.na(n),0,n),
  #        daily_running_total=cumsum(n),
  #        daily_prop=n/sy_total,
  #        daily_cumulative=cumsum(daily_prop),
  #        daily_percent=daily_cumulative*100) %>% 
  # mutate(day_of_year=yday(yfk_entry_date),
  #        dummy_entry_date=if_else(day_of_year<182,
  #                                   as.Date(day_of_year,origin="1977-12-31"),
  #                                   as.Date(day_of_year,origin="1976-12-31"))) |> 
  # filter(!day_of_year==182) |> 
  # mutate(plot_category="Completed Spawn Years") |> 
  # filter(spawn_year>2012)


# get estimates of how much of the run has been
# completed on a given day of the year for use
# in estimating what the total will be based
# on year-to-date numbers in given spawn year

complete_reference <- complete_daily %>% 
  group_by(dummy_date,species) %>% 
  summarize(median_cum=median(daily_cumulative),
            min_cum=min(daily_cumulative),
            max_cum=max(daily_cumulative),
            min_dailyprop=min(daily_prop),
            median_dailyprop=median(daily_prop),
            max_daily_prop=max(daily_prop))


complete_current <- yfk_entry.summary %>% 
  filter(species %in% c("Bull Trout","Chinook",
                        "Steelhead")) |> 
  mutate(day_of_year=yday(yfk_entry_date),
         dummy_date=case_when(
           species=="Steelhead"&day_of_year<183 ~ as.Date(day_of_year,origin="1976-01-01"),
           TRUE ~ as.Date(day_of_year-1,origin="1976-01-01")
         )) |> 
  left_join(species_max_dates,by="species") |> 
  group_by(species) |> 
  mutate(min_date=min(dummy_date,na.rm=T)) |> 
  complete(dummy_date=seq(min(min_date), max(max_date), by="day")) |> 
  ungroup() |> 
  select(-c(min_date,max_date)) |> 
  mutate(across(n,~replace_na(.x,0))) |> 
  mutate(across(daily_prop,~replace_na(.x,0)))|> 
  group_by(species) |> 
  fill(c("sy_total",
         "daily_cumulative"),.direction="down") |> 
  mutate(daily_cumulative=cumsum(n),
         spawn_year=case_when(
           species=="Steelhead"&yday(dummy_date)<183 ~ year(today())+1,
           TRUE ~ year(today())
         ))

# 
#   complete(yfk_entry_date=seq(as_date("2024-07-01"),today(),
#                              by="day")) %>% 
#   mutate(obs_year=year(yfk_entry_date),
#          obs_month=month(yfk_entry_date),
#          spawn_year=ifelse(obs_month>6,(obs_year+1),
#                            obs_year))%>% 
#   group_by(spawn_year) %>% 
#   mutate(n=ifelse(is.na(n),0,n),
#          daily_running_total=cumsum(n),
#          daily_prop=n/sy_total,
#          daily_cumulative=cumsum(daily_prop),
#          daily_percent=daily_cumulative*100) %>% 
#   mutate(day_of_year=yday(yfk_entry_date),
#          dummy_entry_date=if_else(day_of_year<182,
#                                     as.Date(day_of_year,origin="1977-12-31"),
#                                     as.Date(day_of_year,origin="1976-12-31"))) %>% 
#   filter(!day_of_year==182) %>% 
#   select(spawn_year,yfk_entry_date,n,dummy_entry_date) %>% 
#   mutate(daily_cumulative_n=cumsum(n))

today_dummy <- as.Date(yday(today()),
                       origin="1976-01-01")

projected_totals <- complete_current %>% 
  filter(dummy_date==today_dummy) |> 
  left_join(complete_reference,by=c("dummy_date",
                                    "species")) %>% 
  mutate(max_sy_total=daily_cumulative/min_cum,
         median_sy_total=daily_cumulative/median_cum,
         min_sy_total=daily_cumulative/max_cum) %>% 
  select(spawn_year,min_sy_total,median_sy_total,
         max_sy_total) %>% 
  pivot_longer(min_sy_total:max_sy_total,
               values_to = "sy_total") %>%  
  mutate(projection_category=str_to_title(word(name,1,sep="_")))

# make the projections points that can go on the plot

projected_pts <- projected_totals %>% 
  mutate(yfk_entry=as_date("2025-06-01"),
         dummy_sfentry_date=as_date("1978-06-01"))


alldaily <- complete_daily %>% 
  select(spawn_year,yfk_entry_date,n,dummy_date) %>% 
  bind_rows(complete_current) |> 
  group_by(spawn_year,species) |> 
  mutate(daily_cumulative_n=cumsum(n))

# calculate projection stats

run_stats <- alldaily |> 
  left_join(sy_current,by="species") |> 
  filter(spawn_year>2012,
         spawn_year<today_spawn_year) |> 
  group_by(spawn_year,species) |> 
  mutate(total_n=sum(n),
         prop_complete=daily_cumulative_n/total_n) |> 
  group_by(dummy_date,species) |> 
  summarize(median_percentcomplete=round(median(prop_complete)*100),
            min_percentcomplete=round(min(prop_complete)*100),
            max_percentcomplete=round(max(prop_complete)*100))

# save additional parts to include in the shiny app

saveRDS(run_stats,"data/run_stats")
saveRDS(alldaily,"data/alldaily")
saveRDS(projected_pts,"data/projections")

# add USE array current year to track as well

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
  )) |> 
  group_by(species) |> 
  mutate(most_recent=max(spawn_year,na.rm=T)) |> 
  filter(spawn_year==year(today()))


# bring in query from API that searches for USE
# fish detected downstream in the hydrosystem and
# get their latest detection by PIT id; this gets
# other marking sites besides YANKFK - already 
# queried those previously in this script so run this
# then bind with that query

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

# bind with the already run YANKFK query

bind_downstream_detections.dat <-   bind_rows(others_downstream_detections.dat,
                                              yfk_downstream_detections.dat)

# pull out detections at USE that were marked as
# juveniles and drop
# any that don't appear in the downstream detections 
# prior to their latest detection at the USE array

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



if(nrow(use_individuals.summary)>0){
  
  
  use_entry.summary <- use_individuals.summary |>  
    mutate(use_entry_date=as_date(use_first))  |>  
    group_by(use_entry_date,species)  |>  
    summarise(n=n()) |>  
    group_by(species) |>  
    mutate(sy_total=sum(n),
           cumulative_total=cumsum(n),
           daily_prop=n/sy_total,
           daily_cumulative=cumsum(daily_prop)) 
}else{
  use_entry.summary <-  tibble(use_entry_date=as_date(today()),
                               species=c("Bull Trout","Chinook","Steelhead"),
                               n=0,
                               sy_total=0,
                               cumulative_total=0,
                               daily_prop=0,
                               daily_cumulative=0) 
  
}

use_complete_current <- use_entry.summary %>% 
  filter(species %in% c("Bull Trout","Chinook",
                        "Steelhead")) |> 
  mutate(day_of_year=yday(use_entry_date),
         dummy_date=case_when(
           species=="Steelhead"&day_of_year<183 ~ as.Date(day_of_year,origin="1977-01-01"),
           TRUE ~ as.Date(day_of_year-1,origin="1976-01-01")
         )) |> 
  left_join(species_max_dates,by="species") |> 
  group_by(species) |> 
  mutate(min_date=min(dummy_date,na.rm=T)) |> 
  complete(dummy_date=seq(min(min_date), max(max_date), by="day")) |> 
  ungroup() |> 
  select(-c(min_date,max_date)) |> 
  mutate(across(n,~replace_na(.x,0))) |> 
  mutate(across(daily_prop,~replace_na(.x,0)))|> 
  group_by(species) |> 
  fill(c("sy_total",
         "daily_cumulative"),.direction="down") |> 
  mutate(daily_cumulative=cumsum(n))

saveRDS(use_complete_current,"data/use_complete_current")




