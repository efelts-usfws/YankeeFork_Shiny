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

# reead in data from web API where scheduled
# query of the PIT tag array is stored

yfk_detections.dat <- vroom(file = "https://api.ptagis.org/reporting/reports/efelts60/file/YFK%20STHD.csv",
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
         yrs_at_large=observation_year-release_year) %>% 
  select(pit_id=Tag,rear_type=`Rear Type Code`,
         release_sitecode,release_lifestage=`Mark Life Stage`,
         release_datetime,release_year,observation_sitecode,
         observation_datetime,observation_month,
         observation_year,yrs_at_large,spawn_year,
         length_mm=`Mark Length`) |> 
  mutate(most_recent=max(spawn_year,na.rm=T)) |> 
  filter(spawn_year==most_recent) %>% 
  filter(yrs_at_large!=0 | release_lifestage != "Juvenile")


# bring in query from API that searches for YFK
# fish detected downstream in the hydrosystem

downstream_detections.dat <-  vroom(file = "https://api.ptagis.org/reporting/reports/efelts60/file/YFK%20Steelhead%20Downstream.csv",
                                    delim = ",",
                                    locale = locale(encoding= "UTF-16LE")) |>  
  select(pit_id=Tag)


  
# grab all distinct pit ids from downstream

yfk_downstream.filter <- downstream_detections.dat |> 
  distinct(pit_id)


# pull out detections at YFK that were juveniles
# originally marked at YFK and find
# any that don't appear in the downstream detections 
# query; these are the ones to drop, will likely
# be 0

yfk_juvenile.filter <- yfk_detections.dat %>% 
  filter(release_lifestage=="Juvenile",
         release_sitecode=="YANKFK") |> 
  distinct(pit_id) %>% 
  filter(!pit_id %in% yfk_downstream.filter$pit_id)


# now summarize relevant values and pull
# in marking location as well

dat.mark <- yfk_detections.dat  |>  
  group_by(pit_id)  |> 
  summarize(release_sitecode=first(release_sitecode),
            release_datetime=first(release_datetime))  |>  
  filter(!pit_id %in% yfk_juvenile.filter$pit_id)

# took the granite part out, could definitely add
# it back, probably will once I work the other stuff out,
# would likely just add an additional PTAGIS API to
# get all mark lifestage detections at LGR adult ladder,
# or marking at LGR so can get the associated
# passage dates

yfk_logical <- c("YFK","YANKFK")

yfkentry_logical <- c("YFK")

yfk_individuals.summary <- yfk_detections.dat |> 
  filter(!pit_id %in% yfk_juvenile.filter$pit_id) |>  
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
  left_join(dat.mark,by="pit_id") |> 
  mutate(species="Steelhead")


yfk_entry.summary <- yfk_individuals.summary |>  
  mutate(yfk_entry_date=as_date(yfk_first))  |>  
  group_by(yfk_entry_date)  |>  
  summarise(n=n()) |>  
  ungroup() |>  
  mutate(sy_total=sum(n),
         cumulative_total=cumsum(n),
         daily_prop=n/sy_total,
         daily_cumulative=cumsum(daily_prop),
         spawn_year=2025) |> 
  mutate(species="Steelhead")

# get numbers by location as well

yfk_location.summary <- yfk_detections.dat |> 
  group_by(pit_id)  |>  
  summarize(release_sitecode=first(release_sitecode),
            release_lifestage=first(release_lifestage))  |>  
  left_join(ptagis.dat,by=c("release_sitecode"="site_code"))


# add in chinook part; 

chn_yfk_detections.dat <- vroom(file = "https://api.ptagis.org/reporting/reports/efelts60/file/YFK%20CHN.csv",
                                delim = ",",
                                locale = locale(encoding= "UTF-16LE"))|>
  mutate(observation_sitecode=word(`Site`,1,sep=" "),
          release_sitecode=word(`Release Site`,1,sep=" "),
          observation_datetime=as.POSIXct(`Obs Time`,
                                          format = "%m/%d/%Y %I:%M:%S %p",
                                          tz = "America/Los_Angeles"),
          observation_month=month(observation_datetime),
          observation_year=year(observation_datetime),
          spawn_year=observation_year,
          release_datetime=mdy(`Release Date`),
          release_year=year(release_datetime),
          yrs_at_large=observation_year-release_year) |>
  select(pit_id=`Tag`,rear_type=`Rear Type Code`,
          release_sitecode,release_lifestage=`Mark Life Stage`,
          release_datetime,release_year,observation_sitecode,
          observation_datetime,observation_month,
          observation_year,yrs_at_large,spawn_year,
          length_mm=`Mark Length`) |>
  mutate(most_recent=max(spawn_year,na.rm=T)) |>
  filter(!(yrs_at_large==0 & release_lifestage=="Juvenile"),
         !(yrs_at_large==1 & release_sitecode=="YANKFK"))


# bring in chinook downstream detections, mainly
# using these to filter out any potential ghost
# detections


chn_downstream_detections.dat <- vroom(file = "https://api.ptagis.org/reporting/reports/efelts60/file/YFK%20Chinook%20Downstream.csv",
                                       delim = ",",
                                       locale = locale(encoding= "UTF-16LE")) |> 
  select(pit_id=`Tag`)


chn_yfk_downstream.filter <- chn_downstream_detections.dat |> 
  distinct()

chn_yfk_juvenile.filter <- chn_yfk_detections.dat |> 
  filter(release_lifestage=="Juvenile",
         release_sitecode=="YANKFK") |> 
  distinct(pit_id) %>% 
  filter(!pit_id %in% chn_yfk_downstream.filter$pit_id)


chn_dat.mark <- chn_yfk_detections.dat |> 
  group_by(pit_id) |> 
  summarize(release_sitecode=first(release_sitecode),
            release_datetime=first(release_datetime))|> 
  filter(!pit_id %in% chn_yfk_juvenile.filter$pit_id)


# now get individual summaries for chinook

chn_yfk_individuals.summary <- chn_yfk_detections.dat |> 
  filter(!pit_id %in% chn_yfk_juvenile.filter$pit_id) |> 
  mutate(yfk=ifelse(observation_sitecode %in% yfk_logical,TRUE,
                    FALSE),
         yfk_entry=ifelse(observation_sitecode %in% yfkentry_logical,
                          TRUE,FALSE)) %>% 
  group_by(pit_id)  |>  
  summarize(yfk_first=first(observation_datetime[yfk==TRUE]),
            yfk_entry_final=last(observation_datetime[yfk_entry==TRUE]),
            yfk_diff=as.numeric(yfk_entry_final-yfk_first,units="days"),
            length_mm=mean(length_mm,na.rm=T),
            release_lifestage=first(release_lifestage),
            release_sitecode=first(release_sitecode),
            release_datetime=first(release_datetime)) |> 
  mutate(species="Chinook")



chn_yfk_entry.summary <- chn_yfk_individuals.summary |>  
  mutate(yfk_entry_date=as_date(yfk_first))  |>  
  group_by(yfk_entry_date)  |>  
  summarise(n=n()) |>  
  ungroup() |>  
  mutate(sy_total=sum(n),
         cumulative_total=cumsum(n),
         daily_prop=n/sy_total,
         daily_cumulative=cumsum(daily_prop),
         spawn_year=2025) |> 
  mutate(species="Chinook")

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
  filter(date>=as_date("2025-01-01"),
         date<=today()) |>  
  mutate(group=1)



saveRDS(yfk.dat,"data/yfk_flow")

# for individuals bind together species

# save species separately

saveRDS(yfk_individuals.summary,
        "data/individuals")

saveRDS(chn_yfk_individuals.summary,
              "data/individuals_chn")

# save entr summaries separately

# bind summaries together for species


saveRDS(yfk_entry.summary,
              "data/daily")

saveRDS(chn_yfk_entry.summary,
        "data/daily_chn")


# right now the complete range won't update 
# programatically as new spawn years are added;
# I don't think it's a big deal but just need
# to remember when appending a new one

complete_daily <- readRDS("data/daily_completed") |> 
  filter(spawn_year<first(yfk_entry.summary$spawn_year)) |> 
  ungroup() |> 
  complete(yfk_entry_date=seq(as_date("2012-07-01"),as_date("2024-06-30"),
                              by="day")) |> 
  mutate(obs_year=year(yfk_entry_date),
         obs_month=month(yfk_entry_date),
         spawn_year=ifelse(obs_month>6,(obs_year+1),
                                      obs_year)) |> 
  group_by(spawn_year) |> 
  fill(sy_total,.direction="updown") |> 
  mutate(n=ifelse(is.na(n),0,n),
         daily_running_total=cumsum(n),
         daily_prop=n/sy_total,
         daily_cumulative=cumsum(daily_prop),
         daily_percent=daily_cumulative*100) %>% 
  mutate(day_of_year=yday(yfk_entry_date),
         dummy_entry_date=if_else(day_of_year<182,
                                    as.Date(day_of_year,origin="1977-12-31"),
                                    as.Date(day_of_year,origin="1976-12-31"))) |> 
  filter(!day_of_year==182) |> 
  mutate(plot_category="Completed Spawn Years") |> 
  filter(spawn_year>2012)


# get estimates of how much of the run has been
# completed on a given day of the year for use
# in estimating what the total will be based
# on year-to-date numbers in given spawn year

complete_reference <- complete_daily %>% 
  group_by(dummy_entry_date) %>% 
  summarize(median_cum=median(daily_cumulative),
            min_cum=min(daily_cumulative),
            max_cum=max(daily_cumulative),
            min_dailyprop=min(daily_prop),
            median_dailyprop=median(daily_prop),
            max_daily_prop=max(daily_prop))


complete_current <- yfk_entry.summary %>% 
  complete(yfk_entry_date=seq(as_date("2024-07-01"),today(),
                             by="day")) %>% 
  mutate(obs_year=year(yfk_entry_date),
         obs_month=month(yfk_entry_date),
         spawn_year=ifelse(obs_month>6,(obs_year+1),
                           obs_year))%>% 
  group_by(spawn_year) %>% 
  mutate(n=ifelse(is.na(n),0,n),
         daily_running_total=cumsum(n),
         daily_prop=n/sy_total,
         daily_cumulative=cumsum(daily_prop),
         daily_percent=daily_cumulative*100) %>% 
  mutate(day_of_year=yday(yfk_entry_date),
         dummy_entry_date=if_else(day_of_year<182,
                                    as.Date(day_of_year,origin="1977-12-31"),
                                    as.Date(day_of_year,origin="1976-12-31"))) %>% 
  filter(!day_of_year==182) %>% 
  select(spawn_year,yfk_entry_date,n,dummy_entry_date) %>% 
  mutate(daily_cumulative_n=cumsum(n))


projected_totals <- complete_current %>% 
  slice(which.max(yfk_entry_date)) %>% 
  left_join(complete_reference,by="dummy_entry_date") %>% 
  mutate(max_sy_total=daily_cumulative_n/min_cum,
         median_sy_total=daily_cumulative_n/median_cum,
         min_sy_total=daily_cumulative_n/max_cum) %>% 
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
  select(spawn_year,yfk_entry_date,n,dummy_entry_date,
         daily_cumulative_n=daily_running_total) %>% 
  bind_rows(complete_current)

# save additional parts to include in the shiny app

saveRDS(alldaily,"data/alldaily")
saveRDS(projected_pts,"data/projections")

# chinook projection stuff

chn_complete_daily <- readRDS("data/daily_completed_chn") |>  
  filter(spawn_year<first(chn_yfk_entry.summary$spawn_year)) |> 
  ungroup() |> 
  complete(yfk_entry_date=seq(as_date("2013-01-01"),as_date("2024-12-31"),
                              by="day")) |> 
  mutate(obs_year=year(yfk_entry_date),
         obs_month=month(yfk_entry_date),
         spawn_year=obs_year) |> 
  group_by(spawn_year) |> 
  fill(sy_total,.direction="updown") |> 
  mutate(n=ifelse(is.na(n),0,n),
         daily_running_total=cumsum(n),
         daily_prop=n/sy_total,
         daily_cumulative=cumsum(daily_prop),
         daily_percent=daily_cumulative*100) %>% 
  mutate(day_of_year=yday(yfk_entry_date),
         dummy_entry_date= as.Date(day_of_year,origin="1976-12-31")) |> 
  filter(!day_of_year==366) |> 
  mutate(plot_category="Completed Spawn Years") |> 
  filter(spawn_year>2012)

chn_complete_reference <- chn_complete_daily %>% 
  group_by(dummy_entry_date) %>% 
  summarize(median_cum=median(daily_cumulative),
            min_cum=min(daily_cumulative),
            max_cum=max(daily_cumulative),
            min_dailyprop=min(daily_prop),
            median_dailyprop=median(daily_prop),
            max_daily_prop=max(daily_prop))

chn_complete_current <- chn_yfk_entry.summary %>% 
  complete(yfk_entry_date=seq(as_date("2025-01-01"),today(),
                              by="day")) %>% 
  mutate(obs_year=year(yfk_entry_date),
         obs_month=month(yfk_entry_date),
         spawn_year=obs_year)%>% 
  group_by(spawn_year) %>% 
  mutate(n=ifelse(is.na(n),0,n),
         daily_running_total=cumsum(n),
         daily_prop=n/sy_total,
         daily_cumulative=cumsum(daily_prop),
         daily_percent=daily_cumulative*100) %>% 
  mutate(day_of_year=yday(yfk_entry_date),
         dummy_entry_date=as.Date(day_of_year,origin="1976-12-31")) %>% 
  filter(!day_of_year==366) %>% 
  select(spawn_year,yfk_entry_date,n,dummy_entry_date) %>% 
  mutate(daily_cumulative_n=cumsum(n))

chn_projected_totals <- chn_complete_current %>% 
  slice(which.max(yfk_entry_date)) %>% 
  left_join(chn_complete_reference,by="dummy_entry_date") %>% 
  mutate(max_sy_total=daily_cumulative_n/min_cum,
         median_sy_total=daily_cumulative_n/median_cum,
         min_sy_total=daily_cumulative_n/max_cum) %>% 
  select(spawn_year,min_sy_total,median_sy_total,
         max_sy_total) %>% 
  pivot_longer(min_sy_total:max_sy_total,
               values_to = "sy_total") %>% 
  mutate(projection_category=str_to_title(word(name,1,sep="_")))

# make the projections points that can go on the plot

chn_projected_pts <- chn_projected_totals %>% 
  mutate(yfk_entry=as_date("2025-09-15"),
         dummy_sfentry_date=as_date("1978-09-15"))


chn_alldaily <- chn_complete_daily %>% 
  select(spawn_year,yfk_entry_date,n,dummy_entry_date,
         daily_cumulative_n=daily_running_total) %>% 
  bind_rows(chn_complete_current)

# save additional parts to include in the shiny app

saveRDS(chn_alldaily,"data/alldaily_chn")
saveRDS(chn_projected_pts,"data/projections_chn")
