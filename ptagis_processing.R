library(vroom) 
library(dataRetrieval)
library(conflicted)
library(dplyr)
library(stringr)
library(lubridate)


conflicts_prefer(vroom::locale,
                 dplyr::filter)

# set the timeout above default of 60 seconds
# because sometimes the API calls are slow

options(timeout=300)



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
  select(pit_id=Tag,,rear_type=`Rear Type Code`,
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
  filter(!pit_id %in% yfk_juvenile.filter)

# took the granite part out, could definitely add
# it back, probably will once I work the other stuff out,
# would likely just add an additional PTAGIS API to
# get all mark lifestage detections at LGR adult ladder,
# or marking at LGR so can get the associated
# passage dates

yfk_logical <- c("YFK","YANKFK")

yfkentry_logical <- c("YFK")

yfk_individuals.summary <- yfk_detections.dat |> 
  filter(!pit_id %in% yfk_juvenile.filter) |>  
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


yfk_entry.summary <- yfk_individuals.summary |>  
  mutate(yfk_final_date=as_date(yfk_entry_final))  |>  
  group_by(yfk_final_date)  |>  
  summarise(n=n()) |>  
  ungroup() |>  
  mutate(sy_total=sum(n),
         cumulative_total=cumsum(n),
         daily_prop=n/sy_total,
         daily_cumulative=cumsum(daily_prop),
         spawn_year=2025)

# get numbers by location as well

yfk_location.summary <- yfk_detections.dat |> 
  group_by(pit_id)  |>  
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
  filter(date>=as_date("2025-01-01"),
         date<=today()) |>  
  mutate(group=1)



saveRDS(yfk.dat,"data/yfk_flow")


saveRDS(yfk_individuals.summary,
              "data/individuals")

saveRDS(yfk_location.summary,
              "data/locations")

saveRDS(yfk_entry.summary,
              "data/daily")

