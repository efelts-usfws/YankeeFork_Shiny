library(tidyverse) 
library(sf)
library(leaflet)
library(leafem)
library(arrow)
library(plotly)
library(dataRetrieval)
library(patchwork)
library(vroom)
library(readxl)

# set the timeout above default of 60 seconds
# because sometimes the API calls are slow

options(timeout=300)


ptagis.dat <- read_feather("shapes/ptagis_sites")



yfk_sites <- c("YFK","CEY","YANKWF","YANKFK",
               "EIGH3C")


yfk.dat <- ptagis.dat %>% 
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
         length_mm=`Mark Length`) %>% 
  mutate(most_recent=max(spawn_year,na.rm=T)) %>% 
  filter(spawn_year==most_recent) %>% 
  filter(yrs_at_large!=0 | release_lifestage != "Juvenile")


# bring in query from API that searches for YFK
# fish detected downstream in the hydrosystem

downstream_detections.dat <-  vroom(file = "https://api.ptagis.org/reporting/reports/efelts60/file/YFK%20Steelhead%20Downstream.csv",
                                    delim = ",",
                                    locale = locale(encoding= "UTF-16LE")) %>% 
  select(pit_id=Tag)


# pull out detections at YFK that were juveniles
# originally marked at YFK and drop
# any that don't appear in the downstream detections 
# query

yfk_juvenile.filter <- yfk_detections.dat %>% 
  filter(release_lifestage=="Juvenile",
         release_sitecode=="YANKFK") %>% 
  distinct(pit_id)
  



# now summarize relevant values and pull
# in marking location as well

dat.mark <- dat %>% 
  group_by(pit_id) %>%
  summarize(release_sitecode=first(release_sitecode),
            release_datetime=first(release_datetime))

yfk_individuals.summary <- int.complete %>% 
  mutate(yfk=ifelse(observation_sitecode %in% yfk_logical,TRUE,
                   FALSE),
         yfk_entry=ifelse(observation_sitecode %in% yfkentry_logical,
                         TRUE,FALSE)) %>% 
  group_by(pit_id) %>% 
  summarize(lgr_final=last(observation_datetime[observation_sitecode=="GRA"]),
            yfk_first=first(observation_datetime[yfk==TRUE]),
            yfk_entry_final=last(observation_datetime[yfk_entry==TRUE]),
            yfk_diff=as.numeric(yfk_entry_final-yfk_first,units="days"),
            length_mm=mean(length_mm,na.rm=T),
            mark_stage=first(mark_stage)) %>% 
  left_join(dat.mark,by="pit_id")


yfk_entry.summary <- yfk_individuals.summary %>% 
  mutate(yfk_final_date=as_date(yfk_entry_final)) %>% 
  group_by(yfk_final_date) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  mutate(sy_total=sum(n),
         cumulative_total=cumsum(n),
         daily_prop=n/sy_total,
         daily_cumulative=cumsum(daily_prop),
         spawn_year=2025)

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
                           endDate=today.text) %>% 
  select(date=Date,
         mean_temp=X_00010_00003,
         mean_discharge=X_00060_00003) %>% 
  mutate(date=as_date(date))

yfk.dat <-yfk.daily %>% 
  filter(date>=as_date("2025-01-01"),
         date<=today()) %>% 
  mutate(group=1)

write_feather(yfk.dat,"data/yfk_flow")


write_feather(yfk_individuals.summary,
              "data/individuals")

write_feather(yfk_entry.summary,
              "data/daily")

