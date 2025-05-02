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

dat <- read.csv("https://api.ptagis.org/reporting/reports/efelts60/file/YFK%20STHD.csv") 

raw_lines <- readLines("data/YFK STHD (1).csv", encoding="UTF-16LE",
                       skipNul=TRUE)

df <- vroom(file = "https://api.ptagis.org/reporting/reports/efelts60/file/YFK%20STHD.csv",
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
         yrs_at_large=observation_year-release_year)

test2 <- read_table("https://api.ptagis.org/reporting/reports/efelts60/file/YFK%20STHD.txt")



test <- readLines("https://api.ptagis.org/reporting/reports/efelts60/file/YFK%20STHD.csv", warn = FALSE)

# once that part is done, read in the output here

dat <- read_csv("data/YFK STHD.csv")#%>% 
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
  mutate(most_recent=max(spawn_year,na.rm=T))%>% 
  filter(spawn_year==most_recent) %>% 
  filter(yrs_at_large!=0 | release_lifestage != "Juvenile")


lytle_summary_init <- dat %>% 
  group_by(pit_id,release_sitecode,release_datetime) %>% 
  summarize(detections=n(),
            first_yfk_detection=min(observation_datetime))

lytle_summary <- lytle_summary_init %>% 
  group_by(release_sitecode) %>% 
  tally()

# my numbers aren't really aligning with what Lytle's email says?

# get a table of unique pit ids
# to be run through the PTAGIS complete tag
# history

int.dat <- dat %>% 
  ungroup() %>% 
  select(pit_id) %>% 
  distinct()

# write to the data folder

write.table(int.dat,"data/inseason_int.txt",
            quote=FALSE,row.names=FALSE,col.names=FALSE)

# another part to automate, run
# that output text file through the 
# complete tag history in PTAGIS
# and export the results to the
# taghistory folder

# make a key for life stage at tagging
# based on pit id

lifestage.key <- dat %>% 
  group_by(pit_id) %>% 
  summarize(life_stage=first(release_lifestage))

# now read in the interrogation results for the adult-marked
# and compress to unique day/event site combos

inseason.int <- read_csv("data/Complete Tag History.csv") %>% 
  mutate(observation_datetime=as.POSIXct(`Event Date Time Value`,
                                         format = "%m/%d/%Y %I:%M:%S %p", 
                                         tz = "America/Los_Angeles"),
         observation_date=as_date(observation_datetime)) %>% 
  select(pit_id=`Tag Code`,obs_type=`Event Type Name`,
         observation_sitecode=`Event Site Code Value`,
         observation_datetime,observation_date,
         length_mm=`Event Length mm`) %>% 
  group_by(pit_id,observation_date,observation_sitecode) %>% 
  slice(which.min(observation_datetime)) %>% 
  left_join(lifestage.key,by="pit_id")


ptagis.dat <- read_feather("shapes/ptagis_sites")%>% 
  group_by(site_code) %>% 
  slice(which.max(total_rkm))

# also list out site code ids for interrogation
# sites that indicate the South Fork overall,
# and those that are at the mouth so detections
# are interpreted as entry into South Fork

yfk_logical <- c("YFK","YANKFK")

yfkentry_logical <- c("YFK")

# So now split out and deal with juveniles

juv.dat1 <- inseason.int %>% 
  filter(life_stage=="Juvenile",
         obs_type=="Observation") %>% 
  left_join(ptagis.dat,by=c("observation_sitecode"="site_code")) %>% 
  group_by(pit_id,observation_datetime) %>%
  arrange(pit_id,observation_datetime) %>% 
  group_by(pit_id) %>% 
  mutate(last_site=last(observation_sitecode),
         lowest_rkm=min(total_rkm))


# So now filter for those records that are fish that were detected
# at least as far down as lower granite and their last detection was in 
# the south fork

juv.filter <- juv.dat1  %>% 
  filter(lowest_rkm<=695,
         last_site %in% yfk_logical)  %>% 
  group_by(pit_id,observation_date,observation_sitecode) %>% 
  slice(which.min(observation_datetime)) %>% 
  select(pit_id,obs_type,observation_sitecode,
         observation_datetime,observation_date) %>% 
  mutate(length_mm=as.numeric(NA),
         mark_stage="Juvenile")  

# pull out the adults from the initial int data
# distinguish their mark stage then bind
# to the filtered juvenile data

int.complete <- inseason.int %>% 
  filter(life_stage=="Adult") %>% 
  rename(mark_stage=life_stage) %>% 
  bind_rows(juv.filter)

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

