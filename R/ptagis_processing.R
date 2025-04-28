library(tidyverse) 
library(sf)
library(leaflet)
library(leafem)
library(arrow)
library(plotly)
library(dataRetrieval)
library(patchwork)


ptagis.dat <- read_feather("shapes/ptagis_sites")

# just initially look at what infrastructure is
# in the Yankee Fork

ptagis.sf <- st_read(dsn="shapes",layer="ptagis_master")

leaflet_base <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldTopoMap,group="Topographic") %>% 
  addProviderTiles(providers$Esri.WorldImagery,group="Imagery") %>% 
  addProviderTiles(providers$OpenStreetMap.Mapnik,group="Roads") %>% 
  addLayersControl(
    baseGroups = c("Topographic","Imagery","Roads"),
    options = layersControlOptions(collapsed = FALSE),
    position="bottomright") %>% 
  setView(lng=-114.31,lat=44.56308,zoom=8) %>% 
  addMouseCoordinates()  

leaflet_base %>% 
  addCircleMarkers(data=ptagis.dat,
                   lat=~latitude,
                   lng=~longitude,
                   popup=~str_c(site_code))



leaflet_base %>% 
  addCircleMarkers(data=ptagis.sf,
                   popup=~str_c(site_cd))

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

# once that part is done, read in the output here

dat <- read_csv("data/YFK STHD.csv")%>% 
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

