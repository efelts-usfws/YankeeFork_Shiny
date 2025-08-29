
library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(bslib)
library(DT)
library(shinyWidgets)
library(conflicted)
library(plotly)
library(dataRetrieval)
library(bsicons)
library(viridis)
library(scales)
library(fontawesome)
library(leaflet)
library(leafem)

conflicts_prefer(DT::renderDT,
                 dplyr::filter,
                 dplyr::lag,
                 plotly::layout)

# read in data 

flow.dat <- readRDS("data/yfk_flow")

daily.dat <- readRDS("data/daily")

# location.dat <- readRDS("data/locations")

individuals.dat <- readRDS("data/individuals")

individuals.export <- individuals.dat |> 
  select(pit_id,species,release_lifestage,release_sitecode,
         release_datetime,length_mm,yfk_entry=yfk_first) |> 
  mutate(observation_year=year(yfk_entry),
         spawn_year=case_when(
           yday(yfk_entry)>=183 & species=="Steelhead" ~ observation_year+1,
           TRUE ~ observation_year))

# compute summaries of where individuals were marked;
# bring PTAGIS metadata in to get full names

ptagis.dat <- readRDS("data/ptagis_sites") 


mark.summary <- individuals.dat |> 
  left_join(ptagis.dat,by=c("release_sitecode"="site_code")) |> 
  group_by(species,release_sitecode,release_lifestage) |> 
  summarize(n=n(),
            site_name=first(site_name)) |> 
  arrange(-n) |> 
  select(`Site Name`=site_name,
         `Site Code`=release_sitecode,
         `Life Stage at Marking`=release_lifestage,
         Count=n)

sy_current <- tibble(species=c("Steelhead","Chinook",
                               "Bull Trout")) |> 
  mutate(today.year=year(today()),
         today.jday=yday(today()),
         today_spawn_year=case_when(
           species == "Steelhead"& today.jday>=183 ~ today.year,
           TRUE ~ today.year))

alldaily.dat <- readRDS("data/alldaily") |> 
  filter(spawn_year>2012) |> 
  left_join(sy_current,by=c("species")) |> 
  mutate(yr_category=ifelse(spawn_year==today_spawn_year,
                            "Current","Previous"),
         dummy_jday=yday(dummy_date))
  
  
projection.dat <- readRDS("data/projections")

# calculate estimate of how much of the run is complete

run_stats <- readRDS("data/run_stats")

test_min <- alldaily.dat |> 
  group_by(species,spawn_year) |> 
  summarize(earliest=min(yfk_entry_date,na.rm=T))

today_dummy <- as.Date(yday(today())-1,
                       origin="1976-01-01")

today_ref <- as.Date(format(Sys.Date(), "%Y-01-01"))-1



spp_max <- run_stats |> 
  filter(min_percentcomplete==100) |> 
  ungroup() |> 
  group_by(species) |> 
  slice(which.min(dummy_date)) |> 
  mutate(current_max=as.Date(yday(dummy_date),
                                  origin=today_ref))



today_run <- run_stats |> 
  filter(dummy_date==today_dummy)

# lifestage_pal <- colorFactor(palette=c("cyan","magenta"),
#                              levels=c("Juvenile","Adult"))
# 
# leaflet_base <- leaflet() %>% 
#   addProviderTiles(providers$Esri.WorldTopoMap,group="Topographic") %>% 
#   addProviderTiles(providers$Esri.WorldImagery,group="Imagery") %>% 
#   addProviderTiles(providers$OpenStreetMap,group="Roads") %>% 
#   addLayersControl(
#     baseGroups=c("Topographic",
#                  "Imagery",
#                  "Roads"),
#     options=layersControlOptions(collapsed=FALSE)) %>% 
#   addMouseCoordinates() %>% 
#   setView(lng=-114.96094,
#           lat=45.29035,zoom=6) %>% 
#   addLegend(pal=lifestage_pal,
#             values=c("Juvenile",
#                      "Adult"),
#             title="Mark Life Stage")%>% 
#   addCircleMarkers(data=location.dat,
#                    lat=~latitude,
#                    lng=~longitude,
#                    fillColor = ~ lifestage_pal(release_lifestage),
#                    color= ~ lifestage_pal(release_lifestage),
#                    clusterOptions = markerClusterOptions(),
#                    popup=~str_c("<b>","Site Code: ","</b>",release_sitecode,
#                                 "<br>",
#                                 "<b>","Site Name: ","</b>",site_name))






lf.dat <- individuals.dat %>% 
  filter(release_lifestage=="Adult",
         !is.na(length_mm)) %>% 
  group_by(species) |> 
  mutate(length_bin=floor(length_mm/25)*25) %>% 
  mutate(mean_length=mean(length_mm),
         total_n=n()) %>% 
  group_by(species,length_bin) %>% 
  summarize(freq=n(),
            total_sample=first(total_n),
            mean_length=first(mean_length))


slider_min <- as.Date(min(individuals.dat$yfk_entry_final))

lastweek_detections <- individuals.dat %>% 
  filter(yfk_entry_final>=today()-days(7))

lastweek_new <- individuals.dat %>% 
  filter(yfk_first>=today()-days(7))

# find two weeks prior to the first PIT tag
# detection for the year, so that can
# be the start for the date filter

# date_start <- min(daily.dat$yfk_entry_date)-weeks(2)
# 
# user_dates <-     
#   sliderInput(inputId = "user_dates",
#               label="Choose a Date Range",
#               min=as_date("2025-01-01"),
#               max=today(),
#               value=c(date_start,today()))



# make the default species selection depend on 
# today's date: if it's Jan-May, Steelhead, otherwise
# chinook

today_spp <- ifelse(
  
  month(today())>5, "Chinook",
  "Steelhead"
  
)


# build user interface

ui <- page_navbar(
  
  title="Yankee Fork Salmon River PIT Detections",
  
  theme = bs_theme(preset="cyborg"),
  
  sidebar=sidebar(width=300,
                  
                  accordion(
                    
                    accordion_panel(
                      
                      "Explore Data",
                      
                      selectInput(inputId = "user_spp",
                                  label = "Choose a Species",
                                  choices = c("Steelhead",
                                              "Chinook"),
                                  selected = today_spp,
                                  selectize = FALSE
                                  ),
                      
                      uiOutput("user_date_slider"),
                      
                      downloadBttn("download_ind",
                                   "Download Current Year Individual Summaries")
                      
                    )
                    
                  )
                  
                  ),
  
  nav_panel("PIT Tag Detections",
            
            
            layout_columns(
              
              value_box(
                
                title="Unique PIT Tag Detections, Current SY",
                value=textOutput("ind_count_txt"),
                showcase=fa("fish-fins"),
                
             
                ),
              
              value_box(
                title="New in the Last Week",
                value=textOutput("lastweek_count_txt"),
                showcase=bs_icon("graph-up-arrow")),





              value_box(
                title="Estimated Percent of Run Complete",
                value=textOutput("todayrun_median"),
                showcase=bs_icon("circle-half"),
                p(textOutput("todayrun_range"))
              )
              
            ),
            
            page_fillable(
              
              
              layout_columns(
                
                col_widths= c(4,4,4,4,4,4),
                
                card(card_header("Stream Discharge"),
                     plotlyOutput("flow_plot"),
                     full_screen = T),
                
                card(card_header("Stream Temperature"),
                     plotlyOutput("temp_plot"),
                     full_screen = T),
                
                card(card_header("Year-to-date Totals"),
                     plotlyOutput("comp_plot"),
                     full_screen = T),

                card(card_header("Unique Fish In"),
                     plotlyOutput("entry_plot"),
                     full_screen = T),

                card(card_header("Length Frequency"),
                     plotlyOutput("lf_plot"),
                     full_screen = TRUE),

                card(card_header("Marking Locations"),
                     DTOutput("marklocation_summary"),
                     full_screen = TRUE)

                
              )
              
            )
            
            
            )
  
)

# build server side

server <- function(input,output,session){
  
  # make the date slider reactive to species selection
  
  datelimit_reactive <- reactive({
    
    req(input$user_spp)
    
    dat <- spp_max |> 
      filter(species==input$user_spp)
    
  })
  
  # make current spawn year reactive to species selection
  
  sy_reactive <- reactive({
    
    req(input$user_spp)
    
    dat <- sy_current |> 
      filter(species==input$user_spp)
    
    
  })
  
  # number of individuals needs to be reactive to 
  # species selection
  
  ind.dat_reactive <- reactive({
    
    req(input$user_spp)
    
    dat <- individuals.dat |> 
      filter(species==input$user_spp)
    
  })
  
  # make an output of the number of individuals to 
  # got to the value box
  
  output$ind_count_txt <- renderText({
    
    nrow(req(ind.dat_reactive()))
    
  })
  
  # reactive for number of new in the last week
  
  lastweek_reactive <- reactive({
    
    req(input$user_spp)
    
    dat <- individuals.dat %>% 
      filter(species==input$user_spp,
             yfk_first>=today()-days(7))
    
    
  })
  
  # make an output of the number new in last week to 
  # got to the value box
  
  output$lastweek_count_txt <- renderText({
    
    nrow(req(lastweek_reactive()))
    
  })
  
  # reactive for percent of run complete as of today
  
  runstatus_reactive <- reactive({
    
    req(input$user_spp)
    
    dat <- today_run %>% 
      filter(species==input$user_spp)
    
    
  })
  
  # output for median percent complete today
  
  output$todayrun_median <- renderText({
    
   dat <- runstatus_reactive() |> 
      pull(median_percentcomplete)
   
   str_c(dat,"%",sep=" ")
    
  })
  
  # output for range of percent complete today
  
  output$todayrun_range <- renderText({
    
    dat <- runstatus_reactive() 
    
    str_c("Range:",dat$min_percentcomplete,"%",
          "-",
          dat$max_percentcomplete,"%",sep=" ")
    
  })
  
  # make the flow plot as a reactive
  
  flowplot_reactive <- reactive({
    

    flow.plot <- flow.dat %>% 
      mutate(date=as_date(date)) %>% 
      ggplot(aes(x=date,y=mean_discharge,group=group))+
      geom_line(aes(text=str_c(" Date:",date,
                               "<br>","Mean Discharge (cfs): ",mean_discharge,
                               sep=" ")))+
      scale_x_date(date_breaks = "1 month", date_labels="%b")+
      theme_bw()+
      theme(axis.text.x=element_text(angle=45,hjust=1))+
      labs(x="",y="Mean Discharge at Yankee Fork Gaging Station")
    
  })
  
  # Render the flow plot as a plotly object
  
  output$flow_plot <- renderPlotly({
    
    plot1 <- flowplot_reactive()
    
    ggplotly(plot1,
             tooltip=c("text"))
    
  })
  
  # same thing for the temp plot
  
  tempplot_reactive <- reactive({
    
    plot_min <- min(input$user_dates)
    plot_max <- max(input$user_dates)
    
    temp.plot <- flow.dat %>% 
      mutate(date=as_date(date)) %>% 
      ggplot(aes(x=date,y=mean_temp,group=group))+
      geom_line(aes(text=str_c(" Date:",date,
                               "<br>","Mean Temp (C): ",mean_temp,
                               sep=" ")))+
      # scale_x_date(date_breaks = "1 week", date_labels="%b %d",
      #              limits=c(as.Date(plot_min),as.Date(plot_max)))+
      theme_bw()+
      theme(axis.text.x=element_text(angle=45,hjust=1))+
      labs(x="",y="Mean Temperature at Yankee Fork Gaging Station")
    
  })
  
  # Render the flow plot as a plotly object
  
  output$temp_plot <- renderPlotly({
    
    plot1 <- tempplot_reactive()
    
    ggplotly(plot1,
             tooltip=c("text"))
    
  })

  # make all daily data reactive to user selected species
  
  alldaily_reactive <- reactive({
    
    req(input$user_spp)
    
    sy.dat <- sy_reactive()
    
    alldaily.dat |> 
      filter(species==input$user_spp) |> 
      filter(spawn_year<sy.dat$today_spawn_year|
               (spawn_year==sy.dat$today_spawn_year&
                  dummy_jday<=sy.dat$today.jday))
    
  })
  
  # make daily unique fish for current year filter by
  # user selected species
  
  daily_reactive <- reactive({
    
    req(input$user_spp)
    
    dat <- daily.dat |> 
      filter(species==input$user_spp)
    
    
  })
  
  
  # make a reactive plot of cumulative
  # numbers in; this is named comp plot bc
  # eventually it will compare the cumulative
  # curve among years, right now it will
  # just be showing current year (2025)
  
  compplot_reactive <- reactive({
    
    req(input$user_spp)
    
    
    # plot_lim.dat <- tibble(min_doy=yday(min(input$user_dates)),
    #                        max_doy=yday(max(input$user_dates))) |> 
    #   mutate(plot_min=ifelse(min_doy<182,
    #                          as.Date(min_doy,origin="1977-12-31"),
    #                          as.Date(min_doy,origin="1976-12-31")),
    #          plot_max=ifelse(max_doy<182,
    #                          as.Date(max_doy,origin="1977-12-31"),
    #                          as.Date(max_doy,origin="1976-12-31")))
    # 
    dat <- alldaily_reactive()
    
    xlimit <- datelimit_reactive() 

    
    comp_plot <-dat %>% 
      ggplot(aes(x=dummy_date,y=daily_cumulative_n,
                 group=spawn_year,color=as.factor(yr_category)))+
      geom_line(aes(text=str_c(" Date:",format(dummy_date, "%b %d"),
                               "<br>",
                               "Spawn Year:",spawn_year,
                               "<br>",
                               "Number In:",round(daily_cumulative_n),sep=" ")))+
      theme_bw()+
      scale_color_manual(values=c("steelblue","gray70"))+
      theme(axis.text.x=element_text(angle=45,hjust=1))+
      scale_x_date(date_breaks="1 month", 
                   date_labels="%b",
                   limits=c(min(dat$dummy_date,na.rm=T),
                            xlimit$dummy_date))+
      labs(x="Date to Yankee Fork Salmon River",
           y="# PIT Tags in Yankee Fork, Year-To-Date",
           color="")
    
  })
  
  # Render the comp plot as a plotly object
  
  output$comp_plot <- renderPlotly({
    
    plot1 <- compplot_reactive()
    
    ggplotly(plot1,
             tooltip=c("text")) 
    
  })
  
  # make the plot for daily numbers of pit tags entering
  
  dailyentry_reactive <- reactive({
    
    dat <- daily_reactive()
    
    entry.plot <- ggplot()+
      geom_col(data=dat,fill="dodgerblue",color="black",
               aes(x=yfk_entry_date,y=n,
                   text=str_c(" Date:",yfk_entry_date,
                              "<br>","Number Fish Entered:",n,
                              sep=" ")))+
      scale_x_date(date_breaks = "1 week", date_labels="%b %d")+
      theme_bw()+
      theme(axis.text.x=element_text(angle=45,hjust=1))+
      labs(x="Date at Yankee Fork PIT Array",
           y="Number of unique PIT-Tagged Fish")
    
  })
  
  # Render the daily tally plot as a plotly object
  
  output$entry_plot <- renderPlotly({
    
    plot1 <- dailyentry_reactive()
    
    ggplotly(plot1,
             tooltip=c("text"))
    
  })
  
  # make the plotly graph of a length frequency for all the
  # fish to come through this spawn year
  
  # filter length data by user selected species in
  # a reactive
  
  lf_reactive <- reactive({
    
    req(input$user_spp)
    
    dat <- lf.dat |> 
      filter(species==input$user_spp)
  })
  
  output$lf_plot <- renderPlotly({
    
    dat <- lf_reactive()
    
    plot.lf <- dat %>%   
      ggplot(aes(x=length_bin,y=freq))+
      geom_col(aes(text=str_c("Length Bin: ",length_bin,
                              "<br>",
                              "Number: ",freq)),
               fill="steelblue",color="black")+
      geom_vline(data=dat,aes(xintercept = first(mean_length)),
                 linetype="dashed",color="black")+
      # geom_text(x=min(dat$length_bin, na.rm=T)*1.05,
      #           y=max(dat$freq, na.rm=T) * 0.88,
      #           label=str_c("N = ",first(lf.dat$total_sample)),
      #           size=4,hjust=0)+
      # geom_text(x=min(dat$length_bin, na.rm=T)*1.05,
      #           y=max(dat$freq, na.rm=T) * 0.95,
      #           label=str_c("Mean Length = ",
      #                       str_c(round(first(dat$mean_length)),"mm",sep=" ")),
      #           size=4,hjust=0)+
      scale_x_continuous(breaks=seq(min(dat$length_bin),
                                    max(dat$length_bin),25))+
      scale_y_continuous(breaks=scales::breaks_pretty(n=10))+
      theme_bw()+
      labs(x="Length bin (25 mm)",
           y=str_c("Number of Fish","N =",first(lf.dat$total_sample),
                   sep=" "))
    
    ggplotly(plot.lf,
             tooltip=c("text"))
    
  })
  
  # # make the leaflet map in the server
  # 
  # output$marked_map <- renderLeaflet({
  #   
  #   leaflet_base
  #   
  # })
  
  # render a data table of marking summaries
  
  # make the mark summary table reactive
  # to user selected species
  
  marksummary_reactive <- reactive({
    
    req(input$user_spp)
    
    dat <- mark.summary |> 
      filter(species==input$user_spp)
    
  })
  
  output$marklocation_summary <- renderDT({
    
    dat <- marksummary_reactive()
    
    dat
    
  })
  
  # make a download output of individual data
  
  # make it react to user species filter
  
  individuals.export_reactive <- reactive({
    
    req(input$user_spp)
    
    individuals.export |> 
      filter(species==input$user_spp)
    
    
  })
  
  output$download_ind <- downloadHandler(
    
    
    filename=function(){
      
      paste("YFK_detections_",as.integer(today()),".csv",sep="")
      
    },
    
    content=function(file){
      
      write.csv(individuals.export_reactive(),file,row.names=F)
      
    }
    
  )
  
}

shinyApp(ui, server)

