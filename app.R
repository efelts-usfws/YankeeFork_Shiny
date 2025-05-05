
library(shiny)
library(tidyverse)
library(bslib)
library(DT)
library(shinyWidgets)
library(conflicted)
library(plotly)
library(arrow)
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

flow.dat <- read_feather("data/yfk_flow")

daily.dat <- read_feather("data/daily")

location.dat <- read_feather("data/locations")

individuals.dat <- read_feather("data/individuals")

lifestage_pal <- colorFactor(palette=c("cyan","magenta"),
                             levels=c("Juvenile","Adult"))

leaflet_base <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldTopoMap,group="Topographic") %>% 
  addProviderTiles(providers$Esri.WorldImagery,group="Imagery") %>% 
  addProviderTiles(providers$OpenStreetMap,group="Roads") %>% 
  addLayersControl(
    baseGroups=c("Topographic",
                 "Imagery",
                 "Roads"),
    options=layersControlOptions(collapsed=FALSE)) %>% 
  addMouseCoordinates() %>% 
  setView(lng=-114.96094,
          lat=45.29035,zoom=6) %>% 
  addLegend(pal=lifestage_pal,
            values=c("Juvenile",
                     "Adult"),
            title="Mark Life Stage")%>% 
  addCircleMarkers(data=location.dat,
                   lat=~latitude,
                   lng=~longitude,
                   fillColor = ~ lifestage_pal(release_lifestage),
                   color= ~ lifestage_pal(release_lifestage),
                   clusterOptions = markerClusterOptions(),
                   popup=~str_c("<b>","Site Code: ","</b>",release_sitecode,
                                "<br>",
                                "<b>","Site Name: ","</b>",site_name))
leaflet_base





lf.dat <- individuals.dat %>% 
  filter(release_lifestage=="Adult",
         !is.na(length_mm)) %>% 
  mutate(length_bin=floor(length_mm/25)*25) %>% 
  mutate(mean_length=mean(length_mm),
         total_n=n()) %>% 
  group_by(length_bin) %>% 
  summarize(freq=n(),
            total_sample=first(total_n),
            mean_length=first(mean_length))


slider_min <- as.Date(min(individuals.dat$yfk_entry_final))

lastweek <- individuals.dat %>% 
  filter(yfk_entry_final>=today()-days(7))

# find two weeks prior to the first PIT tag
# detection for the year, so that can
# be the start for the date filter

date_start <- min(daily.dat$yfk_final_date)-weeks(2)

user_dates <-     
  sliderInput(inputId = "user_dates",
              label="Choose a Date Range",
              min=as_date("2025-01-01"),
              max=today(),
              value=c(date_start,today()))



# build user interface

ui <- page_navbar(
  
  title="Yankee Fork Salmon River PIT Detections",
  
  theme = bs_theme(preset="cyborg"),
  
  sidebar=sidebar(width=300,
                  
                  accordion(
                    
                    accordion_panel(
                      
                      "Explore Data",
                      
                      user_dates
                      
                    )
                    
                  )
                  
                  ),
  
  nav_panel("Steelhead",
            
            
            layout_columns(
              
              value_box(
                
                title="Unique PIT Tagged Steelhead, SY 2025",
                value=nrow(individuals.dat),
                showcase=fa("fish-fins"),
                
             
                ),
              
              value_box(
                title="New in the Last Week",
                value=nrow(lastweek),
                showcase=bs_icon("graph-up-arrow")
                
                
                
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
                     leafletOutput("marked_map"),
                     full_screen = TRUE)
                
                
              )
              
            )
            
            
            )
  
)

# build server side

server <- function(input,output,session){
  
  # make the flow plot as a reactive
  
  flowplot_reactive <- reactive({
    
    plot_min <- min(input$user_dates)
    plot_max <- max(input$user_dates)
    
    flow.plot <- flow.dat %>% 
      mutate(date=as_date(date)) %>% 
      ggplot(aes(x=date,y=mean_discharge,group=group))+
      geom_line(aes(text=str_c(" Date:",date,
                               "<br>","Mean Discharge (cfs): ",mean_discharge,
                               sep=" ")))+
      scale_x_date(date_breaks = "1 week", date_labels="%b %d",
                   limits=c(as.Date(plot_min),as.Date(plot_max)))+
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
      scale_x_date(date_breaks = "1 week", date_labels="%b %d",
                   limits=c(as.Date(plot_min),as.Date(plot_max)))+
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

  
  # make a reactive plot of cumulative
  # numbers in; this is named comp plot bc
  # eventually it will compare the cumulative
  # curve among years, right now it will
  # just be showing current year (2025)
  
  compplot_reactive <- reactive({
    
    plot_lim.dat <- tibble(min_doy=min(input$user_dates),
                           max_doy=max(input$user_dates))
    
    comp_plot <- daily.dat %>% 
      ggplot(aes(x=yfk_final_date,y=cumulative_total,
                 group=spawn_year))+
      geom_line(aes(text=str_c(" Date:",format(yfk_final_date, "%b %d"),
                               "<br>",
                               "Number In:",round(cumulative_total),sep=" ")))+
      theme_bw()+
      theme(axis.text.x=element_text(angle=45,hjust=1))+
       scale_x_date(date_breaks = "1 week", date_labels="%b %d",
                    limits=c(as.Date(plot_lim.dat$min_doy),as.Date(plot_lim.dat$max_doy)))+
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
    
    plot_min <- min(input$user_dates)
    plot_max <- max(input$user_dates)
    
    entry.plot <- ggplot()+
      geom_col(data=daily.dat,fill="black",color="grey",
               aes(x=yfk_final_date,y=n,group=spawn_year,
                   text=str_c(" Date:",yfk_final_date,
                              "<br>","Number Steelhead Entered:",n,
                              sep=" ")))+
      scale_x_date(date_breaks = "1 week", date_labels="%b %d",
                   limits=c(as.Date(plot_min),as.Date(plot_max)))+
      theme_bw()+
      theme(axis.text.x=element_text(angle=45,hjust=1))+
      labs(x="Latest entry date to Yankee Fork Salmon River",
           y="Number of unique PIT-Tagged Steelhead")
    
  })
  
  # Render the daily tally plot as a plotly object
  
  output$entry_plot <- renderPlotly({
    
    plot1 <- dailyentry_reactive()
    
    ggplotly(plot1,
             tooltip=c("text"))
    
  })
  
  # make the plotly graph of a length frequency for all the
  # fish to come through this spawn year
  
  output$lf_plot <- renderPlotly({
    
    plot.lf <- lf.dat %>%   
      ggplot(aes(x=length_bin,y=freq))+
      geom_col(aes(text=str_c("Length Bin: ",length_bin,
                              "<br>",
                              "Number: ",freq)),
               fill="steelblue",color="black")+
      geom_vline(data=lf.dat,aes(xintercept = first(mean_length)),
                 linetype="dashed",color="black")+
      geom_text(x=min(lf.dat$length_bin, na.rm=T)*1.05,
                y=max(lf.dat$freq, na.rm=T) * 0.88,
                label=str_c("N = ",first(lf.dat$total_sample)),
                size=4,hjust=0)+
      geom_text(x=min(lf.dat$length_bin, na.rm=T)*1.05,
                y=max(lf.dat$freq, na.rm=T) * 0.95,
                label=str_c("Mean Length = ",
                            str_c(round(first(lf.dat$mean_length)),"mm",sep=" ")),
                size=4,hjust=0)+
      scale_x_continuous(breaks=seq(min(lf.dat$length_bin),
                                    max(lf.dat$length_bin),25))+
      scale_y_continuous(breaks=scales::breaks_pretty(n=10))+
      theme_bw()+
      labs(x="Length bin (25 mm)",y="Number of Steelhead")
    
    ggplotly(plot.lf,
             tooltip=c("text"))
    
  })
  
  # make the leaflet map in the server
  
  output$marked_map <- renderLeaflet({
    
    leaflet_base
    
  })
}

shinyApp(ui, server)

