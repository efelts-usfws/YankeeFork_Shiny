
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
library(fontawesome)

conflicts_prefer(DT::renderDT,
                 dplyr::filter,
                 dplyr::lag,
                 plotly::layout)

# read in data 

flow.dat <- read_feather("data/yfk_flow")

daily.dat <- read_feather("data/daily")




individuals.dat <- read_feather("data/individuals")

slider_min <- as.Date(min(individuals.dat$yfk_entry_final))

lastweek <- individuals.dat %>% 
  filter(yfk_entry_final>=today()-days(7))


user_dates <-     
  sliderInput(inputId = "user_dates",
              label="Choose a Date Range",
              min=as_date("2025-01-01"),
              max=today(),
              value=c(as_date("2025-01-01"),today()))

# build user interface

ui <- page_navbar(
  
  title="Yankee Fork Salmon River Steelhead",
  
  theme = bs_theme(preset="cyborg"),
  
  sidebar=sidebar(width=500,
                  
                  accordion(
                    
                    accordion_panel(
                      
                      "Explore Data",
                      
                      user_dates
                      
                    )
                    
                  )
                  
                  ),
  
  nav_panel("Placeholder",
            
            
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
                
                col_widths= c(6,6,6),
                
                card(card_header("Stream Discharge"),
                     plotlyOutput("flow_plot"),
                     full_screen = T),
                
                card(card_header("Year-to-date Totals"),
                     plotlyOutput("comp_plot"),
                     full_screen = T),
                
                card(card_header("Unique Fish In"),
                     plotlyOutput("entry_plot"),
                     full_screen = T)
                
                
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
      scale_x_date(date_breaks = "1 month", date_labels="%b",
                   limits=c(as.Date(plot_min),as.Date(plot_max)))+
      theme_bw()+
      labs(x="",y="Mean Discharge at Yankee Fork Gaging Station")
    
  })
  
  # Render the flow plot as a plotly object
  
  output$flow_plot <- renderPlotly({
    
    plot1 <- flowplot_reactive()
    
    ggplotly(plot1,
             tooltip=c("text"))
    
  })
  
    # make the plot for daily numbers of pit tags entering
  
  dailyentry_reactive <- reactive({
    
    plot_min <- min(input$user_dates)
    plot_max <- max(input$user_dates)
    
    entry.plot <- ggplot()+
      geom_col(data=daily.dat,fill="black",color="grey",
               aes(x=sf_final_date,y=n,group=spawn_year,
                   text=str_c(" Date:",sf_final_date,
                              "<br>","Number Steelhead Entered:",n,
                              sep=" ")))+
      scale_x_date(date_breaks = "1 month", date_labels="%b",
                   limits=c(as.Date(plot_min),as.Date(plot_max)))+
      theme_bw()+
      labs(x="Latest entry date to SF Clearwater",
           y="Number of unique PIT-Tagged Steelhead")
    
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
       scale_x_date(date_breaks = "1 month", date_labels="%b",
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
      scale_x_date(date_breaks = "1 month", date_labels="%b",
                   limits=c(as.Date(plot_min),as.Date(plot_max)))+
      theme_bw()+
      labs(x="Latest entry date to Yankee Fork Salmon River",
           y="Number of unique PIT-Tagged Steelhead")
    
  })
  
  # Render the daily tally plot as a plotly object
  
  output$entry_plot <- renderPlotly({
    
    plot1 <- dailyentry_reactive()
    
    ggplotly(plot1,
             tooltip=c("text"))
    
  })
}

shinyApp(ui, server)
