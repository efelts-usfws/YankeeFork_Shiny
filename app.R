
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
                showcase=fa("fish-fins")
                
                
              )
              
            )
            
            
            )
  
)

# build server side

server <- function(input,output,session){
  
  
}

shinyApp(ui, server)
