library(tidyverse)
library(skimr)
library(readxl)
library(sf)
library(USAboundaries)
library(shiny)
library(shinythemes)
library(lubridate)

counties <- us_counties(resolution = "high", states = c("Minnesota", "Wisconsin")) %>%
  st_transform(crs = 6783)
rivers <- read_sf('../../data/shp_water_lakes_rivers') %>%
  st_transform(crs = 6783)

load("../../data/superfund_site_data/superfund.rds")

superfund_loc <- superfund %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE), LATITUDE > 44, LATITUDE < 46, LONGITUDE < -92, LONGITUDE > -94) %>%
  mutate(year = as.numeric(substr(SAMPLE_DATE, 7, 11)))

superfund_loc_pfas <- superfund_loc %>%
  filter(ANALYTE_NAME %in% c("Perfluorohexanoic acid (PFHxA)", "Perfluorohexanesulfonate (PFHxS)", "Perfluorooctanesulfonate (PFOS)", "Perfluorooctanoic acid (PFOA)", "Perfluoropentanoic acid (PFPeA)", "Perfluorobutanesulfonate (PFBS)", "Perfluorobutanoic acid (PFBA)"))

superfund_loc <- st_as_sf(superfund_loc, coords = c("LONGITUDE", "LATITUDE"), crs = 6783)
superfund_loc_pfas <- st_as_sf(superfund_loc_pfas, coords = c("LONGITUDE", "LATITUDE"), crs = 6783)
  

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("PFAS Exploration in Superfund sites"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          sliderInput(
            inputId = "year",
            label = "Sample Year",
            min = min(superfund_loc$year),
            max = max(superfund_loc$year),
            value = c(2010, 2020),
            ticks = FALSE,
            step = 1
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("superfundSamplePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  counties_cropped <- st_crop(counties, xmin = -93.7, xmax=-92.6, ymin = 44.7, ymax=45.2)

    output$superfundSamplePlot <- renderPlot({
      
      ggplot()+
        geom_sf(data = counties_cropped, color = "navajowhite", fill = "ivory", size = 0.5)+
        geom_sf(data = (superfund_loc %>% filter(year == input$year)), size= 0.9, aes(color = site), alpha = 0.5)+
        theme(legend.position = "none", 
              axis.line = element_blank(), 
              axis.text = element_blank(), 
              axis.ticks = element_blank())+
        labs(title = "Samples from superfund sites in Washington, Hennepin, and Ramsey Counties")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
