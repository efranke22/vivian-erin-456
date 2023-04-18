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

pfas7 <- read_csv("../../data/pfas7.csv")
load("../../data/superfund_site_data/superfund.rds")

superfund_washington <- st_as_sf(superfund %>% filter(county == "Washington", LATITUDE != "NA", LONGITUDE != "NA", LATITUDE >40, LONGITUDE > -94), coords = c("LONGITUDE", "LATITUDE"), crs = 6783) %>%
  mutate(year = as.numeric(substr(SAMPLE_DATE,7,11)))

pfas_super_washington <- superfund_washington %>%
  filter(!is.na(commonName))
  

#pfas_super_washington <- read_csv("../../data/pfas_superfund.csv") %>%
#  filter(LATITUDE != "NA", LONGITUDE != "NA", LATITUDE <= 45.298915338179285, LATITUDE >= 44.7471734793455) %>%
#  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 6783)

ui <- fluidPage(theme = shinytheme("flatly"), 
                navbarPage("Water Contamination", tags$style(
                  ".navbar-nav li a {
        font-size: 20px;
        font-weight: bold;
      }
      "),
                  tabPanel("What are PFAS?", tags$style(
                      ".navbar-nav li a {
        font-size: 20px;
        font-weight: normal;
      }
      "),
                    "PFAS background info!"
                  ),
                  tabPanel(
                    "Sampling Sites", tags$style(
                      ".navbar-nav li a {
        font-size: 20px;
        font-weight: normal;
      }
      "),
                    
                  ),
                  tabPanel(
                "PFAS in Washington County Superfund sites", tags$style(
                  ".navbar-nav li a {
        font-size: 20px;
        font-weight: normal;
      }
      "),
                  fluidRow(
                    column(
                    h4("Please select a PFAS to view most recent sample results"),
                    selectInput(inputId = "commonName",
                      label = "Chemical Name",
                      choices = unique(superfund_washington$commonName)),
                    width = 6
                    ),
                    column(
                    h4("Please select a PFAS to view time series"),
                    selectInput(inputId = "common_name",
                      label = "Chemical Name",
                      choices = unique(pfas_super_washington$commonName)),
                    sliderInput(
                      inputId = "year",
                      label = "Sample Year",
                      min = min(pfas_super_washington$year),
                      max = max(pfas_super_washington$year),
                      sep = "",
                      value = 2005,
                      ticks = FALSE,
                      step = 1,
                      animate = animationOptions(interval = 300, loop = TRUE, playButton = c("Play Animation"))
                    ),
                    width = 6
                    )
                  ),
                fluidRow(
                  column(
                  plotOutput(outputId = "pfasplot"), 
                  width = 6
                  ),
                  column(
                  plotOutput(outputId = "pfasplottemporal",
                             height = 600),
                  width = 6
                  )
                  ),
                fluidRow(
                  p("These data are collected by the MPCA. More information can be found at https://www.pca.state.mn.us/air-water-land-climate/cleaning-up-minnesota-superfund-sites.")
                )
                ),
                tabPanel(
                  "Other Contaminants", tags$style(
                    ".navbar-nav li a {
        font-size: 20px;
        font-weight: normal;
      }
      "),
                  "Look at other contaminants for Hennepin and Ramsey"
                )
)
)

server <- function(input, output) {
  
  counties_cropped <- st_crop(counties, xmin = -93.2, xmax=-92.7, ymin = 44.7, ymax=45.35)
  output$pfasplot <- renderPlot(
      ggplot() +
      geom_sf(data = counties_cropped %>% filter(name == "Washington"), color = "navajowhite", fill = "ivory", size = 1)+
      geom_sf(data = (superfund_washington %>% filter(commonName == input$commonName, DETECT_FLAG == "Y") 
                      %>% mutate(over_limit = RESULT_NUMERIC > MIN_ACTION_LEVEL, 
                                 over_limit = case_when(is.na(over_limit) ~ FALSE, TRUE ~ over_limit))), 
              size= 0.9, aes(color = RESULT_NUMERIC), alpha = 0.5)+
        labs(color = "Detected (ug/L)", title = paste("Detected", input$commonName, "in Washington \n County Superfund sites"))+
        scale_color_gradient(low = "pink", high = "darkred")+
        theme_classic() +
        theme(axis.ticks = element_blank(), 
              axis.text = element_blank(), 
              axis.line = element_blank(),
              title = element_text(size = 20),
              legend.title = element_text(size=16),
              legend.text = element_text(size=14),
              text = element_text(family = "AppleGothic")),
      height = 600
  )
  output$pfasplottemporal <- renderPlot(
    ggplot() + 
      geom_sf(data = counties_cropped %>% filter(name == "Washington"), color = "navajowhite", fill = "ivory", size = 1)+
      geom_sf(data = (pfas_super_washington %>%
                filter(commonName == input$common_name, year == input$year)), aes(color = perc_detected)) +
      labs(color = "% Samples Above Detection Limit", title = paste(input$common_name, "over time in Washington \n County Superfund sites"))+
      scale_color_gradient(low = "pink", high = "darkred")+
      theme_classic() +
      theme(axis.ticks = element_blank(), 
            axis.text = element_blank(), 
            axis.line = element_blank(),
            title = element_text(size = 20),
            legend.title = element_text(size=16),
            legend.text = element_text(size=14),
            text = element_text(family = "AppleGothic"))
  )
}


shinyApp(ui = ui, server = server)
