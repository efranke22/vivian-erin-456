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

min_action_levels <- data.frame(commonName = c("PFHxS", "PFHxA", "PFOA", "PFBA", "PFBS", "PFOS", "PFPeA"), action_level = c(0.027, NA, 0.035, 7.000, 7.000, 0.027, NA))

pfas_super <- read_csv("../../data/pfas_superfund.csv") %>%
  filter(LATITUDE != "NA", LONGITUDE != "NA") %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 6783)

pfas_health_levels <- pfas7 %>% 
  left_join(min_action_levels, by = "commonName") %>%
  mutate(result_num = ifelse(DETECT_FLAG == "Y" & UNIT == "ng/L", `RESULT NUMERIC`/1000, ifelse(DETECT_FLAG == "Y" & UNIT != "ng/L", `RESULT NUMERIC`, 0.00)), above_level = ifelse(DETECT_FLAG == "Y" & result_num > action_level, "Above Health Action Level", ifelse(DETECT_FLAG == "Y" & result_num < action_level, "Below Health Action Level", "Not Detected")), year = year(`SAMPLE DATE`)) %>%
  filter(UNIT != "NA", `FACILITY TYPE` == "Superfund", LATITUDE != "NA", LONGITUDE != "NA")

pfas_health_sf <- pfas_health_levels %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 6783)

#LATITUDE <= 45.298915338179285, LATITUDE >= 44.7471734793455

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
                      h4("Please select a PFAS to view time series"),
                      selectInput(inputId = "common_name",
                                  label = "Chemical Name",
                                  choices = unique(pfas_super$commonName)),
                      sliderInput(
                        inputId = "year",
                        label = "Sample Year",
                        min = min(pfas_super$year),
                        max = max(pfas_super$year),
                        sep = "",
                        value = 2005,
                        ticks = FALSE,
                        step = 1,
                        animate = animationOptions(interval = 300, loop = TRUE, playButton = c("Play Animation"))
                      )
                    ),
                    fluidRow(
                      plotOutput(outputId = "pfasplottemporal",
                                 height = 600)
                    ),
                    fluidRow(
                      h4("Please select a PFAS to view time series"),
                      selectInput(inputId = "Common_Name",
                                  label = "Chemical Name",
                                  choices = unique(pfas_health_sf$commonName)),
                      sliderInput(
                        inputId = "Year",
                        label = "Sample Year",
                        min = min(2005),
                        max = max(2022),
                        sep = "",
                        value = 2005,
                        ticks = FALSE,
                        step = 1,
                        animate = animationOptions(interval = 300, loop = TRUE, playButton = c("Play Animation"))
                      )
                    ),
                    fluidRow(
                      plotOutput(outputId = "pfasplottemporal2",
                                 height = 600)
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
  
  output$pfasplottemporal <- renderPlot(
    ggplot() + 
      geom_sf(data = counties_cropped #%>% filter(name == "Washington")
              , color = "navajowhite", fill = "ivory", size = 1)+
      geom_sf(data = (pfas_super %>%
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
  
  output$pfasplottemporal2 <- renderPlot(
    ggplot() + 
      geom_sf(data = counties_cropped #%>% filter(name == "Washington")
              , color = "navajowhite", fill = "ivory", size = 1)+
      geom_sf(data = (pfas_health_sf %>%
                        st_crop(xmin = -93.2, xmax=-92.7, ymin = 44.7, ymax=45.35) %>%
                        filter(commonName == input$Common_Name, year == input$Year, above_level != "NA")), aes(color = above_level)) +
      labs(color = "% Samples Above Detection Limit", title = paste(input$common_name, "over time in Washington \n County Superfund sites"))+
      scale_color_manual(values=c("#de2d26","#fc9272","#fee0d2"))+
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