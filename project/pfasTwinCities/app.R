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

pfas <- read_xlsx("../../data/Twin Cities PFAS Data Ticket WO00000021224685.xlsx", sheet=2)

pfas7 <- pfas %>%
  filter(CHEMICAL_NAME %in% c("Perfluorobutanoic acid", "Perfluorooctanoic acid", "Perfluoropentanoic acid", "Perfluorohexanoic acid", "Perfluorooctane sulfonate", "Perfluorohexane sulfonate", "Perfluorobutane sulfonate")) %>%
  mutate(commonName = case_when(CHEMICAL_NAME == "Perfluorobutanoic acid" ~ "PFBA", 
                                CHEMICAL_NAME == "Perfluorooctanoic acid" ~ "PFOA", 
                                CHEMICAL_NAME == "Perfluoropentanoic acid" ~ "PFPeA",
                                CHEMICAL_NAME == "Perfluorohexanoic acid" ~ "PFHxA", 
                                CHEMICAL_NAME == "Perfluorooctane sulfonate" ~ "PFOS", 
                                CHEMICAL_NAME == "Perfluorohexane sulfonate" ~ "PFHxS", 
                                CHEMICAL_NAME == "Perfluorobutane sulfonate" ~ "PFBS")) %>%
  mutate(LOC_TYPE_2 = case_when(LOC_TYPE_2 == "Well-DOmestic" ~ "Well-Domestic", 
                                TRUE ~ LOC_TYPE_2))

threeM <- read_csv("../../data/superfund_site_data/3m_chemolite.csv") %>%
  mutate(site = "3M Chemolite", county = "Washington", SYS_LOC_CODE = as.numeric(SYS_LOC_CODE))
oakdale <- read_csv("../../data/superfund_site_data/oakdale.csv") %>%
  mutate(site = "3M Oakland")
ashland <- read_csv("../../data/superfund_site_data/ashland.csv") %>%
  mutate(site = "Ashland Oil - Park Penta")
bayport <- read_csv("../../data/superfund_site_data/bayport.csv") %>%
  mutate(site = "Baytown Township")
lakeland <- read_csv("../../data/superfund_site_data/lakeland.csv") %>%
  mutate(site = "Lakeland")

superfund_washington <- bind_rows(threeM, oakdale, ashland, bayport, lakeland) %>%
  filter(!is.na(LATITUDE), LATITUDE > 40, LONGITUDE > -93, LONGITUDE < -91) %>%
  filter(ANALYTE_NAME %in% c("Perfluorohexanoic acid (PFHxA)", "Perfluorohexanesulfonate (PFHxS)", "Perfluorooctanesulfonate (PFOS)", "Perfluorooctanoic acid (PFOA)", "Perfluoropentanoic acid (PFPeA)", "Perfluorobutanesulfonate (PFBS)", "Perfluorobutanoic acid (PFBA)")) %>%
  mutate(commonName = case_when(ANALYTE_NAME == "Perfluorobutanoic acid (PFBA)" ~ "PFBA", 
                                ANALYTE_NAME == "Perfluorooctanoic acid (PFOA)" ~ "PFOA", 
                                ANALYTE_NAME == "Perfluoropentanoic acid (PFPeA)" ~ "PFPeA",
                                ANALYTE_NAME == "Perfluorohexanoic acid (PFHxA)" ~ "PFHxA", 
                                ANALYTE_NAME == "Perfluorooctanesulfonate (PFOS)" ~ "PFOS", 
                                ANALYTE_NAME == "Perfluorohexanesulfonate (PFHxS)" ~ "PFHxS", 
                                ANALYTE_NAME == "Perfluorobutanesulfonate (PFBS)" ~ "PFBS"), 
         RESULT_NUMERIC = case_when(RESULT_UNIT == "ng/L" ~ RESULT_NUMERIC/ 1000, 
                                    TRUE ~ RESULT_NUMERIC))


superfund_washington <- st_as_sf(superfund_washington, coords = c("LONGITUDE", "LATITUDE"), crs = 6783)

pfas_super_washington <- read_csv("../../data/pfas_superfund.csv") %>%
  filter(LATITUDE != "NA", LONGITUDE != "NA", LATITUDE <= 45.298915338179285, LATITUDE >= 44.7471734793455) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 6783)

ui <- fluidPage(theme = shinytheme("flatly"), 
                titlePanel("PFAS in Washington County Superfund sites"),
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
