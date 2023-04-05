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
  mutate(site = "3M Chemolite")
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

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"), 
                titlePanel("PFAS in Washington County Superfund sites"),
                  mainPanel(
                    h4("Please select a PFAS to detection levels "),
                    selectInput(inputId = "commonName",
                      label = "Chemical Name",
                      choices = unique(superfund_washington$commonName)),
                    submitButton(text = "Submit", ),
                    plotOutput(outputId = "pfasplot"), 
                    p("These data are collected by the MPCA. More information can be found at https://www.pca.state.mn.us/air-water-land-climate/cleaning-up-minnesota-superfund-sites."),
                    width = 6)
                )

server <- function(input, output) {
  counties_cropped <- st_crop(counties, xmin = -93.2, xmax=-92.7, ymin = 44.7, ymax=45.35)
  output$pfasplot <- renderPlot(
      ggplot() +
      geom_sf(data = counties_cropped, color = "navajowhite", fill = "ivory", size = 0.5)+
      geom_sf(data = (superfund_washington %>% filter(commonName == input$commonName, DETECT_FLAG == "Y") 
                      %>% mutate(over_limit = RESULT_NUMERIC > MIN_ACTION_LEVEL, 
                                 over_limit = case_when(is.na(over_limit) ~ FALSE, TRUE ~ over_limit))), 
              size= 0.9, aes(color = RESULT_NUMERIC), alpha = 0.5)+
        labs(color = "Detected (ug/L)", title = paste("Detected", input$commonName, "in Washington County Superfund sites"))+
        scale_color_gradient(low = "pink", high = "darkred")+
        theme(axis.ticks = element_blank(), 
              axis.text = element_blank(), 
              axis.line = element_blank())
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
