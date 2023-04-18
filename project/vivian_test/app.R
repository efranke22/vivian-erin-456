library(tidyverse)
library(skimr)
library(readxl)
library(sf)
library(USAboundaries)
library(shiny)
library(shinythemes)
library(lubridate)

pfas7 <- read_csv("../data/pfas7.csv")

min_action_levels <- data.frame(commonName = c("PFHxS", "PFHxA", "PFOA", "PFBA", "PFBS", "PFOS", "PFPeA"), action_level = c(0.027, NA, 0.035, 7.000, 7.000, 0.027, NA))

pfas_health_levels <- pfas7 %>% 
  left_join(min_action_levels, by = "commonName") %>%
  mutate(result_num = ifelse(UNIT == "ng/L", `RESULT NUMERIC`/1000, `RESULT NUMERIC`),above_level = ifelse(DETECT_FLAG == "Y", `RESULT NUMERIC` > action_level, FALSE)) %>%
  filter(UNIT != "NA", `FACILITY TYPE` == "Superfund")

pfas_health_levels %>%
  filter(commonName == "PFOA", DETECT_FLAG == "Y", result_num < 10*action_level) %>% 
  group_by(`FACILITY NAME`) %>%
  summarize(mean_result_num = mean(result_num), above_level = mean_result_num > action_level) %>%
  ggplot(aes(x = sample_day, y = mean_result_num)) +
  geom_point(aes(color = above_level)) +
  geom_smooth(aes(color = above_level), se = FALSE) +
  theme_classic()

ui <- fluidPage(theme = shinytheme("flatly"),
                
                # Application title
                titlePanel("Locating Superfund Sites: Washington, Hennepin, and Ramsey Counties"),
                
                HTML("<br>"),
                
                fluidRow(column(width = 1),column(width = 11,
                                                  sliderInput(
                                                    inputId = "year",
                                                    label = "Select Date Range",
                                                    min = min(superfund_loc$year),
                                                    max = max(superfund_loc$year),
                                                    value = c(2010, 2020),
                                                    ticks = FALSE,
                                                    step = 1, 
                                                    sep= ""
                                                  ),
                                                  checkboxGroupInput(
                                                    inputId = "stage", 
                                                    label = "Stage of Remediation Process",
                                                    choices = c("Identified", "Investigation", "Response Selected", "Response Implemented", "Complete"),
                                                    selected = unique(site_information$Status),
                                                    inline = TRUE
                                                  ),
                                                  textInput(
                                                    inputId = "chemical",
                                                    label = "Filter to show all samples from sites harmed by a particular contaminant of interest", 
                                                    value = "",
                                                    placeholder = "PFAS"
                                                  ),
                                                  plotlyOutput("superfundSamplePlot")
                )),
                fluidRow(
                  dataTableOutput("siteTable")
                ),
                fluidRow(
                  checkboxInput(
                    inputId = "detectFlag",
                    label = "Only show samples where contaminant is detected",
                    value = TRUE
                  ),
                  selectInput(
                    inputId = "chemicalGroup",
                    label = "Select an analyte group", 
                    selected = c("Perfluorochemicals"),
                    choices = c("Inorganics", "Metals", "Other Organics", "Perfluorochemicals", "Pesticides and PCBs", "Semi-Volatile Organics", "Volatile Organics"), 
                    multiple = TRUE
                  ),
                  sliderInput(
                    inputId = "year2",
                    label = "Select Date Range",
                    min = min(superfund_loc$year),
                    max = max(superfund_loc$year),
                    value = c(2010, 2020),
                    ticks = FALSE,
                    step = 1
                  ), 
                  plotlyOutput("contaminantPlot")
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  counties_cropped <- st_crop(counties, xmin = -93.7, xmax=-92.6, ymin = 44.7, ymax=45.2)
  
  output$superfundSamplePlot <- renderPlotly({
    
    if(input$chemical == ''){ chemInput = '.*'}else{chemInput = input$chemical}
    
    superfund_loc_points <- superfund %>%
      left_join(site_information, by = c("county", "site")) %>%
      filter(!is.na(LATITUDE), !is.na(LONGITUDE), LATITUDE > 44, LATITUDE < 46, LONGITUDE < -92, LONGITUDE > -94,
             Status %in% input$stage) %>%
      mutate(year1 = as.numeric(substr(SAMPLE_DATE, 7, 11))) %>%
      filter(year1 >= input$year[1] & year1 <= input$year[2], str_detect(Contaminants, pattern = chemInput)) %>%
      group_by(LATITUDE, LONGITUDE) %>%
      mutate(Samples = n()) %>%
      distinct(LATITUDE, LONGITUDE, Samples, site) %>%
      rename("Site" = "site") %>%
      arrange(desc(Samples)) %>%
      filter(!is.na(LATITUDE), !is.na(LONGITUDE), LATITUDE > 44, LATITUDE < 46, LONGITUDE < -92, LONGITUDE > -94)
    superfund_loc_points <- st_as_sf(superfund_loc_points, coords = c("LONGITUDE", "LATITUDE"), crs = 6783)
    
    ggplotly(
      ggplot()+
        geom_sf(data = counties_cropped, color = "navajowhite", fill = "ivory", size = 0.5)+
        geom_sf(data = superfund_loc_points, aes(color = Site, size = Samples), alpha = 0.5)+
        labs(title = "Samples from superfund sites in Washington, Hennepin, and Ramsey Counties")+
        theme(legend.position = "none", 
              axis.line = element_blank(), 
              axis.text = element_blank(), 
              axis.ticks = element_blank(), 
              plot.title = element_text(hjust= 0.5)))
  })
  
  output$siteTable <- DT::renderDataTable(
    site_information %>% filter(Status %in% input$stage), 
    options = list(paging = FALSE,    ## paginate the output
                   pageLength = 34,  ## number of rows to output for each page
                   scrollX = TRUE,   ## enable scrolling on X axis
                   scrollY = TRUE,   ## enable scrolling on Y axis
                   autoWidth = TRUE ## use smart column width handling
    ),
    selection = 'single', ## enable selection of a single row
    filter = 'top',              ## include column filters at the bottom
    rownames = TRUE                ## don't show row numbers/names
  )
  output$contaminantPlot <- renderPlotly({
    
    superfund_loc_points2 <- superfund %>%
      left_join(site_information, by = c("county", "site")) %>%
      filter(!is.na(LATITUDE), !is.na(LONGITUDE), LATITUDE > 44, LATITUDE < 46, LONGITUDE < -92, LONGITUDE > -94) %>%
      mutate(DETECT_FLAG = case_when(DETECT_FLAG == "Y" ~ TRUE, 
                                     TRUE ~ FALSE)) %>%
      filter(DETECT_FLAG == input$detectFlag, ANALYTE_GROUP_DESC %in% input$chemicalGroup) %>%
      mutate(year1 = as.numeric(substr(SAMPLE_DATE, 7, 11))) %>%
      filter(year1 >= input$year2[1] & year1 <= input$year2[2])
    superfund_loc_points2 <- st_as_sf(superfund_loc_points2, coords = c("LONGITUDE", "LATITUDE"), crs = 6783)
    
    ggplotly(
      ggplot()+
        geom_sf(data = counties_cropped, color = "navajowhite", fill = "ivory", size = 0.5)+
        geom_sf(data = superfund_loc_points2, alpha = 0.5)+
        labs(title = "Samples from superfund sites in Washington, Hennepin, and Ramsey Counties")+
        theme(legend.position = "none", 
              axis.line = element_blank(), 
              axis.text = element_blank(), 
              axis.ticks = element_blank(), 
              plot.title = element_text(hjust= 0.5)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)