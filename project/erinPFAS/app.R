library(tidyverse)
library(skimr)
library(readxl)
library(sf)
library(USAboundaries)
library(shiny)
library(shinythemes)
library(lubridate)
library(DT)
library(plotly)

###########################################################################################
## DATA LOADING 

counties <- us_counties(resolution = "high", states = c("Minnesota", "Wisconsin")) %>%
  st_transform(crs = 6783)
rivers <- read_sf('../../data/shp_water_lakes_rivers') %>%
  st_transform(crs = 6783)

load("../../data/superfund_site_data/superfund.rds")

superfund_loc <- superfund %>%
   filter(!is.na(LATITUDE), !is.na(LONGITUDE), LATITUDE > 44, LATITUDE < 46, LONGITUDE < -92, LONGITUDE > -94) %>%
   mutate(year = as.numeric(substr(SAMPLE_DATE, 7, 11)))%>%
    mutate(RESULT_NUMERIC = case_when(RESULT_UNIT == "ng/L" ~ RESULT_NUMERIC/1000, 
                                    TRUE ~ RESULT_NUMERIC)) %>%
    mutate(above_level = case_when(DETECT_FLAG == "Y" & RESULT_NUMERIC > MIN_ACTION_LEVEL ~ "Yes", 
                                 TRUE ~ "No"))

superfund_loc <- st_as_sf(superfund_loc, coords = c("LONGITUDE", "LATITUDE"), crs = 6783)

site_information <- superfund %>% distinct(site, county) %>%
  mutate(Contaminants = c("TCE and its degradation products", "PCE, PFAS, TCE", "TCE and its degradation products, PFAS", "PCP, PAHs, dioxins/furans", "PCP and its degradation products", "TCE and its degradation products", "PCE, TCE, DCE, VC", "Arsenic", "SVOCs, metals, organochlorine pesticides, VOCs, PCBs, DRO, PFAS", "TCE", "PCE and its degradation products", "VC, VOCs", "PCE and its degradation products", "Chlorinated VOCs, TCE and its degradation products", "PCE and its degradation products", "PCP, arsenic, chromium, PAHs, dioxins", "BTEX, PAHs", "BTEX, trimethylbenzenes, isopropyl ether, PFAS", "PFAS", "PCE and its degradation products", "chlorinated solvents, including TCE, cis-DCE, and vinyl chloride", "PAHs", "VOCs including PCE, TCE, DCE, VC", "PCE and its degradation products, including TCE, cis-1,2-DCE, and VC", "TCE", "TCE and its degradation products", "TCE, cyanide, and several heavy metals", "VOCs, lead, PCBs, PAHs", "PFAS", "TCE and its degradation products", "PCE, TCE, PFAS", "PCE, TCE", "PCE", "PCE and its degradation products"),
         Status = c("Identified", "Identified", "Response Selected", "Complete", "Response Implemented", "Identified", "Response Selected", "Response Implemented", "Investigation", "Response Selected", "Identified", "Response Selected", "Identified", "Response Implemented", "Identified", "Response Implemented", "Response Implemented", "Response Implemented", "Response Implemented", "Response Implemented", "Investigation", "Investigation", "Identified", "Response Implemented", "Identified", "Identified", "Response Implemented", "Investigation", "Response Implemented", "Response Implemented", "Investigation", "Identified", "Identified", "Response Implemented"), 
         Link = c("https://webapp.pca.state.mn.us/cleanup/search/superfund?text=Arcade%20%26%20Hawthorne%20Ave%20E&siteId=213687-AREA0000000001", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=ASHLAND%20OIL%20-%20PARK%20PENTA&siteId=192580-AREA0000000004", "https://webapp.pca.state.mn.us/cleanup/search/superfund?siteId=21451-AREA0000000002", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=BELL%20LUMBER%20%26%20POLE%20COMPANY&siteId=196669-AREA0000000002", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=Cedar%20Services&siteId=187060-AREA0000000004", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=Centerville%20Road%20Site&siteId=197192-AREA0000000002", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=CHEMICAL%20MARKETING%20CORP%20OF%20AMERICA&siteId=34000-AREA0000000003", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=CMC%20Heartland%20Lite%20Yard&siteId=195132-AREA0000000002", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=Fish%20Hatchery%20Dump&siteId=208492-AREA0000000003", "https://webapp.pca.state.mn.us/cleanup/search/superfund?siteId=102356-AREA0000000002", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=Gold%20Eagle%20Cleaners%20–%20Richfield&siteId=108010-AREA0000000003", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=HIGHWAY%2096%20DUMP&siteId=190060-AREA0000000002", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=Hmong%20American%20Shopping%20Center%2FPilgrim&siteId=104343-AREA0000000001", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=LAKELAND%20GROUND%20WATER%20CONTAMINATION&siteId=195554-AREA0000000001", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=55th%20St%20%26%20Lyndale%20Ave%20S&siteId=213688-AREA0000000001", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=Macgillis%20and%20Gibbs%20Waste%20Site&siteId=195905-AREA0000000002", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=MINNEGASCO%20%20OU-2%20Groundwater&siteId=186829-AREA0000000001", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=3M%20OAKDALE%20DUMP%20SITES&siteId=185391-AREA0000000003", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=PIG%27S%20EYE%20LANDFILL&siteId=197401-AREA0000000002", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=Pilgrim%20Cleaners&siteId=19408-AREA0000000001", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=Precision%20Plating%20Inc&siteId=4292-AREA0000000002", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=REILLY%20TAR%20%26%20CHEM%20SAINT%20LOUIS%20PARK&siteId=193787-AREA0000000002", "https://webapp.pca.state.mn.us/cleanup/search/superfund?siteId=189593-AREA0000000002", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=SCHLOFF%20CHEMICAL%20(SF)&siteId=193207-AREA0000000002", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=SOUTHEAST%20HENNEPIN%20AREA%20GROUNDWATER%20%26%20VAPOR%20SITE&siteId=213685-AREA0000000001", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=SPRING%20PARK%20MUNICIPAL%20WELLS&siteId=88053-AREA0000000003", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=SUPERIOR%20PLATING%20INC%20(SF)&siteId=2555-AREA0000000004", "https://webapp.pca.state.mn.us/cleanup/search/superfund?siteId=47112-AREA0000000014", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=3M%20CHEMOLITE&siteId=1163-AREA0000000005", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=TONKA%20MAIN%20PLANT&siteId=22434-AREA0000000002", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=UNIVERSAL%20PLATING&siteId=23769-AREA0000000006", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=UNIVERSITY%20AVE%20%26%20PASCAL%20ST&siteId=213689-AREA0000000001", "https://webapp.pca.state.mn.us/cleanup/search/superfund?siteId=213683-AREA0000000001", "https://webapp.pca.state.mn.us/cleanup/search/superfund?text=WHITE%20WAY%20CLEANERS%20(SF)&siteId=103215-AREA0000000002")) %>%
  mutate(Link = map(Link, ~ htmltools::a(href = .x, "Website")),
        Link = map(Link, ~ gt::html(as.character(.x))))
###############################################################################################

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage("Water Contamination", tags$style(
                  ".navbar-nav li a {
        font-size: 20px;
        font-weight: bold;
      }
      "),
                  tabPanel("Background: Superfund Sites & PFAS", tags$style(
                    ".navbar-nav li a {
        font-size: 10px;
        font-weight: normal;
      }
      "),
                    titlePanel("Background Information: Superfund Sites and PFAS"),
                      mainPanel(
                        h3("What is a superfund site?"),
                        p("A ", strong("superfund site"), "is an area where where hazardous waste has been spilled or dumped and where contamination poses an actual or potential threat to public health or the environment."),
                        p("Responsible parties are legally responsible for the cleanup. These responsible parties can include past or present property owners, operators, waste haulers, and/or generators who dumped material on the site."),
                        h3("What is PFAS?"),
                        p("Also bad")
                      )
                  ),
                  tabPanel(
                    "Locating Superfund Sites", tags$style(
                      ".navbar-nav li a {
        font-size: 10px;
        font-weight: normal;
      }
      "),
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
                                                        label = "Filter to depict rough site locations for all sites harmed by a particular contaminant of interest.", 
                                                        value = "",
                                                        placeholder = "PFAS"
                                                      ),
                                                      plotlyOutput("superfundSamplePlot", height = "800px")
                    )),
                    fluidRow(
                      
                      h4("The following table provides additional information about each of these 34 superfund sites (as of April 15, 2023) and is filtered according to the inital widgets."),
                      
                      dataTableOutput("siteTable")
                    )         
                  ),

                  tabPanel(
                    "Superfund Sample Information", tags$style(
                      ".navbar-nav li a {
        font-size: 10px;
        font-weight: normal;
      }
      "),
                    h4("The following map provides additional information about the number of samples taken of different analyte groups across the superfund sites."),
                    
                    fluidRow(
                      checkboxGroupInput(
                        inputId = "detectFlag",
                        label = "Sample detected?",
                        choices = unique(superfund$DETECT_FLAG),
                        selected = c("Y"),
                        inline = TRUE
                      ),
                      checkboxGroupInput(
                        inputId = "aboveLimit",
                        label = "Sample above MDH limit? Note that not all analytes have a MDH limit.",
                        choices = unique(superfund_loc$above_level),
                        selected = c("Yes", "No"),
                        inline = TRUE
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
                        step = 1, 
                        sep = ""
                      ), 
                      plotlyOutput("contaminantPlot", height = "800px")         
                  ),
)
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dataInputMap <- reactive({
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
    return(superfund_loc_points)
  })
  
  dataInputTable <- reactive({
    if(input$chemical == ''){ chemInput = '.*'}else{chemInput = input$chemical}
    site_information %>% filter(Status %in% input$stage) %>% filter(str_detect(Contaminants, pattern = chemInput))
  })
  
  counties_cropped <- st_crop(counties, xmin = -93.7, xmax=-92.6, ymin = 44.7, ymax=45.2)

    output$superfundSamplePlot <- renderPlotly({
      
      ggplotly(
        ggplot()+
          geom_sf(data = counties_cropped, color = "navajowhite", fill = "ivory", size = 0.5)+
          geom_sf(data = dataInputMap() %>% st_jitter(amount = 0.02), aes(color = Site), alpha = 0.5, size = 0.8)+
          labs(title = "Samples from superfund sites in Washington, Hennepin, and Ramsey Counties")+
          theme(legend.position = "none", 
                axis.line = element_blank(), 
                axis.text = element_blank(), 
                axis.ticks = element_blank(), 
                plot.title = element_text(hjust= 0.5)))
     })
    
    output$siteTable <- DT::renderDataTable(
              dataInputTable(), 
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
        mutate(RESULT_NUMERIC = case_when(RESULT_UNIT == "ng/L" ~ RESULT_NUMERIC/1000, 
                                          TRUE ~ RESULT_NUMERIC)) %>%
        mutate(above_level = case_when(DETECT_FLAG == "Y" & RESULT_NUMERIC > MIN_ACTION_LEVEL ~ "Yes", 
                                       TRUE ~ "No"))%>%
        filter(!is.na(LATITUDE), !is.na(LONGITUDE), LATITUDE > 44, LATITUDE < 46, LONGITUDE < -92, LONGITUDE > -94) %>%
        filter(DETECT_FLAG %in% input$detectFlag, ANALYTE_GROUP_DESC %in% input$chemicalGroup) %>%
        mutate(DETECT_FLAG = case_when(DETECT_FLAG == "Y" ~ "Yes", 
                                       TRUE ~ "No")) %>%
        mutate(year1 = as.numeric(substr(SAMPLE_DATE, 7, 11))) %>%
        filter(year1 >= input$year2[1] & year1 <= input$year2[2]) %>%
        filter(above_level %in% input$aboveLimit) %>%
        mutate(LONGITUDE_short = round(LONGITUDE, 2), 
               LATITUDE_short = round(LATITUDE, 2)) %>%
        group_by(LONGITUDE_short, LATITUDE_short, site) %>%
        rename("Site" = "site") %>%
        summarize(Samples = n())
      superfund_loc_points2 <- st_as_sf(superfund_loc_points2, coords = c("LONGITUDE_short", "LATITUDE_short"), crs = 6783)
      
      ggplotly(
      ggplot()+
        geom_sf(data = counties_cropped, color = "navajowhite", fill = "ivory", size = 0.5)+
        geom_sf(data = superfund_loc_points2, alpha = 0.5, aes(size = Samples, color = Site))+
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
