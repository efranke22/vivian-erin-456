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
library(dplyr)

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

############################################################################################# vivian data

pfas7 <- read_csv("../../data/pfas7.csv")

min_action_levels <- data.frame(commonName = c("PFHxS", "PFHxA", "PFOA", "PFBA", "PFBS", "PFOS", "PFPeA"), action_level = c(0.027, NA, 0.035, 7.000, 7.000, 0.027, NA))

pfas_super <- read_csv("../../data/pfas_superfund.csv") %>%
  filter(LATITUDE != "NA", LONGITUDE != "NA") %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 6783)

pfas_health_levels <- pfas7 %>% 
  left_join(min_action_levels, by = "commonName") %>%
  mutate(result_num = ifelse(DETECT_FLAG == "Y" & UNIT == "ng/L", `RESULT NUMERIC`/1000, ifelse(DETECT_FLAG == "Y" & UNIT != "ng/L", `RESULT NUMERIC`, 0.00)), above_level = factor(ifelse(DETECT_FLAG == "Y" & result_num > action_level, "Above Health Action Level", ifelse(DETECT_FLAG == "Y" & result_num < action_level, "Below Health Action Level", "Not Detected"))), year = year(`SAMPLE DATE`)) %>%
  filter(UNIT != "NA", `FACILITY TYPE` == "Superfund", LATITUDE != "NA", LONGITUDE != "NA", above_level != "NA")


pfas_health_sf <- pfas_health_levels %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 6783) %>%
  st_crop(xmin = -93.7, xmax=-92.6, ymin = 44.7, ymax=45.2)

###############################################################################################

ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage("Contamination in the Twin Cities", tags$style(
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
                     
                        fluidRow(
                        column(
                        h3("What is a superfund site?"),
                        p("A ", strong("superfund site"), "is an area where where hazardous waste has been spilled or dumped and where contamination poses an actual or potential threat to public health or the environment."),
                        h4("How does cleanup work?"),
                        p("Responsible parties are legally responsible for the cleanup. These responsible parties can include past or present property owners, operators, waste haulers, and/or generators who dumped material on the site."),
                        p("When there is no responsible party to pay for cleanup, the site may be placed on the Minnesota Permanent List of Priorities, making it eligible for state cleanup dollars. Sites may recieve additional federal funding for cleanup if a successful request is made by the MPCA to list the site on the federal superfund", tags$a(href = "https://www.epa.gov/superfund/superfund-national-priorities-list-npl", "National Priorities List."), " Currently, Minnesota has 25 sites on this list."),
                        h4("What is an example of a superfund site in Minnesota?"),
                        p("The image below shows the 3M production facility in Cottage Grove (Washington County). 3M has used these grounds for years as a disposal site for chemical compounds, which are contaminating the Mississippi River. Photo Credit to MPR/Bill Alkofer."),
                        img(src = "threeM.jpg", height = 300, width = 300),
                        width = 6
                          ),
                        column(
                        h3("What is PFAS?"),
                        p("PFAS is the umbrella term for a large group of over 1000 synthetic chemicals called per- and polyfluoroalkyl substances. PFAS are widely found in consumer products since the 1940s, most notably in nonstick cooking pans, food packaging, and water-repellent materials. PFAS from manufacturing and waste disposal do not break down in the environment; instead, PFAS travels through water and soil, leading to bioaccumulation in organisms."),
                        h4("Why do PFAS matter?"),
                        p("Recent studies have found that most people in the United States have been exposed to PFAS through sources like contaminated drinking water and the use of products containing PFAS. More research is needed to understand the specific health impacts of exposure to different levels of PFAS, but PFAS in blood is generally linked to immune problems, thyroid issues, delayed development, and increased risks of certain cancers."),
                        img(src = "infographic.png", height = 300, width = 300),
                        width = 6
                        )
                      ),
      h3("Sources"),
      p("https://www.pca.state.mn.us/air-water-land-climate/cleaning-up-minnesota-superfund-sites"),
      p("https://www.mprnews.org/story/2007/05/11/contamdeal")
                      
                  ),
                  tabPanel(
                    "Research Questions & Data", tags$style(
                      ".navbar-nav li a {
                  font-size: 10px;
                  font-weight: normal;
                  }
                  "),
                  h3("Research Questions"),
                  p("It is clear that superfund sites should be cleaned up as soon as possible. Among other harmful chemicals, they contain PFAS which has both environmental and health hazards. To learn more about superfund sites and PFAS in the Twin Cities, we are asking the following questions:"),
                  p("1. Where are superfund sites located in the Twin Cities area?"),
                  p("2. How have they progressed in containing harmful chemicals, specifically PFAS?"),
                  h3("Data"),
                  p("Our data comes from the Minnesota Pollution Control Agency (MPCA).  Much of the background information about superfund sites is taken from their website, which you can visit", tags$a(href="https://www.pca.state.mn.us/air-water-land-climate/cleaning-up-minnesota-superfund-sites", "here."), " We downloaded all recorded samples from every superfund site in Hennepin, Ramsey, and Washington Counties.  Chemicals sampled range from PFAS to pesticides and PBCs, metals, and beyond.  Each superfund site's data needing to be individually downloaded and the datasets can be combined fairly easily.  There are some instances in which the units should be changed (nanograms to micrograms) or the county is mislabeled, but otherwise the data is fairly clean.  Relevant field include sample latitude and longitude, county, facility name, location type, analysis name, analyte name, analyte group, result numeric, whether the result was detected,  and Minnesota action level."),
                  img(src = "mpca.jpg", height = 400, width = 600),
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
),
tabPanel(
  "PFAS levels in Superfund sites", tags$style(
    ".navbar-nav li a {
        font-size: 20px;
        font-weight: normal;
      }
      "),
  fluidRow(
    h4("Please select a PFAS to view time series"),
    selectInput(inputId = "Common_Name",
                label = "Chemical Name",
                choices = c("PFHxS","PFOA", "PFBA", "PFBS", "PFOS")),
    sliderInput(
      inputId = "Year",
      label = "Sample Year",
      min = min(pfas_health_sf$year),
      max = max(pfas_health_sf$year),
      sep = "",
      value = 2004,
      ticks = FALSE,
      step = 1,
      animate = animationOptions(interval = 500, loop = TRUE, playButton = c("Play Animation"))
    )
    ),
  fluidRow(
    plotOutput(outputId = "pfasplottemporal",
               height = 600)
  ),
  fluidRow(
    column(
      selectInput(inputId = "location",
                  label = "Select a sampling site",
                  selected = c("East Metro PFAS"),
                  choices = c("3M Chemolite / Cottage Grove",
                              "3M Woodbury",
                              "Baytown Twp Groundwater Cont",
                              "East Metro PFAS",
                              "Farmington Ground Water Plume",
                              "Oakdale Dump Sites",
                              "Pigs Eye Landfill",
                              "Pine Street Dump (SF)",
                              "Precision Plating, Inc. (SF)",
                              "St. Louis Park Solvent Plume",
                              "Superior Plating Inc (SF)",
                              "Universal Plating (SF)"),
                  multiple = TRUE
                  
      ),
      width = 2
    ),
    column(
      plotOutput(outputId = "pfaslevelsplot", height = 600), 
      width = 10
    )
  ),
  fluidRow(
    p("These data are collected and managed by the MPCA, and have been used with permission. More information can be found at https://www.pca.state.mn.us/air-water-land-climate/cleaning-up-minnesota-superfund-sites.")
  )
),
tabPanel(
  "Results, Limitations, & Future Work", tags$style(
    ".navbar-nav li a {
                  font-size: 10px;
                  font-weight: normal;
                  }
                  "),
  h3("Main Takeaways"),
  p("insert results paragraph here"),
  h3(""),
  h3("Limitations"),
  p("Environmental data has some natural limitations that can make it tricky to work with. With our specific data, there were so many different variables to consider, such as sample type, chemical, location, medium, and date. As is often the case with water samples, not all observations were taken in the same units, and work had to be done to standardize units before analysis. Recording errors, such as issues with latitude, longitude, and county not matching, made some samples unusable. Additionally, we look at the change in PFAS concentrations over time, which is limited by the inconsistency of samples over the years. Some years a given chemical will have hundreds of samples taken, while other years there won't be any. This makes it difficult to make conclusions from our temporal analysis." ),
  h3("Future Work"),
  p("More research is needed to understand the long-term health and environmental effects of PFAS. This knowledge would help similar projects to link their analyses to real issues and provide recommendations for future remediation efforts. Future research in this area could focus on the correlations between analyte concentrations, specifically in PFAS, to understand whether different types PFAS tend to co-occur or not. Additionally, it would be interesting to look into different mediums, such as soil, and understand how chemicals travel through soils as opposed to water. This analysis would benefit from more time, more consistent data, and a deeper look into the factors that determine where PFAS ends up highly concentrated."),
)
)
)

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
        #mutate(LONGITUDE_short = round(LONGITUDE, 2), 
        #       LATITUDE_short = round(LATITUDE, 2)) %>%
        group_by(LONGITUDE, LATITUDE, site) %>%
        rename("Site" = "site") %>%
        summarize(Samples = n())
      superfund_loc_points2 <- st_as_sf(superfund_loc_points2, coords = c("LONGITUDE", "LATITUDE"), crs = 6783)
      
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
    
    output$pfasplottemporal <- renderPlot(
      ggplot() + 
        geom_sf(data = counties_cropped, color = "navajowhite", fill = "ivory", size = 1)+
        geom_sf(data = (pfas_health_sf %>%
                          filter(commonName == input$Common_Name, year == input$Year)), aes(color = above_level), alpha = 0.4, size = 3) +
        labs(color = "Sample Result", title = paste(input$Common_Name, "over time in Twin Cities Area Superfund sites")) +
        scale_color_manual(values=c('Above Health Action Level' = "#de2d26",'Below Health Action Level' = "goldenrod1",'Not Detected'  = "palegreen3"), drop = FALSE, guide = guide_legend(override.aes = list(shape = 19, size = 3) ) )+
        #guides(fill = guide_legend(override.aes = list(shape = 19, size = 3) ) ) +
        theme_classic() +
        theme(axis.ticks = element_blank(), 
              axis.text = element_blank(), 
              axis.line = element_blank(),
              title = element_text(size = 20),
              legend.title = element_text(size=16),
              legend.text = element_text(size=14),
              text = element_text(family = "AppleGothic"))
    )
    
    output$pfaslevelsplot <- renderPlot(
      pfas_health_levels %>%
        filter(commonName %in% c("PFHxS", "PFOA", "PFBA", "PFBS", "PFOS"),
          `FACILITY NAME` == input$location) %>%
        ggplot(aes(x = `SAMPLE DATE`, color = factor(above_level, levels = c("Not Detected", "Below Health Action Level", "Above Health Action Level")), fill = factor(above_level, levels = c("Not Detected", "Below Health Action Level", "Above Health Action Level")))) +
        geom_histogram(
          binwidth = 92
        ) +
        scale_fill_manual(values=c('Above Health Action Level' = "#de2d26",'Below Health Action Level' = "goldenrod1",'Not Detected'  = "palegreen3"))+
        scale_color_manual(values=c('Above Health Action Level' = "#de2d26",'Below Health Action Level' = "goldenrod1",'Not Detected'  = "palegreen3"))+
        labs(title = "Progression of 5 main PFAS over time",fill = "Sample Result", x = "Sample Date", y = "Number of Samples") +
        guides(color = "none") +
        theme_classic() +
        theme(title = element_text(face = "bold", size = 18), axis.title = element_text(size = 14, face = "plain"),legend.title = element_text(face = "bold", size = 14), legend.text = element_text(size = 14),
              text = element_text(family = "AppleGothic")),
      height = 600
      
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
