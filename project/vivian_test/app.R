library(tidyverse)
library(skimr)
library(readxl)
library(sf)
library(USAboundaries)
library(shiny)
library(shinythemes)
library(lubridate)

pfas7 <- read_csv("../../data/pfas7.csv")

min_action_levels <- data.frame(commonName = c("PFHxS", "PFHxA", "PFOA", "PFBA", "PFBS", "PFOS", "PFPeA"), action_level = c(0.027, NA, 0.035, 7.000, 7.000, 0.027, NA))

pfas_health_levels <- pfas7 %>% 
  left_join(min_action_levels, by = "commonName") %>%
  mutate(result_num = ifelse(DETECT_FLAG == "Y" & UNIT == "ng/L", `RESULT NUMERIC`/1000, ifelse(DETECT_FLAG == "Y" & UNIT != "ng/L", `RESULT NUMERIC`, 0.00)), above_level = ifelse(DETECT_FLAG == "Y" & result_num > action_level, "Above Health Action Level", ifelse(DETECT_FLAG == "Y" & result_num < action_level, "Below Health Action Level", "Not Detected"))) %>%
  filter(UNIT != "NA", `FACILITY TYPE` == "Superfund", LATITUDE != "NA", LONGITUDE != "NA", above_level != "NA")

ui <- fluidPage(theme = shinytheme("flatly"),
                
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
                 plotOutput(outputId = "pfaslevelsplot"), 
                 width = 10
               )
               )
               
)
                

server <- function(input, output) {
  
  output$pfaslevelsplot <- renderPlot(
    pfas_health_levels %>%
      filter(#commonName == input$pfaschoice,
              commonName %in% c("PFHxS", "PFOA", "PFBA", "PFBS", "PFOS"),
             `FACILITY NAME` == input$location) %>%
            # ,DETECT_FLAG == "Y", result_num < 10*action_level) %>% 
      ggplot(aes(x = `SAMPLE DATE`, color = factor(above_level, levels = c("Not Detected", "Below Health Action Level", "Above Health Action Level")), fill = factor(above_level, levels = c("Not Detected", "Below Health Action Level", "Above Health Action Level")))) +
      geom_histogram(
        binwidth = 92
        ) +
      scale_fill_manual(values=c("#fee0d2","#fc9272", "#de2d26"))+
      scale_color_manual(values=c("#fee0d2","#fc9272", "#de2d26"))+
      labs(title = "Progression of Seven Main PFAS over time",fill = "Sample Result", x = "Sample Date", y = "Number of Samples") +
      guides(color = FALSE) +
      theme_classic() +
      theme(title = element_text(face = "bold", size = 18), axis.title = element_text(size = 14, face = "plain"),legend.title = element_text(face = "bold", size = 14), legend.text = element_text(size = 14),
            text = element_text(family = "AppleGothic")),
    height = 600
       
  )
}

# Run the application 
shinyApp(ui = ui, server = server)