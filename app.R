library(shiny)
library(dplyr)
library(leaflet)
library(tidyverse)


pivot_data <- read_csv("/Users/gabrielburcea/Rprojects/data/mapping_data.csv")

pivot_data$Country <-
    dplyr::recode(
        pivot_data$Country,
        'United States of America' = 'USA',
        'Great Britain' = 'United Kingdom'
    )


pivot_data$Gender <- as.factor(pivot_data$Gender)
pivot_data$Country <- as.factor(pivot_data$Country)
pivot_data$Location <- as.factor(pivot_data$Location)
pivot_data$Chills <- as.factor(pivot_data$Chills)
pivot_data$Cough  <- as.factor(pivot_data$Cough)
pivot_data$Diarrhoea  <- as.factor(pivot_data$Diarrhoea)
pivot_data$Fatigue  <- as.factor(pivot_data$Fatigue)
pivot_data$Headache   <- as.factor(pivot_data$Headcahe)
pivot_data$loss_smell_taste  <- as.factor(pivot_data$'Loss of smell and taste')
pivot_data$muscle_ache <- as.factor(pivot_data$'Muscle Ache')
pivot_data$nasal_congestion <- as.factor(pivot_data$'Nasal Congestion')
pivot_data$nausea_vomiting  <- as.factor(pivot_data$'Nausea and Vomiting')
pivot_data$shortness_breath <- as.factor(pivot_data$'Shortness of Breath')
pivot_data$sore_throat <- as.factor(pivot_data$'Sore Throat')
pivot_data$sputum <- as.factor(pivot_data$Sputum)
pivot_data$temperature  <- as.factor(pivot_data$Temperature)



level_key_chills <-
    c(
        'Yes' = "Chills",
        'No' = "No",
        'Yes' = "Mild",
        'Yes' = "Moderate",
        'Yes' = "Severe"
    )
level_key_cough <-
    c(
        'Yes' = "Cough",
        'No' = "No",
        'Yes' = "Mild",
        'Yes' = "Moderate",
        'Yes' = "Severe"
    )
level_key_diarrhoea <-
    c(
        'No' = "No",
        'Yes' = "Mild",
        'Yes' = "Moderate",
        'Yes' = "Severe"
    )
level_key_fatigue <-
    c(
        'No' = "No",
        'Yes' = "Mild",
        'Yes' = "Moderate",
        'Yes' = "Severe"
    )
level_key_headache <-
    c(
        'No' = "No",
        'Yes' = "Mild",
        'Yes' = "Moderate",
        'Yes' = "Severe",
        'Yes' = "Headcahe"
    )
level_key_loss_smell_taste <-
    c(
        'No' = "Loss of smell and taste",
        'No' = "No",
        'Yes' = "Mild",
        'Yes' = "Moderate",
        'Yes' = "Severe"
    )
level_key_muschle_ache <-
    c(
        'No' = "No",
        'No' = "Muscle Ache",
        'Yes' = "Mild",
        'Yes' = "Moderate",
        'Yes' = "Severe"
    )
level_key_nasal_congestion <-
    c(
        'No' = "No",
        'No' = "Nasal Congestion",
        'Yes' = "Mild",
        'Yes' = "Moderate",
        'Yes' = "Severe"
    )
level_key_nausea_vomiting <-
    c(
        'No' = "No",
        'Yes' = "Nausea and Vomiting",
        'Yes' = "Mild",
        'Yes' = "Moderate",
        'Yes' = "Severe"
    )
level_key_self_diagnosis <-
    c(
        'No' = "None",
        'Yes' = "Mild",
        'Yes' = "Moderate",
        'Yes' = "Severe"
    )
level_key_short_breath <-
    c(
        'No' = "No",
        'No' = "Shortness of Breath",
        'Yes' = "Mild",
        'Yes' = "Moderate",
        'Yes' = "Severe"
    )
level_key_sore_throat <-
    c(
        'No' = "No",
        'No' = "Sore Throat",
        'Yes' = "Mild",
        'Yes' = "Moderate",
        'Yes' = "Severe"
    )
level_key_sputum <-
    c(
        'No' = "No",
        'No' = "Sputum",
        'Yes' = "Mild",
        'Yes' = "Moderate",
        'Yes' = "Severe"
    )
level_key_care_home_worker <-
    c('Yes' = 'Yes',
      'No' = 'No')

level_key_temperature <-
    c('No' = 'No',
      Yes = '37.5-38',
      Yes = '37.1-38',
      Yes = '38.1 -39',
      Yes =  '37.5-38',
      Yes = "38.2-39",
      Yes = '39-40',
      Yes = '38.1-39',
      Yes = '39.1-41',
      Yes = 'Temperature'
    )


data_not_sev <- pivot_data %>%
    dplyr::mutate(
        Chills = forcats::fct_recode(Chills,!!!level_key_chills),
        Cough = forcats::fct_recode(Cough,!!!level_key_cough),
        Diarrhoea = forcats::fct_recode(Diarrhoea,!!!level_key_diarrhoea),
        Fatigue = forcats::fct_recode(Fatigue,!!!level_key_fatigue),
        Headache = forcats::fct_recode(Headache,!!!level_key_headache),
        'Loss of smell and taste' = forcats::fct_recode(loss_smell_taste,!!!level_key_loss_smell_taste),
        'Muscle ache' = forcats::fct_recode(muscle_ache,!!!level_key_muschle_ache),
        'Nasal congestion' = forcats::fct_recode(nasal_congestion,!!!level_key_nasal_congestion),
        'Nausea and vomiting' = forcats::fct_recode(nausea_vomiting,!!!level_key_nausea_vomiting),
        'Shortness of breath' = forcats::fct_recode(shortness_breath,!!!level_key_short_breath),
        'Sore throat' = forcats::fct_recode(sore_throat,!!!level_key_sore_throat),
        Temperature = forcats::fct_recode(temperature, !!!level_key_temperature),
        Sputum = forcats::fct_recode(Sputum,!!!level_key_sputum)
    ) %>%
    dplyr::select(
        ID,
        Country,
        Location,
        Chills,
        Cough,
        Diarrhoea,
        Fatigue,
        Headache,
        'Loss of smell and taste',
        'Muscle ache',
        'Nasal congestion',
        'Nausea and vomiting',
        'Shortness of breath',
        'Sore throat',
        Sputum,
        Temperature,
        lat,
        lon
    )


gather_divided <- data_not_sev %>%
    tidyr::pivot_longer(cols = 4:15,
                        names_to = "Symptom",
                        values_to = "Severity") %>%
    dplyr::filter(Severity != "No") %>%
    dplyr::group_by(Symptom, Country, Location, lat, lon) %>%
    tally() 


# Define UI for application that draws a histogram
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  navbarPage("Covid-19 Symptom Tracker", id = "nav",
             
             tabPanel("Interactive map", 
                      div(class = "outer",
                          tags$head(
                            tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                            
                          ),
                          
                          leafletOutput("map", width = "100%", height = "96vh"), #height = "99vh"
                          
                          #Floating panel 
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                        h4("symptoms"),
                                        
                                        selectInput("symptom", "Select symptom", c("Chills",
                                                                                   "Cough", "Diarrhoea",
                                                                                   "Fatigue",
                                                                                   "Headache",
                                                                                   "Loss of smell and taste",
                                                                                   "Muscle ache",
                                                                                   "Nasal congestion",
                                                                                   "Nausea and vomiting",
                                                                                   "Shortness of breath",
                                                                                   "Sore throat",
                                                                                   "Sputum",
                                                                                   "Temperature")
                                        ), 
                                        tags$div(id="cite",
                                                 'Data provided by Your.md'
                                        )
                          )))
  )
  
)



server <- function(input, output) {
  
  filtered_data <- reactive({
    gather_divided %>% 
      dplyr::filter(Symptom %in% input$symptom)
  })
  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      addMarkers(data = filtered_data(), clusterOptions = markerClusterOptions())
    
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
