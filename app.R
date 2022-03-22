#library(rgbif)
#library(kewr)
#library(sf)

#################
#1. add POWO native range
#2. add user csv import with validation
#################
library(here)
library(dplyr)
library(magrittr)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)


source("R/get_data.R")

#### ui ####
ui <- fluidPage(

  # set themes
  theme = shinythemes::shinytheme("darkly"),
  
  # Navigation
  navbarPage(
    "GeoCAT",
    id = "navGeoCAT",
    tabPanel(
      "Home",
      wellPanel(fluidRow(
        column(12,
               align = "center",
               tags$h1("Welcome to GeoCAT - select a species to assess")
        ),
      ),
      
      fluidRow(
        column(3),
        column(6, align = "center",
               # testing the WCVP selector
               selectizeInput('wcvp',
                              #options = list(placeholder = 'select a species name'),
                              label = "Select a name - use 'Backspace' or 'Delete' to reset",
                              choices = NULL,
               )
        ),
        column(3)
      ),
      
      fluidRow(
        column(4),
        column(4, align="center",
               actionButton("gotoAnalysis", "Go to analysis",
                            style="color: #fff; background-color: #FF8C00; border-color: #CD6600")
        ),
        column(4),
      ),
      br(),
      fluidRow(
        column(12, align="center",
               actionLink("gotoBlank", "Or start a blank assessment")
        ),
      ),
      br(),
      fluidRow(
        column(12, align="center",
               tags$h4("Geospatial Conservation Assessment Tool. Perform rapid geospatial analysis of species in a simple and powerful way.")
        )
      )
      )
    ),
    
    # closing wellPanel
    tabPanel("table",
             fluidRow(column(
               dataTableOutput(outputId = "points_tab_selection"), width = 12
             ))),
    tabPanel("map",
             #fluidRow(column(4,
             #sidebarLayout(
             # Sidebar panel for inputs
             sidebarPanel(
               tags$h5("You are assessing:"),
               textOutput("wcvp_selection"),
               tags$hr(style="border-color: white;"),
               br(),
               
               # try the conditional panel to switch on when gbif points or csv loaded
               #conditionalPanel(condition = "input.gbifpointsInput == true",
              shinyWidgets::materialSwitch(
                 inputId = "gbifonoff", 
                 label = "GBIF occurrences",
                 #fill = TRUE, 
                 value = FALSE,
                 status = "success"
               ),
               br(),

             ),
             
             # Main panel for displaying outputs
             #column(8,
             mainPanel(
               leaflet::leafletOutput("mymap", width = "100%", height = 600)
             )),
    
    tabPanel("help",
             fluidRow(
               column(12, align="left",
                      tags$blockquote("More to add here - training material etc.
                                    GeoCAT help
                                    You can check the GeoCAT User Guide to guide you in the usage of the tool. 
                                    The User Guide includes Help, Data layers, FAQs, Links, Documents and a Glossary
                                    to help you make the most of GeoCAT.")
               ))
    )
  )
)




server <- function(input, output, session) {
  
  # species lookup using WCVP  
  updateSelectizeInput(session, 
                       'wcvp', 
                       choices = paste0(wcvp_data$taxon_name," ", wcvp_data$authors), 
                       server = TRUE,
                       options = list(placeholder = 'select a species name'),
                       label = NULL)
  
  # display selected name
  output$wcvp_selection <- renderText(input$wcvp)
  
  # link to navpanel map
  observeEvent(input$gotoAnalysis, {
    updateTabsetPanel(session, "navGeoCAT",
                      selected = "map"
    )
  })
  
  # link to navpanel map
  # need to add something to clear the default selected species
  observeEvent(input$gotoBlank, {
    updateTabsetPanel(session, "navGeoCAT",
                      selected = "map"
    )
  })
  

  # leaflet base output map
  output$mymap <- renderLeaflet({
    leaflet() %>%
      
      setView(lng = 0,
              lat = 0,
              zoom = 2) %>%
      
      #setMaxBounds(-180, 90, 180, -90) %>%
    leaflet.extras::addSearchOSM(options = searchOptions(
        autoCollapse = F,
        collapsed = F,
        minLength = 2,
        position = "topright"
      )) %>%
      
      addScaleBar(position = "bottomright") %>%
      
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#10bae0",
        completedColor = "#241ad9"
      ) %>%
      
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "ESRI World Imagery (default)",
        options = providerTileOptions(noWrap = TRUE)
      )  %>%
      
      addProviderTiles(
        providers$Esri.WorldStreetMap,
        group = "ESRI Open Street map",
        options = providerTileOptions(noWrap = TRUE)
      )  %>%
      
      addProviderTiles(
        providers$OpenStreetMap.Mapnik,
        group = "Open Street Map",
        options = providerTileOptions(noWrap = TRUE)
      )  %>%
      
      addProviderTiles(providers$OpenTopoMap,
                       group = "Open Topo Map",
                       options = providerTileOptions(noWrap = TRUE))  %>%
      
      addLayersControl(
      baseGroups = c(
        "ESRI World Imagery",
        "Open Street Map",
        "Open Topo Map",
        "ESRI Open Street map"
      ),
      #overlayGroups = c("User points", "GBIF points"),
      options = layersControlOptions(collapsed = FALSE)
    )
    
    
  })

}

#shinyApp(ui, server, ...)
shinyApp(ui = ui, server = server)
#}