#library(rgbif)
#library(kewr)
#library(sf)

# to do
# conditional panel for gbif and user csv
# reset everything when user clicks home page
# add distribution map TDWG
# add EOO and analysis on/off
# merge point and GBIF data - 
# when user select blank - the species name is blank, but user can add

#################
#1. add POWO native range
#2. add user csv import with validation
#################
library(here)
library(dplyr)
library(magrittr)
library(shiny)
#library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(red)
library(sf)
library(kewr)
library(igraph)
library(glue)
library(DT)
#devtools::install_github("barnabywalker/kewr")
#install.packages("devtools")
#install.packages("igraph")

source("R/get_data.R")
source("R/get_distributions.R")

#### ui ####
ui <- fluidPage(

  # set theme
  #theme = shinythemes::shinytheme("darkly"),
  #theme = bslib::bs_theme(bootswatch = "united"),

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
                              choices = NULL
                              #selected = "new species",
                              #choices = wcvp_data
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
      ),
      
      fluidRow(
        column(12,
               tags$h2(
               "Note that this is a test version and is not stable - see list of known issues/things to do. Email s.bachman@kew.org to send any requests or highlight bugs"),
               DTOutput('issues')

        ),
      ),
      
      
      
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
               fluidRow(
                 column(8, align="center", offset = 2,
                        tags$h4("You are assessing:")
                 )
               ),

                textOutput("wcvp_selection"),
               tags$hr(style="border-color: white;"),

               materialSwitch(inputId = "Analysis", 
                                label = "Analysis on/off", 
                                value = FALSE,
                                status = "success"),
                 
                 htmlOutput("res_title"),
                 htmlOutput("text"),
 
               fluidRow(
                 column(8, align="center", offset = 2,
                        tags$h4("Add occurrence data:")
                 )
               ),

               # Output: Tabset w/ plot, summary, and table ----
               tabsetPanel(type = "tabs",
                           tabPanel("Import CSV",
                                    # Input: input csv file
                                    helpText("Upload a CSV with 'longitude' and 'latitude' fields"),
                                    fileInput("csv_in", NULL, multiple = FALSE, accept = (".csv")),
                           ), #, plotOutput("plot")),
                           tabPanel("Query GBIF",
                                    # # Input: select a species from GBIF
                                    # helpText("Search for a species name in GBIF"),# Action: run the EOO, AOO analysis
                                    # textInput("searchGBIF", "")
                                    )
                           
               ),
               
               # try the conditional panel to switch on when gbif points or csv loaded
              #conditionalPanel(condition = "input.csv_in == true",
              #conditionalPanel(condition = "input.csv_in == true",
              # shinyWidgets::materialSwitch(
              #    inputId = "csv_onoff", 
              #    label = "User occurrences",
              #    #fill = TRUE, 
              #    value = FALSE,
              #    status = "info",
              #    right = TRUE
              #  ),
              
              # shinyWidgets::materialSwitch(
              #   inputId = "gbif_onoff", 
              #   label = "GBIF occurrences",
              #   #fill = TRUE, 
              #   value = FALSE,
              #   status = "success",
              #   right = TRUE
              # ),
              
               br(),
              
             ),
             
             # Main panel for displaying outputs
             #column(8,
             mainPanel(
               leaflet::leafletOutput("mymap", width = "100%", height = 550),
             )),
    
    tabPanel("help",
             fluidRow(
               column(12, align="center",
                      downloadButton("downloadtempcsv", 
                                     "Download template occurrence csv file")
               )
             ),
             br(),
             fluidRow(
               column(12, align="left",
                     "More to add here - training material etc.
                      GeoCAT help
                      You can check the GeoCAT User Guide to guide you in the usage of the tool. 
                      The User Guide includes Help, Data layers, FAQs, Links, Documents and a Glossary
                      to help you make the most of GeoCAT."
               ))
    )
  )
)




server <- function(input, output, session) {
  
  output$issues = renderDT(
    issues, options = list(lengthChange = FALSE)
  )
  
  # species lookup using WCVP  
  updateSelectizeInput(session,
                       'wcvp',
                       choices = wcvp_data$taxon_name_authors,
                       server = TRUE,
                       options = list(placeholder = 'select a species name'),
                       #selected = "new species",
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
  
  # set up the download for the temp file
  output$downloadtempcsv <- downloadHandler(
    filename = function() {
      paste("csv_temp.csv", sep="")
    },
    content = function(file) {
      write.csv(csv_temp, file, row.names = FALSE)
    }
  )
  
  # prepare the points
  csvpointsInput <- eventReactive(input$csv_in, {
    
    df <- read.csv(input$csv_in$datapath) #encoding = 'UTF-8')
    
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

   #rangepoly <- rangepoly$geometry

   #long_name <- renderText(input$wcvp)
   
   #proxy map to add polygons
   # observeEvent(input$wcvp,{
   # 
   #   leafletProxy("mymap") %>%
   # 
   #     clearShapes() %>%
   # 
   #     rangepoly <- native_geom(long_name)
   # 
   #     addPolygons(
   #       data = rangepoly$geometry,
   #       color = "red",
   #       stroke = T,
   #       weight = 3,
   #       fillOpacity = 0.2,
   #       fill = T,
   #       fillColor = "#999999")
   # 
   #     })
  
  # proxy map to add csv points
  observeEvent(input$csv_in,{
    
    leafletProxy("mymap", data = csvpointsInput()) %>%
      
      clearMarkers() %>%
      
      # zoom to fit - can we buffer this a little?
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%
      
      # add markers from the data
      addCircleMarkers(group = "View Points",
                       lng = ~longitude,
                       lat = ~latitude, 
                       radius = 7, 
                       color = "#FFFFFF", 
                       stroke = T,
                       fillOpacity = 1,
                       fill = T,
                       fillColor = "#0070ff") #%>%
    
    # add convex hull to illustrate EOO
    #addPolygons(hulls <- data %>%
    #              st_as_sf(thepoints, coords = c("longitude", "latitude"), crs = 4326) %>%
    #              geometry = st_combine( geometry )  %>%
    #              st_convex_hull())  
    
    
  })
  
  #output to analysis on/off switch
  switchon = eventReactive(input$Analysis, {
    
    # catch when either one or both data inputs exist
    # if else?
    
    if (input$Analysis == "TRUE"){
      thedata = csvpointsInput()
      lldata = thedata %>%
        dplyr::select(longitude,latitude)
      #lldata = lldata %>%
      #  dplyr::rename(lat = decimalLatitude,
      #                long = decimalLongitude)
      #thepoints <- rCAT::simProjWiz(lldata)
      #theEOO = rCAT::eoo(thepoints)
      #theAOO = rCAT::aoo(thepoints)
      theEOO = red::eoo(lldata)
      theAOO = red::aoo(lldata)
      
      str1 <-
        paste("Extent of occurrence (EOO): ",
              format(round(as.numeric(theEOO)), big.mark = ","),
              "(km squared)")
      str2 <-
        paste("Area of occupancy (AOO): ",
              format(round(as.numeric(theAOO)), big.mark = ","),
              "(km squared)")
      HTML(paste(str1, str2, sep = '<br>')
           )#, icon = icon("exclamation-circle"),
                 #str2, icon = icon("exclamation-circle"),
                 #sep = '<br>'))
      
    } else
    {}
    
  })
  
  # render the output of the EOO and AOO results
  output$res_title <- renderUI({
    HTML(paste0("<b>", "Results:", "</b>"))
  })
  
  output$text <- renderUI({
    switchon()
  })
  
  # make a polygon from imported csv points
   polyInput = eventReactive(input$csv_in, {
     df <- read.csv(input$csv_in$datapath)
     #df = csv_temp 
     poly = df %>%
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
        sf::st_combine()  %>%
        sf::st_convex_hull()  
    })
  
  # proxy map to add polygon
  observeEvent(input$Analysis, {

        if (input$Analysis == "TRUE"){
          
          leafletProxy("mymap",data = polyInput()) %>%

            # clear previous polygons
            #clearShapes() %>%

            # add polygons input from csv
            addPolygons(
              color = "#000000",
              stroke = T,
              weight = 3,
              fillOpacity = 0.4,
              fill = T,
              fillColor = "#999999")

          # TO DO - add AOO cells?
        } else {
          leafletProxy("mymap",data = polyInput()) %>%

            # clear previous polygons
            clearShapes()
        }

      })
}

#shinyApp(ui, server, ...)
shinyApp(ui = ui, server = server)
#}