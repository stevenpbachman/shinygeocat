#testing conditional panel

library(red)

#### ui ####
ui <- fluidPage(
  
  mainPanel(
    materialSwitch(inputId = "Analysis", 
                   label = "Analysis on/off", 
                   value = FALSE,
                   status = "success"),
    
    htmlOutput("res_title"),
    htmlOutput("text"),
    
    leaflet::leafletOutput("mymap", width = "100%", height = 600),
    
    fileInput("csv_in", NULL, multiple = FALSE, accept = (".csv")),
    
    # conditionalPanel(condition = "input.csvpointsInput == true",
    # shinyWidgets::materialSwitch(
    #   inputId = "csv_in", 
    #   label = "User occurrences",
    #   #fill = TRUE, 
    #   value = FALSE,
    #   status = "info",
    #   right = TRUE
    # )
    # ),
  )
  
  
)

server <- function(input, output, session) {
  
  # prepare the points
  csvpointsInput <- eventReactive(input$csv_in, {
    
    df <- read.csv(input$csv_in$datapath) #encoding = 'UTF-8')
    
  })
  
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
      theEOO = eoo(lldata)
      theAOO = aoo(lldata)

      str1 <-
        paste("Extent of occurrence: ",
              format(round(as.numeric(theEOO)), big.mark = ","),
              "(km squared)")
     str2 <-
        paste("Area of occupancy: ",
              format(round(as.numeric(theAOO)), big.mark = ","),
              "(km squared)")
      HTML(paste(str1, icon = icon("exclamation-circle"),
                 str2, icon = icon("exclamation-circle"),
                 sep = '<br>'))

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

shinyApp(ui = ui, server = server)
