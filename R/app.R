geocatApp <- function(...) {
  #### ui ####
  ui <- fluidPage(
    
    shinyjs::useShinyjs(),
    
    tags$html(lang = "en"),
    
    # title
    titlePanel("ShinyGeoCAT - Geospatial Conservation Assessment Tools"),
    tags$head(tags$title("ShinyGeoCAT  - Geospatial Conservation Assessment Tools")), # WCAG modification
    
    # set theme
    theme = shinythemes::shinytheme("darkly"),
    
    # Sidebar panel for inputs
    shiny::sidebarPanel(
      
      p("This is a simple version of the GeoCAT app.", "It currenlty does not have functionality to add GBIF data or edit points."),
      
      
      p("Moat, J., Bachman, S., & Walker, B. (2023). GeoCAT - Geospatial Conservation Assessment Tools (BETA) [Software]. Available from https://spbachman.shinyapps.io/geocat_staging/"),
      br(),
      
      # sidebar ----
      shinyjs::disabled(
        shinyWidgets::materialSwitch(
          inputId = "Analysis",
          label = "Analysis on/off",
          value = FALSE,
          status = "success"
        )
      ),
      
      # swich csv points on/off from map
      shiny::htmlOutput("res_title"),
      # EOO AOO results ----
      shiny::htmlOutput("text"),
      
      br(),
      
      # switch csv points on/off from map
      shinyjs::disabled(
        # csv points on/off ----
        shinyWidgets::materialSwitch(
          inputId = "csv_onoff",
          label = "User occurrences",
          value = FALSE,
          status = "info",
          right = TRUE
        )
      ),
      
      br(),
      
      shiny::fluidRow(column(
        12, align = "center", verbatimTextOutput("validation")
      )),
      
      br(),
      
      shiny::fluidRow(
        # CSV input widget ----
        column(
          12,
          align = "centre",
          shiny::helpText("Upload a CSV with a unique 'id' and 'longitude', 'latitude' fields "),
          shiny::fileInput(
            "csv_in",
            NULL,
            multiple = FALSE,
            accept = (".csv")
          ),
        ),
      ),
      
      fluidRow(
        column(8, align="left",
               tags$h5("Enter a POWO ID for a native range map:")
        )
      ),
      
      shiny::fluidRow(
        column(
          8, align = "center", 
          shiny::textInput("powo_id", label = NULL, placeholder = "68179-1")
        ),
        column(
          4, align = "center", 
          actionButton("queryPOWO", "load map")
        )
      ),
      
      tags$a(href="https://powo.science.kew.org/", "Search POWO to get accepted name ID", target="_blank"),
      
      br(),
      br(),
      
      fluidRow(
        column(
          12, align="center", 
          downloadButton('download', "Download SIS point file")
        )
      )
    ),

  #### Main panel for displaying outputs ####
  
  shiny::mainPanel(#### main panel ####
                   leaflet::leafletOutput(# map ----
                                          "mymap", width = "100%", height = 650))
  )



##### server #####
server <- function(input, output, session) {
  values <- reactiveValues(
    analysis_data=tibble::tibble()
  )
  
  # prepare the points
  csvpointsInput <- shiny::eventReactive(input$csv_in, {
    ext <- tools::file_ext(input$csv_in$datapath)
    if (ext == "csv") {
      data <- read.csv(input$csv_in$datapath)
    } else {
      shiny::validate("Invalid file; please upload a .csv file")
    }
    
    shiny::validate(check_fields_(data, c("longitude", "latitude", "id")))
    shiny::validate(check_numeric_(data, c("longitude", "latitude")))
    
    values$analysis_data <-
      rbind(
        values$analysis_data,
        data %>%
          dplyr::select(longitude,latitude, id) %>%
          dplyr::filter(if_all(everything(), ~!is.na(.))) %>%
          dplyr::filter(longitude < 180, longitude > -180,
                        latitude < 90, latitude > -90) %>%
          dplyr::mutate(source="csv")
      )
    
    data
  })
  
  powo_range <- shiny::eventReactive(input$powo_id, {
    native_geom(input$powo_id)
  })
  
  # leaflet base output map ----
  output$mymap <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 2)) %>%
      
      leaflet::setView(lng = 0,
                       lat = 0,
                       zoom = 2) %>%
      
      leaflet.extras::addSearchOSM(options = leaflet.extras::searchOptions(
        autoCollapse = F,
        collapsed = F,
        minLength = 2,
        position = "topright"
      )) %>%
      
      leaflet::addScaleBar(position = "bottomright") %>%
      
      leaflet::addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#10bae0",
        completedColor = "#241ad9"
      ) %>%
      
      leaflet::addProviderTiles(
        provider = "Esri.WorldImagery",
        #provider$Esri.WorldImagery,
        group = "ESRI World Imagery (default)",
        options = leaflet::providerTileOptions(noWrap = TRUE)
      )  %>%
      
      leaflet::addProviderTiles(
        provider = "Esri.WorldStreetMap",
        group = "ESRI Open Street map",
        options = leaflet::providerTileOptions(noWrap = TRUE)
      )  %>%
      
      leaflet::addProviderTiles(
        provider = "OpenStreetMap.Mapnik",
        group = "Open Street Map",
        options = leaflet::providerTileOptions(noWrap = TRUE)
      )  %>%
      
      leaflet::addProviderTiles(
        provider = "OpenTopoMap",
        #provider$OpenTopoMap,
        group = "Open Topo Map",
        options = leaflet::providerTileOptions(noWrap = TRUE))  %>%
      
      leaflet::addLayersControl(
        baseGroups = c(
          "ESRI World Imagery",
          "Open Street Map",
          "Open Topo Map",
          "ESRI Open Street map"
        ),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
    })
  
  output$validation <- shiny::renderPrint({
    data <- csvpointsInput()
    if (! is.data.frame(data)) {
      data
    }
    msg <- c(
      check_complete_(data, c("longitude", "latitude", "id")),
      check_range_(data, "longitude", -180, 180),
      check_range_(data, "latitude", -90, 90),
      check_rounded_(data, "longitude"),
      check_rounded_(data, "latitude"),
      check_zeros_(data, "longitude"),
      check_zeros_(data, "latitude")
    )
    
    if (! is.null(msg)) {
      msg
    }
  })
  
  shiny::observeEvent(input$csv_in, {
    shinyjs::enable("Analysis")
    shinyjs::enable("csv_onoff")
    shinyWidgets::updateMaterialSwitch(session, "csv_onoff", value=TRUE)
  })
  
  shiny::observeEvent(input$queryPOWO, {

      leaflet::leafletProxy("mymap") %>%
        
        leaflet::addPolygons(
          data = powo_range(),
          color = "red",
          weight = 2,
          fillColor = "red") 
     
})

  shiny::observeEvent(input$csv_in, {
    
    df <- csvpointsInput()
    
    leaflet::leafletProxy("mymap", data=df) %>%
      
      # zoom to fit - can we buffer this a little?
      leaflet::fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%
      
      leaflet::addCircleMarkers(group = "View Points",
                                lng = ~longitude,
                                lat = ~latitude,
                                radius = 7,
                                color = "#FFFFFF",
                                stroke = T,
                                fillOpacity = 1,
                                fill = T,
                                fillColor = "#0070ff",
                                popup = as.character(df$id))
  })
  
  #output to analysis on/off switch
  calculateAnalyisis <- eventReactive(list(input$Analysis, input$gbif_onoff, input$csv_onoff), {
    if (input$Analysis) {
      str1 <-
        paste("Extent of occurrence (EOO): ",
              format(round(as.numeric(values$eooarea)), big.mark = ","),
              "km<sup>2</sup>")
      str2 <-
        paste("Area of occupancy (AOO): ",
              format(round(as.numeric(values$aooarea)), big.mark = ","),
              "km<sup>2</sup>")
      HTML(paste(str1, str2, sep = '<br>')
      )
    }
  })
  
  # render the output of the EOO and AOO results
  output$res_title <- shiny::renderUI({
    HTML(paste0("<b style='color:orange;'>", "Results:", "</b>"))
  })
  
  output$text <- renderUI({
    HTML(paste0("<p style='color:orange;'>", calculateAnalyisis(), "</p>"))
  })
  
  # point file download handler
  output$download = downloadHandler(
    filename = function(){
      date <- format(Sys.Date(), "%Y%m%d")
      species_name <- "SIS_points"
      paste(species_name, "_", date, ".csv", sep = "" )
    },
    content = function(file){
      df = csvpointsInput()
      # merge with sis format
      df = dplyr::bind_cols(df,sis_format)
      df$dec_lat <- df$latitude
      df$dec_long <- df$longitude
      #df <- df |> 
      #  dplyr::select(-c(latitude, longitude))
      write_csv(df, file)
    }
  )
  
  shiny::observeEvent(input$Analysis, {
    
    if (input$Analysis){
      
      #analysis here
      d <- values$analysis_data
      # if (!input$gbif_onoff) {
      #   d <- dplyr::filter(d, source != "gbif")
      # }
      if (!input$csv_onoff) {
        d <- dplyr::filter(d, source != "csv")
      }
      if (nrow(d) == 0)  {
        return()
      }
      d <- dplyr::select(d, -source)#is this needed or is it the above needed, I think they are doing the same thing
      #JMJJMJMJMJM
      #project the data so we can work on it in a sensible space for areas and distance
      projp <- simProjWiz(d)
      EOO <- eoosh(projp)
      AOO <- aoosh(projp)
      values$eooarea <- EOO$area
      values$aooarea <- AOO$area
      #JMJMJMMJ
      leaflet::leafletProxy("mymap",data = AOO$polysf) %>%
        leaflet::addPolygons(
          color = "#000000",
          stroke = T,
          weight = 2,
          fillOpacity = 0.3,
          fill = T,
          fillColor = "red",
          group = "AOOpolys")
      
      leaflet::leafletProxy("mymap",data = EOO$polysf) %>%
        leaflet::addPolygons(
          color = "#000000",
          stroke = T,
          weight = 2,
          fillOpacity = 0.2,
          fill = T,
          fillColor = "grey")
      
    } else {
      
      leaflet::leafletProxy("mymap") %>%
        # clear previous polygons
        leaflet::clearShapes()
      
    }
    
  })
}

shinyApp(ui, server, ...)
}
