#' @import shiny dplyr
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
      
      p("This is a BETA test version. Send any feedback to s.bachman@kew.org."),
      
      
      p("Moat, J., Bachman, S., & Walker, B. (2023). ShinyGeoCAT - Geospatial Conservation Assessment Tools (BETA) [Software]. Available from https://spbachman.shinyapps.io/geocat_staging/"),
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
      
      # switch csv points on/off from map
      shiny::htmlOutput("res_title"),
      ## EOO AOO results ----
      shiny::htmlOutput("text"),
      
      br(),
      
      shiny::fluidRow(
        column(
          6, align = "left", 
          shinyjs::disabled(
            ## csv points on/off ----
            shinyWidgets::prettySwitch(
              inputId = "csv_onoff",
              label = "User occurrences",
              value = FALSE,
              status = "primary",
              fill = TRUE
            ),
            shiny::fluidRow(column(
              12, align = "center", verbatimTextOutput("validation")
            ))
          ),
        ),
        column(
          6, align = "left", 
          shinyjs::disabled(
            ## GBIF points on/off ----
            shinyWidgets::prettySwitch(
              inputId = "gbif_onoff",
              label = "GBIF occurrences",
              value = FALSE,
              status = "success",
              fill = TRUE
            )
          ),
        )
      ),
      
      shiny::fluidRow(
        ## CSV input widget ----
        column(
          12,
          align = "centre",
          shiny::helpText("Upload a CSV with at least 'longitude', 'latitude' fields "),
          shiny::fileInput(
            "csv_in",
            NULL,
            multiple = FALSE,
            accept = (".csv")
          ),
        ),
      ),
      ## GBIF input field ----
      shiny::fluidRow(column(
        12, align = "center", verbatimTextOutput("gbifValidation")
      )),
      
      fluidRow(
        column(12, align="left",
               tags$h5("Enter a taxon name to load points from GBIF:")
        )
      ),
      
      fluidRow(
        column(8, align="center",
               textInput("gbif_name", label=NULL, placeholder="Cyphostemma njegerre")),
        column(4, align="center",
               actionButton("queryGBIF", "load points"))
      ),
      ## POWO ID field ----
      fluidRow(
        column(12, align="left",
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
      ## SIS download widget ----
      fluidRow(
        column(
          4, align="center", 
          downloadButton('download', "Download SIS point file")
        ),
        column(
          4, align="center", 
          downloadButton('download_csv', "Download csv file")
        ),
        column(
          4, align = "center", 
          actionButton("reset", "Reset")
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
    analysis_data=empty_tbl_()
  )
  
  observeEvent(input$reset, {
      session$reload()
  })
  
  ##################################
  # prepare the points
  csvPointsInput <- shiny::eventReactive(input$csv_in, {
    ext <- tools::file_ext(input$csv_in$datapath)
    if (ext == "csv") {
      data <- import_csv(input$csv_in$datapath)
    } else {
      shiny::validate("Invalid file; please upload a .csv file")
    }
    
    shiny::validate(check_fields_(data, c("longitude", "latitude")))
    shiny::validate(check_numeric_(data, c("longitude", "latitude")))
    
    valid_points <- filter(data, longitude < 180, longitude > -180,
                           latitude < 90, latitude > -90)
    
    values$analysis_data <- bind_rows(values$analysis_data, valid_points)
    
    valid_points
  })
  
  gbifPointsInput <- eventReactive(input$queryGBIF, {
    points <- import_gbif(input$gbif_name)
    
    if (nrow(points) == 0) {
      points <- empty_tbl_()
    }
    
    values$analysis_data <- bind_rows(values$analysis_data, points)
    
    points
  })
  
  powo_range <- shiny::eventReactive(input$powo_id, {
    native_geom(input$powo_id)
  })
  
  # leaflet base output map ----
  output$mymap <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 2)) %>%
      leaflet.extras::addSearchOSM(options = leaflet.extras::searchOptions(
        autoCollapse = F,
        collapsed = F,
        minLength = 2,
        position = "topright"
      )) %>%
      
      leaflet::addScaleBar(position = "bottomright") %>%
     
      leafem::addMouseCoordinates() %>%
      
      ######################################################
      #JM
      leaflet.extras::addDrawToolbar(editOptions = editToolbarOptions(edit=TRUE),
                                     targetGroup = 'mappoints',
                                     circleMarkerOptions=FALSE,
                                     rectangleOptions=FALSE,
                                     circleOptions=FALSE,
                                     polygonOptions=FALSE,
                                     polylineOptions=FALSE) %>%
      #####################################################

      leaflet::addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#10bae0",
        completedColor = "#241ad9"
      ) %>%
      
      addTiles() %>%
      
      leaflet::addProviderTiles(
        provider = "CartoDB.Voyager",
        group = "CartoDB Voyager",
        options = leaflet::providerTileOptions(noWrap = FALSE)
      )  %>%
      
      leaflet::addProviderTiles(
        provider = "OpenStreetMap.Mapnik",
        group = "Open Street Map",
        options = leaflet::providerTileOptions(noWrap = FALSE)
      )  %>%
      
      leaflet::addProviderTiles(
        provider = "Esri.WorldImagery",
        group = "ESRI World Imagery (default)",
        options = leaflet::providerTileOptions(noWrap = FALSE)
      )  %>%
      
      leaflet::addProviderTiles(
        provider = "Esri.WorldTopoMap",
         group = "Esri World Topo Map",
        options = leaflet::providerTileOptions(noWrap = FALSE)
        )  %>%
      
      leaflet::addProviderTiles(
        provider = "Stamen.Toner",
        group = "Stamen Toner",
        options = leaflet::providerTileOptions(noWrap = FALSE)
      )  %>%
      
      leaflet::addProviderTiles(
        provider = "Stamen.TonerLite",
        group = "Stamen Toner Lite",
        options = leaflet::providerTileOptions(noWrap = FALSE)
      )  %>%
      
      leaflet::addLayersControl(
        baseGroups = c(
          "CartoDB Voyager",
          "Open Street Map",
          "ESRI World Imagery",
          "Esri World Topo Map",
          "Stamen Toner",
          "Stamen Toner Lite"
        ),
        options = leaflet::layersControlOptions(collapsed = TRUE)
      )
    })

#####Map Events############
##JM
  #add new point
  observeEvent(input$mymap_draw_new_feature, {
    point_data <- add_point(input$mymap_draw_new_feature)
    values$analysis_data <- bind_rows(values$analysis_data, point_data)
    t <- 0
    })
  
  #move points
  observeEvent(input$mymap_draw_edited_features, {
    for (feature in input$mymap_draw_edited_features$features){
        values$analysis_data <- move_point(feature, values$analysis_data)
      }
    })

  #delete points = actually just marks them not to display
  observeEvent(input$mymap_draw_deleted_features, {
    for (feature in input$mymap_draw_deleted_features$features){
      values$analysis_data <- delete_point(feature, values$analysis_data)
    }
    
  })
  
############################

  output$validation <- shiny::renderPrint({
    data <- csvPointsInput()
    if (! is.data.frame(data)) {
      data
    }
    msg <- c(
      check_rounded_(data, "longitude"),
      check_complete_(data, c("longitude", "latitude")),
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

  output$gbifValidation <- shiny::renderPrint({
    data <- gbifPointsInput()
    if (nrow(data) == 0) {
      cat("No records found in GBIF.\nCheck the name is in the GBIF backbone.")
    }
  })
  
  observeEvent(req(nrow(values$analysis_data) > 0), {
    shinyjs::enable("Analysis")
  }, once=TRUE)
  
  shiny::observeEvent(req(sum(values$analysis_data$geocat_source == "User CSV") > 0), {
    shinyjs::enable("csv_onoff")
    shinyWidgets::updateMaterialSwitch(session, "csv_onoff", value=TRUE)
  }, once=TRUE)
  
  shiny::observeEvent(req(sum(values$analysis_data$geocat_source == "GBIF") > 0), {
    shinyjs::enable("gbif_onoff")
    shinyWidgets::updateMaterialSwitch(session, "gbif_onoff", value=TRUE)
  }, once=TRUE)
  
  observeEvent(input$csv_onoff, {
    values$analysis_data <- values$analysis_data %>%
      mutate(geocat_use=ifelse(geocat_source == "User CSV", input$csv_onoff, geocat_use)) %>%
      # make sure deleted points aren't turned back on
      mutate(geocat_use=ifelse(geocat_deleted, FALSE, geocat_use))
  })
  
  observeEvent(input$gbif_onoff, {
    values$analysis_data <- values$analysis_data %>%
      mutate(geocat_use=ifelse(geocat_source == "GBIF", input$gbif_onoff, geocat_use)) %>%
      # make sure deleted points aren't turned back on
      mutate(geocat_use=ifelse(geocat_deleted, FALSE, geocat_use))
  })
  
  shiny::observeEvent(input$queryPOWO, {
    
      leaflet::leafletProxy("mymap") %>%
      
      #bb <- sf::st_bbox(powo_range()) %>%
      
      #leaflet::fitBounds(bb[1],bb[2], bb[3], bb[4]) %>%

      # zoom to fit - can we buffer this a little?
      #leaflet::fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%
     
        leaflet::addPolygons(
          data = powo_range(),
          color = "red",
          weight = 2,
          fillColor = "red") 
     
  })

  #output to analysis on/off switch
  calculateAnalysis <- eventReactive(list(input$Analysis, values$analysis_data), {
    if (input$Analysis) {
      points <- filter(values$analysis_data, geocat_use)
      
      points <- select(points, longitude, latitude)
      
      if (nrow(points) > 0) {
        projected_points <- simProjWiz(points)
  
        EOO <- eoosh(projected_points$p)
        AOO <- aoosh(projected_points$p)
  
        eoo_rating <- ratingEoo(EOO$area, abb=TRUE)
        aoo_rating <- ratingAoo(AOO$area, abb=TRUE)
        
        values$eooarea <- EOO$area
        values$aooarea <- AOO$area
        values$aoo_polygon <- AOO$polysf
        values$eoo_polygon <- EOO$polysf
        values$eoo_rating <- eoo_rating
        values$aoo_rating <- aoo_rating
        
        list(eoo=EOO, aoo=AOO, eoo_rating=eoo_rating, aoo_rating=aoo_rating)
      } else {
        values$eooarea <- NULL
        values$aooarea <- NULL
        values$aoo_polygon <- NULL
        values$eoo_polygon <- NULL
        values$eoo_rating <- NULL
        values$aoo_rating <- NULL
      }
    }
  })
  
  # render the output of the EOO and AOO results
  output$res_title <- shiny::renderUI({
    if (input$Analysis){
      HTML(paste0("<b style='color:orange;'>", "Results:", "</b>"))
    }
  })
  
  output$text <- renderUI({
    if (input$Analysis){
      results <- calculateAnalysis()
      if (! is.null(results)) {
        eoo_str <-
          paste("Extent of occurrence (EOO): ",
                format(round(as.numeric(results$eoo$area)), big.mark=","),
                "km<sup>2</sup>", "-", results$eoo_rating)
        
        aoo_str <-
          paste("Area of occupancy (AOO): ",
                format(round(as.numeric(results$aoo$area)), big.mark=","),
                "km<sup>2</sup>", "-", results$aoo_rating)
        
        results_html <- HTML(paste(eoo_str, aoo_str, sep='<br>'))
                             
        HTML(paste0("<p style='color:orange;'>", results_html, "</p>"))
      }
    }
  })
  
  # point file download handler
  output$download = downloadHandler(
    filename = function(){
      date <- format(Sys.Date(), "%Y%m%d")
      species_name <- "SIS_points"
      paste(species_name, "_", date, ".csv", sep = "" )
    },
    content = function(file){
      df = csvPointsInput()
      # merge with sis format
      df <- dplyr::bind_cols(df,sis_format)
      df$dec_lat <- df$latitude
      df$dec_long <- df$longitude
      #df <- df |> 
      #  dplyr::select(-c(latitude, longitude))
      write_csv(df, file)
    }
  )
  
  # csv file download handler
  output$download_csv = downloadHandler(
    filename = function(){
      date <- format(Sys.Date(), "%Y%m%d")
      species_name <- "sh_geocat" #needs to come from something useful
      paste(species_name, "_", date, ".csv", sep = "" )
    },
    content = function(file){
      write_csv(values$analysis_data, file)
    }
  )
  
  observeEvent(req(nrow(values$analysis_data) > 0), {
    points <- filter(values$analysis_data, ! geocat_deleted)
    
    used_pal <- colorFactor(
      palette=c("#509E2F", "#0078b4", "#e41a1c"),
      domain=c("GBIF", "User CSV", "User point")
    )
    
    used_points <- filter(points, geocat_use)
    
    leafletProxy("mymap", data=used_points) %>%
    leaflet::addCircleMarkers(popup = "popup",#~thetext,
                              layerId = ~geocat_id,
                              group="mappoints",
                              radius = 7,
                              color="#FFFFFF",
                              stroke = T,
                              weight = 2.5,
                              fill = T,
                              fillColor = ~used_pal(geocat_source),
                              fillOpacity = 0.5,
                              options = markerOptions(draggable = FALSE))
    
    unused_pal <- colorFactor(
      palette=c("#a5cd96", "#7fbbd9", "#f5d5bd"),
      domain=c("GBIF", "User CSV", "User point")
    )
    
    unused_points <- filter(points, ! geocat_use)
    
    leafletProxy("mymap", data=unused_points) %>%
      leaflet::addCircleMarkers(layerId = ~geocat_id,
                                group="mappoints_nu",
                                radius = 7,
                                color="#BBBBBB",
                                stroke = T,
                                weight = 2,
                                fill = T,
                                fillColor = ~unused_pal(geocat_source),
                                fillOpacity = 0.2,
                                options = markerOptions(draggable = FALSE))
  })
  
  shiny::observeEvent(list(input$Analysis, values$eoo_polygon, values$aoo_polygon), {
    
    if (input$Analysis & !is.null(values$aoo_polygon)){
      leaflet::leafletProxy("mymap", data=values$aoo_polygon) %>%
        leaflet::addPolygons(
          color = "#000000",
          stroke = T,
          weight = 2,
          fillOpacity = 0.3,
          fill = T,
          fillColor = "red",
          group = "AOOpolys"
        )
    } else {
      leafletProxy("mymap") %>%
        clearGroup("AOOpolys")
    }
    
    if (input$Analysis & !is.null(values$eoo_polygon)) {
      leaflet::leafletProxy("mymap", data=values$eoo_polygon) %>%
        leaflet::clearGroup("EOOpolys") %>%
        leaflet::addPolygons(
          color = "#000000",
          stroke = T,
          weight = 2,
          fillOpacity = 0.2,
          fill = T,
          fillColor = "grey",
          group = "EOOpolys"
        )
    } else {
      leaflet::leafletProxy("mymap") %>%
        leaflet::clearGroup("EOOpolys")
    }
    
  })
}

shinyApp(ui, server, ...)
}
