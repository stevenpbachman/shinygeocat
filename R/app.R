#' @import shiny dplyr
geocatApp <- function(...) {
  timeoutTime = 15
  circleRadius = 7
  customCol = "#ECAC7C"
  gbifCol = "#509E2F"
  csvCol = "#0078b4"
  gbifOffCol = "#a5cd96"
  csvOffCol = "#7fbbd9"
  
  #### ui ####
  ui <- fluidPage(
    theme = shinythemes::shinytheme("darkly"),
    shinyjs::useShinyjs(),
    
    tags$html(lang = "en", `data-timeout-mins` = timeoutTime, `data-circle-radius` = circleRadius),
    tags$head(
      tags$title("ShinyGeoCAT  - Geospatial Conservation Assessment Tools"), # WCAG modification
      tags$link(rel = "stylesheet", href = "style.css"),
      tags$script(src = "script.js", type = "module")
    ),
    
    tags$header(
      tags$h1("ShinyGeoCAT - Geospatial Conservation Assessment Tools"),
      tags$span(
        id="beta",
        "This is a BETA test version. Send any feedback to ",
        tags$a(href="mailtto:s.bachman@kew.org", "s.bachman@kew.org"),
        "."
      )
    ),
    
    tags$main(
      tabsetPanel(
        tabPanel(
          "Analysis",
          # Sidebar panel for inputs
          tags$div(
            id="analysis-grid",
            tags$section(
              id="controls-section",
              class="well",
              
              tags$details(
                open="open",
                tags$summary(tags$h2("Data")),
                
                tags$details(
                  class="help",
                  tags$summary("Help"),
                  "Add point data to the map by uploading a CSV file, by querying GBIF, or manual addition through the circle marker icon. Enter a POWO code to map the native range of a species and use toggle to exclude non-native points."
                ),
                
                tags$div(
                  id="csv-block",
                  class="data-block",
                  tags$label(
                    "for"="csv_in",
                    "Upload a CSV with at least 'longitude', 'latitude' fields",
                  ),
                  tags$div(
                    id = "file-wrapper",
                    tags$input(
                      id="csv_in",
                      type="file",
                      accept = ".csv",
                    ),
                    tags$span(
                      "aria-hidden" = "true",
                      "class" = "file-text",
                      'No file uploaded'
                    )
                  ),
                  tags$div(
                    id = paste("csv_in_progress", sep = ""),
                    class = "progress active shiny-file-input-progress",
                    tags$div(class = "progress-bar")
                  ),
                  shinyjs::hidden(
                    ## csv points on/off ----
                    shinyWidgets::prettySwitch(
                      inputId = "csv_onoff",
                      label = "CSV",
                      value = TRUE,
                      status = "primary",
                      fill = TRUE
                    )
                  )
                ),
                
                tags$div(
                  id="gbif-block",
                  class="data-block text-input-block",
                  tags$label("for"="gbif_name", "Enter a taxon name to load points from GBIF:"),
                  tags$div(
                    "class" = "text-input-wrapper",
                    textInput("gbif_name", label=NULL)
                  ),
                  tags$span(id="gbif-hint", class = "hint", "e.g. Cyphostemma njegerre"),
                  actionButton("queryGBIF", "Load points"),
                  tags$span(
                    "class" = "toggle-wrapper",
                    shinyjs::hidden(
                      ## GBIF points on/off ----
                      shinyWidgets::prettySwitch(
                        inputId = "gbif_onoff",
                        label = "GBIF",
                        value = TRUE,
                        status = "success",
                        fill = TRUE
                      )
                    )
                  )
                ),
                
                tags$div(
                  id="powo-block",
                  class="data-block text-input-block",
                  tags$label("for"="powo_id", "Enter a POWO ID for a native range map:"),
                  tags$div(
                    "class" = "text-input-wrapper",
                    textInput("powo_id", label=NULL)
                  ),
                  tags$span(
                    "id" = "powo-hint",
                    "class" = "hint",
                    "e.g. 68179-1. Search",
                    tags$a(href="https://powo.science.kew.org/", target="_blank", "POWO"),
                    "to get accepted name ID."
                  ),
                  actionButton("queryPOWO", "Load map"),
                  tags$span(
                    "class" = "toggle-wrapper",
                    shinyjs::hidden(
                      shinyWidgets::prettySwitch(
                        inputId = "native_onoff",
                        label = "Exclude non-native",
                        value = FALSE,
                        status = "danger",
                        fill = TRUE
                      )
                    )
                  )
                )
              ),
              
              tags$details(
                open="open",
                tags$summary(tags$h2("Analysis")),
                
                tags$details(
                  class="help",
                  tags$summary("Help"),
                  "Toggle the Analysis on/off button to analyse points and generate extent of occurrence (EOO) and area of occupancy (AOO) estimates and preliminary Red List categories."
                ),
                
                tags$p(
                  "id" = "analysis-info",
                  "Analysis requires at least two data points"  
                ),
                
                shinyjs::hidden(
                  shinyWidgets::prettySwitch(
                    inputId = "Analysis",
                    label = "Analysis on/off",
                    value = FALSE,
                    status = "success",
                  )
                ),
                
                shiny::htmlOutput("res_title"),
                shiny::htmlOutput("text"),
              ),
              
              tags$details(
                open="open",
                tags$summary(tags$h2("Next Steps")),
                
                tags$details(
                  class="help",
                  tags$summary("Help"),
                  "Download your point data in a format aligned with the IUCN Mapping Standards, or click Reset to start over."
                ),
                
                tags$div(
                  id="next-step-buttons",
                  downloadButton("download_csv", "Download CSV file"),
                  actionButton("reset", "Reset")
                )
              )
            ),
            
            #### Main panel for displaying outputs ####
            
            tags$section(
              id="map-section",
              div(
                leaflet::leafletOutput("mymap", height = 550)
              )
            ),
            
            tags$section(
              id="log-section",
              class="well",
              tags$h2("Message Log"),
              tags$div(
                id = "message-log",
                htmlOutput("messages") 
              )
            )
          )
        ),
        tabPanel(
          "Help",
          tags$section(
            class="well",
            tags$h2('Help'),
            tags$p("Help goes here")
          )
        )
      )
    ),
    
    tags$footer(
      p(
        "Moat, J., Bachman, S., & Walker, B. (2023). ShinyGeoCAT - Geospatial Conservation Assessment Tools (BETA) [Software]. ",
        "Available from ",
        tags$a(
          href="https://spbachman.shinyapps.io/geocat_staging/",
          "https://spbachman.shinyapps.io/geocat_staging/"
        )
      )
    ),
    
    tags$section(
      id="offscreen-items",
      tags$div(
        id="key-custom-point",
        class="floating-item",
        tags$div(
          tags$p("Add custom point"),
          tags$div(
            class="floating-form",
            tags$div(
              class="input-wrapper",
              numericInput("custom-long", "Longitude (°)", min=-180, max=180, value=0),
            ),
            tags$div(
              class="input-wrapper",
              numericInput("custom-lat", "Latitude (°)", min=-90, max=90, value=0),
            ),
            actionButton("key-add-point", "Add point"),
          )
        )
      ),
      tags$div(
        id='disconnect-warning-dialog',
        class="floating-item",
        role="dialog",
        `aria-modal`="true",
        `aria-label`="Disconnect-warning dialog",
        `aria-describedby`="disconnect-warning-description",
        tags$div(
          tags$p(
            id="disconnect-warning-description",
            "Your session will timeout in under 60 seconds due to inactivity."
          ),
          actionButton("continue", "Click to continue your session"),
        )
      ),
      tags$div(
        id='disconnect-dialog',
        class="floating-item",
        role="dialog",
        `aria-modal`="true",
        `aria-label`="Disconnection dialog",
        `aria-describedby`="disconnect-description",
        tags$div(
          tags$p(
            id="disconnect-description",
            glue::glue("Your session has unexpectedly ended, or timed out because of {timeoutTime} minutes of inactivity.")
          ),
          actionButton("reload", "Click to restart your session"),
        )
      )
    )
  )
  
  ##### server #####
  server <- function(input, output, session) {
    values <- reactiveValues(
      analysis_data=empty_tbl_(),
      non_native=NA_real_
    )
    
    observeEvent(input$reset, {
      session$reload()
    })
    
    ##################################
    # prepare the points
    observeEvent(input$csv_in, {
      ext <- tools::file_ext(input$csv_in$datapath)
      if (ext == "csv") {
        data <- import_csv(input$csv_in$datapath)
      } else {
        msg <- error_message("Invalid file; please upload a .csv file")
        values$messages <- c(values$messages, msg)
        return()
      }
      
      validated <- validate_csv(data)
      values$messages <- c(values$messages, validated$msg)
      
      valid_points <- validated$valid_data
      if (! is.null(valid_points)) {
        values$analysis_data <- bind_rows(values$analysis_data, valid_points)  
      }
      
      msg <- glue::glue("Loaded {sum(valid_points$geocat_use)} points from a CSV")
      
      session$sendCustomMessage("fileuploaded", input$csv_in$name)
      
      values$messages <- c(values$messages, info_message(msg))
    })
    
    observeEvent(input$queryGBIF, {
      points <- import_gbif(input$gbif_name)
      
      validated <- validate_gbif(points)
      values$messages <- c(values$messages, validated$msg)
      
      valid_points <- validated$valid_data
      if (nrow(valid_points) > 0) {
        values$analysis_data <- bind_rows(values$analysis_data, valid_points)
        
        msg <- glue::glue("Loaded {nrow(valid_points)} points for <i>{input$gbif_name}</i> from GBIF")
        values$messages <- c(values$messages, info_message(msg))
      }
    })
    
    observeEvent(input$queryPOWO, {
      geoms <- import_powo(input$powo_id)
      values$native_geom <- geoms
      
      if (! is.null(geoms)) {
        msg <- glue::glue("Loaded {nrow(geoms)} native regions from POWO for taxon {input$powo_id}")
        msg <- info_message(msg)
      } else {
        msg <- glue::glue("No entry found in POWO for taxon {input$powo_id}")
        msg <- error_message(msg)
      }
      values$messages <- c(values$messages, msg)
    })
    
    output$messages <- renderPrint({
      glue::glue_collapse(rev(values$messages), sep="\n<br>\n")
    })
    
    observeEvent(input$queryGBIF, {
      shinyWidgets::updateMaterialSwitch(session, "native_onoff", value=FALSE)
    })
    
    observeEvent(input$csv_in, {
      shinyWidgets::updateMaterialSwitch(session, "native_onoff", value=FALSE)
    })
    
    shiny::observeEvent(req(!is.null(values$native_geom) & nrow(values$analysis_data) > 0), {
      
      # can't stop this getting called twice cos it updates `values$analysis_data`
      values$analysis_data <- flag_native(values$analysis_data, values$native_geom)
      non_native <- sum(! values$analysis_data$geocat_native)
      
      # a bit hacky but stops this sending more than one message at a time
      updated <- values$non_native != non_native
      if (updated %in% c(FALSE)) {
        return()
      }
      
      msg <- glue::glue("Found {non_native} points outside native range")
      values$messages <- c(values$messages, alert_message(msg))
      values$non_native <- non_native
    })
    
    # leaflet base output map ----
    prot_planet_url <- "https://data-gis.unep-wcmc.org/server/rest/services/ProtectedSites/The_World_Database_of_Protected_Areas/MapServer/tile/{z}/{y}/{x}"
    
    output$mymap <- leaflet::renderLeaflet({
      leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 1, attributionControl=FALSE,worldCopyJump=TRUE)) %>%
        leaflet.extras::addSearchOSM(options = leaflet.extras::searchOptions(
          autoCollapse = F,
          collapsed = F,
          minLength = 2,
          position = "topright"
        )) %>%
        leaflet::setView(0 ,0,zoom = 2) %>%
        
        leaflet::addScaleBar(position = "bottomright") %>%
        
        leafem::addMouseCoordinates() %>%
        
        leaflet.extras::addDrawToolbar(editOptions = editToolbarOptions(edit=TRUE),
                                       targetGroup = 'mappoints',
                                       circleMarkerOptions=drawCircleMarkerOptions(
                                         color="#FFFFFF",
                                         radius = circleRadius,
                                         stroke=T,
                                         weight=2.5,
                                         fill=T,
                                         fillColor=customCol,
                                         opacity=1,
                                         fillOpacity=0.5,
                                         repeatMode = TRUE,
                                         zIndexOffset = 2000 #not making any difference
                                       ),
                                       markerOptions=FALSE,
                                       rectangleOptions=FALSE,
                                       circleOptions=FALSE,
                                       polygonOptions=FALSE,
                                       polylineOptions=FALSE) %>%
        
        leaflet.extras::addFullscreenControl(position= "topleft") %>%
        
        leaflet::addMeasure(
          position = "bottomleft",
          primaryLengthUnit = "meters",
          primaryAreaUnit = "sqmeters",
          activeColor = "#10bae0",
          completedColor = "#241ad9"
        ) %>%
        
        addTiles() %>%
        
        addMapPane("powopolys", zIndex = 410)%>%
        addMapPane("EOOpolys",zIndex = 420)%>%
        addMapPane("AOOpolys",zIndex = 430)%>%
        addMapPane("unmappoints",zIndex = 440)%>%
        addMapPane("mappoints",zIndex = 450)%>%
        
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
          group = "ESRI Imagery",
          options = leaflet::providerTileOptions(noWrap = FALSE)
        )  %>%
        
        leaflet::addProviderTiles(
          provider = "OpenTopoMap",
          group = "Open Topo Map",
          options = leaflet::providerTileOptions(noWrap = FALSE)
        )  %>%
        
        leaflet::addTiles(
          urlTemplate = prot_planet_url,
          group = "Protected Planet"
        )  %>%

        leaflet::hideGroup("Protected Planet") %>%
        
        leaflet::addLayersControl(
          baseGroups = c(
            "CartoDB Voyager",
            "Open Street Map",
            "ESRI Imagery",
            "Open Topo Map"
          ),
          overlayGroups = "Protected Planet",
          options = leaflet::layersControlOptions(collapsed = TRUE)
        ) 
    })
    
    #####Map Events############
    #add new point
    observeEvent(input$mymap_draw_new_feature, {
      point_data <- add_point(input$mymap_draw_new_feature)
      values$analysis_data <- bind_rows(values$analysis_data, point_data)
      msg <- format_new_point(input$mymap_draw_new_feature)
      values$messages <- c(values$messages, info_message(paste(msg)))
    })
    #I DONT THINK THIS IS USED IN ANYWAY
    observeEvent(input$`key-add-point`, {
      long <- input$`custom-long`
      lat <- input$`custom-lat`
      feature <- create_point_feature(long, lat)
      point_data <- add_point(feature)
      values$analysis_data <- bind_rows(values$analysis_data, point_data)
      msg <- format_new_point(feature)
      values$messages <- c(values$messages, info_message(paste(msg)))   
      
      
    })
    
    observeEvent(input$reload, {
      session$reload()
    })
    
    #move points
    observeEvent(input$mymap_draw_edited_features, {
      for (feature in input$mymap_draw_edited_features$features){
        values$analysis_data <- move_point(feature, values$analysis_data)
        
        msg <- format_move_point(feature)
        values$messages <- c(values$messages, info_message(msg))
      }
    })
    
    #delete points = actually just marks them not to display
    observeEvent(input$mymap_draw_deleted_features, {
      for (feature in input$mymap_draw_deleted_features$features){
        values$analysis_data <- delete_point(feature, values$analysis_data)
        
        msg <- format_delete_point(feature)
        values$messages <- c(values$messages, info_message(msg))
      }
    })
    
    ############################
    observeEvent(req(nrow(values$analysis_data) > 1), {
      shinyjs::show("Analysis")
      shinyjs::hide("analysis-info")
    }, once=TRUE)
    
    shiny::observeEvent(req(sum(values$analysis_data$geocat_source == "User CSV") > 0), {
      shinyjs::show("csv_onoff")
      shinyWidgets::updateMaterialSwitch(session, "csv_onoff", value=TRUE)
    }, once=TRUE)
    
    shiny::observeEvent(req(sum(values$analysis_data$geocat_source == "GBIF") > 0), {
      shinyjs::show("gbif_onoff")
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
    
    observeEvent(input$native_onoff, {
      values$analysis_data <- values$analysis_data %>%
        mutate(geocat_use=ifelse(geocat_native, geocat_use, !input$native_onoff)) %>%
        # make sure deleted points aren't turned back on
        mutate(geocat_use=ifelse(geocat_deleted, FALSE, geocat_use))
    }, ignoreInit=TRUE, ignoreNULL=TRUE)
    
    shiny::observeEvent(req(! is.null(values$native_geom)), {
      shinyjs::show("native_onoff")
    })
    
    shiny::observeEvent(req(! is.null(values$native_geom)), {
      bb <- sf::st_bbox(values$native_geom)
      leaflet::leafletProxy("mymap") %>%
        leaflet::clearGroup("powopolys") %>%
        #zoom to
        leaflet::fitBounds(bb[[1]], bb[[2]], bb[[3]], bb[[4]]) %>%
        
        leaflet::addPolygons(
          data = values$native_geom,
          color = "red",
          weight = 2,
          fillColor = "red",
          group = "powopolys",
          options = pathOptions(pane = "powopolys")
        ) 
      
    })
    
    #output to analysis on/off switch
    calculateAnalysis <- eventReactive(list(input$Analysis, values$analysis_data), {
      if (input$Analysis) {
        points <- filter(values$analysis_data, geocat_use)
        
        if (nrow(points) > 0) {
          points <- select(points, longitude, latitude)
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
        } else {
          HTML("<p style='color:red;'> No valid points found</p>")
        }
      }
    })
    
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
      # user points are plotted from the marker tools events not this one
      points <- filter(values$analysis_data, geocat_source != "User point")
      
      used_pal <- colorFactor(
        palette=c(gbifCol, csvCol),
        domain=c("GBIF", "User CSV")
      )
      
      unused_pal <- colorFactor(
        palette=c(gbifOffCol, csvOffCol),
        domain=c("GBIF", "User CSV")
      )
      
      used_points <- filter(points, geocat_use)
      unused_points <- filter(points, ! geocat_use)
      #popup text content
      pcontent <- paste(sep = '<br/>',
                        paste('<b><i><font size = "3">',used_points$genus,used_points$specificEpithet,'</font></i></b>'),
                        paste(sep='','<b>Latitude,Longitude </b>',used_points$latitude,',',used_points$longitude),
                        paste('<b>locality</b>', used_points$locality,'<b>Uncertainty</b>',used_points$coordinateUncertaintyInMeters,'m'),
                        paste('<b>Collector</b>',used_points$recordedBy,'<b>Number</b>', used_points$recordNumber, '<b>Year</b>', used_points$event_year),
                        paste('<font size = "1">','<b>Catalog</b>', used_points$catalogNumber,'<b>Source</b>',used_points$source,'</font>'),
                        paste('<font size = "1">','<b>GeoCAT id</b>', used_points$geocat_id,'<b>GeoCAT notes</b>', used_points$geocat_notes,'</font>'),
                        paste('<font size="1">', '<b>GBIF ID</b>', '<a href="', used_points$gbifID, '" target="_blank">GBIF occurrence link</a>', '</font>')
      )

      leafletProxy("mymap") %>%
        leaflet::addCircleMarkers(
          popup = ~pcontent,
          layerId = ~geocat_id,
          group="mappoints",
          radius = circleRadius,
          color="#FFFFFF",
          stroke = T,
          weight = 2.5,
          fill = T,
          fillColor = ~used_pal(geocat_source),
          fillOpacity = 0.5,
          #options = markerOptions(draggable = FALSE),
          options = pathOptions(pane = "mappoints", className = ~factor(geocat_source, levels=c("GBIF", "User CSV"), labels=c("shape-square", "shape-hexagon"))),
          data=used_points
        ) %>%
        leaflet::addCircleMarkers(
          layerId = ~geocat_id,
          group="unmappoints",
          radius = circleRadius,
          color="#BBBBBB",
          stroke = T,
          weight = 2,
          fill = T,
          fillColor = ~unused_pal(geocat_source),
          fillOpacity = 0.2,
          #options = markerOptions(draggable = FALSE),
          options = pathOptions(pane = "unmappoints", className = ~factor(geocat_source, levels=c("GBIF", "User CSV"), labels=c("shape-square", "shape-hexagon"))),
          data=unused_points
        )
    })
    
    shiny::observeEvent(list(input$Analysis, values$eoo_polygon, values$aoo_polygon), {
      
      if (input$Analysis & !is.null(values$aoo_polygon)){
        bb_aoo <- sf::st_bbox(values$aoo_polygon)
        leaflet::leafletProxy("mymap", data=values$aoo_polygon) %>%
          leaflet::clearGroup("AOOpolys") %>%
          #zoom to
          leaflet::fitBounds(bb_aoo[[1]], bb_aoo[[2]], bb_aoo[[3]], bb_aoo[[4]]) %>%
          leaflet::addPolygons(
            color = "#000000",
            stroke = T,
            weight = 2,
            fillOpacity = 0.3,
            fill = T,
            fillColor = "red",
            group = "AOOpolys",
            options = pathOptions(pane = "AOOpolys")
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
            group = "EOOpolys",
            options = pathOptions(pane = "EOOpolys")
          )
      } else {
        leaflet::leafletProxy("mymap") %>%
          leaflet::clearGroup("EOOpolys")
      }
      
    })
  }
  
  shinyApp(ui, server, ...)
}
