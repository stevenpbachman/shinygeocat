geocatApp <- function(...) {
  #### ui ####
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    
    # set theme
    #theme = shinythemes::shinytheme("darkly"),
    #theme = bslib::bs_theme(bootswatch = "united"),
    
    # Navigation
    shiny::navbarPage(
      
      "GeoCAT",
      id = "navGeoCAT",
      shiny::tabPanel(
        "Home",
        shiny::wellPanel(fluidRow(
          column(12,
                 align = "center",
                 tags$h1("Welcome to GeoCAT - select a species to assess")
          ),
        ),
        
        shiny::fluidRow(
          column(3),
          column(6, align = "center",
                 # testing the WCVP selector
                 shiny::selectizeInput('wcvp',
                                #options = list(placeholder = 'select a species name'),
                                label = "Select a name - use 'Backspace' or 'Delete' to reset",
                                choices = NULL
                                #selected = "new species",
                                #choices = wcvp_data
                 )
          ),
          column(3)
        ),
        
        shiny::fluidRow(
          column(4),
          column(4, align="center",
                 shiny::actionButton("gotoAnalysis", "Go to analysis",
                              style="color: #fff; background-color: #FF8C00; border-color: #CD6600")
          ),
          column(4),
        ),
        br(),
        
        shiny::fluidRow(
          column(12, align="center",
                 shiny::actionLink("gotoBlank", "Or start a blank assessment")
          ),
        ),
        br(),
        shiny::fluidRow(
          column(12, align="center",
                 tags$h4("Geospatial Conservation Assessment Tool. Perform rapid geospatial analysis of species in a simple and powerful way.")
          )
        ),
        
        shiny::fluidRow(
          column(12,
                 tags$h2(
                   "Note that this is a test version and is not stable - see list of known issues/things to do. Email s.bachman@kew.org to send any requests or highlight bugs"),
                 DT::DTOutput('issues')
                 
          ),
        ),
        
        
        
        )
      ),
      
      # closing wellPanel
      shiny::tabPanel("table",
                      shiny::fluidRow(column(
                 DT::dataTableOutput(outputId = "points_tab_selection"), width = 12
               ))),
      shiny::tabPanel("map",
               #fluidRow(column(4,
               #sidebarLayout(
               # Sidebar panel for inputs
               shiny::sidebarPanel(
                 shiny::fluidRow(
                   column(8, align="center", offset = 2,
                          tags$h4("You are assessing:")
                   )
                 ),
                 
                 shiny::textOutput("wcvp_selection"),
                 tags$hr(style="border-color: white;"),
                 
                 shinyjs::disabled(shinyWidgets::materialSwitch(inputId = "Analysis", 
                                label = "Analysis on/off", 
                                value = FALSE,
                                status = "success")),
                 
                 shiny::htmlOutput("res_title"),
                 shiny::htmlOutput("text"),
                 
                 br(),
                 
                 # try the conditional panel to switch on when gbif points or csv loaded
                 #conditionalPanel(condition = "input.csv_in == true",
                 #conditionalPanel(condition = "input.csv_in == true",
                 shinyjs::disabled(
                   shinyWidgets::materialSwitch(
                     inputId = "csv_onoff", 
                     label = "User occurrences",
                     value = FALSE,
                     status = "info",
                     right = TRUE
                   )
                 ),
                 
                 shinyjs::disabled(
                   shinyWidgets::materialSwitch(
                     inputId = "gbif_onoff", 
                     label = "GBIF occurrences",
                     value = FALSE,
                     status = "success",
                     right = TRUE
                   )
                 ),
                 
                 br(),
                 
                 shiny::fluidRow(
                   column(8, align="center", offset = 2,
                          tags$h4("Add occurrence data:")
                   )
                 ),
                 
                 # Output: Tabset w/ plot, summary, and table ----
                 shiny::tabsetPanel(type = "tabs",
                                    shiny::tabPanel("Import CSV",
                                      # Input: input csv file
                                      shiny::helpText("Upload a CSV with 'longitude' and 'latitude' fields"),
                                      shiny::fileInput("csv_in", NULL, multiple = FALSE, accept = (".csv")),
                             ), #, plotOutput("plot")),
                             shiny::tabPanel("Query GBIF",
                                      # Input: select a species from GBIF
                                      #helpText("Enter species name to search GBIF occurrences"),
                                      shiny::textInput("GBIFname", "Enter species to search GBIF"),
                                      # add fluid row here to spread out the maximum input and search button
                                      #helpText("Upper limit for GBIF occurrences (max 10,000)"),
                                      
                                      shiny::fluidRow(
                                        column(12, align="center",
                                               tags$h5("Default 1,000 occurrences (maximum 10,000)")
                                        ),
                                        #tags$h5("Default 1,000, maximum 10,000"),
                                        column(6, align="left", 
                                               # would be good if this defaulted to the wcvp name input
                                               textInput("GBIFmax", label = NULL, value = "1000")
                                        ),
                                        column(6, align="right", 
                                               actionButton("searchGBIF", "Query GBIF")
                                        )
                                      )
                                      
                             )),
                 
                 shiny::fluidRow(
                   column(12, align="center", verbatimTextOutput("validation"))
                 ),
                 
                 br()
                 
               ),
               
               # Main panel for displaying outputs
               #column(8,
               shiny::mainPanel(
                 leaflet::leafletOutput("mymap", width = "100%", height = 550)
               )),
      
      shiny::tabPanel("help",
                      shiny::fluidRow(
                 column(12, align="center",
                        downloadButton("downloadtempcsv", 
                                       "Download template occurrence csv file")
                 )
               ),
               br(),
               shiny::fluidRow(
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
  
  
  ##### server
  server <- function(input, output, session) {
    values <- reactiveValues(
      analysis_data=tibble::tibble()
    )
    
    # render the issues table for info
    # https://stackoverflow.com/questions/70155520/how-to-make-datatable-editable-in-r-shiny
    output$issues = DT::renderDT(
      issues, editable = TRUE, options = list(lengthChange = FALSE)
    )
    
    #observeEvent(input$issues_cell_edit, {
    #  row  <- input$issues_cell_edit$row
    #  clmn <- input$issues_cell_edit$col
    #  issues[row, clmn] <- input$issues_cell_edit$value
    #})
    
    
    # species lookup using WCVP  
    shiny::updateSelectizeInput(session,
                         'wcvp',
                         choices = wcvp_data$taxon_name_authors,
                         server = TRUE,
                         options = list(placeholder = 'select a species name'),
                         #selected = "new species",
                         label = NULL)
    
    # display selected name
    output$wcvp_selection <- shiny::renderText(input$wcvp)
    
    # link to navpanel map
    shiny::observeEvent(input$gotoAnalysis, {
      shiny::updateTabsetPanel(session, "navGeoCAT",
                        selected = "map"
      )
    })
    
    # link to navpanel map
    # need to add something to clear the default selected species
    shiny::observeEvent(input$gotoBlank, {
      shiny::updateTabsetPanel(session, "navGeoCAT",
                        selected = "map"
      )
    })
    
    # set up the download for the temp file
    output$downloadtempcsv <- shiny::downloadHandler(
      filename = function() {
        paste("csv_template.csv", sep="")
      },
      content = function(file) {
        write.csv(csv_template, file, row.names = FALSE)
      }
    )
    
    # prepare the points
    csvpointsInput <- shiny::eventReactive(input$csv_in, {
      ext <- tools::file_ext(input$csv_in$datapath)
      if (ext == "csv") {
        data <- read.csv(input$csv_in$datapath)
      } else {
        shiny::validate("Invalid file; please upload a .csv file")
      }
      
      shiny::validate(check_fields_(data, c("longitude", "latitude")))
      shiny::validate(check_numeric_(data, c("longitude", "latitude")))
      
      values$analysis_data <-
        rbind(
          values$analysis_data,
          data %>%
            dplyr::select(longitude,latitude) %>%
            dplyr::filter(if_all(everything(), ~!is.na(.))) %>%
            dplyr::filter(longitude < 180, longitude > -180,
                          latitude < 90, latitude > -90) %>%
            dplyr::mutate(source="csv")
        )
      
      data
    })
    
    # react to the GBIF search box being used
    # then trigger code to select best match and get occurrence data
    gbifpointsInput <- shiny::eventReactive(input$searchGBIF, {
      req(input$GBIFname)
      gbif_keys <- name_search(input$GBIFname)
      gbif_key <- gbif_keys$GBIF_key
      gbif_points <- get_gbif_points(gbif_key, input$GBIFmax)
      
      values$analysis_data <-
        rbind(
          values$analysis_data,
          gbif_points %>%
            dplyr::select(longitude, latitude) %>%
            dplyr::filter(if_all(everything(), ~!is.na(.))) %>%
            dplyr::filter(longitude < 180, longitude > -180,
                          latitude < 90, latitude > -90) %>%
            dplyr::mutate(source="gbif")
        )
      
      gbif_points
      
    })
    
    # leaflet base output map
    output$mymap <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        
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
    
    
    shiny::observeEvent(input$csv_in, {
      shinyjs::enable("Analysis")
      shinyjs::enable("csv_onoff")
      shinyWidgets::updateMaterialSwitch(session, "csv_onoff", value=TRUE)
    })
    
    shiny::observeEvent(input$csv_in, {
      
      leaflet::leafletProxy("mymap", data=csvpointsInput()) %>%
        
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
                         fillColor = "#0070ff")
    })
    
    # proxy map to add csv points
    shiny::observeEvent(input$csv_in,{
      
      leaflet::leafletProxy("mymap", data = csvpointsInput()) %>%
        
        #clearMarkers() %>%
        
        # zoom to fit - can we buffer this a little?
        leaflet::fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%
        
        # add markers from the data
        leaflet::addCircleMarkers(group = "View Points",
                         lng = ~longitude,
                         lat = ~latitude, 
                         radius = 7, 
                         color = "#FFFFFF", 
                         stroke = T,
                         fillOpacity = 1,
                         fill = T,
                         fillColor = "#0070ff")
    })
    
    shiny::observeEvent(input$searchGBIF, {
      shinyjs::enable("Analysis")
      shinyjs::enable("gbif_onoff")
      shinyWidgets::updateMaterialSwitch(session, "gbif_onoff", value=TRUE)
    })
    
    # proxy map to add gbif points
    shiny::observeEvent(input$searchGBIF,{
      leaflet::leafletProxy("mymap", data = gbifpointsInput()) %>%
        # zoom to fit - can we buffer this a little?
        leaflet::fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%
        # add markers from the data
        leaflet::addCircleMarkers(group = "View Points",
                         lng = ~longitude,
                         lat = ~latitude, 
                         radius = 7, 
                         color = "#FFFFFF", 
                         stroke = T,
                         fillOpacity = 1,
                         fill = T,
                         fillColor = "#008000")
      
    })
    
    #output to analysis on/off switch
    calculateAnalyisis <- eventReactive(list(input$Analysis, input$gbif_onoff, input$csv_onoff), {

      if (input$Analysis) {
        d <- values$analysis_data
        if (!input$gbif_onoff) {
          d <- dplyr::filter(d, source != "gbif")
        }
        
        if (!input$csv_onoff) {
          d <- dplyr::filter(d, source != "csv")
        }
        
        if (nrow(d) == 0)  {
          return()
        }
        
        d <- dplyr::select(d, -source)
        
        EOO <- red::eoo(d)
        AOO <- red::aoo(d)
        
        str1 <-
          paste("Extent of occurrence (EOO): ",
                format(round(as.numeric(EOO)), big.mark = ","),
                "(km squared)")
        str2 <-
          paste("Area of occupancy (AOO): ",
                format(round(as.numeric(AOO)), big.mark = ","),
                "(km squared)")
        HTML(paste(str1, str2, sep = '<br>')
        )
      }
    })
    
    # render the output of the EOO and AOO results
    output$res_title <- shiny::renderUI({
      HTML(paste0("<b>", "Results:", "</b>"))
    })
    
    output$text <- renderUI({
      calculateAnalyisis()
    })
    
    # make a polygon from imported csv points
    polyInput = shiny::reactive({
      df <- csvpointsInput()
      
      poly <- df %>%
        dplyr::filter(if_all(c(longitude, latitude), ~!is.na(.))) %>%
        dplyr::filter(longitude > -180, longitude < 180, 
                      latitude > -90, latitude < 90) %>%
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
        sf::st_combine()  %>%
        sf::st_convex_hull()  
    })
    
    # proxy map to add polygon
    shiny::observeEvent(input$Analysis, {
      
      if (input$Analysis){
        

        leaflet::leafletProxy("mymap",data = polyInput()) %>%
          # add polygons input from csv
          leaflet::addPolygons(
            color = "#000000",
            stroke = T,
            weight = 3,
            fillOpacity = 0.4,
            fill = T,
            fillColor = "#999999")
        
        # TO DO - add AOO cells?
      } else {
        leaflet::leafletProxy("mymap", data=polyInput()) %>%
          # clear previous polygons
          leaflet::clearShapes()
      }
      
    })
  }
  
  shinyApp(ui, server, ...)
}
