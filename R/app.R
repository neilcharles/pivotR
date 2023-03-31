library(shiny)
library(magrittr)

options("spinner.color" = "#FA9B1E")

pivotR <- function(input_raw, ...) {
  ui <- bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(title = bs4Dash::dashboardBrand(
      title = "pivotR",
      color = "white"
    )),
    sidebar = bs4Dash::dashboardSidebar(disable = TRUE),
    controlbar = bs4Dash::dashboardControlbar(collapsed = FALSE,
                                              column(
                                                width = 8,
                                                shiny::uiOutput("metrics_select"),
                                                hr(),
                                                p("Geo"),
                                                shiny::uiOutput("latitude_select"),
                                                shiny::uiOutput("longitude_select")
                                              )),
    dark = NULL,
    body = bs4Dash::dashboardBody(# fresh::use_theme(fresh::create_theme('#000000')),
      # CSS
      tags$head(tags$style(
        HTML(
          "
      .img-circle {
        border-radius: 0 !important;
      }
      .brand-link .brand-image{
        float: none !important;
      }
      .elevation-3{
        box-shadow: 0 0 0 0 !important;
      }"
        )
      )),
      fluidRow(
        column(10,
               bs4Dash::tabsetPanel(tabPanel(
                 title = "Plot",
                 plotly::plotlyOutput("plot", height = 800)
               ),
               tabPanel(
                 title = "Map",
                 leaflet::leafletOutput("map", height = 800)
               ))),
        column(
          2,
          bs4Dash::box(
            width = 12,
            title = "Layout",
            shiny::uiOutput("cols_select"),
            shiny::uiOutput("rows_select")
          ),
          bs4Dash::box(
            width = 12,
            title = "Format",
            shiny::uiOutput("size_select"),
            shiny::uiOutput("colour_select"),
            shiny::uiOutput("text_select")
          ),
          bs4Dash::box(
            width = 12,
            title = "Chart",
            shiny::uiOutput("chart_types"),
            hr(),
            shiny::sliderInput(
              "uiSize",
              "Size",
              min = 1,
              max = 200,
              value = 10
            )
          )
        )
      ),
      title = "BARB Browser")
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    # input_raw <- mtcars |>
    #   dplyr::mutate(model = rownames(mtcars))
    
    # Filter and roll up input -------------------------------------------------
    input_filter <- reactive({
      input_raw
    })
    
    input_names <- reactive({
      names(input_raw)
    })
    
    input_rollup <- reactive({
      req(input$uiRowsSelect, input$uiColsSelect)
      
      # Build a vector of all dimension vars in use in the viz
      grouping_vars <- c(input$uiRowsSelect,
                         input$uiColsSelect)[!c(input$uiRowsSelect,
                                                input$uiColsSelect) %in% input$uiMetricsSelect]
      
      # Group by grouping vars (if there are any)
      if (length(grouping_vars) > 0) {
        rolled_up_input <- input_filter() |>
          dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |>
          dplyr::summarise(dplyr::across(input$uiMetricsSelect, sum))
      } else {
        rolled_up_input <- input_raw
      }
      
      rolled_up_input
    })
    
    output$rows_select <- shiny::renderUI({
      shiny::selectInput("uiRowsSelect", "Rows", input_names(), multiple = FALSE)
    })
    
    output$cols_select <- shiny::renderUI({
      shiny::selectInput("uiColsSelect",
                         "Columns",
                         input_names(),
                         multiple = FALSE)
    })
    
    output$size_select <- shiny::renderUI({
      shiny::selectInput("uiSizeSelect",
                         "Size",
                         c("[NONE]", input$uiMetricsSelect),
                         multiple = FALSE)
    })
    
    output$colour_select <- shiny::renderUI({
      shiny::selectInput("uiColourSelect",
                         "Colour",
                         c("[NONE]", input_names()),
                         multiple = FALSE)
    })
    
    output$text_select <- shiny::renderUI({
      shiny::selectInput("uiTextSelect", "Text", c("[NONE]", input_names()), multiple = FALSE)
    })
    
    output$metrics_select <- shiny::renderUI({
      shiny::selectInput(
        "uiMetricsSelect",
        "Metrics",
        names(input_raw[, purrr::map_lgl(input_raw, is.numeric)]),
        names(input_raw[, purrr::map_lgl(input_raw, is.numeric)]),
        multiple = TRUE
      )
    })
    
    # Geo Setup ----------------------------------------------------------------
    output$latitude_select <- shiny::renderUI({
      
      default_lat_id <- which(input_names() %in% c('latitude', 'lat'))
      
      if(length(default_lat_id>0)){
        default_lat <- input_names()[default_lat_id][1]
      } else {
        default_lat <- ""
      }
      
      shiny::selectInput(
        "uiLatitudeSelect",
        "Latitude",
        input_names(),
        default_lat,
        multiple = FALSE
      )
    })
    
    output$longitude_select <- shiny::renderUI({
      
      default_lon_id <- which(input_names() %in% c('longitude', 'lon', 'long'))
      
      if(length(default_lon_id>0)){
        default_lon <- input_names()[default_lon_id][1]
      } else {
        default_lon <- ""
      }
      
      shiny::selectInput(
        "uiLongitudeSelect",
        "Longitude",
        input_names(),
        default_lon,
        multiple = FALSE
      )
    })
    
    input_sf <- reactive({
      sf::st_as_sf(input_filter(),
                   coords = c(input$uiLongitudeSelect, input$uiLatitudeSelect), 
                       crs = 4326)
    })
    
    # Charts -------------------------------------------------------------------
    output$chart_types <- shiny::renderUI({
      shiny::selectInput("uiChartTypes",
                         "Chart Type",
                         c("Line", "Bar", "Scatter"),
                         multiple = FALSE)
    })
    
    output$plot <- plotly::renderPlotly({
      plot <- input_rollup() |>
        plotly::plot_ly()
      
      if (input$uiChartTypes == "Line") {
        plot <- plot |>
          plotly::add_lines(y = as.formula(glue::glue("~{input$uiRowsSelect}")),
                            x = as.formula(glue::glue("~{input$uiColsSelect}")))
      }
      
      if (input$uiChartTypes == "Bar") {
        plot <- plot |>
          plotly::add_bars(y = as.formula(glue::glue("~{input$uiRowsSelect}")),
                           x = as.formula(glue::glue("~{input$uiColsSelect}")))
      }
      
      if (input$uiChartTypes == "Scatter") {
        plot <- plot |>
          plotly::add_markers(
            y = as.formula(glue::glue("~{input$uiRowsSelect}")),
            x = as.formula(glue::glue("~{input$uiColsSelect}")),
            mode = "markers",
            marker = list(
              size =
                if (input$uiSizeSelect == "[NONE]") {
                  as.formula(glue::glue("~{input$uiSize}"))
                } else {
                  as.formula(
                    glue::glue(
                      "~{input$uiSizeSelect} / max({input$uiSizeSelect}) * {input$uiSize}"
                    )
                  )
                },
              color =
                if (input$uiColourSelect == "[NONE]") {
                  "light blue"
                } else {
                  as.formula(glue::glue("~{input$uiColourSelect}"))
                },
              opacity = 0.5
            )
          )
        
        # Text
        if (input$uiTextSelect != "[NONE]") {
          plot <- plot |>
            plotly::add_text(
              y = as.formula(glue::glue("~{input$uiRowsSelect}")),
              x = as.formula(glue::glue("~{input$uiColsSelect}")),
              text = as.formula(glue::glue("~{input$uiTextSelect}")),
              textposition = "top right"
            )
        }
        
      }
      
      plot
    })
   
    # Maps -----------------------------------------------------------------------
    output$map <- leaflet::renderLeaflet({
      
      input_sf() |> 
        leaflet::leaflet() |> 
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |> 
        leaflet::addCircleMarkers(radius = 
                                    if (input$uiSizeSelect == "[NONE]") {
                                      input$uiSize / 10
                                    } else {
                                      as.formula(
                                        glue::glue(
                                          "~{input$uiSizeSelect} / max({input$uiSizeSelect}) * {input$uiSize} / 10"
                                        )
                                      )
                                    },
                                  weight = 1)
      
    })
     
  }
  
  # shinyApp(ui, server)
  shiny::runGadget(ui, server, viewer = browserViewer())
}
