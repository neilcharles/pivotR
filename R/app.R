#' Call the pivotR app as an RStudio addin
#'
#' @return A shiny gadget
#' @export
#'
#' @examples
#' # test
runAddin <- function(){
  selection <- rstudioapi::getActiveDocumentContext()[["selection"]][[1]][["text"]]
  
  # Check selection is a loaded data frame
  objs <- eapply(.GlobalEnv, is.data.frame) |> 
    tibble::as_tibble()
  
  if(!selection %in% names(objs)){
    stop('Selection is not loaded in the environment')
  }
  
  if(objs[selection]==FALSE){
    stop('Selection is not a data frame')
  }
  
  pivotR(get(selection))
}

#' Run the pivotR app
#'
#' @param input_raw A data frame
#' @param ... Further arguments passed to pivotR
#'
#' @return A Shiny gadget
#' @export
#'
#' @examples
#' #pivotR(mtcars)
pivotR <- function(input_raw, ...) {
  
  ui <- bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(title = bs4Dash::dashboardBrand(
      title = "pivotR",
      color = "white"
    )),
    # Right sidebar ------------------------------------------------------------
    sidebar = bs4Dash::dashboardSidebar(disable = TRUE),
    controlbar = bs4Dash::dashboardControlbar(collapsed = FALSE,
                                              shiny::column(
                                                width = 8,
                                                shiny::p("Metric Fields"),
                                                shiny::uiOutput("metrics_select"),
                                                shiny::hr(),
                                                shiny::p("Date Fields"),
                                                shiny::uiOutput("date_select"),
                                                shiny::hr(),
                                                shiny::p("Geo Fields"),
                                                shiny::uiOutput("latitude_select"),
                                                shiny::uiOutput("longitude_select")
                                              )),
    dark = NULL,
    # Body ---------------------------------------------------------------------
    body = bs4Dash::dashboardBody(# fresh::use_theme(fresh::create_theme('#000000')),
      # CSS
      shiny::tags$head(shiny::tags$style(
        shiny::HTML(
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
      # Plot Area --------------------------------------------------------------
      shiny::fluidRow(
        shiny::column(10,
               bs4Dash::tabsetPanel(shiny::tabPanel(
                 title = "Plot",
                 plotly::plotlyOutput("plot", height = 800)
               ),
               shiny::tabPanel(
                 title = "Map",
                 leaflet::leafletOutput("map", height = 800)
               ))),
        # RHS Options ----------------------------------------------------------
        shiny::column(
          2,
          bs4Dash::box(
            id = "box_filter",
            width = 12,
            title = "Filter",
            shiny::uiOutput("filter_fields_select"),
            shiny::uiOutput("filter_values_select")
          ),
          bs4Dash::box(
            id = "box_rollup",
            width = 12,
            title = "Rollup",
            shiny::uiOutput("grouping_fields_select"),
            shiny::uiOutput("grouping_calc_select")
          ),
          bs4Dash::box(
            id = "box_layout",
            width = 12,
            title = "Layout",
            shiny::uiOutput("cols_select"),
            shiny::uiOutput("rows_select"),
            shiny::hr(),
            shiny::uiOutput("size_select"),
            shiny::uiOutput("colour_select"),
            shiny::uiOutput("text_select")
          ),
          bs4Dash::box(
            id = "box_chart",
            width = 12,
            title = "Chart",
            shiny::uiOutput("chart_types"),
            shiny::hr(),
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
  
  server <- function(input, output, session) {
    # Filter and roll up input -------------------------------------------------
    input_pre_processed <- reactive({
      input_processed <- input_raw
      
      req(input$uiDateSelect)
      
      # Add date variables
      if(input$uiDateSelect!="[NONE]"){
        input_processed <- input_processed |> 
          dplyr::mutate(!!glue::glue("{input$uiDateSelect}_Year") := lubridate::floor_date(!!rlang::sym(input$uiDateSelect), unit = "year")) |> 
          dplyr::mutate(!!glue::glue("{input$uiDateSelect}_Month") := lubridate::floor_date(!!rlang::sym(input$uiDateSelect), unit = "month")) |> 
          dplyr::mutate(!!glue::glue("{input$uiDateSelect}_Week") := lubridate::floor_date(!!rlang::sym(input$uiDateSelect), unit = "week")) |> 
          dplyr::select(c(input$uiDateSelect,
                          glue::glue("{input$uiDateSelect}_Year"),
                          glue::glue("{input$uiDateSelect}_Month"),
                          glue::glue("{input$uiDateSelect}_Week")),
                          everything())
      }
      
      # Filter the input
      if(!is.null(input$uiFilterValuesSelect)){
        input_processed <- input_processed |> 
          dplyr::filter((!!rlang::sym(input$uiFilterFieldsSelect)) %in% input$uiFilterValuesSelect)
      }
      
      input_processed
      })
    
    input_names <- reactive({
      names(input_pre_processed())
    })
    
    summary_function <- reactive({
      match.fun(input$uiGroupingCalcSelect)
    })
    
    input_rollup <- reactive({
      shiny::req(input$uiRowsSelect, input$uiColsSelect)
      
      # Build a vector of all dimension vars in use in the viz
      grouping_vars_all <- c(input$uiRowsSelect, input$uiColsSelect)
      
      grouping_vars_no_metrics <- grouping_vars_all[!grouping_vars_all %in% input$uiMetricsSelect]
      
      # Pre visualisation rollup
      if (!is.null(input$uiGroupingFieldsSelect)) {
        rolled_up_input <- input_pre_processed() |>
          dplyr::group_by(dplyr::across(dplyr::all_of(c(input$uiGroupingFieldsSelect, grouping_vars_no_metrics)))) |> # Rollup grouping
          dplyr::summarise(dplyr::across(input$uiMetricsSelect, summary_function())) |> 
          dplyr::ungroup()
      } else {
        rolled_up_input <- input_pre_processed()
      }
      
      # Group by grouping vars for rows and cols (if there are any)
      if (length(grouping_vars_no_metrics) > 0) {
        rolled_up_input <- rolled_up_input |>
          dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars_no_metrics))) |> # Rows and cols grouping
          dplyr::summarise(dplyr::across(input$uiMetricsSelect, summary_function())) |> 
          dplyr::ungroup()
      }
      
      rolled_up_input
    })
    
    output$filter_fields_select <- shiny::renderUI({
      shiny::selectInput("uiFilterFieldsSelect", "Fields", names(input_raw), multiple = FALSE)
    })
    
    output$filter_values_select <- shiny::renderUI({
      req(input$uiFilterFieldsSelect)
      shiny::selectInput("uiFilterValuesSelect", input$filterFieldsSelect, unique(input_raw[input$uiFilterFieldsSelect]), multiple = TRUE)
    })
    
    output$grouping_fields_select <- shiny::renderUI({
      shiny::selectInput("uiGroupingFieldsSelect", "Fields", input_names()[!input_names() %in% input$uiMetricsSelect], multiple = TRUE)
    })
    
    output$grouping_calc_select <- shiny::renderUI({
      shiny::selectInput("uiGroupingCalcSelect", "Calculation", c("sum", "mean", "min", "max"), multiple = FALSE)
    })
    
    output$rows_select <- shiny::renderUI({
      shiny::selectInput("uiRowsSelect", "Rows", input_names(), input$uiMetricsSelect[1], multiple = FALSE)
    })
    
    output$cols_select <- shiny::renderUI({
      shiny::selectInput("uiColsSelect",
                         "Columns",
                         input_names(),
                         input_names()[1],
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
  
    # Dates setup --------------------------------------------------------------
    output$date_select <- shiny::renderUI({
      
      default_date <- names(input_raw)[sapply(input_raw, lubridate::is.Date)][1]

      if(is.na(default_date)) default_date <- "[NONE]"
        
      shiny::selectInput(
        "uiDateSelect",
        "Date",
        c("[NONE]", names(input_raw)),
        default_date,
        multiple = FALSE
      )
      
    })
    
    # Geo Setup ----------------------------------------------------------------
    output$latitude_select <- shiny::renderUI({
      
      default_lat_id <- which(names(input_raw) %in% c('latitude', 'lat'))
      
      if(length(default_lat_id>0)){
        default_lat <- names(input_raw)[default_lat_id][1]
      } else {
        default_lat <- ""
      }
      
      shiny::selectInput(
        "uiLatitudeSelect",
        "Latitude",
        names(input_raw),
        default_lat,
        multiple = FALSE
      )
    })
    
    output$longitude_select <- shiny::renderUI({
      
      default_lon_id <- which(names(input_raw) %in% c('longitude', 'lon', 'long'))
      
      if(length(default_lon_id>0)){
        default_lon <- names(input_raw)[default_lon_id][1]
      } else {
        default_lon <- ""
      }
      
      shiny::selectInput(
        "uiLongitudeSelect",
        "Longitude",
        names(input_raw),
        default_lon,
        multiple = FALSE
      )
    })
    
    input_sf <- reactive({
      sf::st_as_sf(input_pre_processed(),
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
    
    # Collapse boxes prior to app start ----------------------------------------
    observe({
      bs4Dash::updateBox('box_filter', action = "toggle")
      bs4Dash::updateBox("box_rollup", action = "toggle")
      bs4Dash::updateBox("box_layout", action = "toggle")
      bs4Dash::updateBox("box_chart", action = "toggle")
    })
  }
  
  # shinyApp(ui, server)
  shiny::runGadget(ui, server, viewer = shiny::browserViewer())
}
