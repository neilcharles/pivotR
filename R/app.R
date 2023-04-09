#' Call the pivotR app as an RStudio addin
#'
#' @return A shiny gadget
#' @export
#'
#' @examples
#' # test
runAddin <- function() {
  selection <-
    rstudioapi::getActiveDocumentContext()[["selection"]][[1]][["text"]]
  
  # Check selection is a loaded data frame
  objs <- eapply(.GlobalEnv, is.data.frame) |>
    tibble::as_tibble()
  
  if (!selection %in% names(objs)) {
    stop('Selection is not loaded in the environment')
  }
  
  if (objs[selection] == FALSE) {
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
    header = bs4Dash::dashboardHeader(title = bs4Dash::dashboardBrand(title = "pivotR",
                                                                      color = "white")),
    # Right sidebar ------------------------------------------------------------
    sidebar = bs4Dash::dashboardSidebar(disable = TRUE),
    controlbar = bs4Dash::dashboardControlbar(
      id = 'control_bar',
      collapsed = FALSE,
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
      )
    ),
    dark = NULL,
    # Body ---------------------------------------------------------------------
    body = bs4Dash::dashboardBody(
      # fresh::use_theme(fresh::create_theme('#000000')),
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
        shiny::column(
          10,
          bs4Dash::tabsetPanel(
            shiny::tabPanel(title = "Plot",
                            plotly::plotlyOutput("plot", height = 800)),
            shiny::tabPanel(title = "Map",
                            leaflet::leafletOutput("map", height = 800))
          )
        ),
        # RHS Options ----------------------------------------------------------
        shiny::column(
          width = 2,
          bs4Dash::tabsetPanel(
            id = "main_menu",
            shiny::tabPanel(title = "Pre-Chart",
                            
                            shiny::uiOutput("ui_menu_pre_chart")),
            shiny::tabPanel(title = "Layout",
                            
                            shiny::uiOutput("ui_menu_layout")),
            shiny::tabPanel(title = "Chart",
                            shiny::uiOutput("ui_menu_chart"))
          )
        ),
        title = "BARB Browser"
      )
    )
  )
  
  server <- function(input, output, session) {
    
    output$ui_menu_pre_chart <- shiny::renderUI({
      shiny::tagList(
        shiny::br(),
        p("Pre-Chart Filter"),
        shiny::uiOutput("filter_fields_select"),
        shiny::uiOutput("filter_values_select"),
        shiny::hr(),
        p("Pre-Chart Roll-Up"),
        shiny::uiOutput("grouping_fields_select"),
        shiny::uiOutput("grouping_calc_select_pre")
      )
    })
    
    output$ui_menu_layout <- shiny::renderUI({
      shiny::tagList(
        shiny::br(),
        
        shiny::uiOutput("cols_select"),
        shiny::uiOutput("rows_select"),
        shiny::hr(),
        shiny::uiOutput("grouping_calc_select_chart"),
        shiny::hr(),
        shiny::uiOutput("detail_select"),
        shiny::uiOutput("size_select"),
        shiny::uiOutput("colour_select"),
        shiny::uiOutput("text_select")
      )
    })
    
    output$ui_menu_chart <- shiny::renderUI({
      shiny::tagList(
        shiny::br(),
        
        shiny::uiOutput("chart_types"),
        shiny::hr(),
        shiny::uiOutput("size_slider")
      )
    })
    
    # Filter and roll up input -------------------------------------------------
    input_pre_processed <- reactive({
      input_processed <- input_raw
      
      req(input$uiDateSelect)
      
      # Add date variables
      if (input$uiDateSelect != "[NONE]") {
        input_processed <- input_processed |>
          dplyr::mutate(
            !!glue::glue("{input$uiDateSelect}_Year") := lubridate::floor_date(!!rlang::sym(input$uiDateSelect), unit = "year")
          ) |>
          dplyr::mutate(
            !!glue::glue("{input$uiDateSelect}_Month") := lubridate::floor_date(!!rlang::sym(input$uiDateSelect), unit = "month")
          ) |>
          dplyr::mutate(
            !!glue::glue("{input$uiDateSelect}_Week") := lubridate::floor_date(!!rlang::sym(input$uiDateSelect), unit = "week")
          ) |>
          dplyr::select(c(
            input$uiDateSelect,
            glue::glue("{input$uiDateSelect}_Year"),
            glue::glue("{input$uiDateSelect}_Month"),
            glue::glue("{input$uiDateSelect}_Week")
          ),
          everything())
      }
      
      # Filter the input
      if (!is.null(input$uiFilterValuesSelect)) {
        input_processed <- input_processed |>
          dplyr::filter((as.character(
            !!rlang::sym(input$uiFilterFieldsSelect)
          )) %in% input$uiFilterValuesSelect)
      }
      
      input_processed
    })
    
    input_names <- reactive({
      names(input_pre_processed())
    })
    
    summary_function_pre <- reactive({
      match.fun(input$uiGroupingCalcSelectPre)
    })
    
    summary_function_chart <- reactive({
      match.fun(input$uiGroupingCalcSelectChart)
    })
    
    vars_in_use <- reactive({
      shiny::req(input$uiRowsSelect, input$uiColsSelect)
      
      vars_all <- c(
        row = input$uiRowsSelect,
        col = input$uiColsSelect,
        detail = input$uiDetailSelect,
        colour = input$uiColourSelect,
        size = input$uiSizeSelect,
        text = input$uiTextSelect
      )
      
      vars_all <- vars_all[vars_all != "[NONE]"]
      
      vars_all
    })
    
    dimensions_in_use <- reactive({
      shiny::req(input$uiRowsSelect, input$uiColsSelect)
      
      dimension_vars <- vars_in_use()
      
      # Don't group by text for table viz
      if (input$uiChartTypes == "Table") {
        dimension_vars <-
          dimension_vars[-which(names(dimension_vars) == "text")]
      }
      
      # Remove any metrics that have been classed as grouping vars
      dimension_vars_no_metrics <-
        dimension_vars[!dimension_vars %in% input$uiMetricsSelect]
      
      dimension_vars_no_metrics
    })
    
    metrics_in_use <- reactive({
      shiny::req(input$uiRowsSelect, input$uiColsSelect)
      
      vars_in_use()[vars_in_use() %in% input$uiMetricsSelect]
    })
    
    # Build viz rollup table ---------------------------------------------------
    input_rollup <- reactive({
      req(dimensions_in_use(), metrics_in_use())
      
      # Pre visualisation rollup
      if (!is.null(input$uiGroupingFieldsSelect)) {
        rolled_up_input <- input_pre_processed() |>
          dplyr::group_by(dplyr::across(dplyr::all_of(
            c(
              input$uiGroupingFieldsSelect,
              unname(dimensions_in_use())
            )
          ))) |> # Rollup grouping
          dplyr::summarise(dplyr::across(input$uiMetricsSelect, summary_function_pre())) |>
          dplyr::ungroup()
      } else {
        rolled_up_input <- input_pre_processed()
      }
      
      # Group by grouping vars for rows and cols
      rolled_up_input <- rolled_up_input |>
        dplyr::group_by(dplyr::across(dplyr::all_of(unname(
          dimensions_in_use()
        )))) |> # Rows and cols grouping
        dplyr::summarise(dplyr::across(unname(metrics_in_use()), summary_function_chart())) |>
        dplyr::ungroup()
      
      rolled_up_input
    })
    
    output$filter_fields_select <- shiny::renderUI({
      shiny::selectInput("uiFilterFieldsSelect",
                         "Fields",
                         names(input_raw),
                         multiple = FALSE)
    })
    
    output$filter_values_select <- shiny::renderUI({
      req(input$uiFilterFieldsSelect)
      shiny::selectInput(
        "uiFilterValuesSelect",
        input$filterFieldsSelect,
        unique(input_raw[input$uiFilterFieldsSelect]),
        multiple = TRUE
      )
    })
    
    output$grouping_fields_select <- shiny::renderUI({
      shiny::selectInput("uiGroupingFieldsSelect",
                         "Fields",
                         input_names()[!input_names() %in% input$uiMetricsSelect],
                         multiple = TRUE)
    })
    
    output$grouping_calc_select_pre <- shiny::renderUI({
      shiny::selectInput(
        "uiGroupingCalcSelectPre",
        "Calculation",
        c("sum", "mean", "min", "max"),
        multiple = FALSE
      )
    })
    
    output$grouping_calc_select_chart <- shiny::renderUI({
      shiny::selectInput(
        "uiGroupingCalcSelectChart",
        "Calculation",
        c("sum", "mean", "min", "max"),
        multiple = FALSE
      )
    })
    
    
    output$rows_select <- shiny::renderUI({
      shiny::selectInput(
        "uiRowsSelect",
        "Vertical (y)",
        input_names(),
        input$uiMetricsSelect[1],
        multiple = FALSE
      )
    })
    
    output$cols_select <- shiny::renderUI({
      shiny::selectInput("uiColsSelect",
                         "Horizontal (x)",
                         input_names(),
                         input_names()[1],
                         multiple = FALSE)
    })
    
    output$detail_select <- shiny::renderUI({
      shiny::selectInput("uiDetailSelect",
                         "Detail",
                         c("[NONE]", input_names()),
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
      shiny::selectInput("uiTextSelect",
                         "Text",
                         c("[NONE]", input_names()),
                         multiple = FALSE)
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
    
    output$size_slider <- renderUI({
      shiny::sliderInput(
        "uiSize",
        "Size",
        min = 1,
        max = 200,
        value = 10
      )
    })
    
    # Dates setup --------------------------------------------------------------
    output$date_select <- shiny::renderUI({
      default_date <-
        names(input_raw)[sapply(input_raw, lubridate::is.Date)][1]
      
      if (is.na(default_date))
        default_date <- "[NONE]"
      
      shiny::selectInput("uiDateSelect",
                         "Date",
                         c("[NONE]", names(input_raw)),
                         default_date,
                         multiple = FALSE)
      
    })
    
    # Geo Setup ----------------------------------------------------------------
    output$latitude_select <- shiny::renderUI({
      default_lat_id <- which(names(input_raw) %in% c('latitude', 'lat'))
      
      if (length(default_lat_id > 0)) {
        default_lat <- names(input_raw)[default_lat_id][1]
      } else {
        default_lat <- ""
      }
      
      shiny::selectInput("uiLatitudeSelect",
                         "Latitude",
                         names(input_raw),
                         default_lat,
                         multiple = FALSE)
    })
    
    output$longitude_select <- shiny::renderUI({
      default_lon_id <-
        which(names(input_raw) %in% c('longitude', 'lon', 'long'))
      
      if (length(default_lon_id > 0)) {
        default_lon <- names(input_raw)[default_lon_id][1]
      } else {
        default_lon <- ""
      }
      
      shiny::selectInput("uiLongitudeSelect",
                         "Longitude",
                         names(input_raw),
                         default_lon,
                         multiple = FALSE)
    })
    
    input_sf <- reactive({
      sf::st_as_sf(
        input_pre_processed(),
        coords = c(input$uiLongitudeSelect, input$uiLatitudeSelect),
        crs = 4326
      )
    })
    
    # Chart formatting switches ------------------------------------------------
    
    plotly_size <- reactive({
      plotly_format <-
        if (input$uiSizeSelect == "[NONE]") {
          as.formula(glue::glue("~{input$uiSize}"))
        } else {
          as.formula(
            glue::glue(
              "~{input$uiSizeSelect} / max({input$uiSizeSelect}) * {input$uiSize}"
            )
          )
        }
      
      plotly_format
    })
    
    plotly_colour <- reactive({
      plotly_format <-
        if (input$uiColourSelect == "[NONE]") {
          "light blue"
        } else {
          as.formula(glue::glue("~{input$uiColourSelect}"))
        }
      
      plotly_format
    })
    
    plotly_detail <- reactive({
      plotly_format <-
        if (input$uiDetailSelect == "[NONE]") {
          "x"
        } else {
          as.formula(glue::glue("~{input$uiDetailSelect}"))
        }
      
      plotly_format
    })
    
    # Charts -------------------------------------------------------------------
    output$chart_types <- shiny::renderUI({
      
      shiny::selectInput(
        "uiChartTypes",
        "Chart Type",
        c("Line", "Scatter", "Column", "Bar", "Table"),
        "Line",
        multiple = FALSE
      )
    })
    
    output$plot <- plotly::renderPlotly({
      
      req(input$uiChartTypes)
      
      plot <- input_rollup() |>
        plotly::plot_ly()
      
      if (input$uiChartTypes == "Line") {
        plot <- plot |>
          plotly::add_lines(
            y = as.formula(glue::glue("~{input$uiRowsSelect}")),
            x = as.formula(glue::glue("~{input$uiColsSelect}")),
            color = plotly_detail(),
            line = list(width = as.formula(
              glue::glue("~{input$uiSize} / 10")
            ))
          )
      }
      
      if (input$uiChartTypes == "Column") {
        plot <- plot |>
          plotly::add_bars(
            y = as.formula(glue::glue("~{input$uiRowsSelect}")),
            x = as.formula(glue::glue("~{input$uiColsSelect}")),
            color = plotly_detail(),
            marker = list(color = plotly_colour())
          ) |>
          plotly::layout(barmode = "stack")
      }
      
      if (input$uiChartTypes == "Bar") {
        plot <- plot |>
          plotly::add_bars(
            y = as.formula(glue::glue("~{input$uiRowsSelect}")),
            x = as.formula(glue::glue("~{input$uiColsSelect}")),
            orientation = 'h',
            color = plotly_detail(),
            marker = list(color = plotly_colour())
          ) |>
          plotly::layout(barmode = "stack")
      }
      
      if (input$uiChartTypes == "Table") {
        td <- input_rollup() |>
          tidyr::pivot_wider(names_from = input$uiColsSelect,
                             values_from = input$uiTextSelect)
        
        plot <- plotly::plot_ly(
          type = 'table',
          columnwidth = c(100, 100),
          header = list(
            values = names(td),
            align = c("center", "center"),
            line = list(width = 1, color = 'black'),
            fill = list(color = c("grey", "grey")),
            font = list(
              family = "Arial",
              size = 14,
              color = "white"
            )
          ),
          cells = list(
            values = t(as.matrix(unname(td))),
            align = c("center", "center"),
            line = list(color = "black", width = 1),
            font = list(
              family = "Arial",
              size = 12,
              color = c("black")
            )
          )
        )
      }
      
      if (input$uiChartTypes == "Scatter") {
        plot <- plot |>
          plotly::add_markers(
            y = as.formula(glue::glue("~{input$uiRowsSelect}")),
            x = as.formula(glue::glue("~{input$uiColsSelect}")),
            mode = "markers",
            marker = list(
              size = plotly_size(),
              color = plotly_colour(),
              opacity = 0.5
            )
          ) |>
          plotly::layout(showlegend = FALSE)
        
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
      
      plot <- plot |>
        plotly::layout(
          xaxis = list(zeroline = FALSE,
                       showline = FALSE),
          yaxis = list(zeroline = FALSE,
                       showline = FALSE)
        )
      
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
    
    #Quit app when the browser window is closed
    session$onSessionEnded(function() {
      shiny::stopApp()
    })

    # Enable everything that's hidden at the start
    shiny::outputOptions(output, "ui_menu_pre_chart", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "ui_menu_layout", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "ui_menu_chart", suspendWhenHidden = FALSE)
    
    shiny::outputOptions(output, "cols_select", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "rows_select", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "grouping_calc_select_chart", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "detail_select", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "size_select", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "colour_select", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "text_select", suspendWhenHidden = FALSE)

    shiny::outputOptions(output, "chart_types", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "size_slider", suspendWhenHidden = FALSE)
    
    bs4Dash::updateControlbar(id = 'control_bar')
    
  }
  
  # shinyApp(ui, server)
  shiny::runGadget(ui, server, viewer = shiny::browserViewer())
}
