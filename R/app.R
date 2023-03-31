library(shiny)
library(magrittr)

options("spinner.color" = itvPalette::itv_palette()$blue)

pivotR <- function(input_table, ...) {
  ui <- bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(
      title = bs4Dash::dashboardBrand(
        title = "pivotR",
        color = "white"
      )
    ),
    sidebar = bs4Dash::dashboardSidebar(disable = TRUE),
    controlbar = bs4Dash::dashboardControlbar(
      collapsed = FALSE,
      column(width = 8,
      shiny::uiOutput("metrics_select")
      )
    ),
    dark = NULL,
    body = bs4Dash::dashboardBody(
      # fresh::use_theme(fresh::create_theme(itvPalette::itv_bs4dash())),
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
             plotly::plotlyOutput("plot", height = 800)),
      column(2,
        bs4Dash::box(
          width = 12,
          title = "Data",
          shiny::uiOutput("rows_select"),
          shiny::uiOutput("cols_select"),
          shiny::uiOutput("size_select"),
          shiny::uiOutput("colour_select"),
          shiny::uiOutput("text_select")
        ),
        bs4Dash::box(
          width = 12,
          title = "Chart",           
          shiny::uiOutput("chart_types"),
          hr(),
          shiny::sliderInput("uiSize", "Size", min = 1, max = 200, value = 10)
        )
      )),      
    title = "BARB Browser"
  ))
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    
    # input_table <- mtcars |> 
    #   dplyr::mutate(model = rownames(mtcars))
    
    output$metrics <- shiny::renderUI({
      shiny::selectInput("uiMetrics", "Metrics",
                         c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),
                         multiple = TRUE)
    })
    
    rollup_table <- reactive({
      
      req(input$uiRowsSelect, input$uiColsSelect)
      
      # Build a vector of all dimension vars in use in the viz
      grouping_vars <- c(input$uiRowsSelect,
                         input$uiColsSelect)[!c(input$uiRowsSelect,
                                                input$uiColsSelect) %in% input$uiMetricsSelect]
      
      # Group by grouping vars (if there are any)
      if(length(grouping_vars)>0){
        rolled_up_input <- input_table |> 
          dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |> 
          dplyr::summarise(dplyr::across(
            input$uiMetricsSelect, sum
          ))
      } else {
        rolled_up_input <- input_table
      }
      
      rolled_up_input        
    })
    
    output$rows_select <- shiny::renderUI({
      shiny::selectInput("uiRowsSelect", "Rows", names(input_table), multiple = FALSE)
    })

    output$cols_select <- shiny::renderUI({
      shiny::selectInput("uiColsSelect", "Columns", names(input_table), multiple = FALSE)
    })
    
    output$size_select <- shiny::renderUI({
      shiny::selectInput("uiSizeSelect", "Size", c("[NONE]", input$uiMetricsSelect), multiple = FALSE)
    })
    
    output$colour_select <- shiny::renderUI({
      shiny::selectInput("uiColourSelect", "Colour", c("[NONE]", names(input_table)), multiple = FALSE)
    })
    
    output$text_select <- shiny::renderUI({
      shiny::selectInput("uiTextSelect", "Text", c("[NONE]", names(input_table)), multiple = FALSE)
    })
    
    output$metrics_select <- shiny::renderUI({
      shiny::selectInput("uiMetricsSelect", "Metrics",
                         names(input_table[, purrr::map_lgl(input_table, is.numeric)]),
                         names(input_table[, purrr::map_lgl(input_table, is.numeric)]),
                         multiple = TRUE)
    })
    
    output$chart_types <- shiny::renderUI({
      shiny::selectInput("uiChartTypes", "Chart Type", c("Line", "Bar", "Scatter"), multiple = FALSE)
    })
    
    output$plot <- plotly::renderPlotly({
      plot <- rollup_table() |> 
        plotly::plot_ly()
    
      if(input$uiChartTypes=="Line"){  
        plot<- plot |> 
          plotly::add_lines(y = as.formula(glue::glue("~{input$uiRowsSelect}")),
                            x = as.formula(glue::glue("~{input$uiColsSelect}")))
      }
      
      if(input$uiChartTypes=="Bar"){  
        plot<- plot |> 
          plotly::add_bars(y = as.formula(glue::glue("~{input$uiRowsSelect}")),
                           x = as.formula(glue::glue("~{input$uiColsSelect}")))
      }

      if(input$uiChartTypes=="Scatter"){
        browser()
        plot<- plot |> 
          plotly::add_markers(y = as.formula(glue::glue("~{input$uiRowsSelect}")),
                              x = as.formula(glue::glue("~{input$uiColsSelect}")),
                              mode = "markers",
                              marker = list(
                                size = 
                                  if(input$uiSizeSelect=="[NONE]"){
                                    as.formula(glue::glue("~{input$uiSize}"))
                                  } else {
                                    as.formula(glue::glue("~{input$uiSizeSelect} / max({input$uiSizeSelect}) * {input$uiSize}"))
                                  },
                                color =
                                  if(input$uiColourSelect=="[NONE]"){
                                    "light blue"
                                  } else {
                                    as.formula(glue::glue("~{input$uiColourSelect}"))
                                  },
                                opacity = 0.5
                              ))
        
        # Text
        if(input$uiTextSelect!="[NONE]"){
          plot <- plot |>
            plotly::add_text(y = as.formula(glue::glue("~{input$uiRowsSelect}")),
                             x = as.formula(glue::glue("~{input$uiColsSelect}")),
                             text = as.formula(glue::glue("~{input$uiTextSelect}")),
                             textposition = "top right")
        }
        
      }
      
      plot
    })
    
  }
  
  # shinyApp(ui, server)
  shiny::runGadget(ui, server, viewer = browserViewer())
}
