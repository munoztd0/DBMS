#' graph_table UI Function
#'
#' @description A shiny Module for viewing tables using GWalkR.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom DBI dbListTables
#' @importFrom GWalkR gwalkrOutput
#' 
mod_graph_table_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(1),
      column(10,
             fluidRow(
               column(12,
                      uiOutput(ns('sel_table_1_ui')),
                      h4(strong("Interactive Data Visualization")),
                      br(),
                      shinycssloaders::withSpinner(uiOutput(ns('dynamic_gwalkr')))
               )
             )
      )
    )
  )
}

#' graph_table Server Functions
#'
#' @import dplyr
#' @importFrom GWalkR renderGwalkr gwalkr
#'
#' @noRd 
#' 
mod_graph_table_server <- function(id, table_names){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    conn <- golem::get_golem_options("conn_SQL_Lite")

    # Render UI for table selection
    output$sel_table_1_ui <- renderUI({
      print("Rendering table selection UI")
      selectInput(ns("sel_table_1"), "Tables in Database", choices = sort(table_names(), decreasing = TRUE))
    })

    # Reactive value to store the current table data
    current_data <- reactiveVal(NULL)

    # Function to fetch and process data
    fetch_data <- function() {
      print(paste("Fetching data for table:", input$sel_table_1))
      req(input$sel_table_1)
      withProgress(message = 'Fetching data...', value = 0, {
        data <- dplyr::tbl(conn, input$sel_table_1) |> collect()
        current_data(data)
        print(paste("Data fetched for table:", input$sel_table_1))
      })
    }

    # Observe changes in table selection and generate visualization automatically
    observeEvent(input$sel_table_1, {
      print(paste("Table selected:", input$sel_table_1))
      fetch_data()

      # Dynamically render the GWalkR output with a new ID to force re-rendering
      output$dynamic_gwalkr <- renderUI({
        print("Generating dynamic UI for GWalkR")
        ns_id <- ns(paste0('sel_table_view_', input$sel_table_1))
        GWalkR::gwalkrOutput(ns_id)
      })

      output[[paste0('sel_table_view_', input$sel_table_1)]] <- GWalkR::renderGwalkr({
        print("Generating GWalkR visualization")
        req(current_data())
        GWalkR::gwalkr(current_data())
      })
    })

    # Trigger visualization generation for the first table by default
    observe({
      if (is.null(input$sel_table_1)) {
        first_table <- sort(table_names(), decreasing = TRUE)[1]
        print(paste("Default table selected:", first_table))
        updateSelectInput(session, "sel_table_1", selected = first_table)
      }
    })
  })
}
