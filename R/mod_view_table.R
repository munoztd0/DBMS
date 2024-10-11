#' view_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom DBI dbListTables
#' @importFrom DT DTOutput
#' 

mod_view_table_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(1),
      column(10,
             fluidRow(
               column(12,
                      uiOutput(ns('sel_table_1_ui')),  # Use uiOutput here
                      h4(strong("Table Preview")),
                      br(),
                      DT::DTOutput(
                        outputId = ns('sel_table_view'))
                      )
               ),
              fluidRow(  column(12, 
                          downloadButton(ns('download'),"Download the data")          
                        )
                    )  
      )
    )
  )
}



#' view_table Server Functions
#'
#'
#' @import dplyr
#' @importFrom DT renderDT datatable
#' @importFrom readr write_excel_csv
#'
#' @noRd 
#' 
mod_view_table_server <- function(id, table_names){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    conn <- golem::get_golem_options("conn_SQL_Lite")

    # Render UI for table selection
    output$sel_table_1_ui <- renderUI({  # Use renderUI here
      selectInput(ns("sel_table_1"), "Tables in Database", choices = sort(table_names(), decreasing = F))
    })

  

    # show table
    output$sel_table_view <- DT::renderDT({
      req(input$sel_table_1)
        # Fetch data from database
      dplyr::tbl(conn, input$sel_table_1) |> collect() |> 
      DT::datatable(
        escape = FALSE,  
        filter = "top",
        extensions = 'Scroller',
        options = list(deferRender = F, 
                       dom = 'Bfrtip',
                       columnDefs = list(list(className = 'dt-left',
                                             targets = "_all")),
                       scroller = TRUE, scrollX = T, scrollY = 500,
                       pageLength = 20)
      )
    })

    res_auth <- shinymanager::secure_server(check_credentials = shinymanager::check_credentials(credentials))

    # Create reactive values including all credentials
    creds_reactive <- reactive({
      reactiveValuesToList(res_auth)
    })

    # Hide extraOutput only when credentials of level 0
    observe({
      if (!is.null(creds_reactive()$level) && creds_reactive()$level > 0 ) {
        output$download <- downloadHandler(
          filename = function() {
            paste0("data_", input$sel_table_1, ".csv")
          },
          content = function(file) {
            db <- dplyr::tbl(conn, input$sel_table_1) |> collect()
            readr::write_excel_csv(db, file)
          }
        )
      } 
    })
  })
}

## To be copied in the UI
# mod_view_table_ui("view_table_1")

## To be copied in the server
# mod_view_table_server("view_table_1")
