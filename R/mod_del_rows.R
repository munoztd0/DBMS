#' del_table UI Function
#'
#' @description A shiny Module for deleting rows from a selected table.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom DT DTOutput
mod_del_rows_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(1),
        column(10,
          fluidRow(
            column(12,
              uiOutput(ns('sel_table_ui')),
              h4(strong("Table Preview")),
              br(),
              DT::DTOutput(ns('table_view')),
              br(),
              actionButton(ns('delete_rows'), "Delete Selected Rows", class = "btn-danger"),
              textOutput(ns('delete_message')),
              verbatimTextOutput(ns('debug_output'))
            )
          )
        )
      )
    )
  )
}

#' del_table Server Functions
#'
#' @noRd 
#' @import dplyr
#' @importFrom DT renderDT datatable replaceData dataTableProxy
#' @importFrom shiny showNotification reactive
mod_del_rows_server <- function(id, table_names){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    conn <- golem::get_golem_options("conn_SQL_Lite")

    # Render UI for table selection
    output$sel_table_ui <- renderUI({
      selectInput(ns("sel_table"), "Select Table to Delete From", choices = sort(table_names(), decreasing = TRUE))
    })

    # Reactive value to trigger table refresh
    refresh_trigger <- reactiveVal(0)

    # Reactive expression for table data
    table_data <- reactive({
      req(input$sel_table)
      refresh_trigger()  # Depend on the refresh trigger
      dplyr::tbl(conn, input$sel_table) |> collect()
    })

    # Show table
    output$table_view <- DT::renderDT({
      req(table_data())
      DT::datatable(
        table_data(),
        filter = "top",
        extensions = 'Scroller',
        options = list(
          deferRender = FALSE,
          dom = 'Bfrtip',
          columnDefs = list(list(className = 'dt-left', targets = "_all")),
          scroller = TRUE,
          scrollX = TRUE,
          scrollY = 500,
          pageLength = 20,
          searching = TRUE
        ),
        selection = 'multiple'
      )
    })

    # Delete selected rows
    observeEvent(input$delete_rows, {
      req(input$sel_table)
      selected_rows <- input$table_view_rows_selected
      
      if (length(selected_rows) > 0) {
        # Get the current table data
        current_data <- table_data()
        
        # Identify the rows to be deleted
        rows_to_delete <- current_data[selected_rows, ]
        
        # Log the structure of rows_to_delete
        message("Structure of rows_to_delete:")
        message(capture.output(str(rows_to_delete)))
        
        # Delete the rows from the database
        tryCatch({
          # Start a transaction
          DBI::dbExecute(conn, "BEGIN TRANSACTION")
          
          deleted_count <- 0
          for (i in 1:nrow(rows_to_delete)) {
            row <- rows_to_delete[i, ]
            where_clauses <- sapply(names(row), function(col) {
              value <- row[[col]]
              if (is.na(value)) {
                paste(col, "IS NULL")
              } else if (is.character(value) || is.factor(value)) {
                paste0(col, " = '", gsub("'", "''", as.character(value)), "'")
              } else if (is.numeric(value)) {
                paste0(col, " = ", value)
              } else {
                paste0(col, " = '", as.character(value), "'")
              }
            })
            where_clause <- paste(where_clauses, collapse = " AND ")
            query <- paste("DELETE FROM", input$sel_table, "WHERE", where_clause)
            result <- DBI::dbExecute(conn, query)
            deleted_count <- deleted_count + result
            # Log the query and result
            message(paste("Query:", query))
            message(paste("Rows affected:", result))
          }
          
          # Commit the changes
          DBI::dbExecute(conn, "COMMIT")
          
          # Trigger a refresh of the table data
          refresh_trigger(refresh_trigger() + 1)
          
          # Show a notification
           showModal(modalDialog(
            title = "Success",
            paste(deleted_count, "row(s) deleted successfully"),
            easyClose = TRUE
        ))
         
          
        }, error = function(e) {
          # Rollback the transaction if an error occurs
          DBI::dbExecute(conn, "ROLLBACK")
          showModal(modalDialog(
            title = "Failure",
            paste("Error deleting rows:", e$message),
            easyClose = TRUE
          ))
  
        })
      } else {
        showModal(modalDialog(
            title = "Warning",
            "No rows selected for deletion",,
            easyClose = TRUE
          ))
        
      }
    })

    # Display delete message
    output$delete_message <- renderText({
      if (!is.null(input$delete_rows) && input$delete_rows > 0) {
        paste("Attempted to delete", length(input$table_view_rows_selected), "row(s)")
      }
    })

    # Debug output
    output$debug_output <- renderPrint({
      if (!is.null(input$delete_rows) && input$delete_rows > 0) {
        cat("Table name:", input$sel_table, "\n")
        cat("Selected rows:", paste(input$table_view_rows_selected, collapse = ", "), "\n")
        cat("Current data in reactive:\n")
        print(head(table_data()))
      }
    })
  })
}

## To be copied in the UI
# mod_del_rows_ui("del_table_1")

## To be copied in the server
# mod_del_rows_server("del_table_1", table_names)