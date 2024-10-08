#' import_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_import_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Choose CSV or Excel File",
              accept = c(".csv", ".xlsx", ".xls")),
    textInput(ns("table_name"), "Enter table name for import:"),
    actionButton(ns("import"), "Import File"),
  )
}

#' import_table Server Functions
#'
#' @noRd 
#'
#' @importFrom shiny moduleServer reactiveVal
#' @importFrom  readxl read_excel

# Server Function
mod_import_table_server <- function(id, table_names) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    import_status <- reactiveVal()

    observeEvent(input$import, {
      req(input$file, input$table_name)
      
      tryCatch({
        # Read the file
        if (tools::file_ext(input$file$datapath) == "csv") {
          df <- read.csv(input$file$datapath, stringsAsFactors = FALSE, row.names = NULL)
        } else {
          df <- readxl::read_excel(input$file$datapath)
        }
        
        # Validate table name
        table_name <- input$table_name
        if (!grepl("^[a-zA-Z][a-zA-Z0-9_]*$", table_name)) {
          showModal(modalDialog(
            title = "Error",
            "Invalid table name. Use only letters, numbers, and underscores, starting with a letter.",
            easyClose = TRUE
          ))
        
        return()
        }
        if (tolower(table_name) %in% tolower(table_names())) {
          showModal(modalDialog(
            title = "Error",
            "Table name already exists. Please choose a different name.",
            easyClose = TRUE
          ))
        #
        return()
        }
        
        # Get database connection
        conn <- golem::get_golem_options("conn_SQL_Lite")
        
        # Write to database
        DBI::dbWriteTable(conn, table_name, df, overwrite = TRUE)
        
        # Update table names
        table_names(DBI::dbListTables(conn))
        # Show a notification
           showModal(modalDialog(
            title = "Success",
            paste("File imported!"),
            easyClose = TRUE
        ))
      }, error = function(e) {
        # Show a notification
           showModal(modalDialog(
            title = "Failure",
            paste("Error:", e$message),
            easyClose = TRUE
        ))
      })
    })

    
  })
}  
    
## To be copied in the UI
# mod_import_table_ui("import_table_1")
    
## To be copied in the server
# mod_import_table_server("import_table_1")


