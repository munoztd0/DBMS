# UI function for the insert_rows module
mod_insert_rows_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(1),
      column(10,
        fluidRow(
          column(12,
            uiOutput(ns('sel_table_ui')),
            uiOutput(ns('dynamic_inputs')),
            actionButton(ns('insert_row'), "Insert Row"),
            br(),
            textOutput(ns('insert_status'))
          )
        )
      )
    )
  )
}

# Server function for the insert_rows module
mod_insert_rows_server <- function(id, table_names) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    conn <- golem::get_golem_options("conn_SQL_Lite")
    
    # Render UI for table selection
    output$sel_table_ui <- renderUI({
      selectInput(ns("sel_table"), "Select Table to Insert Into", choices = sort(table_names(), decreasing = TRUE))
    })
    
    # Dynamically create input fields based on the selected table
    output$dynamic_inputs <- renderUI({
      req(input$sel_table)
      table_info <- dbGetQuery(conn, sprintf("PRAGMA table_info(%s)", input$sel_table))
      
      lapply(seq_len(nrow(table_info)), function(i) {
        col_name <- table_info$name[i]
        col_type <- table_info$type[i]
        
        # Determine input type based on column type
        input_type <- switch(tolower(col_type),
                             "integer" = "numericInput",
                             "real" = "numericInput",
                             "text" = "textInput",
                             "date" = "dateInput",
                             "datetime" = "dateInput",
                             "boolean" = "checkboxInput",
                             "textInput")  # Default to text input for unknown types
        
        # Create the input with a helper text showing the data type
        tagList(
          do.call(input_type, list(
            inputId = ns(paste0("input_", col_name)),
            label = col_name,
            value = if(input_type == "checkboxInput") FALSE else NULL
          )),
          helpText(paste("Data type:", col_type)),
          br()
        )
      })
    })
    
    # Insert row when button is clicked
    observeEvent(input$insert_row, {
      req(input$sel_table)
      
      table_info <- dbGetQuery(conn, sprintf("PRAGMA table_info(%s)", input$sel_table))
      
      # Collect values from dynamic inputs
      values <- sapply(table_info$name, function(col_name) {
        val <- input[[paste0("input_", col_name)]]
        if (is.character(val) && val == "") {
          return(NA)
        } else {
          return(val)
        }
      })
      
      # Remove NA values and their corresponding columns
      valid_indices <- !is.na(values)
      valid_values <- values[valid_indices]
      valid_columns <- table_info$name[valid_indices]
      
      # Construct INSERT query
      columns <- paste(valid_columns, collapse = ", ")
      placeholders <- paste(rep("?", length(valid_values)), collapse = ", ")
      query <- sprintf("INSERT INTO %s (%s) VALUES (%s)", input$sel_table, columns, placeholders)
      
      tryCatch({
        # Convert valid_values to an unnamed list
        param_list <- as.list(unname(valid_values))
        result <- dbExecute(conn, query, params = param_list)
        showModal(modalDialog(
            title = "Success",
            "Row inserted successfully!",
            easyClose = TRUE
        ))
      }, error = function(e) {
         showModal(modalDialog(
            title = "Failure",
            paste("Error inserting row:", e$message),
            easyClose = TRUE
        ))
      })
    })
  })
}

