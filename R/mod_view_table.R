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
            paste0("data_", input$sel_table_1, ".xlsx")
          },
          content = function(file) {
            db <- dplyr::tbl(conn, input$sel_table_1) |> collect()
            # Check if the column 'Link' exists in the dataframe and clean the HTML tags
            if ("Link" %in% colnames(db)) {
              # Get the column index of the "Link" column
               link_col_index <- which(colnames(db) == "Link")
              
              db <- db %>%
                mutate(
                  # Remove the opening part of the <a> tag
                  Link = stringr::str_replace(Link, "<a href='", ""),
                  
                  # Remove the closing part of the <a> tag and everything after the URL
                  Link = stringr::str_replace(Link, "' target='_blank'>.*?</a>", ""),
                  # Link = paste0('HYPERLINK("', Link, '", "CAS Link")')
                )
            }
            # Create a workbook
            library(openxlsx)
            wb <- createWorkbook()

            # Add a worksheet
            addWorksheet(wb, "")

            # Write the entire data frame to the worksheet
            writeData(wb, sheet = 1, x = db, startCol = 1, startRow = 1, colNames = TRUE)
            
            
            # Loop through each row to add clickable hyperlinks in the 'Link' column
            if ("Link" %in% colnames(db)) {


            # Create clickable hyperlinks in the 'Link' column
            for(i in 1:nrow(db)) {
              # Generate the hyperlink formula using the link URL and desired display text
              hyperlink_formula <- paste0('HYPERLINK("', db$Link[i], '", "Click Here")')
              
              # Write the formula to the appropriate column (dynamic column based on Link)
              writeFormula(wb, sheet = 1, x = hyperlink_formula, startCol = link_col_index, startRow = i + 1)
            }
            }
            # Save the workbook as an xlsx file
            saveWorkbook(wb, file, overwrite = TRUE)        
            #openxlsx::write.xlsx(db, file, rowNames=FALSE)
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
