#' create_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_create_table_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' create_table Server Functions
#'
#' @noRd 
mod_create_table_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_create_table_ui("create_table_1")
    
## To be copied in the server
# mod_create_table_server("create_table_1")
