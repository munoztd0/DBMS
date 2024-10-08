#' modify_row UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_modify_row_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' modify_row Server Functions
#'
#' @noRd 
mod_modify_row_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_modify_row_ui("modify_row_1")
    
## To be copied in the server
# mod_modify_row_server("modify_row_1")
