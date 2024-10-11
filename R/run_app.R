#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom DBI dbDisconnect
#' 

run_app <- function(onStart = NULL,
                    options = list(),
                    enableBookmarking = NULL,
                    uiPattern = "/",
                    conn = NULL,
                    ...) {
  
  # when exiting app, disconnect from database
  onStart = function() {
    cat("Doing application setup\n")
    
    #conn = create_conn()
    
    onStop(function() {
      cat("Doing application cleanup\n")
      #remove temp files
      #unlink("test_db_file")
      
      DBI::dbDisconnect(conn)
    })
  }
  
  
  with_golem_options(
    app = shinyApp(
      ui = shinymanager::secure_app(app_ui, head_auth = tags$script(delayButton), theme = shinythemes::shinytheme("flatly")),
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list("conn_SQL_Lite" =  create_conn() )
  )
}
