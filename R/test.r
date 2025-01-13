#' Run the Shiny Application Demo
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

run_test <- function(onStart = NULL,
                    options = list(),
                    enableBookmarking = NULL,
                    uiPattern = "/",
                    conn = NULL,
                    ...) {

  demo_conn <- create_conn()

  onStart = function() {
    #cat("Doing application setup\n")
    
  }

  # when exiting app, disconnect from database
  # onStop= function() {
  #     cat("Doing application cleanup\n")
  #     #remove temp files
  #     unlink("test_db_file")
      
  #     DBI::dbDisconnect(conn)
  #   }
  #TODO: Fix on.exit



  
  with_golem_options(
    app = shinyApp(
      ui = shinymanager::secure_app(app_ui, head_auth = tags$script(delayButton), theme = shinythemes::shinytheme("flatly")),
      server = app_server,
      options = options,
      onStart = onStart,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list("conn" =  demo_conn)
  )
}
