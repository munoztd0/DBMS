#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' 
#' 
#' @noRd
app_ui <- function(request) {
  
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage( # @importFrom shinydashboardPlus dashboardPage
      title = HTML(glue::glue('DbVieweR')),
		# Dashboard Page Setup ----------------------------------------------------
		# options = list(sidebarExpandOnHover = TRUE),
		skin = "purple",
		dashboardHeader(
			title = HTML(glue::glue('DbVieweR'))
		),

		# Dashboard Sidebar -------------------------------------------------------
		  dashboardSidebar(#collapsed = TRUE, 
		                   #minified = TRUE, 
                   div(textOutput("welcome"), style = "padding: 20px"),
                   sidebarMenu(
                     id = "sidebarmenu",  # Add this line
                     menuItem(" View Tables", tabName = "view_table", icon = icon("search")),
                     menuItem(" Update Tables", tabName = "update_table", icon = icon("exchange-alt")),
                     menuItem(" Import Tables", tabName = "import_table", icon = icon("file-import")),
                     menuItem(" Insert Entries", tabName = "insert_rows", icon = icon("plus-square")),
                     menuItem(" Delete Rows", tabName = "delete_rows", icon = icon("trash-alt")),
                     menuItem(" Visualize Data", tabName = "graph_table", icon = icon("chart-line")),
                     menuItem(" About", tabName = "about", icon = icon("info-circle"))
                   )
  ),

  
		# Dashboard Body ----------------------------------------------------------

    dashboardBody(
            title = HTML(glue::glue('DbVieweR')),
            shinyjs::useShinyjs(),
            tags$head(
                      tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                                  type="text/javascript")
            ),

            tabItems(
              # #
              tabItem(
                tabName = 'view_table',
                mod_view_table_ui("view_table_1")
              ),
              # # 
              tabItem(
                tabName = 'update_table',
                mod_update_table_ui("update_table_1")
              ),
              # # 
              tabItem(
                tabName = 'import_table',
                mod_import_table_ui("import_table_1")
              ),
              # # 
              tabItem(
                tabName = 'insert_rows',
                mod_insert_rows_ui("insert_rows_1")
              ),
              # # 
              tabItem(
                tabName = 'delete_rows',
                mod_del_rows_ui("delete_rows_1")
              ),
              # #
              tabItem(
                tabName = 'graph_table',
                mod_graph_table_ui("graph_table_1")
              ),
             # # 
              tabItem(
                tabName = 'about',
                mod_about_ui("about_1")
              )
            )
          )
        )



			)
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "DbVieweR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}


#TODO: Deal with favicon