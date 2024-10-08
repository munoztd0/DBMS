#' about UI Function
#'
#' @description A sleek shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    box(title = 'About DBvieweR', width = NULL, status = "primary", solidHeader = FALSE,
        
        # Hex logo
        img(src = 'https://github.com/munoztd0/DbVieweR/blob/main/man/figures/hex-DBvieweR.png?raw=true', height = 120),
        
        p("Welcome to ", strong("DBvieweR"), 
          "! This Shiny app is a browser-based database management dashboard offering clean UI and robust backend features."),
        
        p(tags$strong("Authors:"), "David Munoz Tord"),
        
        h5(strong("Abstract")),
        p("DBvieweR helps you explore databases through an intuitive interface, allowing you to view, import, rename, update, and delete tables."),
        
        p(tags$strong("GitHub:"),
          tags$a(icon("github"), href = "https://github.com/munoztd0/DbVieweR/tree/demo", 
                 title = "DBvieweR GitHub Repo", " See Full Project")),

        br(),
        h5(strong("Developer")),
        div(style = "display: flex; align-items: center;",
            img(src = 'https://avatars.githubusercontent.com/u/43644805?s=400&u=41bc00f6ee310ed215298c9af27ed53e9b3e1a60&v=4', height = 80, style = "border-radius: 50%; margin-right: 15px;"),
            div(
              p("David Munoz Tord", br(), 
                "Fullstack Data Scientist / R Developer"),
              tags$a(icon("linkedin"), 
                     href = "https://www.linkedin.com/in/david-munoz-tord-409639150", " Connect on LinkedIn"),
              br(),
              tags$a(icon("twitter"), href = "https://x.com/tord_munoz", " @tord_munoz"),
              br(),
              tags$a(icon("github"), href = "https://github.com/munoztd0", " GitHub"),
              br(),
              tags$a(icon("globe"), href = "https://david-munoztord.com", " Website")
            )
        ),
        
        p("A data enthusiast driven by ambition and curiosity.")
    )  
  )
}
    
#' about Server Functions
#'
#' @noRd 
# mod_about_server <- function(id){
#   moduleServer( id, function(input, output, session){
#     ns <- session$ns
#   })
# }
    
## To be copied in the UI
# mod_about_ui("about_1")
    
## To be copied in the server
# mod_about_server("about_1")
