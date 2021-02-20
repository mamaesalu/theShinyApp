#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      titlePanel(h1("TESTREG")),
      sidebarLayout(
        sidebarPanel(
          h3("Vali võrdlusandmestik"),
          h2("Eellaetud andmestik seisuga 04.02.2021"),
          mod_choose_referencedata_ui("choose_referencedata_ui_1")
          , width = 3
        ),
        
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Andmed", 
                               fluidPage(
                                 fluidRow(tableOutput("preloaded_table")),
                                 mod_load_fromweb_ui("load_fromweb_ui_1")
                               )
                      ),
                      tabPanel("Analüüs", verbatimTextOutput("summary"))
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
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'TheApp'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

