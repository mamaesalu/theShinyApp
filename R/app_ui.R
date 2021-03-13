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
          mod_choose_referencedata_ui("choose_referencedata_ui_1"),
          mod_choose_userdata_ui("choose_userdata_ui_1"),
          actionButton("analyzeButton", "Analüüsi")
          , width = 3
        ),
        
        mainPanel(
          tabsetPanel(type = "tabs", id="theTabs",
                      tabPanel(title = "Võrdlusandmed",
                               value = "reference",
                               fluidPage(
                                 fluidRow(
                                            tags$h3("Võrdlusandmestik"),
                                            fluidRow(dataTableOutput("table"))
                                            )
                                          )
                                 
                                 ),
                      tabPanel(title ="Kasutaja andmestik",
                               value = "user",
                               fluidRow(
                                          tags$h3("Kasutaja andmestik"),
                                          fluidRow(dataTableOutput("table2"))
                                        )
                               ),
                      tabPanel(title ="Analüüsi tulemused",
                               value = "analysis",
                               fluidRow(
                                 mod_analysis_ui("analysis_ui_1")
                               ))
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

