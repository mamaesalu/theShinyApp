#' analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    #tags$h3("Kasutaja andmestiku täielikkuse hinnang"),
    tags$h3("random graphs"),
    fluidRow(
      column(plotOutput(ns("numberPie")), width = 5),
      column(plotOutput(ns("namePie")), width = 5)
            ),
    #tags$h3("Mittevastavad kirjed kasutaja andmestikus"),
    tags$h3("random table"),
    fluidRow(dataTableOutput(ns("compResult")))
  )
}
    
#' analysis Server Function
#'
#' @noRd 
mod_analysis_server <- function(input, output, session, r){
  ns <- session$ns
  output$numberPie <- renderPlot({
    shinipsum::random_ggplot("bar")
  })
  
  output$namePie <- renderPlot({
    shinipsum::random_ggplot("bar")
  })
  
  output$compResult <- renderDataTable({
    shinipsum::random_table(5, 8)
  })
}
    
## To be copied in the UI
# mod_analysis_ui("analysis_ui_1")
    
## To be copied in the server
# callModule(mod_analysis_server, "analysis_ui_1")
 
