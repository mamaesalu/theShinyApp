#' choose_referencedata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_choose_referencedata_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("load"), "Laadi värskem andmestik")
  )
}
    
#' choose_referencedata Server Function
#'
#' @noRd 
mod_choose_referencedata_server <- function(input, output, session){
  ns <- session$ns
  reactive(input$load)
}
    
## To be copied in the UI
# mod_choose_referencedata_ui("choose_referencedata_ui_1")
    
## To be copied in the server
# callModule(mod_choose_referencedata_server, "choose_referencedata_ui_1")
 
