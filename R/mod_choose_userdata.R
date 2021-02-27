#' choose_userdata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_choose_userdata_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$hr(),
    
    h3("Kasutaja andmestik"), 
    
    fileInput(ns("file"), "Vali CSV fail",
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"))
  )
}
    
#' choose_userdata Server Function
#'
#' @noRd 

mod_choose_userdata_server <- function(input, output, session, r){
  ns <- session$ns
  
  observeEvent(input$file, {
    r$userdata <- data.table::fread(input$file$datapath, encoding="UTF-8")
  })
}
    
## To be copied in the UI
# mod_choose_userdata_ui("choose_userdata_ui_1")
    
## To be copied in the server
# callModule(mod_choose_userdata_server, "choose_userdata_ui_1")
 
