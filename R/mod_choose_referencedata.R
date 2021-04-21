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
    h3("Võrdlusandmestik"),
    p("Allikas: "),
    tags$a(href="https://www.rik.ee/et/avaandmed", "Registrite ja Infosüsteemide Keskuse avaandmed"),
    tags$hr(),
    radioButtons(ns("data"), "Vali:",
                 c("Eellaetud andmestik - seisuga 12.2.2021" = "preloaded",
                   "Laadi uusim - suurus 15 MB" = "load_fromweb"),
                 selected = "preloaded"),
    p("Hiljem seda valikut muutes, vajuta uuesti nuppu 'Analüüsi'")
    )
}
    
#' choose_referencedata Server Function
#'
#' @noRd 
mod_choose_referencedata_server <- function(input, output, session, r){
  ns <- session$ns
  
  observeEvent(input$data, {
    if (input$data == "load_fromweb"){
      paste("laadi veebist")
      
      if (is.null(r$new_dataset)){
        callModule(mod_load_fromweb_server, "load_fromweb_ui_1", r=r)
      }
      r$new_data <- r$new_dataset
    }
    else if (input$data == "preloaded"){
      r$new_data <- NULL 
    }
  })
}
    
## To be copied in the UI
# mod_choose_referencedata_ui("choose_referencedata_ui_1")
    
## To be copied in the server
# callModule(mod_choose_referencedata_server, "choose_referencedata_ui_1")
 
