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
                         ".csv")),
    h3("Määra vastavus"),
    selectInput(inputId = ns("data1"), "Registrikood: ", choices= c()),
    selectInput(inputId = ns("data2"), "Ettevõtte nimi: ", choices= c())
  )
}
    
#' choose_userdata Server Function
#'
#' @noRd 

mod_choose_userdata_server <- function(input, output, session, r){
  ns <- session$ns
  
  observeEvent(input$file, {
    req(input$file)
    r$userdata <- data.table::fread(input$file$datapath, encoding="UTF-8")
    user_data <- r$userdata[, 1:5]
    updateSelectInput(session, inputId = "data1", choices=colnames(user_data))
    updateSelectInput(session, inputId = "data2", choices=colnames(user_data))
    msg <- paste(colnames(user_data))
    cat(msg, "\n")
  })
}
    
## To be copied in the UI
# mod_choose_userdata_ui("choose_userdata_ui_1")
    
## To be copied in the server
# callModule(mod_choose_userdata_server, "choose_userdata_ui_1")
 
