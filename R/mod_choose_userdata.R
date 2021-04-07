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
    
    fileInput(ns("file"), label = "Vali CSV fail",
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    h3("Määra andmeväljade vastavus"),
    selectInput(inputId = ns("data1"), "Registrikood: ", choices= c()),
    selectInput(inputId = ns("data2"), "Ettevõtte nimi: ", choices= c())
  )
}
    
#' choose_userdata Server Function
#'
#' @noRd 

mod_choose_userdata_server <- function(input, output, session, r, parent_session){
  ns <- session$ns
  
  observeEvent(input$file, {
    req(input$file)
    r$userdata <- data.table::fread(input$file$datapath, na.strings = c("",NA), encoding="UTF-8")
    updateTabsetPanel(parent_session, "theTabs",
                      selected = "user")
    updateSelectInput(session, inputId = "data1", choices=colnames(r$userdata))
    updateSelectInput(session, inputId = "data2", choices=colnames(r$userdata))
  })
  
  observe({
    r$data1 <- input$data1
    r$data2 <- input$data2
    msg <- paste(r$data2)
    cat(msg, "\n")
  })
}
    
## To be copied in the UI
# mod_choose_userdata_ui("choose_userdata_ui_1")
    
## To be copied in the server
# callModule(mod_choose_userdata_server, "choose_userdata_ui_1")
 
