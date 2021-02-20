#' load_fromweb UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_load_fromweb_ui <- function(id){
  ns <- NS(id)
  tagList(
    tableOutput(ns("table"))
  )
}
    
#' load_fromweb Server Function
#'
#' @noRd 
mod_load_fromweb_server <- function(input, output, session, mod_choose_1){
  ns <- session$ns
  observeEvent(mod_choose_1(), {
    temp_dir = tempdir()
    temp = tempfile(tmpdir = temp_dir)
    download.file("https://avaandmed.rik.ee/andmed/ARIREGISTER/ariregister_csv.zip", temp)
    out = unzip(temp, list = TRUE)$Name
    unzip(temp, out, exdir = temp_dir)
    new_dataset <- data.table::fread(file.path(temp_dir, out), encoding="UTF-8")
    output$table <- renderTable({
    head(new_dataset[,1:3])   })
    #r$new_dataset <- new_dataset
  })
}
    
## To be copied in the UI
# mod_load_fromweb_ui("load_fromweb_ui_1")
    
## To be copied in the server
# callModule(mod_load_fromweb_server, "load_fromweb_ui_1")
 
