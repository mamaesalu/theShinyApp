#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session) {
  # List the first level callModules here
  output$preloaded_table <- renderTable({
    head(my_dataset[,1:3])   })
  
  #r <- reactiveValue()
  mod_choose_1 <- callModule(mod_choose_referencedata_server, "choose_referencedata_ui_1")
  callModule(mod_load_fromweb_server, "load_fromweb_ui_1", mod_choose_1)
  #my_dataset <- r$new_dataset
  
  
}
