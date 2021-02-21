#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session) {
  # List the first level callModules here

  #used for passing values to and from modules
  r <- reactiveValues()
  
  callModule(mod_choose_referencedata_server, "choose_referencedata_ui_1", r=r)
  
  output$table <- renderTable({
    if(!is.null(r$new_data)){
      reference_data <- r$new_data
      #head(r$new_data[,1:3])
    }
    else {
      reference_data <- my_dataset
      #head(my_dataset[,1:3])
    }
    if(!is.null(reference_data)){
      head(reference_data[,1:3])
    }
  })
  
}
