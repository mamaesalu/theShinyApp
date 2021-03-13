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
  
  output$table <- renderDataTable({
    if(!is.null(r$new_data)){
      reference_data <- r$new_data
    }
    else {
      reference_data <- my_dataset
    }
    if(!is.null(reference_data)){
      reference_data[,1:5]
    }
  })
  
  callModule(mod_choose_userdata_server, "choose_userdata_ui_1", r=r, parent_session = session)
 
  output$table2 <- renderDataTable({
    if (!is.null(r$userdata)){
      r$userdata[,1:5]
    }
  })
  
  observeEvent(input$analyzeButton, {
    cat("analüüsi vajutus", "\n")
    updateTabsetPanel(session, "theTabs",
                      selected = "analysis")
    callModule(mod_analysis_server, "analysis_ui_1", r=r)
  })
}
