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
      r$reference_data <- r$new_data
    }
    else {
      r$reference_data <- my_dataset
    }
    if(!is.null(r$reference_data)){
      r$reference_data
    }
  })
  
  callModule(mod_choose_userdata_server, "choose_userdata_ui_1", r=r, parent_session = session)
 
  output$table2 <- renderDataTable({
    if (!is.null(r$userdata)){
      r$userdata
    }
  })
  
  observeEvent(input$analyzeButton, {
    if (r$data1 == r$data2){
      showNotification("Vaata üle andmeväljade vastavus! Registrikood ja nimi ei saa olla samad.", type = "error", duration = 10, closeButton = TRUE)
    }
    else if(typeof(r$userdata[, r$data1]) != "integer"){
      showNotification("Registrikoodi väljade andmetüüp peab olema täisarv!", type = "error", duration = 10, closeButton = TRUE)
    }
    else {
      updateTabsetPanel(session, "theTabs",
                        selected = "analysis")
      callModule(mod_analysis_server, "analysis_ui_1", r=r)
      callModule(mod_analysis2_server, "analysis2_ui_1", r=r)
    }
  })
}
