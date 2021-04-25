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
  )
}
    
#' load_fromweb Server Function
#'
#' @noRd 
mod_load_fromweb_server <- function(input, output, session, r, parent_session){
  ns <- session$ns
  
  #UNCOMMENT FOR PROD!
  tryCatch(
    expr = {
      temp_dir = tempdir()
      temp = tempfile(tmpdir = temp_dir)
      download.file("https://avaandmed.rik.ee/andmed/ARIREGISTER/ariregister_csv.zip", temp)
      out = unzip(temp, list = TRUE)$Name
      unzip(temp, out, exdir = temp_dir)
      dataset <- data.table::fread(file.path(temp_dir, out), encoding="UTF-8")
      r$new_dataset <- dataset
      golem::invoke_js(
        "loadfromweb",
        list(
          filename = out
        )
      )
    },
    error = function(e){ 
      showNotification("Faili laadimine ebaõnnestus, kasutan analüüsiks eellaaditud andmeid", type = "error", duration = 10, closeButton = TRUE, session = getDefaultReactiveDomain())
      updateRadioButtons(parent_session, "data", selected = "preloaded")
    },
    finally = {
      
    })
    
    
    

  #COMMENT FOR PROD!
    # dataset <- data.table::fread("C:\\Users\\mai_m\\Desktop\\Loputoo\\Andmed\\ettevotja_rekvisiidid_fordev.csv", na.strings = c("",NA), encoding="UTF-8")
    #r$new_dataset <- dataset
    
}
    
## To be copied in the UI
# mod_load_fromweb_ui("load_fromweb_ui_1")
    
## To be copied in the server
# callModule(mod_load_fromweb_server, "load_fromweb_ui_1")
 
