#' analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(plotOutput(ns("missingBar")), width = 6),
      column(plotOutput(ns("duplicatesBar")), width = 6)
            ),
    fluidRow(
      h3("Kuva mittevastavad kirjed:"),
      selectInput(ns("select1"), "",
                  c("Väärtustamata nime väli" = "no_name",
                    "Väärtustamata registrikoodi väli" = "no_regcode",
                    "Korduvad väärtused nime väljal" = "multiple_names",
                    "Korduvad väärtused registrikoodi väljal" = "multiple_regcodes")),
      tags$style("#select1 {border: 2px solid #dd4b39;}"),
      downloadButton(ns("downloadData"), "Laadi kirjed alla (.csv fail)", class = "downloadbutton"),
      tags$head(tags$style(".downloadbutton{background-color:#dcedc1;} .downloadbutton{color: #133337;}")),
      dataTableOutput(ns("resultsTable"))
    ),
    fluidRow(
      
    )
  )
}
    
#' analysis Server Function
#'
#' @noRd 



mod_analysis_server <- function(input, output, session, r){
  ns <- session$ns
  
  #r$reference_data is the reference datatset
  #r$userdata is the dataset uploaded by the user
  #r$data1 is the name of the column containing registry code in user dataset
  #r$data2 is the name of the column containing the name in user dataset
  
  #analyze missing values
  userdata <- r$userdata
  #userdata[userdata==""]<-NA
  
  missingvalues <-  userdata %>%
                    dplyr::select(r$data1, r$data2) %>%
                    tidyr::gather(key = "key", value = "val") %>%
                    dplyr::mutate(isna = !is.na(val)) %>%
                    dplyr::group_by(key) %>%
                    dplyr::mutate(total = dplyr::n()) %>%
                    dplyr::group_by(key, total, isna) %>%
                    dplyr::summarise(num.isna = dplyr::n()) %>%
                    dplyr::mutate(pct = num.isna / total * 100)

  
  output$missingBar <- renderPlot({
      ggplot2::ggplot(missingvalues) +
      ggplot2::geom_bar(ggplot2::aes(x = key,
                                     y = pct,
                                     fill=isna),
                        stat = 'identity', alpha=0.8) +
      ggplot2::theme(title=ggplot2::element_text(size=20)) +
      ggplot2::labs(title = "Täielikkuse hinnang (%)", x = "", y = "%") +
      ggplot2::guides(fill=ggplot2::guide_legend(title = "Väärtustatud"))
  })
  
  
  
  #analyze unique values
  uniquevalues <-  userdata %>%
    dplyr::select(r$data1, r$data2) %>%
    tidyr::gather(key = "key", value = "val") %>%
    dplyr::filter(!is.na(val)) %>%
    dplyr::group_by(key) %>%
    dplyr::mutate(total = dplyr::n()) %>%
    dplyr::group_by(key, total) %>%
    dplyr::summarise(num.isunique = dplyr::n_distinct(val, na.rm = TRUE)) %>%
    dplyr::mutate(pct = num.isunique / total * 100)
  
  
  
  output$duplicatesBar <- renderPlot({
    ggplot2::ggplot(uniquevalues) +
      ggplot2::geom_bar(ggplot2::aes(x = key,
                                     y = pct, fill=num.isunique),
                        stat = 'identity', alpha=0.8) +
      ggplot2::theme(title=ggplot2::element_text(size=20)) +
      ggplot2::labs(title = "Unikaalsuse hinnang (%)", x = "", y = "%") +
      ggplot2::guides(fill=ggplot2::guide_legend(title = "Unikaalsed"))
  })
  
  
  dataToDisplay <- reactive({
    if(input$select == "no_name"){
      getMissing(userdata, r$data2)
    }
    else if(input$select == "multiple_regcodes"){
      getDuplicates(userdata, r$data1)
    }
    else if(input$select == "no_regcode"){
      getMissing(userdata, r$data1)
    }
    else if(input$select == "multiple_names"){
      getDuplicates(userdata, r$data2)
      
    }
  })
  
  getMissing <- function(data, col){
    emptyRows <- dplyr::filter_(userdata, sprintf("is.na(%s)", col))
    return(emptyRows)
  }
  
  getDuplicates <- function(data, col){
    duplicatesInCol <- data %>%
                        dplyr::group_by_at(col) %>%
                        dplyr::filter(dplyr::n() > 1) %>%
                        dplyr::filter(!is.na(!!rlang::sym(col))) %>%
                        data.frame()
                            
    return(duplicatesInCol)
  }
  
  observeEvent(input$select, {
    output$resultsTable <- renderDataTable({
      dataToDisplay()
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$select, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataToDisplay(), file, row.names = FALSE)
    }
  )

}
    
## To be copied in the UI
# mod_analysis_ui("analysis_ui_1")
    
## To be copied in the server
# callModule(mod_analysis_server, "analysis_ui_1")
 
