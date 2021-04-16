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
      selectInput(ns("select"), "",
                  c("Väärtustamata nime väli" = "no_name",
                    "Väärtustamata registrikoodi väli" = "no_regcode",
                    "Korduvad väärtused nime väljal" = "multiple_names",
                    "Korduvad väärtused registrikoodi väljal" = "multiple_regcodes")),
      downloadButton(ns("downloadData"), "Laadi kirjed alla (.csv fail)", class = "downloadbutton"),
      tags$head(tags$style(".downloadbutton{background-color:#dcedc1;} .downloadbutton{color: #133337;}")),
      DT::dataTableOutput(ns("resultsTable"))
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
  
  missingsummary <- getsummarymissing(userdata, r$data1, r$data2)
  
  output$missingBar <- renderPlot({
      ggplot2::ggplot(missingsummary) +
      ggplot2::geom_bar(ggplot2::aes(x = key,
                                     y = pct,
                                     fill=isna),
                        stat = 'identity', alpha=0.8) +
      ggplot2::theme(title=ggplot2::element_text(size=15),
                     axis.text.x = element_text(size = 15)) +
      ggplot2::labs(title = "Täielikkuse hinnang (%)", x = "", y = "%") +
      ggplot2::guides(fill=ggplot2::guide_legend(title = "Väärtustatud"))
  })

  uniquesummary <- getsummaryunique(userdata, r$data1, r$data2)
  
  output$duplicatesBar <- renderPlot({
    ggplot2::ggplot(uniquesummary) +
      ggplot2::geom_bar(ggplot2::aes(x = key,
                                     y = pct, fill=num.isunique),
                        stat = 'identity', alpha=0.8) +
      ggplot2::theme(title=ggplot2::element_text(size=15),
                     axis.text.x = element_text(size = 15),
                     legend.position = "none") +
      ggplot2::labs(title = "Ühekordsuse hinnang (%)", x = "", y = "%")
  })
  
  data <- reactiveValues()
  
  observeEvent(input$select, {
    if(input$select == "no_name"){
      data$in_table <- getMissing(userdata, r$data2)
    }
    else if(input$select == "multiple_regcodes"){
      data$in_table <- getDuplicates(userdata, r$data1)
    }
    else if(input$select == "no_regcode"){
      data$in_table <- getMissing(userdata, r$data1)
    }
    else if(input$select == "multiple_names"){
      data$in_table <- getDuplicates(userdata, r$data2)
    }
    proxy <- DT::dataTableProxy('resultsTable')
    
    DT::replaceData(proxy, data$in_table)
  })
  
  output$resultsTable <- DT::renderDataTable({
    isolate(data$in_table)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$select, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data$in_table, file, row.names = FALSE)
    }
  )

}

## To be copied in the UI
# mod_analysis_ui("analysis_ui_1")
    
## To be copied in the server
# callModule(mod_analysis_server, "analysis_ui_1")
 
