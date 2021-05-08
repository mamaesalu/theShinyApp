#' analysis2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysis2_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(plotly::plotlyOutput(ns("missingBar")), width = 6),
      column(plotly::plotlyOutput(ns("namemismatchPie")), width = 6)
    ),
    fluidRow(
      selectInput(ns("select"), "",
                  c("Registrikoodid, mida ei ole äriregistris" = "not_present",
                    "Nimi äriregistris on erinev" = "diff_name")),
      downloadButton(ns("downloadData"), "Laadi kirjed alla (.csv fail)", class = "downloadbutton"),
      tags$head(tags$style(".downloadbutton{background-color:#dcedc1;} .downloadbutton{color: #133337;}"))
    ),
    fluidRow(
      dataTableOutput(ns("resultsTable"))
    )
  )
}
    
#' analysis2 Server Function
#'#r$reference_data is the reference datatset
# r$userdata is the dataset uploaded by the user
# r$data1 is the name of the column containing registry code in user dataset
# r$data2 is the name of the column containing the name in user dataset
#' @noRd 
mod_analysis2_server <- function(input, output, session, r){
  ns <- session$ns
 
  userdata <- r$userdata
  refdata <- r$reference_data
  
  codesmatch <- codesmatchingsummary(userdata, refdata, r$data1)

  output$missingBar <- plotly::renderPlotly({

    plotly::plot_ly(codesmatch, labels = ~tulemus, values = ~count) %>%
      plotly::add_pie(hole = 0.4) %>%
      plotly::layout(title = "Registrikoodide vastavuse % kasutaja andmestikus <br> võrdluses äriregistri andmetega", font=list(size = 10),
                     showlegend = F,
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     margin = list(l = 50, r = 50,
                                   b = 100, t = 100,
                                   pad = 50))

  })
  
  namesnotmatching <- namesmismatch(userdata, refdata, r$data1, r$data2)
  namesnotmatchingtotal <-namesmismatchtotal(userdata, refdata, r$data1, r$data2)

  output$namemismatchPie <- plotly::renderPlotly({

    plotly::plot_ly(namesnotmatching, labels = ~tulemus, values = ~count) %>%
      plotly::add_pie(hole = 0.4) %>%
      plotly::layout(title = "Nime vastavuse % kasutaja andmestikus <br> võrdluses äriregistri andmetega", font=list(size = 10),
                     showlegend = F,
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     margin = list(l = 50, r = 50,
                                   b = 100, t = 100,
                                   pad = 50))

  })

  observeEvent(input$select, {
    output$resultsTable <- renderDataTable({
      dataToDisplay()
    })
  })
  
  codesmatching <- regcodesmatch(userdata, refdata, r$data1)

  dataToDisplay <- reactive({
    if(input$select == "not_present"){
      codesmatching %>%
        dplyr::filter(result == F) %>%
        dplyr::select(-result)
    }
    else if(input$select == "diff_name"){
      namesnotmatchingtotal %>%
        dplyr::filter(result2 == T) %>%
        dplyr::select(-result2)
    }
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$select, ".csv", sep = ";")
    },
    content = function(file) {
      write.csv(dataToDisplay(), file, row.names = FALSE)
    }
  )
}
    
## To be copied in the UI
# mod_analysis2_ui("analysis2_ui_1")
    
## To be copied in the server
# callModule(mod_analysis2_server, "analysis2_ui_1")
 
