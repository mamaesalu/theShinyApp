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
      column(plotlyOutput(ns("missingBar")), width = 6),
      column(plotlyOutput(ns("namemismatchPie")), width = 6)
    ),
    fluidRow(
      h3("Mittevastavad kirjed:"),
      selectInput(ns("select"), "",
                  c("Registrikoodid, mida ei ole äriregistris" = "not_present",
                    "Nimi äriregistris on erinev" = "diff_name")),
      downloadButton(ns("downloadData"), "Laadi kirjed alla (.csv fail)", class = "downloadbutton"),
      tags$head(tags$style(".downloadbutton{background-color:#dcedc1;} .downloadbutton{color: #133337;}")),
      dataTableOutput(ns("resultsTable"))
    ),
    fluidRow(
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
  
  getcol <- unlist(userdata[, grep(r$data1, colnames(userdata)), with=FALSE], use.names = F)
  getcol2 <- unlist(userdata[, grep(r$data2, colnames(userdata)), with=FALSE], use.names = F)
  
  file3 <-  dplyr::mutate(userdata, result = getcol %in% refdata$ariregistri_kood) %>%
            dplyr::filter(!is.na(getcol))
  
  
  df <- file3 %>% dplyr::group_by(result) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::mutate(tulemus = ifelse(result == "TRUE", "Omab vastet", "Ei oma vastet")) %>%
    data.frame()


  output$missingBar <- renderPlotly({

    plotly::plot_ly(df, labels = ~tulemus, values = ~count) %>%
      plotly::add_pie(hole = 0.4) %>%
      plotly::layout(title = "Registrikoodide vastavuse % kasutaja andmestikus <br> võrdluses äriregistri andmetega", font=list(size = 10),
                     showlegend = F,
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     margin = list(l = 50, r = 50,
                                   b = 100, t = 100,
                                   pad = 50))

  })
  
  filex <- file3 %>%
    dplyr::select(all_of(r$data1), all_of(r$data2), result) %>%
    dplyr::filter(result != FALSE)

  file2x <- refdata %>%
    dplyr::select(ariregistri_kood, nimi) %>%
    dplyr::rename(nimi_ariregistris = nimi)

  total <- filex %>% dplyr::left_join(file2x, by=setNames("ariregistri_kood", r$data1)) %>%
    dplyr::select(-result) %>%
    dplyr::mutate(result2 = (.data[[r$data2]] != nimi_ariregistris |is.na(.data[[r$data2]])))


  df2 <- total %>% dplyr::group_by(result2) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::mutate(tulemus = ifelse(result2 == "TRUE", "Nimi ei ole vastavuses", "Nimi on vastavuses"))


  output$namemismatchPie <- renderPlotly({

    plotly::plot_ly(df2, labels = ~tulemus, values = ~count) %>%
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

  dataToDisplay <- reactive({
    if(input$select == "not_present"){
      file3 %>%
        dplyr::filter(result == F) %>%
        dplyr::select(-result)
    }
    else if(input$select == "diff_name"){
      total %>%
        dplyr::filter(result2 == T) %>%
        dplyr::select(-result2)
    }
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
# mod_analysis2_ui("analysis2_ui_1")
    
## To be copied in the server
# callModule(mod_analysis2_server, "analysis2_ui_1")
 
