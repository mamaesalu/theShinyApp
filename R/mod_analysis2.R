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
      column(plotOutput(ns("duplicatesBar")), width = 6)
    ),
    fluidRow(
      h3("Mittevastavad kirjed:"),
      selectInput(ns("select"), "",
                  c("Registrikoodid, mida ei ole äriregistris" = "not_present",
                    "Registrikoodile vastav nimi äriregistris on erinev" = "diff_name")),
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
  
  file3 <-  dplyr::mutate(userdata, result = getcol %in% refdata$ariregistri_kood) %>%
            dplyr::filter(!is.na(ariregistri_kood))
  
  
  df <- file3 %>% dplyr::group_by(result) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::mutate(tulemus = ifelse(result == "TRUE", "Omab vastet", "Ei oma vastet")) %>%
    data.frame()


  msg <- paste(colnames(df))
  cat(msg, "\n")
  
  output$missingBar <- renderPlotly({

    plotly::plot_ly(df, labels = ~tulemus, values = ~count) %>%
      plotly::add_pie(hole = 0.6) %>%
      plotly::layout(title = "Registrikoodide vastavuse % kasutaja andmestikus <br> võrdluses äriregistri andmetega", font=list(size = 10),
                     showlegend = F,
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     margin = list(l = 50, r = 50,
                                   b = 100, t = 100,
                                   pad = 50))

  })
  
  # output$duplicatesBar <- renderPlot(
  #   #shinipsum::random_ggplot("random")
  # )
  # 
  # output$resultsTable <- renderDataTable({
  #   #shinipsum::random_DT(5, 5)
  # })
}
    
## To be copied in the UI
# mod_analysis2_ui("analysis2_ui_1")
    
## To be copied in the server
# callModule(mod_analysis2_server, "analysis2_ui_1")
 
