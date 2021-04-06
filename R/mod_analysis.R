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
      column(plotOutput(ns("namePie")), width = 6)
            ),
    fluidRow(
      column(dataTableOutput(ns("missingTable")), width = 6),
      column(dataTableOutput(ns("uniqueTable")), width = 6)
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
  userdata[userdata==""]<-NA
  
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
  
  
  
  output$namePie <- renderPlot({
    ggplot2::ggplot(uniquevalues) +
      ggplot2::geom_bar(ggplot2::aes(x = key,
                                     y = pct, fill=num.isunique),
                        stat = 'identity', alpha=0.8) +
      ggplot2::theme(title=ggplot2::element_text(size=20)) +
      ggplot2::labs(title = "Unikaalsuse hinnang (%)", x = "", y = "%") +
      ggplot2::guides(fill=ggplot2::guide_legend(title = "Unikaalsed"))
  })
  
  output$missingTable <- renderDataTable({
    missingvalues
  })
  output$uniqueTable <- renderDataTable({
    uniquevalues
  })
  
}
    
## To be copied in the UI
# mod_analysis_ui("analysis_ui_1")
    
## To be copied in the server
# callModule(mod_analysis_server, "analysis_ui_1")
 
