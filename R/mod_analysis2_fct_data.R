
#' Function for evaluating matching/nonmatcing entries for codes
#'
#' @param data 
#' @param data2 
#' @param col1 
#'
#' @return Returns a dataframe of matching records (in column col1) in data and data2
#' @export  
#'
#' @examples regcodesmatch(data, data2, col1)
regcodesmatch <- function(data, data2, col1){
  getcol <- unlist(data[, grep(col1, colnames(data))], use.names = F)
  
  file3 <-  dplyr::mutate(data, result = getcol %in% data2$ariregistri_kood) %>%
    dplyr::filter(!is.na(getcol))
  
  return(file3)
}

#' Function for summarizing matching/nonmatcing entries for codes
#'
#' @param data 
#' @param data2 
#' @param col1 
#'
#' @return Returns a % summary of matching and non-matching entries in column col1 in data and data2
#' @export
#'
#' @examples codesmatchingsummary(data, data2, col1)
codesmatchingsummary <- function(data, data2, col1){
  
  codesmatching <- regcodesmatch(data, data2, col1)
  
  df <- codesmatching %>% dplyr::group_by(result) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::mutate(tulemus = ifelse(result == "TRUE", "Omab vastet", "Ei oma vastet")) %>%
    data.frame()
  
  return(df)
}

#' Function for evaluating the timeliness of names for the same registry code
#'
#' @param data 
#' @param data2 
#' @param col1 
#' @param col2 
#'
#' @return Returns result of matching/mismatching in col2 with equal value in col1
#' @export 
#'
#' @examples namesmismatchtotal(data, data2, col1, col2)
namesmismatchtotal <- function(data, data2, col1, col2){
  codesmatching <- regcodesmatch(data, data2, col1)
  
  filex <- codesmatching %>%
    dplyr::select(all_of(col1), all_of(col2), result) %>%
    dplyr::filter(result != FALSE)
  
  file2x <- data2 %>%
    dplyr::select(ariregistri_kood, nimi) %>%
    dplyr::rename(nimi_ariregistris = nimi)
  
  total <- filex %>% dplyr::left_join(file2x, by=setNames("ariregistri_kood", col1)) %>%
    dplyr::select(-result) %>%
    dplyr::mutate(result2 = (.data[[col2]] != nimi_ariregistris |is.na(.data[[col2]])))
  
  return(total)
}

#' Function for summarizing the timeliness of names for the same registry code
#'
#' @param data 
#' @param data2 
#' @param col1 
#' @param col2 
#'
#' @return Returns a summary of % of the names matching/mismatching in data and data2 and same value in col1
#' @export
#'
#' @examples namesmismatch(data, data2, col1, col2)
namesmismatch <- function(data, data2, col1, col2){
  total <- namesmismatchtotal(data, data2, col1, col2)
  
  df2 <- total %>% dplyr::group_by(result2) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::mutate(tulemus = ifelse(result2 == "TRUE", "Nimi ei ole vastavuses", "Nimi on vastavuses"))
  
  return(df2)
}