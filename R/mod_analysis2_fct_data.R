
regcodesmatch <- function(data, data2, col1){
  getcol <- unlist(data[, grep(col1, colnames(data))], use.names = F)
  
  file3 <-  dplyr::mutate(data, result = getcol %in% data2$ariregistri_kood) %>%
    dplyr::filter(!is.na(getcol))
  
  return(file3)
}

codesmatchingsummary <- function(data, data2, col1){
  
  codesmatching <- regcodesmatch(data, data2, col1)
  
  df <- codesmatching %>% dplyr::group_by(result) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::mutate(tulemus = ifelse(result == "TRUE", "Omab vastet", "Ei oma vastet")) %>%
    data.frame()
  
  return(df)
}

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

namesmismatch <- function(data, data2, col1, col2){
  total <- namesmismatchtotal(data, data2, col1, col2)
  
  df2 <- total %>% dplyr::group_by(result2) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::mutate(tulemus = ifelse(result2 == "TRUE", "Nimi ei ole vastavuses", "Nimi on vastavuses"))
  
  return(df2)
}