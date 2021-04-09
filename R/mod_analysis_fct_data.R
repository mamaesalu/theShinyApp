
getMissing <- function(data, col){
  getcol <- unlist(data[, grep(col, colnames(data))], use.names = F)
  emptyRows <- dplyr::filter(data, is.na(getcol))
  
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