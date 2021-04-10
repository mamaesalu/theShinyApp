
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

getsummarymissing <- function(data, col1, col2){
  missingvalues <-  data %>%
    dplyr::select(col1, col2) %>%
    tidyr::gather(key = "key", value = "val") %>%
    dplyr::mutate(isna = !is.na(val)) %>%
    dplyr::group_by(key) %>%
    dplyr::mutate(total = dplyr::n()) %>%
    dplyr::group_by(key, total, isna) %>%
    dplyr::summarise(num.isna = dplyr::n()) %>%
    dplyr::mutate(pct = num.isna / total * 100)
  
  return(missingvalues)
}

getsummaryunique <- function(data, col1, col2){
  uniquevalues <-  data %>%
    dplyr::select(col1, col2) %>%
    tidyr::gather(key = "key", value = "val") %>%
    dplyr::filter(!is.na(val)) %>%
    dplyr::group_by(key) %>%
    dplyr::mutate(total = dplyr::n()) %>%
    dplyr::group_by(key, total) %>%
    dplyr::summarise(num.isunique = dplyr::n_distinct(val, na.rm = TRUE)) %>%
    dplyr::mutate(pct = num.isunique / total * 100)
  
  return(uniquevalues)
}