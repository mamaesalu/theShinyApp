
#' Function for empty rows in column
#'
#' @param data input dataset
#' @param col name of column
#'
#' @return Returns empty rows in column col in data
#' @export
#'
#' @examples getMissing(data, col)
getMissing <- function(data, col){
  getcol <- unlist(data[, grep(col, colnames(data))], use.names = F)
  emptyRows <- dplyr::filter(data, is.na(getcol))
  
  return(emptyRows)
}

#' Function for duplicate rows in column
#'
#' @param data input dataset
#' @param col name of column
#'
#' @return Returns duplicate data rows in column col in data
#' @export
#'
#' @examples getDuplicates(data, col)
getDuplicates <- function(data, col){
  duplicatesInCol <- data %>%
    dplyr::group_by_at(col) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::filter(!is.na(!!rlang::sym(col))) %>%
    data.frame()
  
  return(duplicatesInCol)
}

#' Function for summarizing empty rows for two columns
#'
#' @param data input dataset
#' @param col1 name of column
#' @param col2 name of column
#'
#' @return Returns a summary of missing values % for col1 and col2
#' @export
#'
#' @examples getsummarymissing(data, col1, col2)
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

#' Function for summarizing duplicate rows for two columns
#'
#' @param data input dataset
#' @param col1 name of column
#' @param col2 name of column
#'
#' @return Returns a summary of unique/duplicate values for col1 and col2
#' @export 
#'
#' @examples getsummaryunique(data, col1, col2)
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