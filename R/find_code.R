#' Find ticker code
#'
#' \code{find_code} returns ticker code from Daum finance when stock name is given.
#'
#' @param x is a name of the item to find the ticker code.
#'
#' @return character string of specific ticker code.
#' @export
#'
#' @examples
#' find_code("stockname")
find_code <- function(x) {

  library(rvest, quietly = TRUE)
  library(xml2, quietly = TRUE)
  library(dplyr, quietly = TRUE)

  ## Create URL
  url <- paste("http://finance.daum.net/search/search.daum?name=", x, "&nil_profile=vsearch&nil_src=stock", sep = "")

  ## Import HTML
  html <- url %>% read_html(encoding = "UTF-8")
  data <- html %>% html_nodes(css = ".list_stockinfo")

  ## Import TEXT code
  if (length(data) != 0) {
    data <- data %>% html_node('.stockCode') %>% html_text
  } else {
    data <- html %>% html_nodes(css = ".gTable.clr.subPrice") %>% html_nodes(css = ".txt") %>% .[1]
    data <- data %>% substring(47, 52)
  }

  return(data)
}
