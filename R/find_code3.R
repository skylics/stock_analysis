#' Find ticker code
#'
#' \code{find_code3} returns ticker code from Daum finance when stock name is given.
#'
#' @param x is a name of the item to find the ticker code.
#'
#' @return character string of specific ticker code.
#' @export
#'
#' @examples
#' find_code3("stockname")
find_code3 <- function(ticker) {


  ## Package settings
  if (!require("dplyr")) install.packages("dplyr")
  library(dplyr, quietly = TRUE)

  if (!require("rvest")) install.packages("rvest")
  library(rvest, quietly = TRUE)


  ## Bring all stocks code & name
  url_code <- paste("http://companyinfo.stock.naver.com/v1/company/c1020001.aspx?cmp_cd=", ticker, "&cn=", sep = "")
  tem_code <- read_html(url_code, encoding = "UTF-8")

  ticker <- tem_code %>% html_nodes(".wrapper-table") %>% html_nodes("table") %>% html_nodes("tr") %>% .[1] %>% html_nodes(".num") %>% html_text

  ticker
}


"아이이" %>% find_code3
