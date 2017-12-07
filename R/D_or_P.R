#' Find the place, KOSPI or KOSDAQ?
#'
#' @param ticker a ticker code in the format as "000000"
#'
#' @return a single character "P" or "D"
#' @export
#'
#' @examples
#' "stock name" %>% find_code2 %>% D_or_P
D_or_P <- function(ticker) {
  url_code <- paste("http://finance.daum.net/item/company.daum?code=", ticker, "&type=11", sep = "")
  tem_code <- read_html(url_code, encoding = "UTF-8")
  dat_code <- tem_code %>% html_nodes(".list_stockinfo") %>% html_text

  if (dat_code %>% stringr::str_detect("코스피")) {
    output <- "P"
  } else if (dat_code %>% stringr::str_detect("코스닥")) {
    output <- "D"
  }
  output
}

