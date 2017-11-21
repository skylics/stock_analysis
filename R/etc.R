#' Find the place, KOSPI or KOSDAQ?
#'
#' @param ticker a ticker code in the format as "000000"
#'
#' @return a single character "P" or "D"
#' @export
#'
#' @examples
#' "stock name" %>% find_code2 %>% D_or_P
book_open2 <- function(ticker) {

  library("stringr")

  url_code <- paste("http://companyinfo.stock.naver.com/v1/company/c1020001.aspx?cmp_cd=", ticker, "&cn=", sep = "")
  tem_code <- read_html(url_code, encoding = "UTF-8")

  industry <- tem_code %>% html_nodes(".wrapper-table") %>% html_nodes("table") %>% html_nodes("tr") %>%
    .[1] %>% html_nodes("dt") %>% html_text %>% .[3] %>% str_match(": " %R% capture(one_or_more(NOT_SPC))) %>% .[2]

  WICS <- tem_code %>% html_nodes(".wrapper-table") %>% html_nodes("table") %>% html_nodes("tr") %>%
    .[1] %>% html_nodes("dt") %>% html_text %>% .[4] %>% str_match(": " %R% capture(one_or_more(NOT_SPC))) %>% .[2]
  # WISEfn Industry Classification Standard, modified by WISEfn

  tem_code %>% html_nodes(".wrapper-table") %>% html_nodes("table") %>% html_nodes("tr") %>%
    .[3] %>% html_nodes("dt") %>% html_text %>% .[4] %>% str_match(": " %R% capture(one_or_more(NOT_SPC))) %>% .[2]


}
