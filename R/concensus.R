#' Concensus of the stock, gathered in naver
#'
#' @param ticker a ticker code in the format as "000000"
#'
#' @return a data frame which contain c("opinion", "target price", "EPS", PER", "sample number")
#' @export
#'
#' @examples
#' "stock name" %>% find_code3 %>% concensus
concensus <- function(ticker) {


  library(magrittr, quietly = TRUE)
  library(wrapr, quietly = TRUE)
  library(xts, quietly = TRUE)


  url <- paste("http://companyinfo.stock.naver.com/v1/company/c1010001.aspx?cmp_cd=", ticker, "&target=finsum_more", sep = "")
  html_read <- read_html(url_code, encoding = "UTF-8")

  concensus <-
    html_read %>% html_node("body") %>% html_nodes("table") %>% .[12] %>% html_nodes("tr") %>% .[2] %>% html_children() %>% html_text %>%
    str_match(capture(one_or_more(DGT) %R% optional(".") %R% optional(",") %R% zero_or_more(DGT) %R% optional(",") %R% zero_or_more(DGT))) %>%
    .[, 2] %>% str_replace_all(",", "") %>% as.numeric %>% t %>% as.data.frame %<>% {colnames(.) <- c("평균의견", "목표(원)", "EPS(원)", "PER(배)", "추정기관수"); .}

  base_date <- tem_code %>% html_node("body") %>% html_nodes(".header-table-cell") %>% .[6] %>% html_text %>%
    str_match(capture(dgt(4) %R% "." %R% dgt(2) %R% "." %R% dgt(2))) %>% .[, 2] %>% str_replace_all(fixed("."), "-") %>% as.Date

  final_df <- concensus %>% {.$기준일 <- base_date; .} %>% xts(., order.by = Sys.Date())

  final_df

}
