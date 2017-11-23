#' Real time window price (actually lag exists)
#'
#' @param ticker a ticker code in the format as "000000"
#'
#' @return OHLC price of given stock on the day
#' @export
#'
#' @examples
#' "stock name" %>% find_code2 %>% real_time
real_time <- function(ticker) {

  library(rebus, quietly = TRUE)
  library(rvest, quietly = TRUE)

  url_code <- paste("http://finance.daum.net/item/quote.daum?code=", ticker, sep = "")
  tem_code <- read_html(url_code, encoding = "UTF-8")
  text <- tem_code %>% html_nodes("body") %>% html_nodes("div") %>% html_nodes(".leftDiv") %>% html_nodes("td") %>% html_text
  text_clean <- text %>%
    (function(df) {
      data.frame("Open" = df[which(df == "시가") + 1] %>% stringr::str_replace_all(",", "") %>% as.numeric,
                 "High" = df[which(df == "고가") + 1] %>% stringr::str_replace_all(",", "") %>% as.numeric,
                 "Low" = df[which(df == "저가") + 1] %>% stringr::str_replace_all(",", "") %>% as.numeric,
                 "Close" = df[which(df == "현재가") + 1] %>% stringr::str_replace_all(",", "") %>% as.numeric,
                 "Volumn" = df[which(df == "거래량") + 1] %>% stringr::str_replace_all(",", "") %>% as.numeric
                 ) %>% xts::xts(order.by = Sys.Date())
      })

  text_clean
}



