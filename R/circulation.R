#' Number of circulated listed shares
#'
#' @param ticker a ticker code in the format as "000000"
#'
#' @return number of circulated shares
#' @export
#'
#' @examples
#' "stock name" %>% find_code2 %>% circulation
circulation <- function(ticker) {

  library(rebus, quietly = TRUE)
  library(rvest, quietly = TRUE)

  paste("http://finance.daum.net/item/quote.daum?code=", ticker, sep = "") ->.;
  read_html(., encoding = "UTF-8") ->.;
  html_nodes(., "body") ->.;
  html_nodes(., "div") ->.;
  html_nodes(., ".leftDiv") ->.;
  html_nodes(., "td") ->.;
  html_text(.) -> text

  text %>%
    (function(df) {
      data.frame("상장주식수" = df[which(df == "상장주식수") + 1] %>% stringr::str_replace_all(",", "") %>% as.numeric) %>%
        xts::xts(order.by = Sys.Date())
    })
}


