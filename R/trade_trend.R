#' Buying and Sell trend of foreign and organization
#'
#' \code{trade_trend} returns data frame of trading trends
#'
#' @param code a ticker code in the format as "000000" (use find_code())
#' @param from should be given as a format "2011-01-01"
#'
#' @return a data frame
#' @export
#'
#' @examples
#' find_code("stockname") %>% trade_trend("2015-01-01")
trade_trend <- function(code, from) {

  library(XML, quietly = TRUE)
  library(stringr, quietly = TRUE)

  theurl <- paste("http://finance.daum.net/item/foreign_yyyymmdd.daum?page=1&code=", code, sep = "")
  volumn_html <- readHTMLTable(theurl)
  volumn_table <- volumn_html[[1]][c(2:6, 9:13, 16:20, 23:27, 30:34, 37:41), c(1:5)]
  names(volumn_table) <- c("date", "fore_total", "fore_perc", "fore_buy", "org_buy")
  volumn_table$date <- paste("20", volumn_table$date, sep = "") %>% as.character %>% stringr::str_replace_all(fixed("."), "-") %>% as.Date

  i <- 1

  while (volumn_table$date[nrow(volumn_table)] > from) {
    i <- i + 1
    url <- paste("http://finance.daum.net/item/foreign_yyyymmdd.daum?page=", i, "&code=", code, sep = "")
    html <- readHTMLTable(url)
    table <- html[[1]][c(2:6, 9:13, 16:20, 23:27, 30:34, 37:41), c(1:5)]
    names(table) <- c("date", "fore_total", "fore_perc", "fore_buy", "org_buy")
    table$date <- paste("20", table$date, sep = "") %>% as.character %>%  str_replace_all(fixed("."), "-") %>% as.Date

    volumn_table <- rbind(volumn_table, table)

  }

  final_table <- volumn_table %>% (function(df) {(df$date >= from) %>% df[., ]})
  final_table[, c(2, 4, 5)] <- final_table[, c(2, 4, 5)] %>%
    apply(2, function(df) {df %>% stringr::str_replace_all(",", "") %>% as.numeric})
  final_table[, 3] <- (final_table[, 3] %>% as.character %>% str_replace_all("%", "") %>% str_replace_all(fixed("."), "") %>% as.numeric)/10000
  final_table$total <- final_table$fore_total/final_table$fore_perc
  final_table[, c("date", "fore_total", "fore_perc", "fore_buy", "org_buy")]
}
