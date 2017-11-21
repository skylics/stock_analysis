#' Get price history of KOSDAQ stock
#'
#' \code{kosd_stock2} returns price history of a given KOSDAQ stock from naver finance
#'
#' @param ticker a ticker code in the format as 000000
#' @param start_date should be given as a format "2011-01-01"
#' @param end_date should be given as a format "2012-01-01"
#'
#' @return a df in the format as OHLC
#' @export
#'
#' @examples
#' kosd_stock2("011000", "2014-01-01", "2017-09-22")
kosd_stock2 <- function(ticker, start_date, end_date) {


  library(dplyr, quietly = TRUE)
  library(rvest, quietly = TRUE)
  library(stringr, quietly = TRUE)
  library(rebus, quietly = TRUE)
  library(purrr, quietly = TRUE)


  ##### Create scrapping function when is given
  scrap_page <- function(i) {

    url_code <- paste("http://finance.naver.com/item/sise_day.nhn?code=", ticker, "&page=", i, sep = "")
    tem_code <- read_html(url_code)
    text_code <- tem_code %>% html_node("body") %>% html_node("table") %>% html_nodes("td") %>% html_text
    place_date <- text_code %>% str_detect(dgt(4) %R% "." %R% dgt(2) %R% "." %R% dgt(2)) %>% which()

    ##### Scrapping web page
    scrap <- 1:length(place_date) %>% map(function(num) {

      ## See which should be extracted
      anchor <- c(place_date, length(text_code) + 1)
      raw <- (anchor[num] + 1):(anchor[num + 1] - 1) %>% text_code[.]

      ## Get data cleaned
      raw %>% str_detect(START %R% one_or_more(DGT) %R% optional(",") %R% one_or_more(DGT) %R% END) %>% raw[.] %>%
        str_replace_all(",", "") %>% as.numeric %>% as.data.frame(stringsAsFactor = FALSE) %>% t %>%
        (function(df) {
          colnames(df) <- c("Close", "Open", "High", "Low", "Volumn")
          df %>% xts(order.by = place_date[num] %>% text_code[.] %>% str_replace_all(fixed("."), "-") %>% as.Date)
        })
    }) %>% do.call(rbind, .)
  }


  ##### Initial Scrap for (i = 1)
  i <- 1
  scrap <- scrap_page(i)


  ##### Loop until
  while (scrap %>% index %>% first > start_date) {
    i <- i + 1
    scrap <- rbind(scrap, scrap_page(i))
  }


  ##### Final cleaning
  final_data <- scrap[(scrap %>% index >= start_date) & (scrap %>% index <= end_date), ]

  final_data

}
