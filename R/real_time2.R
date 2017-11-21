#' Get real time minute price of choosen stock
#'
#' \code{real_time2} returns daily variation of stock price per minute
#'
#' @param ticker a ticker code in the format as 000000
#' @param start_time time in the format as "MM:SS" (internally transformed into POSIXct), and default is given by "09:00"
#' @param end_time time in the format as "MM:SS" (internally transformed into POSIXct), and default is given by Sys.time()
#'
#' @return a df in the format as OHLC
#' @export
#'
#' @examples
#' real_time("011000")
real_time2 <- function(ticker, start_time = NULL, end_time = NULL) {


  library(dplyr, quietly = TRUE)
  library(rvest, quietly = TRUE)
  library(stringr, quietly = TRUE)
  library(rebus, quietly = TRUE)
  library(purrr, quietly = TRUE)


  ##### start_time & end_time
  if (is.null(start_time)) {
    start_time <- paste(as.character(Sys.Date()), " ", "09:00:00", " KST", sep = "") %>% as.POSIXct
  } else {
    start_time <- paste(as.character(Sys.Date()), " ", start_time, ":00", " KST", sep = "") %>% as.POSIXct
  }

  if (is.null(end_time)) {
    end_time <- Sys.time()
  } else {
    end_time <- paste(as.character(Sys.Date()), " ", end_time, ":00", " KST", sep = "") %>% as.POSIXct
  }


  ##### Create scrapping function when is given
  scrap_page <- function(i) {

    search_time <- end_time %>% as.character() %>% str_replace_all("-", "") %>% str_replace_all(":", "") %>% str_replace_all(" ", "")
    url_code <- paste("http://finance.naver.com/item/sise_time.nhn?code=", ticker , "&thistime=", search_time, "&page=", i, sep = "")
    tem_code <- read_html(url_code)
    text_code <- tem_code %>% html_node("body") %>% html_node("table") %>% html_nodes("td") %>% html_text
    place_date <- text_code %>% str_detect(START %R% dgt(2) %R% ":" %R% dgt(2) %R% END) %>% which()

    ##### Scrapping web page
    scrap <- 1:length(place_date) %>% map(function(num) {

      ## See which should be extracted
      anchor <- c(place_date, length(text_code) + 1)
      raw <- (anchor[num] + 1):(anchor[num + 1] - 1) %>% text_code[.]

      ## Get data cleaned
      raw %>% str_detect(START %R% one_or_more(DGT) %R% optional(",")) %>% raw[.] %>%
        str_replace_all(",", "") %>% as.numeric %>% as.data.frame(stringsAsFactor = FALSE) %>% t %>%
        (function(df) {
          colnames(df) <- c("Done", "Offer", "Bid", "Volumn", "Volumn_diff")
          df %>% xts(order.by = paste(as.character(Sys.Date()), " ", place_date[num] %>% text_code[.], ":00", " KST", sep = "") %>% as.POSIXct)
        })
    }) %>% do.call(rbind, .)
  }


  ##### Initial Scrap for (i = 1)
  i <- 1
  scrap <- scrap_page(i)


  ##### Loop until
  while (scrap %>% index %>% first > start_time) {
    i <- i + 1
    scrap <- rbind(scrap, scrap_page(i))
  }



  ##### Final cleaning
  final_data <- scrap[(scrap %>% index >= start_time) & (scrap %>% index <= end_time), ]

  final_data

}
