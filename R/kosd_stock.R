#' Get price history of KOSDAQ stock
#'
#' \code{kosd_stock} returns price history of a given KOSDAQ stock from Google finance
#'
#' Original: July 2017 by "personalinvesting (naver blog)"
#'
#' @param ticker a ticker code in the format as KOSDAQ::000000
#' @param start_date should be given as a format "2011-01-01"
#' @param end_date should be given as a format "2012-01-01"
#'
#' @return a df in the format as OHLC
#' @export
#'
#' @examples
#' kosd_stock("011000", "2014-01-01", "2017-09-22")
kosd_stock <- function(ticker, start_date, end_date){

  library(xts, quietly = TRUE)
  library(dplyr, quietly = TRUE)
  library(XML, quietly = TRUE)

  ticker <- paste("KOSDAQ:", ticker, sep = "")

  ##### URL generate function
  url_gen <- function(t, s, e) {

    ## changing ticker format
    ticker_m <- gsub(":", "%3A", t)

    ## setting locale
    lct <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")

    ## changing Date format 1
    s <- as.Date(s)
    e <- as.Date(e)

    ## changing Date format 2
    fmtstr <- "%b+%d%%2C+%Y" #ex) Jan+01,+2017
    start_date_fmt <- as.character(format(s, fmtstr))
    end_date_fmt <- as.character(format(e, fmtstr))

    ## URL completion
    url <- 'http://finance.google.com/finance/historical?q='
    paste(url, ticker_m, '&startdate=', start_date_fmt, '&enddate=', end_date_fmt, '&num=200', sep = '')
  }


  ##### Data importing function
  output_gen <- function(t2, s2, e2) {

    ## Data loading
    url <- url_gen(t2, s2, e2)
    output <- readHTMLTable(url, stringsAsFactors = FALSE)[4] %>% as.data.frame

    ## Bring Date col and changing colnames
    output[, 1] <- as.Date(output[, 1], format = "%b %d, %Y")
    names(output) <- c("Date", "Open", "High", "Low", "Close", "Volume")

    ## change format as numeric
    for (k in 2:6) {
      output[, k] <- as.numeric(gsub(",", "", output[, k]))
    }

    output
  }


  ##### Final output
  output <- output_gen(t = ticker, s = start_date, e = end_date)
  start <- as.Date(output[output %>% nrow, 1])


  ##### For LOOP
  while (start_date < start) {

    new_end <- start - 1

    if (new_end == start_date) {
      output <- rbind(output, output_gen(t = ticker, s = new_end - 10, e = new_end))
      output <- (output[, 1] >= new_end) %>% output[., ]
      break()
      } else {
        output <- rbind(output, output_gen(t = ticker, s = start_date, e = new_end))
      }

    start <- as.Date(output[output %>% nrow, 1])

  }
  output %>% (function(df) {xts(df[2:5], order.by = df[, 1])})
}
