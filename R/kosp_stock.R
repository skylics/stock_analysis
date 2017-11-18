#' Get price history of KOSPI stock
#'
#' \code{kosp_stock} returns price history of a given KOSPI stock from Google finance
#'
#' @param ticker a ticker code in the format as KRX:000000
#' @param start_date should be given as a format "2011-01-01"
#' @param end_date should be given as a format "2012-01-01"
#'
#' @return a df in the format as OHLC
#' @export
#'
#' @examples
#' kosp_stock("011000", "2011-01-01")
kosp_stock <- (function(code, from) {
  df <- code %>% paste("KRX:", ., sep = "") %>% quantmod::getSymbols(from = from, src = "google", auto.assign = FALSE)
  df
})
