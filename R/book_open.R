#' Bring yearly BS book data from sejong
#'
#' \code{book_open} returns major accounting book data

#' @param code a ticker code in the format as "000000" (use find_code())
#'
#' @return a data frame
#' @export
#'
#' @examples
#' find_code("stockname") %>% book_open
book_open <- function(code) {

  url <- paste("http://www.sejongdata.com/business_include_fr/table_main0_bus_01.html?&no=", code, sep = "")

  html_sejong <- xml2::read_html(url, encoding = "UTF-8")

  sejongdata <- html_sejong %>% rvest::html_nodes(".bus_board_txt1") %>% html_text %>% data.frame

  date <- rvest::html_nodes(html_sejong, css = ".bus_board_tit1") %>% html_text %>% data.frame

  ## create empty frame
  data <- matrix(data = NA, nrow = 15, ncol = 10)

  ## column name / row name
  colnames(data) <- as.vector(date[2:11, ])
  tmp <- c(1, 12, 23, 34, 45, 56, 67, 78, 89, 100, 111, 122, 133, 144, 155)
  rownames(data) <- as.vector(sejongdata[tmp, 1])

  ## transpose
  data <- t(data)

  for(i in 1:15) {
    data[, i] <-
      sejongdata[((tmp[i] + 1):(tmp[i] + 1 + 9)), ] %>% as.vector %>% sub(",", "", .) %>% as.numeric %>%
      (function(df) {df[is.na(df)] <- ""; df})
  }

  data <- t(data)
  data <- data %>% as.data.frame
  data
}
