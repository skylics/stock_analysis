#' Get operating profit data in sejong
#'
#' @param ticker a ticker code in the format as 000000
#' @param which which Financial statements item to import
#'
#' @return a data frame
#' @export
#'
#' @examples
#' "롯데하이마트" %>% find_code %>% sejong_data("영업이익")
sejong_data <- function(ticker, which_item) {

  ## Package settings
  if (!require("dplyr")) install.packages("dplyr")
  library(dplyr, quietly = TRUE)

  if (!require("rvest")) install.packages("rvest")
  library(rvest, quietly = TRUE)


  ## which item to be loaded
  item <- (c("매출액", "영업이익", "순이익", "자산", "부채", "자본") == which_item) %>% which %>%
    c("sales", "bp", "np", "asset", "liability", "capital")[.]


  ## Bring all stocks code & name
  url_code <- "http://www.sejongdata.com/query/value.html"

  tem_code <- read_html(url_code, encoding = "UTF-8")
  dat_code <- tem_code %>% html_nodes(".bus_board_txt1") %>% html_text
  tit_code <- tem_code %>% html_nodes('.bus_board_tit1') %>% html_text
  data_code <- data.frame(matrix(dat_code, ncol = 5, byrow = T))[, c(1:2)]


  ## Bring all stocks operating profits
  data_op <- data.frame(matrix(vector(), 0, 7), stringsAsFactors = FALSE)

  for (i in 1:5) {
    url_op <- paste("http://www.sejongdata.com/query/", item, i, ".html", sep = "")

    tem_op <- read_html(url_op, encoding = "UTF-8")
    dat_op <- tem_op %>% html_nodes(".bus_board_txt1") %>% html_text
    tit_op <- tem_op %>% html_nodes('.bus_board_tit1') %>% html_text
    data_op <- rbind(data_op, data.frame(matrix(dat_op, ncol = 7, byrow = T)))
  }

  data_op %>%
    (function(df) {
      rownames(df) <- df$X2
      names(df) <- c("코드", "종목", "T-4기", "T-3기", "T-2기", "T-1기", "T기")
      df$단위 <- "억원"
      df[(df$코드 == ticker), c(1, 3, 4, 5, 6, 7, 8)]

      })
}
