#' Find ticker code
#'
#' \code{find_code} returns ticker code from Daum finance when stock name is given.
#'
#' @param x is a name of the item to find the ticker code.
#'
#' @return character string of specific ticker code.
#' @export
#'
#' @examples
#' find_code2("stockname")
find_code2 <- function(x) {

  ## Package settings
  if (!require("dplyr")) install.packages("dplyr")
  library(dplyr, quietly = TRUE)

  if (!require("rvest")) install.packages("rvest")
  library(rvest, quietly = TRUE)


  ## Bring all stocks code & name
  url_code <- "http://www.sejongdata.com/query/value.html"

  tem_code <- read_html(url_code, encoding = "UTF-8")
  dat_code <- tem_code %>% html_nodes(".bus_board_txt1") %>% html_text
  tit_code <- tem_code %>% html_nodes('.bus_board_tit1') %>% html_text
  data_code <- data.frame(matrix(dat_code, ncol = 5, byrow = T))[, c(1:2)]

  data_code %>%
    (function(df) {
      df[which(df$X2 == x), 1]
    }) %>% as.character

}
