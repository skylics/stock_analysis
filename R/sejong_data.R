#' Get operating profit data in sejong
#'
#' @param which_item choose among c("sales", "bp", "np", "asset", "liability", "capital")
#' @param ticker If specified in the format as 000000, the function returns only a given stock information. If total, the default, returns whole information.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' "stock_name" %>% find_code2 %>% sejong_data("name of statement item")
sejong_data <- function(ticker = "total", which_item) {

  ## Package settings
  if (!require("dplyr")) install.packages("dplyr")
  library(dplyr, quietly = TRUE)

  if (!require("rvest")) install.packages("rvest")
  library(rvest, quietly = TRUE)


  ## which item to be loaded
  item <- (c("sales", "bp", "np", "asset", "liability", "capital") == which_item) %>% which %>%
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

  if (ticker == "total") {

    assign(x = "data_code", value = data_code, envir = .GlobalEnv)
    assign(x = "data_op", value = data_op, envir = .GlobalEnv)

    } else {

      data_op %>%
        (function(df) {
          rownames(df) <- df$X2
          names(df) <- c("code", "name", "T-4", "T-3", "T-2", "T-1", "T")
          df$unit <- "10^8"
          df[(df$code == ticker), c(1, 3, 4, 5, 6, 7, 8)]})
      }
}
