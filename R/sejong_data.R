#' Get operating profit data in sejong
#'
#' @param which_item choose among c("sales", "bp", "np", "asset", "liability", "capital"). If you want only ticker code, choose "code".
#' @param ticker If specified in the format as 000000, the function returns only a given stock information. If total, the default, returns whole information.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' "stock_name" %>% find_code2 %>% sejong_data("name of statement item")
sejong_data <- function(ticker = "total", which_item) {


  ##### Package settings
  if (!require("dplyr")) install.packages("dplyr")
  library(dplyr, quietly = TRUE)

  if (!require("rvest")) install.packages("rvest")
  library(rvest, quietly = TRUE)


  ##### You can put which_item arg as "code" if you only need codes of all stocks
  if (which_item == "code") {

    ## Bring all stocks code & name
    url_code <- "http://www.sejongdata.com/query/value.html"
    tem_code <- read_html(url_code, encoding = "UTF-8")
    dat_code <- tem_code %>% html_nodes(".bus_board_txt1") %>% html_text
    tit_code <- tem_code %>% html_nodes('.bus_board_tit1') %>% html_text
    data_code <- data.frame(matrix(dat_code, ncol = 5, byrow = T))[, c(1:2)]

    if (ticker == "total") {
      final_data <- data_code
    } else {
      final_data <- data_code[which(data_code[, 1] == ticker), ]
    }

  } else {

  ##### If you need financial statements, keep going by select proper which_item

    ## Which item to be loaded
    item <- (c("sales", "bp", "np", "asset", "liability", "capital") == which_item) %>%
      which %>% c("sales", "bp", "np", "asset", "liability", "capital")[.]

    col_num <- (c("sales", "bp", "np", "asset", "liability", "capital") == which_item) %>%
      which %>% c(7, 7, 7, 6, 6, 6)[.]


    ## All stocks statements should be gathered first
    data_op <- data.frame(matrix(vector(), 0, col_num), stringsAsFactors = FALSE)

    for (i in 1:5) {

      url_op <- paste("http://www.sejongdata.com/query/", item, i, ".html", sep = "")
      tem_op <- read_html(url_op, encoding = "UTF-8")
      dat_op <- tem_op %>% html_nodes(".bus_board_txt1") %>% html_text
      tit_op <- tem_op %>% html_nodes('.bus_board_tit1') %>% html_text
      data_op <- rbind(data_op, data.frame(matrix(dat_op, ncol = col_num, byrow = T)))

    }


    ## Cleaning more
    cleaned <- data_op %>% (function(df) {

      if (col_num == 7) {
        names(df) <- c("code", "name", "T-4", "T-3", "T-2", "T-1", "T")
      } else if (col_num == 6) {
        names(df) <- c("code", "name", "T-3", "T-2", "T-1", "T")
      }

      rownames(df) <- df$name
      df[, -2]
    })


    ## If you want all stocks statements, choose ticker arg as "total"
    if (ticker == "total") {
      final_data <- cleaned
    } else {
        final_data <- cleaned[which(cleaned$code == ticker), ]
    }

  }

  final_data

}
