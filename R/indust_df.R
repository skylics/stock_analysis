#' Title
#'
#' @param all_stocks
#'
#' @return
#' @export
#'
#' @examples
indust_df <- function(all_stocks = NULL) {

  library(purrr, quietly = TRUE)
  library(dplyr, quietly = TRUE)
  library(magrittr, quietly = TRUE)

  #####
  all_stocks <- entire_modf %>% filter(id == "sales") %>% .[, 1:2]
  cate_entire <- all_stocks$code %>% map(function(code) {code %>% industry}) %>% bind_rows
  all_stocks <- cbind(all_stocks, cate_entire)

  #####
  all_stocks$WICS %<>% {Encoding(.) <- "UTF-8"; .}
  all_stocks$IDST %<>% {Encoding(.) <- "UTF-8"; .}

  ##### Save file as rda format
  path <- paste(path1, "/data", sep = "")
  setwd(path)
  save(all_stocks, file = "all_stocks.rda")

}
