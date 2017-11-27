#' Title
#'
#' @param sejong_nms
#'
#' @return
#' @export
#'
#' @examples
sejong_modf <- function(sejong_nms = NULL) {


  library(purrr, quietly = TRUE)
  library(dplyr, quietly = TRUE)
  library(tibble, quietly = TRUE)
  library(magrittr, quietly = TRUE)
  library(stringr, quietly = TRUE)
  library(rebus, quietly = TRUE)
  library(Quantrade, quietly = TRUE)


  ##### 세종 전체 데이터 크롤링
  sejong_nms <- c("sales", "bp", "np", "asset", "liability", "capital")
  entire_sejong <- sejong_nms %>% purrr::map(function(char) {Quantrade::sejong_data(which_item = char)})


  ##### List -> data.frame으로 변형
  entire_modf <- 1:length(entire_sejong) %>%
    map(function(num) {entire_sejong[[num]]$id <- sejong_nms[num];
    rownames_to_column(entire_sejong[[num]], "name")
    }) %>% bind_rows %>% arrange(name)


  ##### 결산 분기 character string 생성
  year_num <- last_account %>% str_match(capture(dgt(2)) %R% "년") %>% .[2] %>% as.numeric
  quarter_num <- last_account %>% str_match(capture(dgt(1)) %R% "분기") %>% .[2] %>% as.numeric
  quarter_place <- names(entire_modf) %>% str_detect("T") %>% which

  nm_entire <- c(1:5) %>% map(function(num) {
    value <- quarter_num - (quarter_place %>% length - num)
    if (value <= 0) {paste(year_num - 1, ".", value + 4, "Q", sep = "")
    } else {paste(year_num, ".", value, "Q", sep = "")}
  }) %>% flatten %>% unlist


  ##### 결산 분기 column name 달기
  entire_modf %<>%
  {names(.)[quarter_place] <- nm_entire;
  .$code <-  .$code %>% as.character;
  .[, c(1, 2, length(.), 3:(length(.) - 1))]}


  ##### Save file as rda format
  path <- paste(path1, "/data", sep = "")
  setwd(path)
  save(entire_modf, file = "entire_modf.rda")

}
