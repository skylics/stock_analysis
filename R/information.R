#' Title
#'
#' @param code
#'
#' @return
#' @export
#'
#' @examples
information <- function(code = NULL) {

  ##### ticker 코드 불러오기
  code <- find_code2(stock_name)
  assign("code", code, envir = .GlobalEnv)

  ##### 코스닥 or 코스피
  assign("where_belong", code %>% D_or_P, envir = .GlobalEnv)

  ##### 상장일
  assign("start_date", code %>% listing_date, envir = .GlobalEnv)

}
