#' Title
#'
#' @param code
#'
#' @return
#' @export
#'
#' @examples
listing_date <- function(code) {
  url_code <- paste("http://wisefn.stock.daum.net/company/c1020001.aspx?cmp_cd=", code, "&frq=&rpt=", sep = "")
  tem_code <- read_html(url_code, encoding = "UTF-8")
  text <- tem_code %>% html_nodes("td") %>% html_text()
  text_clean <- text %>% stringr::str_detect("상장일") %>% text[.] %>%
    stringr::str_match("상장일: " %R% capture(one_or_more(NOT_SPC)) %R% "\\)" %R% END) %>% .[2] %>%
    as.Date("%Y/%m/%d")

  text_clean
}
