#' Find Total number of shares, main holder's amounts.
#'
#' @param ticker a ticker code in the format as "000000"
#'
#' @return a numeric data.frame
#' @export
#'
#' @examples
#' "stock name" %>% find_code2 %>% listing_date
share_inform <- function(ticker) {

  library(rebus, quietly = TRUE)
  library(wrapr, quietly = TRUE)
  library(rvest, quietly = TRUE)


  ## Total shares
  paste("http://companyinfo.stock.naver.com/v1/company/c1010001.aspx?cmp_cd=", ticker, "&cn=", sep = "") ->.;
  read_html(., encoding = "UTF-8") ->.;
  html_nodes(., "body") ->.;
  html_nodes(., "div") ->.;
  html_nodes(., "div") ->.;
  .[[15]] ->.;
  html_nodes(., "div") ->.;
  .[[2]] ->.;
  html_nodes(., "tr") ->.;
  .[[7]] ->.;
  html_text(.) ->.;
  stringr::str_match(., capture(dgt(1) %R% zero_or_more(NOT_SPC)) %R% "주" %R% zero_or_more(NOT_DGT) %R%
                       capture(dgt(1) %R% zero_or_more(NOT_SPC)) %R% "%") ->.;
  .[2] %>%
    stringr::str_replace_all(stringr::fixed(","), "") %>%
    as.numeric -> total_share


  ## Circulation rate / main holder's rate
  paste("http://companyinfo.stock.naver.com/v1/company/c1070001.aspx?cmp_cd=", ticker, "&cn=", sep = "") ->.;
  read_html(., encoding = "UTF-8") ->.;
  html_nodes(., "body") ->.;
  html_nodes(., "div") ->.;
  .[[13]] ->.;
  html_nodes(., "table") ->.;
  .[[1]] ->.;
  html_nodes(., "tbody") ->.;
  html_nodes(., "td") ->.;
  html_text(.) ->.;

  data.frame(
    total = total_share,
    circul = .[9] %>% stringr::str_match(capture(dgt(1) %R% zero_or_more(NOT_SPC)) %R% "주") %>% .[2] %>%
      stringr::str_replace_all(stringr::fixed(","), "") %>% as.numeric %>%
      (function(df) {ifelse(is.na(df), 0, df)}),
    main = .[2] %>% stringr::str_match(capture(dgt(1) %R% zero_or_more(NOT_SPC)) %R% "주") %>% .[2] %>%
      stringr::str_replace_all(stringr::fixed(","), "") %>% as.numeric %>%
      (function(df) {ifelse(is.na(df), 0, df)}),
    perc_10 = .[4] %>% stringr::str_match(capture(dgt(1) %R% zero_or_more(NOT_SPC)) %R% "주") %>% .[2] %>%
      stringr::str_replace_all(stringr::fixed(","), "") %>% as.numeric %>%
      (function(df) {ifelse(is.na(df), 0, df)}),
    perc_5 = .[6] %>% stringr::str_match(capture(dgt(1) %R% zero_or_more(NOT_SPC)) %R% "주") %>% .[2] %>%
      stringr::str_replace_all(stringr::fixed(","), "") %>% as.numeric %>%
      (function(df) {ifelse(is.na(df), 0, df)}),
    perc_etc = .[8] %>% stringr::str_match(capture(dgt(1) %R% zero_or_more(NOT_SPC)) %R% "주") %>% .[2] %>%
      stringr::str_replace_all(stringr::fixed(","), "") %>% as.numeric %>%
      (function(df) {ifelse(is.na(df), 0, df)})
  ) -> final

  final$sum <- final[, c(3:6)] %>% sum
  rownames(final) <- ticker

  final

}
