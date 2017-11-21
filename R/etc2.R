#' Find the place, KOSPI or KOSDAQ?
#'
#' @param ticker a ticker code in the format as "000000"
#'
#' @return a single character "P" or "D"
#' @export
#'
#' @examples
#' "stock name" %>% find_code2 %>% D_or_P
book_open2 <- function(ticker) {

  http://companyinfo.stock.naver.com/v1/company/c1010001.aspx?cmp_cd=071840&target=finsum_more

  url_code <- paste("http://companyinfo.stock.naver.com/v1/company/c1010001.aspx?cmp_cd=", "071840", "&target=finsum_more", sep = "")
  tem_code <- read_html(url_code, encoding = "UTF-8")

  major_node <- tem_code %>% html_node("body") %>% html_node("div") %>% html_node("form") %>% html_node("div") %>%
    html_node("div") %>% html_nodes("div") %>% .[4] %>% html_nodes("div") %>% .[5] %>% html_node("div") %>%
    html_node("div")

  ## 신용정보
  credit_grade <- major_node %>% html_nodes("div") %>% .[12] %>% html_node("div") %>% html_node("table") %>% html_table %>% as.data.frame

  ## 주요 지분
  stock_stake <- major_node %>% html_nodes("div") %>% .[14] %>% html_nodes("table") %>% .[2] %>% html_table %>% as.data.frame

  ## 기업 정보
  comp_inform <- major_node %>% html_nodes("div") %>% .[17] %>% html_node(".cmp_comment") %>% html_nodes("li") %>% html_text

  ## 재무 지표
  indexes <- major_node %>% html_nodes("div") %>% .[21] %>% html_node("table") %>% html_table %>% as.data.frame()

  ## 어닝 서프라이즈
  indexes
  major_node %>% html_nodes("div") %>% .[22] %>% html_text

  major_node %>% html_nodes("div") %>% .[37]

  major_node %>% html_nodes("div") %>% .[37] %>% html_nodes("")

  major_node %>% html_nodes(".gHead01")

  tem_code %>% html_nodes(".gHead01")


  major_node %>% html_nodes(".schbox") %>% .[3] %>% html_structure()
  major_node %>% html_nodes(".gHead01") %>% .[3] %>% html_text
  tem_code %>% html_nodes(".gHead01") %>% .[3] %>% html_text


}

