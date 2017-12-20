#' Get price history of single equity
#'
#' @param ticker
#' @param start_date
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
stock_price <- function (ticker, start_date, end_date) {


  ###4### Make scrape function
  scrape_page <- function(i) {


    ###5### Scrape
    paste0("http://finance.daum.net/item/quote_yyyymmdd_sub.daum?page=", i, "&code=",
           ticker, "&modify=1") ->.;
    xml2::read_html(.) ->.;
    rvest::html_node(., "body") ->.;
    rvest::html_node(., "table") ->.;
    rvest::html_nodes(., "tr") ->.;
    rvest::html_text(.) -> text_code ->.;   # Assign
    stringr::str_detect(., dgt(2) %R% "." %R% dgt(2) %R% "." %R% dgt(2)) ->.;
    which(.) -> place_date   # Assign
    #text_code[place]

    ###5### Cleaning
    c(1:length(place_date)) ->.;

    purrr::map_df(., function(num) {

      place_date[num] ->.;
      text_code[.] ->.;
      stringr::str_replace_all(., "\\n" , "") ->.;
      stringr::str_replace_all(., "\\t" , " ") ->.;
      stringr::str_match(., START %R%
                           capture(one_or_more(NOT_SPC)) %R% one_or_more(SPC) %R%
                           capture(one_or_more(NOT_SPC)) %R% one_or_more(SPC) %R%
                           capture(one_or_more(NOT_SPC)) %R% one_or_more(SPC) %R%
                           capture(one_or_more(NOT_SPC)) %R% one_or_more(SPC) %R%
                           capture(one_or_more(NOT_SPC)) %R% one_or_more(SPC) %R%
                           one_or_more(NOT_SPC) %R% one_or_more(SPC) %R%
                           one_or_more(NOT_SPC) %R% one_or_more(SPC) %R%
                           capture(one_or_more(NOT_SPC)) %R% zero_or_more(SPC) %R% END
      ) ->.;
      .[, -1] -> df
      stringr::str_replace_all(df[-1], ",", "") ->.;
      as.numeric(.) ->.;
      t(.) ->.;
      as.data.frame(.) ->.;
      {colnames(.) <- c("Open", "High", "Low", "Close", "Volumn"); .} ->.;
      {rownames(.) <- num; .} ->.;
      {.$date <-  df[1] %>% paste0("20", .) %>%
        stringr::str_replace_all(stringr::fixed("."), "-") %>%
        zoo::as.Date(.); .}

    }) ->.;


    ###5### Result
    if (nrow(.) == 1) {
      xts::xts(.[, -6] %>% apply(2, as.numeric) %>% t %>% as.data.frame(stringsAsFactors = TRUE), order.by = .$date)
    } else {
      xts::xts(.[, -6] %>% apply(2, as.numeric), order.by = .$date)
    }

  } ###4### Last line of scrape()


  ###4### Loop
  i <- 1
  scrape <- scrape_page(i)
  scrape ->.; zoo::index(.) ->.; xts::first(.) -> scrape_first

  while (scrape_first > start_date) {
    i <- i + 1
    scrape %<>% rbind(., scrape_page(i))
    scrape ->.; zoo::index(.) ->.; xts::first(.) -> scrape_first
  }


  ###4### Final data
  scrape[(zoo::index(scrape) >= start_date) & (zoo::index(scrape) <= end_date), ]


} ###3### Last line of kosd_stock2()
