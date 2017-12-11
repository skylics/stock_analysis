library(dplyr)
library(parallel)
library(tictoc)
library(profvis)
library(htmlwidgets)


##############################
############### 변수 설정

## 코어수 감지
(core_num <- parallel::detectCores(all.tests = FALSE, logical = TRUE))


## 데이터 load
#setwd("P:/LICS/Rwd/Stock_analysis/data")
setwd("C:/Users/user/Desktop/R project/Github/Stock_analysis/data")
load("all_stocks.rda")
all_stocks %>% str


## INPUT data - X 생성
all_stocks$code %>% as.data.frame(stringsAsFactors = FALSE) ->.;
#.[c(500:600), ] -> datax
.[c(1:nrow(.)), ] -> data
#code_part -> dataxx


## INDEX data - z 생성
zlevels <- 1:core_num
dataz <- if (sum(1:(length(datax) %% core_num)) == 1) {
  rep(1:core_num, length(datax) %/% core_num)
  } else {
    c(rep(1:core_num, length(datax) %/% core_num), 1:(length(datax) %% core_num))
  }


##############################
############### 함수 설정


###1### WRAPPER 함수 생성
wrapper <- function(datax, dataz) {

  force(datax)
  force(dataz)


  ###2### 내용 함수 생성
  price <- function(zlevel, datax, dataz) {

    require(magrittr, quietly = TRUE)
    require(wrapr, quietly = TRUE)
    require(rebus, quietly = TRUE)


    ###3### Define variables
    #code_part <- datax[dataz == 3]
    code_part <- datax[dataz == zlevel]


    ###3### Define listing_date functions
    listing_date <- function(ticker) {

      paste("http://wisefn.stock.daum.net/company/c1020001.aspx?cmp_cd=", ticker, "&frq=&rpt=", sep = "") ->.;
      xml2::read_html(., encoding = "UTF-8") ->.;
      rvest::html_nodes(., "td") ->.;
      rvest::html_text(.) ->.;
      stringr::str_detect(., "상장일") ->..;
      .[..] -> .;
      stringr::str_match(., "상장일: " %R% capture(one_or_more(NOT_SPC)) %R% "\\)" %R% END) ->.;
      .[2] ->.;
      stringr::str_replace_all(., stringr::fixed("/"), "-") ->.;
      zoo::as.Date(.)

    } ###3### Last line of listing_date()


    ###3### Define kosd_stock2 functions
    kosd_stock2 <- function (ticker, start_date, end_date) {


      ###4### Make scrape function
      scrape_page <- function(i) {


        ###5### Scrape
        paste0("http://finance.daum.net/item/quote_yyyymmdd_sub.daum?page=1&code=",
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
          #.
          num <- 4
          place_date[num] ->.;
          text_code[.] ->.;
          stringr::str_split(., SPC)
          stringr::str_extract_all(., NOT_SPC) ->.;
          .[[1]] -> .
          paste(., collapse =)

          stringr::str_c(.)
          stringr::str_replace_all(., " ", "") ->.;

          stringr::str_match(., START %R%
                               capture(one_or_more(NOT_SPC)) %R% "\n\t\t\t\t" %R%
                               capture(one_or_more(DGT) %R% one_or_more(NOT_SPC)) %R% "\n\t\t\t\t" %R%
                               capture(one_or_more(DGT) %R% one_or_more(NOT_SPC)) %R% "\n\t\t\t\t" %R%
                               capture(one_or_more(DGT) %R% one_or_more(NOT_SPC)) %R% "\n\t\t\t\t" %R%
                               capture(one_or_more(DGT) %R% one_or_more(NOT_SPC)) %R% "\n\t\t\t\t" %R%
                               one_or_more(NOT_SPC) %R% "\n\t\t\t\t" %R%
                               one_or_more(NOT_SPC) %R% "%" %R% "\n\t\t\t\t" %R%
                               capture(one_or_more(DGT) %R% zero_or_more(NOT_SPC)) %R% "\n") ->.;

          .[-1] -> df
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


    ###3### Do Map iteration
    from <- "2017-12-07"
    today <- Sys.Date()

    code_part ->.;

    purrr::map(., function(ticker) {

      ticker ->.; listing_date(.) -> start_date
      ifelse(from > start_date, from, start_date) ->.;
      zoo::as.Date(.) -> price_from

      ticker ->.;
      kosd_safe <- purrr::safely(kosd_stock2)
      kosd_safe(., price_from, today)
      #kosd_stock2(., price_from, today)
    }) ###3### Last line of code_part

    #result %>% purrr::map(function(df) {df$error})
    #models[[1]] %>% purrr::map(function(df) {df$error})
    #code_part[25] -> ticker

  } ####2#### Last line of price()


  ####2####
  worker <- function(zlevel) {price(zlevel, datax, dataz)}

  return(worker)

} ###1### Last line of price


##############################
############### 시간 체크 - tictoc 패키지 활용

tic("전체시간")
parallelcluster <- parallel::makeCluster(parallel::detectCores())

tic("10개 크롤링")
models <- parallel::parLapply(parallelcluster, zlevels, wrapper(datax, dataz))
toc()

parallel::stopCluster(parallelcluster)
toc()


1 %>% purrr::map(function(num) {models[[num]] %>%
    purrr::map(function(df) {ifelse(is.null(df$error), 0, 1)}) %>% unlist}) %>% unlist %>%
  as.logical %>% which -> errors

i <- 1
ticker <- datax[dataz == 1][errors[1]]



##############################
############### 프로파일링 - profvis 패키지 활용

model_profvis <- profvis({

  parallelcluster <- parallel::makeCluster(parallel::detectCores())

  models <- parallel::parLapply(parallelcluster, zlevels, wrapper(datax, dataz))

  parallel::stopCluster(parallelcluster)

  })

saveWidget(widget = model_profvis, file = "model_profvis.html")
getwd()



