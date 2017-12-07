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
setwd("P:/LICS/Rwd/Stock_analysis/data")
setwd("C:/Users/user/Desktop/R project/Github/Stock_analysis/data")
load("all_stocks.rda")


## INPUT data - X 생성
all_stocks$code %>% as.data.frame(stringsAsFactors = FALSE) ->.;
#.[c(1:nrow(.)), ] -> datax
.[c(1:10), ] -> datax


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
  activation <- function(zlevel, datax, dataz) {


    ###3### Attach packages
    library(rebus, quietly = TRUE)
    library(rvest, quietly = TRUE)


    ###3### Define variables
    code_part <- datax[dataz == zlevel]


    ###3### Function body
    real_time <- function(ticker) {

      ticker ->.;
      paste("http://finance.daum.net/item/quote.daum?code=", ., sep = "") ->.;
      read_html(., encoding = "UTF-8") ->.;
      html_nodes(., "body") ->.;
      html_nodes(., "div") ->.;
      html_nodes(., ".leftDiv") ->.;
      html_nodes(., "td") ->.;
      html_text(.) %>%
        (function(df) {c("시가", "고가", "저가", "현재가") %>%
            purrr::map(function(char) {
              char ->.; df[which(df == .) + 1] ->.; stringr::str_replace_all(., ",", "") ->.; as.numeric(.)
            }) ->.;
          unlist(.) ->.; scale(.) ->.;
          c(., df[which(df == "거래량") + 1] %>% stringr::str_replace_all(",", "") %>% as.numeric)
        }) %>% as.data.frame %>% t %>% {colnames(.) <- c("Open", "High", "Low", "Close", "deal_done"); .} %>%
        {rownames(.) <- ticker; .}
    }


    ###3### Activate the function with loop using map()
    code_part ->.;
    purrr::map(., function(ticker) {

      real_time(ticker)

    }) ###3### Last line of code_part

  } ###2### Last line of Activation function()


  ####2####
  worker <- function(zlevel) {activation(zlevel, datax, dataz)}

  return(worker)

} ###1### Last line of price


##############################
############### 코어 활용 Scrape 시행 + 시간 체크

tic("전체시간")
parallelcluster <- parallel::makeCluster(parallel::detectCores())

tic("10개 크롤링")
models <- parallel::parLapply(parallelcluster, zlevels, wrapper(datax, dataz))
toc()

parallel::stopCluster(parallelcluster)
toc()


##############################
############### 저장하기

df <- models %>% rlist::list.flatten() %>% do.call(rbind, .) %>% as.data.frame %>%
  tibble::rownames_to_column("code") %>% (function(df) {df$num <- c(1:nrow(df)); df})

k <- 8
cl <- stats::kmeans(df[, c(1:4)], centers = k, iter.max = 100, nstart = k)

par(mfrow = c(2, k/2))
myCol <- c("blue", "red", "black", "brown", "green", "purple", "pink", "green")

for(i in 1:k) {
  title <- sprintf("Pattern - %d", i)

  # 각 클러스터의 중점 패턴을 그린다 (대표 패턴)
  plot(cl$centers[i, ], type = 'o', main = title, col = myCol[i], pch = 19)
}

library(ggplot2)

ggplot(df, aes(x = code))+
  geom_linerange(aes(ymin = Low, ymax = High)) +
  theme_bw() +
  labs(title = "df") +
  geom_rect(aes(xmin = num - 0.2, xmax = num + 0.2, ymin = pmin(Open, Close), ymax = pmax(Open, Close)))



##############################
############### 저장하기

###1### Export models data

save(models, file = "price_scrape.rda")

# tic("저장시간")
#
# setwd("P:/LICS/Rwd/Stock_analysis/stock_price")
# Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
#
# for (j in 1:core_num) {
#
#   core_codes <- datax[dataz == j]
#   core_nms <- all_stocks$name[dataz == j]
#
#   for (i in 1:length(core_nms)) {
#     rm(wb)
#     wb <- openxlsx::createWorkbook()
#     addWorksheet(wb, core_codes[i])
#     writeData(wb,
#               sheet = core_codes[i],
#               models[[j]][[i]] %>% as.data.frame(stringsAsFactors = FALSE) %>% rownames_to_column("date"),
#               startCol = 1, startRow = 1)
#     openxlsx::saveWorkbook(wb, paste(core_nms[i], ".xlsx", sep = ""), overwrite = TRUE)
#   }
# }
#
# toc()


##############################
############### 엑셀 파일 불러오기

###1### Import models data.xlsx

tic("저장시간")

setwd("P:/LICS/Rwd/Stock_analysis/stock_price")
dir_nms <- dir()


## Create empty models list
daily_candle_multi <- vector("list", length(dir_nms))


## Import excel files
for (i in 1:length(dir_nms)) {
  readxl::read_excel(dir_nms[i], col_types = rep("text", 6)) ->.;
  xts::xts(.[, -1] %>% apply(2, as.numeric), order.by = zoo::as.Date(.$date)) %>%
    (function(df) {list(code = which(dir_nms[i] %>% stringr::str_replace_all(".xlsx", "") == all_stocks$name) %>% all_stocks$code[.],
                        price = df)
    }) -> models[[i]]
}

names(models) <- dir_nms %>% stringr::str_replace_all(".xlsx", "")
save(models, file = "price_scrape.rda")

toc()


##############################
############### 프로파일링 - profvis 패키지 활용

# model_profvis <- profvis({
#
#   parallelcluster <- parallel::makeCluster(parallel::detectCores())
#
#   models <- parallel::parLapply(parallelcluster, zlevels, wrapper(datax, dataz))
#
#   parallel::stopCluster(parallelcluster)
#
#   })
#
# saveWidget(widget = model_profvis, file = "model_profvis.html")
