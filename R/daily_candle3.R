library(dplyr)
library(parallel)
library(tictoc)
library(ggplot2)
library(NbClust)
library(factoextra)
install.packages("factoextra")
install.packages("NbClust")


##############################
############### 변수 설정

## 코어수 감지
(core_num <- parallel::detectCores(all.tests = FALSE, logical = TRUE))


## 데이터 load
#setwd("P:/LICS/Rwd/Stock_analysis/data")
setwd("C:/Users/user/Desktop/R project/Github/Stock_analysis/data")
load("all_stocks.rda")


## INPUT data - X 생성
all_stocks$code %>% as.data.frame(stringsAsFactors = FALSE) ->.;
.[c(1:nrow(.)), ] -> datax
#.[c(501:550), ] -> datax


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
    
    
    ###3### Function body - real_time()
    real_time <- function(ticker) {
      
      ticker ->.;
      paste("http://finance.daum.net/item/quote.daum?code=", ., sep = "") ->.;
      read_html(., encoding = "UTF-8") ->.;
      html_nodes(., "body") ->.;
      html_nodes(., "div") ->.;
      html_nodes(., ".leftDiv") ->.;
      html_nodes(., "td") ->.;
      html_text(.) %>%
        (function(df) {c("시가", "고가", "저가", "현재가", "거래량") %>%
            purrr::map(function(char) {
              char ->.; df[which(df == .) + 1] ->.; stringr::str_replace_all(., ",", "") ->.; as.numeric(.)
            })
        }) ->.;
      unlist(.) ->.;
      c(ticker, .)
      
    } ###3### Last line of real_time()
    
    
    ###3### Function body - scale_cluster
    make_scale <- function(df) {
      
      df[2:5] -> .
      as.numeric(.) ->.;
      log(.) ->.;
      
      if (sum(is.infinite(.)) > 0) {
        c(0, 0, 0, 0) ->.;
        c(df[1], ., df[6]) ->.;
        
      } else if (sum(is.na(.)) > 0) {
        c(0, 0, 0, 0) ->.;
        c(paste(df[1], "_X", sep = ""), ., df[6]) ->.;
        
      } else if (sum(is.infinite(.)) == 0) {
        (. - .[1]) ->.;
        c(df[1], ., df[6]) ->.;
      }
      
      as.data.frame(., stringsAsFactors = FALSE) ->.;
      t(.) ->.;
      {colnames(.) <- c("code", "Open", "High", "Low", "Close", "done_Q"); .} ->.;
      .
      
    } ###3### Last line of scale_cluster()
    
    
    ###3### Function body - scale_kmeans
    scale_kmeans <- function(df, k, n) {
      
      unlist(df, recursive = TRUE) ->.;
      matrix(., ncol = 6, byrow = TRUE) -> df2
      
      df2[, c(2:5)] ->.;
      as.data.frame(., stringsAsFactors = FALSE) ->.;
      apply(., 2, as.numeric) ->.;
      {data.frame(Close = .[, 4],
                  lower = ifelse(.[, 1] > .[, 4], .[, 4], .[, 1]) - .[, 3],
                  upper = .[, 2] - ifelse(.[, 1] > .[, 4], .[, 1], .[, 4]),
                  stringsAsFactors = FALSE)} ->.;
                  #updown = ifelse(.[, 1] > .[, 4], 1, 2))} ->.;
      
      stats::kmeans(., centers = k, iter.max = n, nstart = k) ->.;
      
      cbind(df2[, c(2:5)], .$cluster, df2[, 6]) ->.;
      {colnames(.) <- c("Open", "High", "Low", "Close", "cluster", "done_Q"); .} ->.;
      apply(., 2, as.numeric) %>% as.data.frame(stringsAsFactors = FALSE) ->.;
      {.$cluster <- factor(.$cluster); .} ->.;
      {rownames(.) <- df2[, 1]; .} ->.;
      .
      
    } ###3### Last line of scale_cluster()
    
    
    ###3### Function body - share_inform
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
      
      if (length(.) != 0) {
        
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
      
      } else if (length(.) == 0) {
        
        total_share <- 0
        
      }
      
      
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
    
    
    ###3### Activate the function with loop using map()
    num_kmeans <- 6
    iter_kmeans <- 1000000
    
    code_part %>% purrr::map(function(ticker) {
      
      real_time(ticker) ->.;
      make_scale(.)
      
    }) %>% scale_kmeans(num_kmeans, iter_kmeans) -> part_1 
    
    code_part %>% purrr::map(function(ticker) {
      
      share_inform(ticker)
      
    }) %>% do.call(rbind, .) -> part_2 
    
    
    cbind(part_1, part_2)
    
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

# # 캔들 클러스트링할 때, 
# 1. 시가 - 종가 차이 = 종가 (시가 = 0)
# 2. 아래꼬리 길이 min(시가, 종가) - Low
# 3. 윗꼬리 길이 High - max(시가, 종가) 
# 4. 음, 양봉은 수기로 구분
# 5. 클러스트링 numbering fix


##############################
############### cluster 갯수

fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")


##############################
############### 결과 분석
 
##### Unlist
model_unlist <- models %>% purrr::map_df(function(df) {
  df %>% tibble::rownames_to_column("code")
}) 


##### blox plot
model_long <- model_unlist[, c(2:5)] %>% {.$num <- 1:nrow(.); .} %>%
  tidyr::gather(-num, key = "OHLC", value = "perc") %>% 
  {.$OHLC <- factor(.$OHLC, levels = c("Open", "High", "Low", "Close")); .}


##### See quantile
ggplot(model_long, aes(x = OHLC, y = perc)) + geom_boxplot()


##### 아래 꼬리 긴 Case
model_unlist[, c(2:5)] %>% apply(1, function(row) {
  min(row[c(1, 4)]) - row[3]
}) %>% 
  {model_unlist$tail <- .; model_unlist} -> model_unlist


##### 아래 꼬리 길기
## 전체
model_unlist$tail %>% quantile
ggplot(model_unlist, aes(tail)) + geom_histogram(binwidth = 0.005)


## 음봉
data <- dplyr::filter(model_unlist, Open > Close)
data %>% nrow
ggplot(data, aes(tail)) + geom_histogram(binwidth = 0.005)


## 양봉
data <- dplyr::filter(model_unlist, Close >= Open)
data %>% nrow
ggplot(data, aes(tail)) + geom_histogram(binwidth = 0.001) +
  scale_y_continuous(trans = )


##### Ploting
model_unlist ->.;
dplyr::select(., Open, High, Low, Close, cluster) ->.;
dplyr::mutate(., 
              body = Close, 
              lower = min(Open, Close) - Low, 
              upper = High - max(Open, Close), 
              updown = ifelse(Open > Close, "down", ifelse(Open == Close, "stay", "up"))
) -> data_plot


#####
cluster_i <- 3
data_plot_i <- data_plot %>% dplyr::filter(cluster == cluster_i) ->.;
{.$num <- 1:nrow(.); .} -> data_plot_i

ggplot(data_plot_i, aes(x = num)) +
  geom_linerange(aes(ymin = Low, ymax = High)) +
  theme_bw() +
  geom_rect(
    aes(xmin = num - 0.2, 
        xmax = num + 0.2, 
        ymin = pmin(Open, Close), 
        ymax = pmax(Open, Close))
    )



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
