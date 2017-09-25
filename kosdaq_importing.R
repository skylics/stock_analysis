######################################################################
################ 코스닥 가격 불러오기
######################################################################

##############################
############### Settings

##### Attaching Packages

library(astsa)
library(tidyverse)
library(lubridate)
library(xts)
library(forecast)
library(readxl)
library(stringr)
library(plyr)
library(dplyr)
library(tidyr)
library(tseries)
library(quantmod)
library(XML)


##############################
############### 종목/날자 예시

##### 종목
ticker <- "KOSDAQ:011000"     # 진원생명과학


##### 날자
start_date <- "2014-01-01"
end_date <- "2017-09-22"


##############################
############### 함수 생성

##### 종목 불러오기
g.hist2 <- function(ticker, start_date, end_date){
  
  ##### URL generate function
  url_gen <- function(t, s, e) {
    
    ## 종목명 수정
    ticker_m <- gsub(":", "%3A", t)
    
    ## locale 지정
    lct <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")
    
    ## 날자 지정
    s <- as.Date(s)
    e <- as.Date(e)
    
    ## URL에 맞게 날자 형식 바꾸기
    fmtstr <- "%b+%d%%2C+%Y" #ex) Jan+01,+2017
    start_date_fmt <- as.character(format(s, fmtstr))
    end_date_fmt <- as.character(format(e, fmtstr))
    
    ## URL 완성
    url <- 'http://finance.google.com/finance/historical?q='
    paste(url, ticker_m, '&startdate=', start_date_fmt, '&enddate=', end_date_fmt, '&num=200', sep = '')
  }
  
  
  ##### Data importing function
  output_gen <- function(t2, s2, e2) {
    
    ## 데이터 불러오기
    url <- url_gen(t2, s2, e2)
    output <- readHTMLTable(url, stringsAsFactors = FALSE)[4] %>% as.data.frame 
    
    ## 날자 형식, 변수명 지정
    output[, 1] <- as.Date(output[, 1], format = "%b %d, %Y")
    names(output) <- c("Date", "Open", "High", "Low", "Close", "Volume")
    
    ## 숫자형으로 형식 바꾸기
    for (k in 2:6) {
      output[, k] <- as.numeric(gsub(",", "", output[, k]))
    }
    
    output
  }
    
  
  ##### output 생성
  output <- output_gen(t = ticker, s = start_date, e = end_date)
  start <- as.Date(output[output %>% nrow, 1])
  
  
  ##### LOOP 돌리기
  while (start_date < start) {
    
    new_end <- start - 1
    
    if (new_end == start_date) {
      output <- rbind(output, output_gen(t = ticker, s = new_end - 10, e = new_end))
      output <- (output[, 1] >= new_end) %>% output[., ]
      break()
      } else {
        output <- rbind(output, output_gen(t = ticker, s = start_date, e = new_end))
      }
    
    start <- as.Date(output[output %>% nrow, 1])
    
  }
  output
}