devtools::install_github("skylics/stock_analysis")
library(Quantrade)
library(quantmod)
library(purrr)


## Choose whick stock to be analyzed
stock_name <- "쎄노텍"
D_or_P <- "D"
from <- "2017-01-01"
today <- Sys.Date()


## Find ticker code
(code <- find_code2(stock_name))


## Load daily movement
price <- if (D_or_P == "P") {
  code %>% kosp_stock(from)
  } else {
    code %>% kosd_stock(from, today)
  }


## Draw chart
chart1_from <- from
chartSeries(price[paste(chart1_from, "::", sep = "")], 
            up.col = "red", dn.col = "blue", 
            theme = "white", name = stock_name)
addBBands()


## Foreign volumn percent
chart2_from <- chart1_from
traded <- code %>% trade_trend(chart2_from)
traded %>% 
  (function(df) {xts(df$fore_perc, order.by = df$date)}) %>% 
  plot(main = "외인비중")


## Entire Financial statements
#entire_sejong <- c("sales", "bp", "np", "asset", "liability", "capital") %>% map(function(char) {sejong_data(which_item = char)})


## Choose specific stock statement
code_sejong <- entire_sejong %>% map(function(df) {df[which(df[, 1] == code), ]}) %>% bind_rows() %>%
  (function(df) {
    rownames(df) <- c("매출", "영업이익", "순이익", "자산", "부채", "자본")
    df$단위 <- "억원"
    df %>% as.data.frame
  })

code_sejong %>% View
   








