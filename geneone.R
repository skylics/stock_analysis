ticker <- "KOSDAQ:011000"     # 진원생명과학
start_date <- "2013-01-02"
end_date <- "2017-09-22"

geneone <- g.hist2(ticker, start_date, end_date) %>% (function(df) {
  xts(df[, 5], order.by = df[, 1] %>% ymd)
})

inovio <- g.hist2("NASDAQ:INO", start_date, end_date) %>% (function(df) {
  xts(df[, 5], order.by = df[, 1] %>% ymd)
})

data_merge <- merge(geneone, inovio)
cleaned <- data_merge %>% complete.cases %>% data_merge[., ]

cor(cleaned)
reg <- lm(geneone ~ ., data = cleaned)
reg %>% summary

x <- data_merge[, 1]
y <- data_merge[, 2]
  
x <- SMA(cleaned[, 1], 20)
y <- SMA(cleaned[, 2], 20)

plot(as.zoo(x), las = 1, ylab = "")
par(new = TRUE)               
plot(as.zoo(y), col = 2, bty = 'n', xaxt = "n", yaxt = "n", xlab = "", ylab = "")
axis(4, las = 1)
legend("topright", legend = c("gene", "ino"), col = 1:2, lty = 1, cex = 0.85) 


