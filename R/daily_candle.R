# library(dplyr)
# library(parallel)
# library(tictoc)
# library(profvis)
# library(htmlwidgets)
#
#
# ##############################
# ############### ???? ??��
#
# ## ?ھ??? ????
# (core_num <- parallel::detectCores(all.tests = FALSE, logical = TRUE))
#
#
# ## ?????? load
# setwd("P:/LICS/Rwd/Stock_analysis/data")
# load("all_stocks.rda")
#
#
# ## INPUT data - X ????
# all_stocks$code %>% as.data.frame(stringsAsFactors = FALSE) ->.;
# #.[c(1:nrow(.)), ] -> datax
# .[c(1:10), ] -> datax
#
#
# ## INDEX data - z ????
# zlevels <- 1:core_num
# dataz <- if (sum(1:(length(datax) %% core_num)) == 1) {
#   rep(1:core_num, length(datax) %/% core_num)
# } else {
#   c(rep(1:core_num, length(datax) %/% core_num), 1:(length(datax) %% core_num))
# }
#
#
# ##############################
# ############### ?Լ? ??��
#
#
# ###1### WRAPPER ?Լ? ????
# wrapper <- function(datax, dataz) {
#
#
#   force(datax)
#   force(dataz)
#
#
#   ###2### ???? ?Լ? ????
#   activation <- function(zlevel, datax, dataz) {
#
#
#     ###3### Attach packages
#     library(rebus, quietly = TRUE)
#     library(rvest, quietly = TRUE)
#
#
#     ###3### Define variables
#     code_part <- datax[dataz == zlevel]
#
#
#     ###3### Function body
#     real_time <- function(ticker) {
#
#       ticker ->.;
#       paste("http://finance.daum.net/item/quote.daum?code=", ., sep = "") ->.;
#       read_html(., encoding = "UTF-8") ->.;
#       html_nodes(., "body") ->.;
#       html_nodes(., "div") ->.;
#       html_nodes(., ".leftDiv") ->.;
#       html_nodes(., "td") ->.;
#       html_text(.) %>%
#         (function(df) {
#           data.frame("Open" = df[which(df == "?ð?") + 1] %>% stringr::str_replace_all(",", "") %>% as.numeric,
#                      "High" = df[which(df == "?���") + 1] %>% stringr::str_replace_all(",", "") %>% as.numeric,
#                      "Low" = df[which(df == "????") + 1] %>% stringr::str_replace_all(",", "") %>% as.numeric,
#                      "Close" = df[which(df == "???簡") + 1] %>% stringr::str_replace_all(",", "") %>% as.numeric,
#                      "Volumn" = df[which(df == "?ŷ???") + 1] %>% stringr::str_replace_all(",", "") %>% as.numeric
#                      ) %>% xts::xts(order.by = Sys.Date())
#         }) %>%
#         (function(df) {
#           df %>% apply(1, scale)
#
#         })
#     }
#
#
#     ###3### Activate the function with loop using map()
#     code_part ->.;
#     purrr::map(., function(ticker) {
#
#       real_time(ticker)
#
#     }) ###3### Last line of code_part
#
#   } ###2### Last line of Activation function()
#
#
#   ####2####
#   worker <- function(zlevel) {activation(zlevel, datax, dataz)}
#
#   return(worker)
#
# } ###1### Last line of price
#
#
# ##############################
# ############### ?ھ? Ȱ?? Scrape ???? + ?ð? üũ
#
# tic("??ü?ð?")
# parallelcluster <- parallel::makeCluster(parallel::detectCores())
#
# tic("10?? ũ?Ѹ?")
# models <- parallel::parLapply(parallelcluster, zlevels, wrapper(datax, dataz))
# toc()
#
# parallel::stopCluster(parallelcluster)
# toc()
#
#
# ##############################
# ############### ?????ϱ?
#
# ###1### Export models data
#
# save(models, file = "price_scrape.rda")
#
# # tic("?????ð?")
# #
# # setwd("P:/LICS/Rwd/Stock_analysis/stock_price")
# # Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")
# #
# # for (j in 1:core_num) {
# #
# #   core_codes <- datax[dataz == j]
# #   core_nms <- all_stocks$name[dataz == j]
# #
# #   for (i in 1:length(core_nms)) {
# #     rm(wb)
# #     wb <- openxlsx::createWorkbook()
# #     addWorksheet(wb, core_codes[i])
# #     writeData(wb,
# #               sheet = core_codes[i],
# #               models[[j]][[i]] %>% as.data.frame(stringsAsFactors = FALSE) %>% rownames_to_column("date"),
# #               startCol = 1, startRow = 1)
# #     openxlsx::saveWorkbook(wb, paste(core_nms[i], ".xlsx", sep = ""), overwrite = TRUE)
# #   }
# # }
# #
# # toc()
#
#
# ##############################
# ############### ???? ???? ?ҷ??��?
#
# ###1### Import models data.xlsx
#
# tic("?????ð?")
#
# setwd("P:/LICS/Rwd/Stock_analysis/stock_price")
# dir_nms <- dir()
#
#
# ## Create empty models list
# daily_candle_multi <- vector("list", length(dir_nms))
#
#
# ## Import excel files
# for (i in 1:length(dir_nms)) {
#   readxl::read_excel(dir_nms[i], col_types = rep("text", 6)) ->.;
#   xts::xts(.[, -1] %>% apply(2, as.numeric), order.by = zoo::as.Date(.$date)) %>%
#     (function(df) {list(code = which(dir_nms[i] %>% stringr::str_replace_all(".xlsx", "") == all_stocks$name) %>% all_stocks$code[.],
#                         price = df)
#     }) -> models[[i]]
# }
#
# names(models) <- dir_nms %>% stringr::str_replace_all(".xlsx", "")
# save(models, file = "price_scrape.rda")
#
# toc()
#
#
# ##############################
# ############### ?��????ϸ? - profvis ??Ű?? Ȱ??
#
# # model_profvis <- profvis({
# #
# #   parallelcluster <- parallel::makeCluster(parallel::detectCores())
# #
# #   models <- parallel::parLapply(parallelcluster, zlevels, wrapper(datax, dataz))
# #
# #   parallel::stopCluster(parallelcluster)
# #
# #   })
# #
# # saveWidget(widget = model_profvis, file = "model_profvis.html")
