library(tidyverse)
library(googlesheets)
library(jsonlite)

######################################################################################################
# Load annotations from an .rdata file
# Assign whatever source data frame we want to examine to ts_df

# System data
load('~/Work/DS/dataset1.Rdata')
ts_df <-
  system_data

# # Cleaned system data
# ts_df <-
#   clean_ts_df

print(summary(ts_df))

######################################################################################################
# Google Sheets preparation
# n <- 5
# filler <- matrix("-", nrow = n, ncol = n,
#                  dimnames = list(NULL, paste0("V", seq_len(n))))

## prepare the OAuth token and set up the target sheet:
##  - do this interactively
##  - do this EXACTLY ONCE

# shiny_token <- gs_auth() # authenticate w/ your desired Google identity here
# saveRDS(shiny_token, "shiny_app_google_sheet_token.rds")
# ss <- gs_new("ts_annotations",
#              row_extent = n, col_extent = n, input = filler)
# ss$sheet_key # 1Y8MoUBi1CtzLNv0b_VPQS3MTAeapRE9ep5_7cMTnJUE

## if you version control your app, don't forget to ignore the token file!
## e.g., put it into .gitignore

googlesheets::gs_auth(token = "~/Work/ronan/shiny_app_google_sheet_token.rds")
sheet_key <-
  "1Y8MoUBi1CtzLNv0b_VPQS3MTAeapRE9ep5_7cMTnJUE"
gsheet_ts_annotations <-
  googlesheets::gs_key(sheet_key)

print(gs_ws_ls(gsheet_ts_annotations))
#
# ###################################################################################################
# # Generate some reoccuring labels
#
# start_7am_an <-
#   data_frame(date.time = seq(as.POSIXct(strptime("01/04/2016 00:00:00", "%d/%m/%Y %H:%M:%S", tz = "Europe/London")),
#                              as.POSIXct(strptime("10/08/2016 00:00:00", "%d/%m/%Y %H:%M:%S", tz = "Europe/London")),
#                              by = "min"),
#              value = pi,
#              annotation = "s_dt") %>%
#   filter(strftime(date.time, format = "%H") == "07") %>%
#   filter(strftime(date.time, format = "%M") == "00")
#
# end_7am_an <-
#   data_frame(date.time = seq(as.POSIXct(strptime("01/04/2016 00:00:00", "%d/%m/%Y %H:%M:%S", tz = "Europe/London")),
#                              as.POSIXct(strptime("10/08/2016 00:00:00", "%d/%m/%Y %H:%M:%S", tz = "Europe/London")),
#                              by = "min"),
#              value = pi,
#              annotation = "e_dt") %>%
#   filter(strftime(date.time, format = "%H") == "07", tz = "Europe/London") %>%
#   filter(strftime(date.time, format = "%M") == "35", tz = "Europe/London")
#
# all_an <-
#   bind_rows(start_7am_an,
#             end_7am_an) %>%
#   arrange(date.time)
#
# ts_name <-
#   "mobile.reqs.new"
# gs_edit_cells(
#   gsheet_ts_annotations,
#   ws = ts_name,
#   input = all_an,
#   col_names = TRUE,
#   trim = TRUE,
#   verbose = TRUE
# )
#
# ###################################################################################################
# # Export for Anton
#
# tses <- c("date.time", "desktop.reqs", "mobile.reqs", "desktop.orders", "mobile.orders")
# system_data_ts <- system_data[, tses]
# system_data_clean_ts <- clean_ts_df[, tses]
#
# ###################################################################################################
# library(vars)
# library(urca)
# # library(parallel)
# # library(forecast)
# #
# # cluster <-
# #   makeCluster(detectCores() / 2)
#
# col_names <-
#   c(
#     # "date.time",
#     # "monitor1.uptime",
#     # "monitor2.uptime",
#     # "funds.in.success",
#     # "funds.in.fail",
#     # "login.success",
#     # "login.fail",
#     "desktop.orders",
#     "desktop.reqs",
#     "mobile.orders",
#     "mobile.reqs"
#   )
#
# # clean_ts <-
# #   parLapply(cluster, clean_ts_df[, col_names], ts, frequency = 60 * 24 * 7)
# #
# # system.time(
# #   result <-
# #     parLapply(cluster, clean_ts, nsdiffs)
# # )
# # stopCluster(cluster)
# # > result
# # $funds.in.success
# # [1] 0
# #
# # $funds.in.fail
# # [1] 0
# #
# # $login.success
# # [1] 0
# #
# # $login.fail
# # [1] 0
# #
# # $desktop.orders
# # [1] 0
# #
# # $desktop.reqs
# # [1] 0
# #
# # $mobile.orders
# # [1] 0
# #
# # $mobile.reqs
# # [1] 0
#
# # jo_test <-
# #   ca.jo(clean_ts_df[, col_names], type="trace", K=1, ecdet="none", spec="longrun")
# # summary(jo_test)
#
# # var_select <-
# #   VARselect(clean_ts_df[, col_names], lag.max = 60, type = c("both"))
#
# var_fits <-
#   function(lag) {
#   var_fit <-
#     VAR(clean_ts_df[, col_names], p = lag, type = "none")
#
#   # lm_summaries <-
#     lapply(var_fit$varresult, summary)
#
#   # lapply(lm_summaries, function(x) coefficients)
# }
#
# result <-
#   lapply(1, var_fits)
#
###################################################################################################
# library(forecast)
# 
# ts_seasonal <-
#   clean_ts_df %>%
#   mutate(
#     min.of.hour = as.integer(strftime(
#       date.time, format = "%M", tz = "Europe/London"
#     )),
#     hour.of.day = as.integer(strftime(
#       date.time, format = "%H", tz = "GMT"
#     )),
#     day.of.week = as.integer(strftime(
#       date.time, format = "%w", tz = "Europe/London"
#     ))
#   )
# 
# col_names <-
#   c(
#     # "date.time",
#     # "monitor1.uptime",
#     # "monitor2.uptime",
#     # "funds.in.success",
#     # "funds.in.fail",
#     # "login.success",
#     # "login.fail",
#     "desktop.orders",
#     # "mobile.orders"
#     "desktop.reqs",
#     "mobile.reqs",
#     "min.of.hour",
#     "hour.of.day",
#     "day.of.week"
#   )
# 
# ts_test <-
#   ts_seasonal %>%
#   filter(date.time > as.POSIXct(
#     strptime("2016-02-25 15:00:00", "%Y-%m-%d %H:%M:%S", tz = "Europe/London")
#   ))
# 
# ts_train <-
#   ts_seasonal %>%
#   filter(date.time < as.POSIXct(
#     strptime("2016-02-16 15:00:00", "%Y-%m-%d %H:%M:%S", tz = "Europe/London")
#   ))
# 
# set.seed(666)
# 
# run_nn_fit <-
#   function(ts_train, ts_test, col_names) {
#     print(system.time(
#       nn_fit <-
#         nnetar(
#           ts_train$mobile.orders,
#           repeats = 50,
#           size = 25,
#           xreg = ts_train[, col_names],
#           MaxNWts = 12000
#         )
#     ))
#     
#     print(system.time(acc_nn <-
#                         accuracy(
#                           forecast(nn_fit, xreg = ts_test[, col_names]), ts_test$mobile.orders
#                         )))
#     print(acc_nn)
#     acc_nn
#   }
# 
# acc_nn <-
#   run_nn_fit(ts_train, ts_test, col_names)
# # acc_nns <-
# #   lapply(1:10 * 5, run_nn_fit)