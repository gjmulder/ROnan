library(tidyverse)
library(googlesheets)
library(jsonlite)

######################################################################################################
# Load annotations from an .rdata file 
# Assign whatever source data frame we want to examine to ts_df

# # System data
# load('~/Work/DS/dataset1.Rdata')
# ts_df <-
#   system_data

# Cleaned system data
ts_df <-
  clean_ts_df

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

###################################################################################################
# Generate some reoccuring labels
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

###################################################################################################
# Export for Anton
#
# tses <- c("date.time", "desktop.reqs", "mobile.reqs", "desktop.orders", "mobile.orders")
# system_data_ts <- system_data[, tses]
# system_data_clean_ts <- clean_ts_df[, tses]