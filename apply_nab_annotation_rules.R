library(tidyverse)
library(googlesheets)
library(jsonlite)

######################################################################################################
# Compute subsequences that meet criteria specified in
# https://drive.google.com/file/d/0B1_XUjaAXeV3YlgwRXdsb3Voa1k/vie

# 1. For each start of anomaly
# 2.   Find start or previous end of anomaly
# 3.   This time range is 15% of data
# 4.   Take 85% of data after start of anomaly
# 5.   15% + 85% = data set

######################################################################################################
# Get data and corresponding annotations for desktop.orders
ts <-
  ts_df %>%
  transmute(timestamp = date.time, value = desktop.orders)
ts_an <-
  gs_read(gsheet_ts_annotations,
          ws = "desktop.orders") %>%
  filter(annotation == "s_an" | annotation == "e_an")

setwd("~/Work/NAB/")

######################################################################################################
# Get start and end of annotated anomalies
ts_an_start <-
  ts_an %>%
  filter(annotation == "s_an") %>%
  select(date.time)
ts_an_end <-
  ts_an %>%
  filter(annotation == "e_an") %>%
  select(date.time)

# Compute subsequences
ts_subseq <-
  list()
ts_an_subseq <-
  list()
for (s_an_element in ts_an_start$date.time) {
  previous_e_an_df <-
    ts_an_end %>%
    filter(date.time < s_an_element) %>%
    tail(1)
  
  previous_e_an <-
    previous_e_an_df$date.time
  start_e_an <-
    as.POSIXct(s_an_element, origin = "1970-01-01")
  # print(paste('End last  :', previous_e_an))
  # print(paste('Start new :', start_e_an))
  
  time_diff_15pct <-
    start_e_an - previous_e_an
  # print(paste0("15%: ", time_diff_15pct))
  
  time_diff_85pct <-
    time_diff_15pct * 100 / 15
  # print(paste0("85%: ", time_diff_85pct))
  
  time_start <-
    previous_e_an
  time_end <-
    start_e_an + time_diff_85pct
  print(paste0("Start time  : ", time_start))
  print(paste0("Time length : ", time_end - time_start))
  print(paste0("End time    : ", time_end))
  
  ts_subseq_name <-
    paste0(as.integer(time_start), "_", as.integer(time_end))
  ts_subseq[[ts_subseq_name]] <-
    ts %>%
    filter(timestamp > time_start & timestamp < time_end)
  ts_an_subseq[[ts_subseq_name]] <-
    ts_an_start %>%
    filter(date.time > time_start & date.time < time_end)
}

######################################################################################################
# Output data as .csv and annotations as NAB json labels
to_nab_csv_and_anom_jason <-
  function(ts, ts_an) {
    fc <-
      file(nab_fname)
    writeLines(toJSON(anoms_list), fc)
    close(fc)
  }