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

bst_start <-
  as.POSIXct(strptime("2016-03-27 03:00:00", "%Y-%m-%d %H:%M:%S"))
ts <-
  ts_df %>%
  filter(date.time > bst_start) %>%
  mutate(value = desktop.orders) %>%
  select(date.time, value)

ts_an <-
  gs_read(gsheet_ts_annotations,
          ws = "desktop.orders") %>%
  filter(annotation == "s_an" | annotation == "e_an",
         date.time > bst_start)
  
setwd("~/Work/NAB/")

######################################################################################################
# Get start and end of annotated anomalies
ts_an_start <-
  ts_an %>%
  filter(annotation == "s_an" | annotation == "s_dt" | annotation == "s_ld") %>%
  select(date.time)
ts_an_end <-
  ts_an %>%
  filter(annotation == "e_an" | annotation == "e_dt" | annotation == "e_ld") %>%
  select(date.time)

# Compute subsequences
ts_subseq <-
  list()
ts_an_subseq <-
  list()
# TODO: This should be functionalised
for (s_an_element in ts_an_start$date.time) {
  print("")
  
  previous_e_an_df <-
    ts_an_end %>%
    filter(date.time < s_an_element) %>%
    tail(1)
  
  previous_e_an <-
    previous_e_an_df$date.time
  start_e_an <-
    as.POSIXct(s_an_element, origin = "1970-01-01")
  print(paste('End last  :', previous_e_an))
  print(paste('Start new :', start_e_an))
  
  time_diff_15pct <-
    start_e_an - previous_e_an
  print(paste0("15%: ", time_diff_15pct))
  
  time_diff_85pct <-
    time_diff_15pct * 85 / 15
  print(paste0("85%: ", time_diff_85pct))
  
  time_start <-
    previous_e_an
  time_end <-
    start_e_an + time_diff_85pct
  
  # Use all of the time series, or if we need more than the time series, skip
  if (time_end <= max(ts$date.time)) {
    time_end <-
      max(ts$date.time)
  } else
    break
  
  ts_subseq_name <-
    paste0(as.integer(time_start), "_", as.integer(time_end))

  print(paste0("Start time  : ", time_start))
  print(paste0("First anom  : ", start_e_an))
  print(time_end - time_start)
  print(paste0("End time    : ", time_end))
  print(paste0("File name   : ", ts_subseq_name))

  ts_subseq[[ts_subseq_name]] <-
    ts %>%
    filter(date.time >= time_start & date.time <= time_end)
  print(paste0("Number rows: ", nrow(ts_subseq[[ts_subseq_name]])))
  
  ts_an_subseq[[ts_subseq_name]] <-
    ts_an %>%
    filter(annotation == "s_an",
           date.time >= time_start & date.time < time_end)
}

######################################################################################################
# Output data as .csv and annotations as NAB json labels
save_to_nab_csv <-
  function(ts_name, ts_subseq) {
    # Format timestamps, set all NAs = 0.0
    sub_seq <-
      ts_subseq[[ts_name]]
    csv_data <-
      sub_seq %>%
      mutate(
        timestamp = strftime(date.time, format = "%F %T"),
        value = ifelse(is.na(value), 0, value)
      ) %>%
      select(timestamp, value)
    
    # Write .csv
    nab_data_fname <-
      paste0("dataSet/", ts_name, ".csv")
    write_csv(csv_data, path = paste0("./data/", nab_data_fname))
  }

save_to_nab_json <-
  function(ts_an_subseq) {
    ts_an <-
      lapply(ts_an_subseq, function(x) {x$date.time})
    names(ts_an) <-
      lapply(names(ts_an_subseq), function(x) paste0("dataSet/", x, ".csv"))

    # Write JSON anomalies
    nab_label_fname <-
      "./labels/raw/GM_dataSet_labels_v0.1.json"
    fc <-
      file(nab_label_fname)
    writeLines(toJSON(ts_an), fc)
    close(fc)
  }

# Just write the first .csv and anom subseq
lapply(names(ts_subseq), save_to_nab_csv, ts_subseq)
save_to_nab_json(ts_an_subseq)
