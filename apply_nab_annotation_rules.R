library(zoo)
library(googlesheets)
library(jsonlite)
library(plyr)
library(tidyverse)

######################################################################################################
# Compute subsequences that meet criteria specified in
# https://drive.google.com/file/d/0B1_XUjaAXeV3YlgwRXdsb3Voa1k/vie

# 1. For each start of anomaly
# 2.   Find start or previous end of anomaly
# 3.   This time range is 15% of data
# 4.   Take 85% of data after start of anomaly
# 5.   15% + 85% = data set

######################################################################################################
# Get data and corresponding annotations for mobile.orders

# Just look at daylight savings data as there's a problem with the timestamps being in BST
bst_start <-
  as.POSIXct(strptime("2016-03-27 10:00:00", "%Y-%m-%d %H:%M:%S"))

# Get a time series and set all NAs to 0
ts <-
  ts_df %>%
  mutate(value = desktop.orders) %>%
  filter(date.time > bst_start) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(date.time, value)

# Get corresponding annotated labels, and pair the start and end times of each label
ts_an <-
  gs_read(gsheet_ts_annotations,
          ws = "desktop.orders") %>%
  filter(date.time > bst_start) %>%
  select(-value) %>%
  mutate(pairs = sort(rep(1:(nrow(.) / 2), 2))) %>%
  spread(annotation, date.time)

######################################################################################################
# Linearly interpolate human generated events (down time and load) and morning hours
# by setting the labelled time ranges to NA

down_time_ranges <-
  ts_an %>%
  select(s_dt, e_dt) %>%
  filter(complete.cases(.)) %>%
  rowwise %>%
  do(ranges = seq(.$s_dt - 60, .$e_dt, by = "min")) %>%
  unlist %>%
  as.POSIXct(origin = "1970-01-01")

load_time_ranges <-
  ts_an %>%
  select(s_ld, e_ld) %>%
  filter(complete.cases(.)) %>%
  rowwise %>%
  do(ranges = seq(.$s_ld - 60, .$e_ld, by = "min")) %>%
  unlist %>%
  as.POSIXct(origin = "1970-01-01")

ts$value[ts$date.time %in% c(down_time_ranges, load_time_ranges)] <-
  NA
ts$value <-
  na.approx(ts$value)

quiet_time_ranges <-
  data_frame(date.time = seq(min(ts$date.time), max(ts$date.time), by = "min")) %>%
  filter(as.integer(strftime(date.time, format = "%H")) %in% 0:8)
ts$value[ts$date.time %in% quiet_time_ranges$date.time] <-
  NA
ts$value <-
  na.approx(ts$value)

######################################################################################################
# Create subranges of time series using start and end of anomalies

real_an <-
  ts_an %>%
  select(s_an, e_an) %>%
  filter(complete.cases(.))

subset_an_ranges <-
  data_frame(start.date.time = real_an$e_an[1:nrow(real_an) - 1],   # end of last anomaly
             start.an = real_an$s_an[2:nrow(real_an)]) %>%          # start of next anomaly
  mutate(probationary.15pct.range = start.an - start.date.time) %>%
  mutate(detect.85pct.range = probationary.15pct.range * 85 / 15) %>%
  mutate(end.date.time = start.an + detect.85pct.range) %>%
  filter(end.date.time <= max(ts$date.time))

######################################################################################################
# Output data as .csv and annotations as NAB json labels

save_to_nab_csv <-
  function(start_date_time, end_date_time, ts) {
    csv_data <-
      ts %>%
      filter(date.time > start_date_time & date.time < end_date_time) %>%
      mutate(timestamp = strftime(date.time, format = "%F %T")) %>%
      select(timestamp, value)
    
    # Write .csv
    nab_data_fname <-
      paste0(
        "./data/dataSet/",
        as.integer(start_date_time),
        "_",
        as.integer(end_date_time),
        ".csv"
      )
    write_csv(csv_data, path = nab_data_fname)
  }

save_to_nab_json <-
  function(subset_an_ranges, real_an)
  {
    create_json_array <-
      function(an_range, real_an) {
        real_an %>%
          filter(s_an > an_range$start.date.time & s_an < an_range$end.date.time) %>%
          select(s_an) %>%
          unlist %>%
          unname %>%
          as.POSIXct(origin = "1970-01-01")
      }
    
    an_lists <-
      alply(.data = subset_an_ranges, .margins = 1, .fun = create_json_array, real_an)
    
    an_lists_names <-
      subset_an_ranges %>%
      rowwise %>%
      do(
        as_data_frame(paste0("dataSet/",
                             as.integer(.$start.date.time),
                             "_",
                             as.integer(.$end.date.time),
                             ".csv")))
    names(an_lists) <-
      an_lists_names$value
    
    # Write JSON anomalies
    nab_label_fname <-
      "./labels/raw/GM_dataSet_labels_v0.1.json"
    fc <-
      file(nab_label_fname)
    writeLines(toJSON(an_lists), fc)
    close(fc)
  }

print("Writing data. Make sure any old junk is deleted!")
setwd("~/Work/NAB/")

# Create .csv data files
subset_an_ranges %>%
  rowwise %>%
  do(ts_range = save_to_nab_csv(.$start.date.time, .$end.date.time, ts))

# Create raw label .json
subset_an_ranges %>%
  save_to_nab_json(real_an)