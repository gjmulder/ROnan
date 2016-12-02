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

clean_ts_data <-
  function(ts_df, ts_an, dst_start) {
    # Get a time series and set all NAs to 0
    ts <-
      ts_df %>%
      filter(date.time > dst_start) %>%
      mutate(value = ifelse(is.na(value), 0, value))
    
    ######################################################################################################
    # Linearly interpolate human generated events (down time and load) by setting the labeled time ranges
    # to NA. Start label is at the first anomalous data point so we want to interpolate from the prior
    # "good" data point.
    
    dst_offset <-
      60 * 60
    
    down_time_ranges <-
      ts_an %>%
      select(s_dt, e_dt) %>%
      filter(complete.cases(.)) %>%
      rowwise %>%
      do(ranges = seq(.$s_dt - 60 + dst_offset, .$e_dt + dst_offset, by = "min")) %>%
      unlist %>%
      as.POSIXct(origin = "1970-01-01")
    
    load_time_ranges <-
      ts_an %>%
      select(s_ld, e_ld) %>%
      filter(complete.cases(.)) %>%
      rowwise %>%
      do(ranges = seq(.$s_ld - 60 + dst_offset, .$e_ld + dst_offset, by = "min")) %>%
      unlist %>%
      as.POSIXct(origin = "1970-01-01")
    
    ts$value[ts$date.time %in% c(down_time_ranges, load_time_ranges)] <-
      NA
    ts$value <-
      na.approx(ts$value)
    
    # quiet_time_ranges <-
    #   data_frame(date.time = seq(min(ts$date.time), max(ts$date.time), by = "min")) %>%
    #   filter(as.integer(strftime(date.time, format = "%H")) %in% 0:8)
    # ts$value[ts$date.time %in% quiet_time_ranges$date.time] <-
    #   NA
    # ts$value <-
    #   na.approx(ts$value)
    
    ts
  }

write_NAB_sebsequences <-
  function(ts, ts_name, ts_an) {
    ######################################################################################################
    # Create subranges of time series using start and end of anomalies
    
    real_an <-
      ts_an %>%
      select(s_an, e_an) %>%
      filter(complete.cases(.))
    
    subset_an_ranges <-
      data_frame(start.date.time = real_an$e_an[1:nrow(real_an) - 1],
                 # end of last anomaly
                 start.an = real_an$s_an[2:nrow(real_an)]) %>%             # start of next anomaly
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
          filter(date.time > start_date_time &
                   date.time < end_date_time) %>%
          mutate(timestamp = strftime(date.time, format = "%F %T")) %>%
          select(timestamp, value)
        
        # Write .csv
        nab_data_fname <-
          paste0(
            "./data/dataSet/",
            ts_name,
            "_",
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
              filter(s_an > an_range$start.date.time &
                       s_an < an_range$end.date.time) %>%
              mutate(mid_an = round((
                as.integer(s_an) / 2 + as.integer(e_an) / 2
              ) / 60) * 60) %>%
              select(mid_an) %>%
              unlist %>%
              unname %>%
              as.POSIXct(origin = "1970-01-01")
          }
        
        an_lists <-
          alply(.data = subset_an_ranges,
                .margins = 1,
                .fun = create_json_array,
                real_an)
        
        an_lists_names <-
          subset_an_ranges %>%
          rowwise %>%
          do(as_data_frame(
            paste0(
              "dataSet/",
              ts_name,
              "_",
              as.integer(.$start.date.time),
              "_",
              as.integer(.$end.date.time),
              ".csv"
            )
          ))
        names(an_lists) <-
          an_lists_names$value
        
        # Write JSON anomalies
        nab_label_fname <-
          paste0("./labels/raw/GM_",
                 ts_name,
                 "_labels_v0.2.json")
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
  }

######################################################################################################
# Google Sheets preparation
#
# prepare the OAuth token and set up the target sheet:
#  - do this interactively
#  - do this EXACTLY ONCE
#
# shiny_token <- gs_auth() # authenticate w/ your desired Google identity here
# saveRDS(shiny_token, "shiny_app_google_sheet_token.rds")
# ss <- gs_new("ts_annotations",
#              row_extent = n, col_extent = n, input = filler)
# ss$sheet_key # 1D6nHybwCpanaw0pynRRWfFJ2QRtIMvIXw8rm2s0xGos
#
# if you version control your app, don't forget to ignore the token file!
# e.g., put it into .gitignore

get_labels_from_google_sheets <-
  function(ts_name, ts_start) {
    googlesheets::gs_auth(token = "~/Work/ronan/shiny_app_google_sheet_token.rds")
    sheet_key <-
      "1D6nHybwCpanaw0pynRRWfFJ2QRtIMvIXw8rm2s0xGos"
    gsheet_ts_annotations <-
      googlesheets::gs_key(sheet_key)
    print(gs_ws_ls(gsheet_ts_annotations))
    
    # Get corresponding annotated labels, and pair the start and end times of each label
    gs_annotations <-
      gs_read(gsheet_ts_annotations, ws = ts_name)
    if (typeof(gs_annotations$date.time) == "character") {
      gs_annotations$date.time <-
        as.POSIXct(strptime(gs_annotations$date.time, "%d/%m/%Y %H:%M:%S"))
    }
    if (typeof(gs_annotations$date.time) == "double") {
      gs_annotations$date.time <-
        as.POSIXct(gs_annotations$date.time)
    }
    
    gs_annotations %>%
      filter(date.time > ts_start) %>%
      select(-value) %>%
      mutate(pairs = sort(rep(1:(nrow(
        .
      ) / 2), 2))) %>%
      spread(annotation, date.time)
  }

######################################################################################################
# Load time series data

load('~/Work/DS/dataset1.Rdata')
ts_df <-
  system_data
summary(ts_df)

# Just look at daylight savings data as there's a problem with the timestamps being in BST
# Also, we need a time later in the morning so that na.approx has a value to start approximating from.
ts_start <-
  as.POSIXct(strptime("2016-03-27 10:00:00", "%Y-%m-%d %H:%M:%S"))

######################################################################################################
# Get labels, clean ts, write ts subsequences and labels to NAB .csvs

ts_name <-
  "mobile.orders.vs.reqs"

ts_an <-
  get_labels_from_google_sheets("mobile.orders", ts_start)

ts_df %>%
  mutate(value = mobile.orders / mobile.reqs) %>%
  select(date.time, value) %>%
  clean_ts_data(ts_an, ts_start) %>%
  write_NAB_sebsequences(ts_name, ts_an)

# ######################################################################################################
# # Clean all columns of ts_df using combined order labels
#
# ts_an <-
#   get_labels_from_google_sheets("combined.orders", ts_start)
#
# combine_clean_ts_data <-
#   function(col_name, ts_df, ts_an, ts_start) {
#     ts_df[, c("date.time", col_name)] %>%
#       setNames(c("date.time", "value")) %>%
#       clean_ts_data(ts_an, dst_start) %>%
#       setNames(c("date.time.dupe", col_name))
#   }
#
# clean_ts_df <-
#   names(ts_df[, -1]) %>%
#   lapply(
#     combine_clean_ts_data,
#     ts_df,
#     ts_an,
#     ts_start
#   ) %>%
#   bind_cols
# clean_ts_df[duplicated(names(clean_ts_df))] <-
#   NULL
# colnames(clean_ts_df)[1] <-
#   "date.time"
