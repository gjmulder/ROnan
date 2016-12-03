library(zoo)
library(googlesheets)
library(jsonlite)
library(plyr)
library(tidyverse)

options(warn = 0)

######################################################################################################
#
# Clean data using s_dt/e_dt and s_ld/e_ld labels to linearly interpolate human anomalies
#
# Compute subsequences that meet criteria specified in
# https://drive.google.com/file/d/0B1_XUjaAXeV3YlgwRXdsb3Voa1k/vie

clean_ts_data <-
  function(ts_df, ts_an, dst_start) {
    # Linearly interpolate any human generated events (downtime and load) by setting the labeled time
    # ranges to NA.
    
    # Get a time series and set all NAs to 0
    ts <-
      ts_df %>%
      filter(date.time > dst_start) %>%
      mutate(value = ifelse(is.na(value), 0, value))
    
    if ("s_dt" %in% names(ts_an)) {
      down_time_ranges <-
        ts_an %>%
        select(s_dt, e_dt) %>%
        filter(complete.cases(.)) %>%
        rowwise %>%
        do(ranges = seq(.$s_dt - 60, .$e_dt, by = "min")) %>%
        unlist %>%
        as.POSIXct(origin = "1970-01-01")
    } else {
      down_time_ranges <-
        NULL
    }
    
    if ("s_ld" %in% names(ts_an)) {
      load_time_ranges <-
        ts_an %>%
        select(s_ld, e_ld) %>%
        filter(complete.cases(.)) %>%
        rowwise %>%
        do(ranges = seq(.$s_ld - 60, .$e_ld, by = "min")) %>%
        unlist %>%
        as.POSIXct(origin = "1970-01-01")
    } else {
      load_time_ranges <-
        NULL
    }
    
    ts$value[ts$date.time %in% c(down_time_ranges, load_time_ranges)] <-
      NA
    ts$value <-
      na.approx(ts$value)
    
    # quiet_time_ranges <-
    #   data_frame(date.time = seq(min(ts$date.time), max(ts$date.time), by = "min")) %>%
    #   filter(as.integer(strftime(date.time, format = "%H", tz = "Europe/London")) %in% 0:8)
    # ts$value[ts$date.time %in% quiet_time_ranges$date.time] <-
    #   NA
    # ts$value <-
    #   na.approx(ts$value)
    
    ts
  }

# Create subranges of time series using start and end of anomalies
write_nab_sebsequences <-
  function(ts, ts_name, ts_an) {
    # Output data as .csv and annotations as NAB json labels
    save_to_nab_csv <-
      function(start_date_time, end_date_time, ts) {
        csv_data <-
          ts %>%
          filter(date.time > start_date_time &
                   date.time < end_date_time) %>%
          mutate(timestamp = strftime(date.time, format = "%F %T", tz = "Europe/London")) %>%
          select(timestamp, value)
        
        summary(csv_data)
        
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
    
    # Get non-human anomailes
    real_an <-
      ts_an %>%
      select(s_an, e_an) %>%
      filter(complete.cases(.))
    
    # Get subranges based on anomalies, minimum 14 days in length
    #
    # 1. For each start of anomaly
    # 2.   Find start or previous end of anomaly
    # 3.   This time range is 15% of data
    # 4.   Take 85% of data after start of anomaly
    # 5.   15% + 85% = time series subset
    
    subset_an_ranges <-
      data_frame(start.date.time = real_an$e_an[1:nrow(real_an) - 1],
                 # end of last anomaly
                 start.an = real_an$s_an[2:nrow(real_an)]) %>%             # start of next anomaly
      mutate(probationary.15pct.range = start.an - start.date.time) %>%
      mutate(detect.85pct.range = probationary.15pct.range * 85 / 15) %>%
      mutate(end.date.time = start.an + detect.85pct.range) %>%
      filter(end.date.time <= max(ts$date.time)) %>%
      filter((end.date.time - start.date.time) > as.difftime(14, units = "days"))
    
    # Write .csv data files
    subset_an_ranges %>%
      rowwise %>%
      do(ts_range = save_to_nab_csv(.$start.date.time, .$end.date.time, ts))
    
    # Write raw label .json
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
    # Get corresponding annotated labels, and pair the start and end times of each label
    Sys.sleep(6) # prevent "Too Many Requests (RFC 6585) (HTTP 429)" from GSheets
    gs_annotations <-
      gs_read(gsheet_ts_annotations, ws = ts_name)
    
    if (typeof(gs_annotations$date.time) == "character") {
      gs_annotations$date.time <-
        as.POSIXct(strptime(gs_annotations$date.time, "%d/%m/%Y %H:%M:%S", tz = "Europe/London"))
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

# GSheets auth and choose the spreadsheet
googlesheets::gs_auth(token = "~/Work/ronan/shiny_app_google_sheet_token.rds")
sheet_key <-
  "1Y8MoUBi1CtzLNv0b_VPQS3MTAeapRE9ep5_7cMTnJUE"
gsheet_ts_annotations <-
  googlesheets::gs_key(sheet_key)

######################################################################################################
# Clean all columns of ts_df using combined order labels

# Skip missing data at start of ts
ts_start <-
  as.POSIXct(strptime("2016-02-25 15:00:00", "%Y-%m-%d %H:%M:%S", tz = "Europe/London"))

clean_ts <-
  function(col_name, ts_df, ts_start) {
    # For a given ts in ts_df, apply the cleaning process using Google labels, and return a cleaned ts
    ts_df[, c("date.time", col_name)] %>%
      setNames(c("date.time", "value")) %>%
      clean_ts_data(get_labels_from_google_sheets(col_name, ts_start),
                    ts_start) %>%
      setNames(c("date.time.dupe", col_name))
  }

clean_ts_df <-
  names(ts_df[,-1]) %>%
  lapply(clean_ts,
         ts_df,
         ts_start) %>%
  bind_cols
clean_ts_df[duplicated(names(clean_ts_df))] <-
  NULL
colnames(clean_ts_df)[1] <-
  "date.time"

######################################################################################################
# Generate ratios of orders vs. reqs and write tio NAB .csv's

setwd("~/Work/NAB/")
unlink(
  c(
    "./data/dataSet/*csv",
    "./data/labels/*json",
    "./data/labels/raw/*json"
  ),
  force = TRUE
)

# Mobile
ts_name <-
  "mobile.orders.vs.reqs"
ts_an <-
  bind_rows(
    get_labels_from_google_sheets("mobile.orders", ts_start),
    get_labels_from_google_sheets("mobile.reqs", ts_start)
  ) %>%
  arrange(s_an, s_dt, s_ld)

clean_ts_df %>%
  mutate(value = mobile.orders / mobile.reqs) %>%
  select(date.time, value) %>%
  write_nab_sebsequences(ts_name, ts_an)

# Desktop
ts_name <-
  "desktop.orders.vs.reqs"
ts_an <-
  bind_rows(
    get_labels_from_google_sheets("desktop.orders", ts_start),
    get_labels_from_google_sheets("desktop.reqs", ts_start)
  ) %>%
  arrange(s_an, s_dt, s_ld)

clean_ts_df %>%
  mutate(value = desktop.orders / desktop.reqs) %>%
  select(date.time, value) %>%
  write_nab_sebsequences(ts_name, ts_an)
