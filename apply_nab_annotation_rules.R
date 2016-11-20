library(tidyverse)
library(googlesheets)

# 1. Read in data
# 2. Find start and end of data
# 3. For each s_an
# 4.   Find start or previous e_an
# 5.   This time range is 15% of data
# 6.   Take 85% of data after s_an
# 7.   15% + 85% = data set

ts_an <-
  gs_read(gsheet_ts_annotations,
          ws = "desktop.orders")

ts_an_start <-
  ts_ann %>%
  filter(annotation == "s_an") %>%
  select(date.time)

ts_an_end <-
  ts_ann %>%
  filter(annotation == "e_an") %>%
  select(date.time)

ts <-
  ts_df %>%
  select(date.time, desktop.orders)

for (s_an_element in ts_an_start$date.time) {
  previous_e_an_df <-
    ts_an_end %>%
    filter(date.time < s_an_element) %>%
    tail(1)
  
  previous_e_an <-
    previous_e_an_df$date.time
  
  start_e_an <-
    as.POSIXct(s_an, origin = "1970-01-01")

  print(paste('End last  :', previous_e_an))
  print(paste('Start new :', start_e_an))
  
  time_diff <-
    start_e_an - previous_e_an
  
  print(time_diff)
}