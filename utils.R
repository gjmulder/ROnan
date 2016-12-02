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

# Cleaned system data
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
# ss$sheet_key # 1D6nHybwCpanaw0pynRRWfFJ2QRtIMvIXw8rm2s0xGos

## if you version control your app, don't forget to ignore the token file!
## e.g., put it into .gitignore

googlesheets::gs_auth(token = "~/Work/ronan/shiny_app_google_sheet_token.rds")
sheet_key <-
  "1D6nHybwCpanaw0pynRRWfFJ2QRtIMvIXw8rm2s0xGos"
gsheet_ts_annotations <-
  googlesheets::gs_key(sheet_key)

print(gs_ws_ls(gsheet_ts_annotations))

annotations_to_csv <-
  function(annotation_base_path, rdata_fname) {
    load(file = rdata_fname, verbose = TRUE)
    
    write_annotation <-
      function(ts_name,
               annotation_base_path,
               ts_annotations) {
        ts_annotation <-
          ts_annotations[[ts_name]]
        ts_csv_fname <-
          paste0(annotation_base_path,
                 "ts_annotations_",
                 ts_name,
                 ".csv")
        if (!file.exists(ts_csv_fname)) {
          write_csv(ts_annotation[order(ts_annotation$date.time), ],
                    path = ts_csv_fname)
          paste0("Wrote: ",
                 ts_name)
        } else {
          paste0("Not overwriting: ",
                 ts_name)
        }
      }
    
    # Write each individual .csv file using the list names
    lapply(names(ts_annotations),
           write_annotation,
           annotation_base_path,
           ts_annotations)
  }

###################################################################################################
# Generate some reoccuring labels
# start_7am_an <-
#   data_frame(date.time = seq(as.POSIXct(strptime("01/04/2016 00:00:00", "%d/%m/%Y %H:%M:%S")),
#                              as.POSIXct(strptime("10/08/2016 00:00:00", "%d/%m/%Y %H:%M:%S")),
#                              by = "min"),
#              value = pi,
#              annotation = "s_dt") %>%
#   filter(strftime(date.time, format = "%H") == "07") %>%
#   filter(strftime(date.time, format = "%M") == "00")
# 
# end_7am_an <-
#   data_frame(date.time = seq(as.POSIXct(strptime("01/04/2016 00:00:00", "%d/%m/%Y %H:%M:%S")),
#                              as.POSIXct(strptime("10/08/2016 00:00:00", "%d/%m/%Y %H:%M:%S")),
#                              by = "min"),
#              value = pi,
#              annotation = "e_dt") %>%
#   filter(strftime(date.time, format = "%H") == "07") %>%
#   filter(strftime(date.time, format = "%M") == "35")
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