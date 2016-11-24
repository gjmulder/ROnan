library(tidyverse)
library(googlesheets)
library(jsonlite)

######################################################################################################
# Load time series data

load('~/Work/DS/dataset1.Rdata')
ts_df <-
  system_data
summary(ts_df)

annotation_base_path <-
  "~/Work/DS_annotations/"

ts_name_from_fname <-
  function(fname) {
    # "ts_annotations_.*.csv
    substr(fname, 16, nchar(fname) - 4)
  }

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

setwd("~/Work/ronan/")
googlesheets::gs_auth(token = "shiny_app_google_sheet_token.rds")
sheet_key <-
  "1D6nHybwCpanaw0pynRRWfFJ2QRtIMvIXw8rm2s0xGos"
gsheet_ts_annotations <-
  googlesheets::gs_key(sheet_key)

print(gs_ws_ls(gsheet_ts_annotations))

######################################################################################################
# Load annotations from an .rdata file and save to individual .csv files

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

######################################################################################################
# Load individual .csv files and save annotations in an .rdata file

csv_to_annotations <-
  function(annotation_base_path, rdata_fname) {
    if (!file.exists(rdata_fname)) {
      fname_list <-
        list.files(path = annotation_base_path,
                   pattern = "ts_annotations_.*\\.csv$")
      print(fname_list)
      
      # Read .csv files
      ts_annotations <-
        lapply(paste0(annotation_base_path, fname_list), read_csv)
      
      # Generate names for ts_annotation list
      ts_names <-
        lapply(fname_list, ts_name_from_fname) %>%
        unlist
      names(ts_annotations) <-
        ts_names
      
      # Save .rdata file
      str(ts_annotations)
      save(ts_annotations, file = rdata_fname)
      paste0("Saved files ",
             fname_list,
             " into ",
             rdata_fname)
      
    } else {
      paste0("Not overwriting: ",
             rdata_fname)
    }
  }

######################################################################################################
# Output annotations as NAB json labels

csv_to_nab_jason <-
  function(annotation_csv_fname, csv_fname, nab_fname) {
    ts_annotation <-
      read_csv(annotation_csv_fname)
    
    start <-
      ts_annotation %>%
      filter(annotation == "s_an") %>%
      select(date.time)
    end <-
      ts_annotation %>%
      filter(annotation == "e_an") %>%
      select(date.time)
    
    anoms <-
      cbind(start, end)
    names(anoms) <-
      NULL

    anoms_list <-
      list()
    anoms_list[[csv_fname]] <-
      anoms
    
    fc <-
      file(nab_fname)
    writeLines(toJSON(anoms_list), fc)
    close(fc)
  }


######################################################################################################
# print(annotations_to_csv(annotation_base_path, "~/Work/ts_annotations.Rdata"))
#
# print(csv_to_annotations(annotation_base_path, "~/Work/new_annotations.Rdata"))
#
# csv_to_nab_jason(
#     "~/Work/DS_annotations/ts_annotations_desktop.orders.csv",
#     "desktop.orders.csv",
#     "~/Work/desktop.orders_labels.json"
#   )
#
# write_csv(system_data[, c("date.time", "desktop.orders")], path = "~/Work/desktop.orders.csv")
#
# g <- ggplot(data = system_data) + geom_line(aes(x = date.time, y = desktop.orders))
# ggsave("~/Work/desktop.orders.png", width = 100, height = 50, limitsize = FALSE)