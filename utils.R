library(tidyverse)

# Load data

base_path <-
  "~/Work/SB_annotations/"

load(paste0(base_path,
            'dataset1.Rdata'))
ts_df <-
  system_data
summary(ts_df)

######################################################################################################
# Load annotations from an .rdata file and save to individual .csv files

load_annotations_save_csv <-
  function(rdata_fname) {
    load(file = rdata_fname, verbose = TRUE)
    
    write_annotation <-
      function(ts_name, ts_annotations) {
        ts_annotation <-
          ts_annotations[[annotation_name]]
        ts_csv_fname <-
          paste0(base_path,
                 "ts_annotations_",
                 annotation_name,
                 ".csv")
        if (!file.exists(ts_csv_fname)) {
          write_csv(ts_annotation[order(ts_annotation$date.time),],
                    path = ts_csv_fname)
          paste0("Wrote: ",
                 ts_name)
        } else {
          paste0("Not overwriting: ",
                 ts_name)
        }
      }
    
    lapply(names(ts_annotations), write_annotation, ts_annotations)
  }

######################################################################################################
# Load individual .csv files and save annotations in an .rdata file

load_csv_save_annotations <-
  function(base_path, rdata_fname) {
    if (!file.exists(rdata_fname)) {
      fname_list <-
        list.files(pattern = paste0(base_path, "ts_annotations_.*.csv"))
      print(fname_list)
      
      ts_annotations <-
        lapply(fname_list, read_csv)
      save(ts_annotations, file = rdata_fname)
      paste0("Saved files (",
             lapply(fname_list, paste0, ","),
             ") into: ",
             rdata_fname)
    } else {
      paste0("Not overwriting: ",
             rdata_fname)
    }
  }

# load_annotations_save_csv(paste0(base_path, "ts_annotations.Rdata")

load_csv_save_annotations(base_path, "~/Work/new_annotations.Rdata")
