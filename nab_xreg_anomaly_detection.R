################################################################################
# This script runs the Twitter AnomalyDetection algorithms on the NAB data set.
#
# You must first install the AnomalyDetection package:
# https://github.com/twitter/AnomalyDetection#how-to-get-started
#
# You must also have NAB installed and specify the path at the bottom of this
# script.
################################################################################

# library(methods)
# library(AnomalyDetection)
library(jsonlite)
library(forecast)
library(parallel)
library(tidyverse)

options(error = NULL)
options(error = recover)

load_detect_save <-
  function(meta_param,
           algorithmName,
           model_name,
           pathToNAB,
           tz,
           skipFiles = NULL) {
    # pathToNAB (character): string specifying path to the NAB dir.
    # algorithmName (character): either 'twitterADTs' or 'twitterADVec'.
    # skipFiles (list): file names to skip; useful in debugging.
    
    nnetarAD <-
      function(p, ts_df, tz, do_plot = FALSE) {
        create_datetime_xreg <-
          function(ts_df) {
            xreg <-
              ts_df %>%
              mutate(
                day.of.week = as.integer(strftime(
                  timestamp, format = "%w", tz = "Europe/London"
                )),
                hour.of.day = as.integer(strftime(
                  timestamp, format = "%H", tz = "Europe/London"
                )),
                min.of.hour = as.integer(strftime(
                  timestamp, format = "%M", tz = "Europe/London"
                ))
              ) %>%
              select(-timestamp, -value)
          }

        xreg <-
          create_datetime_xreg(ts_df)
          
        # p <-
        #   10
        size <-
          5
        message("Training nnetar with size=", size, ", p=", p)
        print(system.time(
          nn_fit <-
            nnetar(
              y = ts_df$value,
              p = p,
              # repeats = 5,
              size = size,
              MaxNWts = 10000,
              xreg = xreg
            )
        ))

        message("Computing nnetar forecasts...")
        nn_forecast <-
          forecast(nn_fit, xreg = xreg)
        
        message("Computing nnetar accuracy...")
        print(accuracy(nn_forecast))
        
        nn_forecast_values <-
          nn_forecast$fitted
        
        point_smape <-
          abs(nn_forecast_values - ts_df$value) / abs(nn_forecast_values + ts_df$value)
        point_smape[is.na(point_smape)] <-
          0.0
        normalised_point_smape <-
          point_smape / max(point_smape)
        
        if (do_plot) {
          fitted_df <-
            data_frame(
              timestamp = ts_df$timestamp,
              # point_smape = normalised_point_smape,
              forecast_y = nn_forecast_values / max(nn_forecast_values) - min(nn_forecast_values),
              actual_y = ts_df$value / max(ts_df$value) - min(ts_df$value)
            ) %>%
            gather(series, value, actual_y, forecast_y)
          gg <-
            ggplot() +
            ggtitle(paste0("nnetarAD, size=", size, ", p=", p)) +
            geom_line(
              data = fitted_df,
              aes(
                x = timestamp,
                y = value,
                colour = series
              ),
              size = 0.2,
              alpha = 0.4
            ) +
            scale_colour_manual(values = c("black", "gold", "green")) +
            guides(colour = guide_legend(override.aes = list(size = 2, alpha = 1)))
          print(gg)
        }
        
        data.frame(anoms = normalised_point_smape)
      }
    
    addDetections <-
      function(anomalyDataFrame,
               detections,
               algorithmName,
               tz) {
        if (algorithmName == "nnetarAD") {
          anomalyDataFrame$anomaly_score <-
            detections$anoms
        } else {
          anomalyDataFrame$anomaly_score <-
            0.0
          
          if (length(detections$anoms) > 0) {
            for (i in 1:nrow(detections$anoms)) {
              if (algorithmName == "twitterADTs") {
                idx = match(as.POSIXct(detections$anoms[i, 1], tz = tz),
                            anomalyDataFrame$timestamp)
              } else if (algorithmName == "twitterADVec") {
                idx <-
                  detections$anoms[i, 1]
              }
              
              anomalyDataFrame[idx, ]$anomaly_score <-
                1.0
            }
          }
        }
        return(anomalyDataFrame)
      }
    
    addLabels <-
      function(anomalyDataFrame, anomalyBounds) {
        anomalyDataFrame$label = 0
        
        if (length(anomalyBounds) != 0) {
          for (i in 1:nrow(anomalyBounds)) {
            lower <-
              anomalyBounds[i, 1]
            upper <-
              anomalyBounds[i, 2]
            idx <-
              anomalyDataFrame$timestamp >= lower &
              anomalyDataFrame$timestamp <= upper
            idx[is.na(idx)] <-
              FALSE
            anomalyDataFrame[idx, ]$label <-
              1
          }
        }
        return(anomalyDataFrame)
      }
    
    runDetection <-
      function(meta_param,
               algorithmName,
               nab_data,
               filename,
               tz) {
        if (algorithmName == "twitterADTs") {
          results <-
            tryCatch({
              message(paste(
                "Attempting detection w/ AnomalyDetectionTS on ",
                filename
              ))
              AnomalyDetectionTs(
                nab_data,
                max_anoms = 0.0008,
                direction = 'both',
                plot = FALSE
              )
            },
            error = function(cond) {
              message(paste("Unable to run the algorithm for ", filename))
              message(cond)
              return()
            })
        } else if (algorithmName == "twitterADVec") {
          message(paste(
            "Attempting detection w/ AnomalyDetectionVec on ",
            filename
          ))
          results <-
            AnomalyDetectionVec(
              nab_data[, 2],
              alpha = 0.05,
              period = 150,
              max_anoms = 0.0020,
              direction = 'both',
              plot = FALSE
            )
        } else if (algorithmName == "nnetarAD") {
          message(paste("Attempting detection w/ nnetarAD on ", filename))
          results <-
            nnetarAD(meta_param, nab_data, tz, do_plot = FALSE)
        }
        
        message("Results...")
        print(summary(results$anoms))
        return(results)
      }
    
    processDataFiles <-
      function(dFile,
               dDir,
               nabDataDir,
               meta_param,
               algorithmName,
               model_name,
               windows,
               resultsDir,
               tz,
               skipFiles) {
        if (dFile %in% skipFiles)
          return(NULL)
        
        # Get the data and run the detector
        dataName <-
          paste(dDir, dFile, sep = '/')
        dFilePath <-
          paste(nabDataDir, dataName, sep = '/')
        nab_data <-
          read.csv(dFilePath, colClasses = c("nabDate", "numeric"))
        
        # Run the detector algorithm
        results <-
          runDetection(meta_param,
                       algorithmName,
                       nab_data,
                       dFilePath,
                       tz)
        
        # Populate dataframe with anomaly scores and truth labels
        nab_data <-
          addDetections(nab_data, results, algorithmName, tz)
        nab_data <-
          addLabels(nab_data, windows[[dataName]])
        print("Results summary:")
        print(summary(nab_data))
        
        # Write results to csv
        dir.create(paste(resultsDir, dDir, sep = "/"),
                   recursive = TRUE,
                   mode = "0755")
        
        resultsFileName <-
          paste0(algorithmName,
                 "-",
                 model_name,
                 "-",
                 as.character(meta_param),
                 "_",
                 dFile)
        
        write.csv(nab_data,
                  paste(resultsDir, dDir, resultsFileName, sep = '/'),
                  row.names = FALSE)
      }
    
    processDataDir <-
      function(dDir,
               nabDataDir,
               meta_param,
               algorithmName,
               model_name,
               windows,
               resultsDir,
               tz,
               skipFiles) {
        dataFiles <-
          list.files(paste(nabDataDir, dDir, sep = '/'))
        lapply(
          dataFiles,
          processDataFiles,
          dDir,
          nabDataDir,
          meta_param,
          algorithmName,
          model_name,
          windows,
          resultsDir,
          tz,
          skipFiles
        )
      }
    
    # Format dates: coerce from character class to nabDate class
    setClass("nabDate")
    setAs("character",
          "nabDate",
          function(from)
            as.POSIXct(from, format = "%Y-%m-%d %H:%M:%S", tz = tz))
    
    # Setup paths to NAB data and results
    nabDataDir <-
      paste(pathToNAB, "data", sep = '/')
    dataDirs <-
      list.files(nabDataDir)
    resultsDir <-
      paste0(
        pathToNAB,
        "/results/",
        algorithmName,
        "-",
        model_name,
        "-",
        as.character(meta_param)
      )
    # Clean any old results
    message("Removing dir: ", resultsDir)
    unlink(resultsDir, recursive = TRUE)
    
    # Get the truth anomaly windows
    windows <-
      fromJSON(paste(pathToNAB, "labels/combined_windows.json", sep = '/'))
    
    lapply(
      dataDirs,
      processDataDir,
      nabDataDir,
      meta_param,
      algorithmName,
      model_name,
      windows,
      resultsDir,
      tz,
      skipFiles
    )
  }


################################################################################
meta_param_vec <-
  # c(1:8 * 5)
  # c(c(1:4, 8:5) * 10)
  c(1)

tz <-
  "Europe/London"

# cluster <-
#   # makeCluster(detectCores() / 2)
#   makeCluster(3)
# 
# clusterEvalQ(cluster, {
#   library(jsonlite)
#   library(forecast)
#   library(tidyverse)
#   library(AnomalyDetection)
# })

results <-
  lapply(
  # parLapplyLB(
  #   cluster,
    meta_param_vec,
    load_detect_save,
    algorithmName = "nnetarAD",
    # twitterADTs, twitterADVec, nnetarAD
    pathToNAB = "~/Work/NAB",
    tz,
    model_name = "lag-p",
    skipFiles = NULL
  )

# stopCluster(cluster)
