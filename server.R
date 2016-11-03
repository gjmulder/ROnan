# Shiny server interface for Time Series visualiastion and annotation
#
# Version 0.2 - Gary Mulder - 03/11/2016

library(shiny)
library(tidyverse)

load('~/Work/dataset1.Rdata')
ts_df <-
  system_data
summary(ts_df)

# Define server logic required to draw the time series
shinyServer(function(input, output) {
  # TODO: Prompt user for data sources
  result <-
    try(load(file = "~/Work/ts_annotations.Rdata", verbose = TRUE))
  # str(result)
  if (class(result) == 'try-error') {
    ts_annotations <-
      list()
  }

  # State for the UI
  rv <-
    reactiveValues(
      start_date_time = min(system_data$date.time),
      old_start_date_time = min(system_data$date.time),
      end_date_time = max(system_data$date.time),
      old_end_date_time = max(system_data$date.time),
      ts_annotations = ts_annotations
      # ,
      # y_log_axis = TRUE
    )
  
  # Main ggplot function
  plot_time_series <-
    function() {
      message("=====================================")
      message("Time series: ", input$time_series_name)
      message("Start date : ", strftime(rv$start_date_time, format = "%c"))
      message("End date   : ", strftime(rv$end_date_time, format = "%c"))
      
      # str(ts_df)
      ts_df %>%
        filter(date.time >= rv$start_date_time &
                 date.time <= rv$end_date_time) %>%
        ggplot(aes_string(x = "date.time", y = input$time_series_name)) +
        geom_point() ->
        gg_plot
      
      # Add filtered annotations, if they exist
      if (!is.null(rv$ts_annotations[[input$time_series_name]])) {
        rv$ts_annotations[[input$time_series_name]] %>%
          filter(date.time >= rv$start_date_time &
                   date.time <= rv$end_date_time) ->
          filtered_annotations
        # str(filtered_annotations)
        if (nrow(filtered_annotations) > 0) {
          gg_plot <-
            gg_plot + geom_label(data = filtered_annotations,
                                 aes(
                                   x = date.time,
                                   y = value,
                                   label = annotation
                                 ))
        }
      }
      
      gg_plot
      
      
      # if (rv$y_log_axis)
      #   gg + scale_y_log10()
      # else
      #   gg + scale_y_continuous()
    }
  
  # UI event state changes we need to handle
  
  # Choose a date range
  observeEvent(input$date_range,
               {
                 message("Date range")
                 rv$start_date_time <-
                   as.POSIXct(input$date_range[1])
                 rv$end_date_time <-
                   as.POSIXct(input$date_range[2])
               })
  
  # Navigate by hour, day or week
  observeEvent(input$minus_hour,
               {
                 message("-1 hour")
                 rv$start_date_time <-
                   rv$start_date_time - 60 * 60
                 rv$end_date_time <-
                   rv$end_date_time - 60 * 60
               })
  observeEvent(input$minus_day,
               {
                 message("-1 day")
                 rv$start_date_time <-
                   rv$start_date_time - 24 * 60 * 60
                 rv$end_date_time <-
                   rv$end_date_time - 24 * 60 * 60
               })
  observeEvent(input$minus_week,
               {
                 message("-1 week")
                 rv$start_date_time <-
                   rv$start_date_time - 7 * 24 * 60 * 60
                 rv$end_date_time <-
                   rv$end_date_time - 7 * 24 * 60 * 60
               })
  observeEvent(input$plus_hour,
               {
                 message("+1 hour")
                 rv$start_date_time <-
                   rv$start_date_time + 60 * 60
                 rv$end_date_time <-
                   rv$end_date_time + 60 * 60
               })
  observeEvent(input$plus_day,
               {
                 message("+1 day")
                 rv$start_date_time <-
                   rv$start_date_time + 24 * 60 * 60
                 rv$end_date_time <-
                   rv$end_date_time + 24 * 60 * 60
               })
  observeEvent(input$plus_week,
               {
                 message("+1 week")
                 rv$start_date_time <-
                   rv$start_date_time + 7 * 24 * 60 * 60
                 rv$end_date_time <-
                   rv$end_date_time + 7 * 24 * 60 * 60
               })
  
  # Zoom in using a "brush" drag
  observeEvent(input$plot_brush,
               {
                 message("Brush")
                 rv$old_start_date_time <-
                   rv$start_date_time
                 rv$start_date_time <-
                   as.POSIXct(as.integer(input$plot_brush$xmin), origin = "1970-01-01")
                 
                 rv$old_end_date_time <-
                   rv$end_date_time
                 rv$end_date_time <-
                   as.POSIXct(as.integer(input$plot_brush$xmax), origin = "1970-01-01")
               })
  
  # Unzoom brush drag
  # TODO: Use a stack to push and pop history
  observeEvent(input$unzoom,
               {
                 message("Unzoom")
                 rv$start_date_time <-
                   rv$old_start_date_time
                 rv$end_date_time <-
                   rv$old_end_date_time
               })
  
  # Double-click annotates the current time series
  observeEvent(input$dbl_click, {
    # str(input$dbl_click)
    message(
        "Annotation on ",
        input$time_series_name,
        " at ",
        as.POSIXct(as.integer(input$dbl_click$x), origin = "1970-01-01"),
        " labelled: >",
        input$annotation_text,
        "<"
      )
    # We store the annotations in a list of data_frames. Each data_frame is indexed through the time series name
    rv$ts_annotations[[input$time_series_name]] <-
      rbind(
        rv$ts_annotations[[input$time_series_name]],
        data_frame(
          date.time = as.POSIXct(as.integer(input$dbl_click$x), origin = "1970-01-01"),
          value = as.integer(input$dbl_click$y),
          annotation = input$annotation_text
        )
      )
    # str(rv$ts_annotations)
  })
  
  # Load historical annotations
  observeEvent(input$load_annotations,
               {
                 load(file = "~/Work/ts_annotations.Rdata", verbose = TRUE)
                 rv$ts_annotations <-
                   ts_annotations
               })
  
  # Save current annotations
  observeEvent(input$save_annotations,
               {
                 ts_annotations <-
                   rv$ts_annotations
                 save(ts_annotations, file = "~/Work/ts_annotations.Rdata")
               })
  
  # observeEvent(input$toggle_log_scale,
  #              {
  #                print("Toggle Log Scale")
  #                rv$y_log_axis <-
  #                  !rv$y_log_axis
  #              })
  
  output$time_series_plot <- renderPlot({
    plot_time_series()
  })
})
