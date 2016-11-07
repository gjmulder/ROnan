# Shiny server interface for Time Series visualiastion and annotation
#
# Version 0.2 - Gary Mulder - 03/11/2016

library(shiny)
library(tidyverse)

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
      time_range = list(
        start = min(ts_df$date.time),
        end = max(ts_df$date.time)
      ),
      undo_stack = list(),
      ts_annotations = ts_annotations
      # ,
      # y_log_axis = TRUE
    )
  
  # Main ggplot function
  plot_time_series <-
    function() {
      time_range <-
        rv$time_range
      
      message("=====================================")
      message("Time series: ", input$time_series_name)
      message("Start date : ", strftime(time_range$start, format = "%c"))
      message("End date   : ", strftime(time_range$end, format = "%c"))
      
      # str(ts_df)
      # str(rv$time_range)
      
      ts_df %>%
        filter(date.time >= time_range$start &
                 date.time <= time_range$end)  %>%
        ggplot(aes_string(x = "date.time", y = input$time_series_name)) +
        geom_line(size = 0.5) ->
        gg_plot
      
      # Add filtered annotations, if they exist
      if (!is.null(rv$ts_annotations[[input$time_series_name]])) {
        rv$ts_annotations[[input$time_series_name]] %>%
          filter(date.time >= time_range$start &
                   date.time <= time_range$end) ->
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
                 rv$time_range$start <-
                   as.POSIXct(input$date_range[1])
                 rv$time_range$end <-
                   as.POSIXct(input$date_range[2])
               })
  
  # Navigate by hour, day or week
  observeEvent(input$minus_hour,
               {
                 message("-1 hour")
                 rv$time_range$start <-
                   rv$time_range$start - 60 * 60
                 rv$time_range$end <-
                   rv$time_range$end - 60 * 60
               })
  observeEvent(input$minus_day,
               {
                 message("-1 day")
                 rv$time_range$start <-
                   rv$time_range$start - 24 * 60 * 60
                 rv$time_range$end <-
                   rv$time_range$end - 24 * 60 * 60
               })
  observeEvent(input$minus_week,
               {
                 message("-1 week")
                 rv$time_range$start <-
                   rv$time_range$start - 7 * 24 * 60 * 60
                 rv$time_range$end <-
                   rv$time_range$end - 7 * 24 * 60 * 60
               })
  observeEvent(input$plus_hour,
               {
                 message("+1 hour")
                 rv$time_range$start <-
                   rv$time_range$start + 60 * 60
                 rv$time_range$end <-
                   rv$time_range$end + 60 * 60
               })
  observeEvent(input$plus_day,
               {
                 message("+1 day")
                 rv$time_range$start <-
                   rv$time_range$start + 24 * 60 * 60
                 rv$time_range$end <-
                   rv$time_range$end + 24 * 60 * 60
               })
  observeEvent(input$plus_week,
               {
                 message("+1 week")
                 rv$time_range$start <-
                   rv$time_range$start + 7 * 24 * 60 * 60
                 rv$time_range$end <-
                   rv$time_range$end + 7 * 24 * 60 * 60
               })
  
  push <-
    function(time_range) {
      # str(rv$undo_stack)
      if (length(rv$undo_stack) > 0)
        rv$undo_stack[[length(rv$undo_stack) + 1]] <-
          time_range
      else
        rv$undo_stack <-
          list(time_range)
    }
  
  pop <-
    function() {
      # str(rv$undo_stack)
      if (length(rv$undo_stack) == 0)
        NULL
      else {
        time_range <-
          rv$undo_stack[[length(rv$undo_stack)]]
        rv$undo_stack[[length(rv$undo_stack)]] <-
          NULL
        time_range
      }
    }
  # Zoom in using a "brush" drag
  observeEvent(input$plot_brush,
               {
                 message("Brush")
                 push(rv$time_range)
                 rv$time_range <-
                   list(
                     start = as.POSIXct(as.integer(input$plot_brush$xmin), origin = "1970-01-01"),
                     end = as.POSIXct(as.integer(input$plot_brush$xmax), origin = "1970-01-01")
                   )
               })
  
  # Unzoom the previous brush drag
  observeEvent(input$unzoom,
               {
                 message("Unzoom")
                 time_range <-
                   pop()
                 if (!is.null(time_range))
                   rv$time_range <-
                   time_range
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
                 str(ts_annotations)
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
