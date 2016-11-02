#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)

load('~/Work/dataset1.Rdata')

# define server logic required to draw the time series
shinyServer(function(input, output) {
  rv <-
    reactiveValues(
      start_date_time = min(system_data$date.time),
      old_start_date_time = min(system_data$date.time),
      end_date_time = max(system_data$date.time),
      old_end_date_time = max(system_data$date.time)
      # ,
      # y_log_axis = TRUE
    )
  
  plot_time_series <-
    function() {
      print("=====================================")
      print(paste0("Start date : ", rv$start_date_time))
      print(paste0("End date   : ", rv$end_date_time))
      
      system_data %>%
        filter(date.time >= rv$start_date_time &
                 date.time <= rv$end_date_time) %>%
        ggplot(aes_string(x = "date.time", y = input$time_series_name)) +
        geom_point() ->
        gg
      
      gg
      
      # if (rv$y_log_axis)
      #   gg + scale_y_log10()
      # else
      #   gg + scale_y_continuous()
    }

  observeEvent(input$date_range,
               {
                 print("Date range")
                 rv$start_date_time <-
                   as.POSIXct(input$date_range[1])
                 rv$end_date_time <-
                   as.POSIXct(input$date_range[2])
               })
  
  observeEvent(input$plus_hour,
               {
                 print("+1 hour")
                 rv$start_date_time <-
                   rv$start_date_time + 60 * 60
                 rv$end_date_time <-
                   rv$end_date_time + 60 * 60
               })
  
  observeEvent(input$plus_day,
               {
                 print("+1 day")
                 rv$start_date_time <-
                   rv$start_date_time + 24 * 60 * 60
                 rv$end_date_time <-
                   rv$end_date_time + 24 * 60 * 60
               })
  
  observeEvent(input$plus_week,
               {
                 print("+1 week")
                 rv$start_date_time <-
                   rv$start_date_time + 7 * 24 * 60 * 60
                 rv$end_date_time <-
                   rv$end_date_time + 7 * 24 * 60 * 60
               })
  
  observeEvent(input$plot_brush,
               {
                 print("Brush")
                 rv$old_start_date_time <-
                   rv$start_date_time
                 rv$start_date_time <-
                   as.POSIXct(as.integer(input$plot_brush$xmin), origin = "1970-01-01")
                 
                 rv$old_end_date_time <-
                   rv$end_date_time
                 rv$end_date_time <-
                   as.POSIXct(as.integer(input$plot_brush$xmax), origin = "1970-01-01")
               })
  
  observeEvent(input$unzoom,
               {
                 print("Unzoom")
                 rv$start_date_time <-
                   rv$old_start_date_time
                 rv$end_date_time <-
                   rv$old_end_date_time
               })
  
  observeEvent(input$dbl_click, {
    str(input$dbl_click)
    print(paste0(
      "Anomaly in ",
      input$time_series_name,
      " at ",
      as.POSIXct(as.integer(input$dbl_click$x), origin = "1970-01-01"),
      " labelled: >",
      isolate(input$annotation_text),
      "<"
    ))
  })

  
  # observeEvent(input$toggle_log_scale,
  #              {
  #                print("Toggle Log Scale")
  #                rv$y_log_axis <-
  #                  !rv$y_log_axis
  #              })
  
  output$time_series_plot <- renderPlot({
    # str(input$date_range)
    # str(input$dbl_click)
    # str(input$plot_brush)
    # str(input$time_series_name)
    
    plot_time_series()
  })
})
