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
  # input$date_range
  
  output$time_series_plot <- renderPlot({
    str(input$date_range)
    str(input$dbl_click)
    str(input$plot_brush)
    str(input$time_series_name)
    
    if (!is.null(input$dbl_click)) {
      # note anomaly
      print(paste0("Anomaly at ", input$dbl_click))
      return()
    }
    
    # determine start and end date time ranges
    start_datetime <- min(system_data$date.time)
    end_datetime <- max(system_data$date.time)
    
    if (is.list(input$plot_brush)) {
      print("Brush area selected")
      start_date_time <-
        as.POSIXct(as.integer(input$plot_brush$xmin), origin = "1970-01-01")
      end_date_time <-
        as.POSIXct(as.integer(input$plot_brush$xmax), origin = "1970-01-01")
    } else if (!is.null(input$date_range)) {
      print("Date range selected")
      start_date_time <-
        as.POSIXct(input$date_range[1])
      end_date_time <-
        as.POSIXct(input$date_range[2])
    }
    
    print(paste0("Start date : ", start_date_time))
    print(paste0("End date   : ", end_date_time))
    # Plot the data
    system_data %>%
      filter(date.time >= start_date_time &
               date.time <= end_date_time) %>%
      ggplot(aes_string(x = "date.time", y = input$time_series_name)) +
      geom_point()
  })
  
})
