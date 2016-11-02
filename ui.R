#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("ROnan - manual exploration of time series"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(sidebarPanel(
    dateRangeInput(
      inputId = "date_range",
      label = "Specify a date range, or Click + Drag an area of the plot",
      start = "2016-01-01",
      end = "2016-12-31"
    ),
    # actionButton(inputId = "redraw",
    #              label = "Redraw"),
    actionButton(inputId = "plus_hour",
                 label = "+1 Hour"),
    actionButton(inputId = "plus_day",
                 label = "+1 Day"),
    actionButton(inputId = "plus_week",
                 label = "+1 Week"),
    actionButton(inputId ="unzoom",
                 label = "Unzoom"),
    # actionButton(inputId ="toggle_log_scale",
    #              label = "Toggle Log Scale"),
    selectInput(inputId = "time_series_name",
                label = "Time Series",
                choices = colnames(system_data)),
    textInput(inputId = "annotation_text",
              label = "Annotation",
              value = "anomaly")
  ),
  
  # 1. choose a time series
  # 2. double click to annotate with text
  # 3. show annotations for the chosen time series
  # 4. move forward an hour / day / week / month
  
  #  . log or linear scale
  #  . choose two time series
  #  . colour with a function (e.g. is.na())
  #  . geom_point or geom_line
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput(
      outputId = "time_series_plot",
      dblclick = "dbl_click",
      brush = brushOpts(
        id = "plot_brush",
        direction = "x",
        resetOnNew = TRUE
      )
    )
  ))
))
