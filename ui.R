# Shuny UI design for Time Series visualiastion and annotation
#
# Version 0.2 - Gary Mulder - 03/11/2016

# 1. choose a time series
# 2. double click to annotate with text
# 3. show annotations for the chosen time series
# 4. move forward an hour / day / week / month

# TODO:
#
#  . log or linear scale
#  . choose two time series
#  . colour with a function (e.g. is.na())
#  . geom_point or geom_line

library(shiny)

shinyUI(fluidPage(
  titlePanel("ROnan - manual exploration of time series"),
  
  # The ggplot visualisation area
  plotOutput(
    outputId = "time_series_plot",
    dblclick = "dbl_click",
    brush = brushOpts(
      id = "plot_brush",
      direction = "x",
      resetOnNew = TRUE
    )
  ),
  
  hr(),
  
  # We display the plots above the controls to maximise horizontal viewing area
  fluidRow(
    column(6,
      dateRangeInput(
        inputId = "date_range",
        label = "Specify a date range, or Click + Drag an area of the plot",
        start = "2016-01-01",
        end = "2016-12-31"
      ),
      actionButton(inputId = "minus_hour",
                   label = "-1 Hour"),
      actionButton(inputId = "minus_day",
                   label = "-1 Day"),
      actionButton(inputId = "minus_week",
                   label = "-1 Week"),
      actionButton(inputId = "plus_hour",
                   label = "+1 Hour"),
      actionButton(inputId = "plus_day",
                   label = "+1 Day"),
      actionButton(inputId = "plus_week",
                   label = "+1 Week"),
      actionButton(inputId = "unzoom",
                   label = "Unzoom"),
      # actionButton(inputId ="toggle_log_scale",
      #              label = "Toggle Log Scale"),
      selectInput(
        inputId = "time_series_name",
        label = "Time Series",
        choices = colnames(ts_df[-1])
      ),
      selectInput(
        inputId = "annotation_text",
        label = "Annotation Text",
        choices = c(
          "s_anom",
          "e_anom",
          "s_load",
          "e_load",
          "s_inc",
          "e_inc",
          "s_miss",
          "e_miss"
        )
      ),
      actionButton(inputId = "load_annotations",
                   label = "Load Annotations"),
      actionButton(inputId = "save_annotations",
                   label = "Save Annotations")
    )
  )
))
