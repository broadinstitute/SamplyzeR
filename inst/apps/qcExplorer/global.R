if (!require("shiny")) install.packages("shiny")
if (!require("samplyzer")) install.packages("samplyzer")
if (!require("shinythemes")) install.packages("shinythemes")
library(shiny)
library(ggplot2)
library(prettyGraphs)
library(grDevices)
library(RColorBrewer)
library(samplyzer)
library(gdata)
library(shinythemes)

subsetTable <- function(input, sds, id){
  # sort data frame
  qc_tag = input[[paste('qcMetrics', id, sep = '')]]
  max_sample_id <- max(as.numeric(sub("Sample-", "", sds$df$SampleID)), na.rm = TRUE)
  max_qc_metric_value <- max(sds$df[, input$qcMetr1], na.rm = TRUE)
  # filter table to brush area
  brush = input[[paste('plot_brush', id, sep = '')]]
  actual_xmin = brush$xmin * max_sample_id
  actual_xmax = brush$xmax * max_sample_id
  actual_ymin = brush$ymin * max_qc_metric_value
  actual_ymax = brush$ymax * max_qc_metric_value
  tab <- sds$df[
    sds$df[, input$qcMetr1] > actual_ymin &
      sds$df[, input$qcMetr1] < actual_ymax &
      as.numeric(sub("Sample-", "", sds$df$SampleID)) > actual_xmin &
      as.numeric(sub("Sample-", "", sds$df$SampleID)) < actual_xmax,
  ]
  return(tab)
}

getColor<- function(n) {
  if (n < 7) {
    col = palette(rainbow(n))
  } else {
    col = prettyGraphs::prettyGraphsColorSelection(
      n.colors=n, offset = 10, starting.color = 210)
  }
  return(col)
}

source("ui.R")
source("server.R")
#shinyApp(ui, server)
shiny::shinyApp(ui, server, options = list(launch.browser = TRUE), "https://xlab.shinyapps.io/samplyzer/")
