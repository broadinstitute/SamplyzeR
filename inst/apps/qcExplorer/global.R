library(shiny)
library(ggplot2)
library(prettyGraphs)
library(grDevices)
library(RColorBrewer)
library(SamplyzeR)
library(gdata)

getColor<- function(n) {
  if (n < 7) {
    col = palette(rainbow(n))
  } else {
    col = prettyGraphs::prettyGraphsColorSelection(
      n.colors=n, offset = 10, starting.color = 210)
  }
  return(col)
}


subsetTable <- function(input, sds, id){
  # sort data frame
  anno_tag = input[[paste('anno', id, sep='')]]
  qc_tag = input[[paste('qcMetrics', id, sep = '')]]
  sds$df = sds$df[order(sds$df[anno_tag], na.last = T), ] # sort data frame
  sds$df$index = 1:dim(sds$df[qc_tag])[1] # create index

  # filter table to brush area
  brush = input[[paste('plot_brush', id, sep = '')]]
  tab = sds$df[
    sds$df[, input$qcMetrics1] > brush$ymin &
      sds$df[, input$qcMetrics1] < brush$ymax &
      sds$df$index > brush$xmin &
      sds$df$index < brush$xmax, ]
  return(tab)
}

shinyApp(ui, server)

plot(sampleQcPlot(
  sds, annotation = 'SeqProject',
  qcMetrics = c('Mean_Coverage', 'Contamination_Estimation'),
  geom='scatter', legend=F #, outliers = input$outliers
))
