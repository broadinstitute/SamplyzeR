server <- function(input, output) {
  values <- reactiveValues(df_data = NULL, outliers = NULL)
  observeEvent(input$plot_brush1, {
    values$df_data = subsetTable(input, sds, 1)
    values$outliers = values$df_data$sampleId
  })

  # QC scatterplot
  output$plot1 <- renderPlot({
    plot(sampleQcPlot(
      sds, annotation = input$anno1, qcMetrics = input$qcMetrics1,
      geom='scatter' #, outliers = input$outliers
    ))
  })

  # QC violin plot
  output$violin <- renderPlot({
    plot(sampleQcPlot(sds, qcMetrics = input$qcMetrics1, geom = 'violin',
                      annotation = input$anno1)) # outliers = input$outliers
  })

  # QC metrics correlation
  output$qcCorr <- renderPlot({
    samplyzer::scatter(
      data = subsetTable(input, sds, 1), x = input$qcMetr1, y = input$qcMetr2,
      strat = input$attr, primaryID = sds$primaryID)
  })

  # pca correlation
  output$pca <- renderPlot({
    samplyzer::scatter(
      data = sds$df, x = input$PCx, y = input$PCy, strat = input$attr2,
      outliers = input$outliers, primaryID = sds$primaryID)
  })

  # render outlier info
  output$table1 <- renderTable(values$df_data)
}

