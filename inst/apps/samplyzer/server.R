# Server
server <- shinyServer(function(input, output, session) {
  values <- reactiveValues(dataLoaded = FALSE, sampleDataClicked = FALSE, uploadedFilesClicked = FALSE)
  observeEvent(input$loadSampleData, {
    values$sampleDataClicked <- TRUE
    processData(
      file.path("./data/bamQcMetr.tsv"),
      file.path("./data/sampleAnnotations.tsv"),
      file.path("./data/vcfQcMetr.tsv"),
      file.path("./data/samplePCs.tsv"),
      file.path("./data/refPCs.tsv")
    )
  })
  observeEvent(input$runUploadedFiles, {
    values$uploadedFilesClicked <- TRUE
    processData(
      input$bamQcMetrFile$datapath,
      input$annotationsFile$datapath,
      input$vcfQcMetrFile$datapath,
      input$samplePCsFile$datapath,
      input$refPCsFile$datapath
    )
  })
  observe({
    # default
    if (!values$sampleDataClicked && !values$uploadedFilesClicked && !values$dataLoaded) {
      processData(
        file.path("./data/bamQcMetr.tsv"),
        file.path("./data/sampleAnnotations.tsv"),
        file.path("./data/vcfQcMetr.tsv"),
        file.path("./data/samplePCs.tsv"),
        file.path("./data/refPCs.tsv")
      )
      values$dataLoaded <- TRUE
    }
  })
  ## example
  output$downloadLink1 <- downloadHandler(
    filename = function() {
      "sampleAnnotations.tsv"  # Replace with your actual file name
    },
    content = function(file) {
      file.copy("./data/sampleAnnotations.tsv", file)
    }
  )
  output$downloadLink2 <- downloadHandler(
    filename = function() {
      "bamQcMetr.tsv"  # Replace with your actual file name
    },
    content = function(file) {
      file.copy("./data/bamQcMetr.tsv", file)
    }
  )
  output$downloadLink3 <- downloadHandler(
    filename = function() {
      "samplePCs.tsv"  # Replace with your actual file name
    },
    content = function(file) {
      file.copy("./data/samplePCs.tsv", file)
    }
  )
  output$downloadLink4 <- downloadHandler(
    filename = function() {
      "refPCs.tsv"  # Replace with your actual file name
    },
    content = function(file) {
      file.copy("./data/refPCs.tsv", file)
    }
  )
  output$downloadLink5 <- downloadHandler(
    filename = function() {
      "vcfQcMetr.tsv"  # Replace with your actual file name
    },
    content = function(file) {
      file.copy("./data/vcfQcMetr.tsv", file)
    }
  )
  ## inner function
  processData <- function(bamQcMetrPath = NULL, annotationsPath, vcfQcMetrPath  = NULL, samplePcPath = NULL,  refPCPath = NULL) {
    file_data <- reactiveValues(
      bamQcMetr = NULL,
      annotations = NULL,
      vcfQcMetr = NULL,
      samplePc = NULL,
      refPC = NULL,
      sds = NULL,
    )
    reactive_sds <- reactive({1
      file_data$sds()
    })

    selectedAnno <- reactive({
      input$anno1
    })

    selectedAnno2 <- reactive({
      input$anno2
    })

    selectedCol <- reactive({
      input$ncol
    })

    selectedQcMetrics <- reactive({
      input$qcMetr1
    })

    selectedOutliers <- reactive({
      input$outliers
    })

    if (!is.null(samplePcPath)) {
      file_data$samplePc <- read.csv(samplePcPath, sep = '\t')
    }
    if (!is.null(refPCPath)) {
      file_data$refPC <- read.csv(refPCPath, sep = '\t')
    }
    if (!is.null(vcfQcMetrPath)) {
      file_data$vcfQcMetr <- read.csv(vcfQcMetrPath, sep = '\t')
    }
    if (!is.null(bamQcMetrPath)) {
      file_data$bamQcMetr <- read.csv(bamQcMetrPath, sep = '\t')
    }
    file_data$annotations <- read.csv(annotationsPath, sep = '\t', row.names = NULL)

    file_data$sds <- sampleDataset(
      annotInput = file_data$annotations,
      bamQcInput = file_data$bamQcMetr,
      vcfQcInput = file_data$vcfQcMetr,
      primaryID = input$primaryID
    )
    updateSelectInput(session, "qcMetr1", choices = unique(file_data$sds$qcMetrics))
    updateSelectInput(session, "qcMetr2", choices = unique(file_data$sds$qcMetrics))
    updateSelectInput(session, "anno1", choices = unique(file_data$sds$annot))
    updateSelectInput(session, "anno2", choices = unique(file_data$sds$annot))
    updateSelectInput(session, "outliers", choices = unique(file_data$sds$df[[input$primaryID]]))

    # sds data
    output$table2 <- renderDataTable({
      DT::datatable(file_data$sds$df, options = list(pageLength = 10))
    })

    # QC scatterplot
    output$plot1 <- renderPlot({
      sampleQcPlot(
        file_data$sds, annot= selectedAnno(), qcMetrics = selectedQcMetrics(),
        geom= "scatter", outliers = selectedOutliers(), show = T)
    })

    # QC violin plot
    output$plot2 <- renderPlot({
      sampleQcPlot(
        file_data$sds, annot= selectedAnno(), qcMetrics = selectedQcMetrics(),
        geom= "violin", outliers = selectedOutliers(), show = T)
    })

    # panel plot
    output$multPlot <- renderPlot({
      ncols <- as.numeric(selectedCol())
      plot <- sampleQcPlot(
        file_data$sds, qcMetrics = file_data$sds$bamQcMetr, geom = 'scatter',
        annot = selectedAnno2(), ncols = ncols)
      grid.newpage()
      grid.draw(plot)
    })

    #plot_brush
    observeEvent(input$plot_brush1, {
      sds_subset = subsetTable(input, file_data$sds, 1)
      # QC metrics correlation
      output$qcCorr <- renderPlot({
        samplyzer:::scatter(
          data = sds_subset, x = input$qcMetr1, y = input$qcMetr2, strat = input$anno1,
          outliers = input$outliers, primaryID = file_data$sds$primaryID
        )})

      #output$table1 <- renderTable(sds_subset)
      output$table1 <- renderDataTable({
        DT::datatable(sds_subset, options = list(pageLength = 15))
      })

    })
    if(!is.null(file_data$samplePc)){
      file_data$sds = setAttr(file_data$sds, attributes = 'PC', data = file_data$samplePc, primaryID = input$primaryID)
      if(!is.null(file_data$refPC)){
        file_data$sds <- inferAncestry(
          file_data$sds,
          trainSet = file_data$refPC[, grep("^PC", names(file_data$refPC))],
          knownAncestry =  file_data$refPC$group,
        )
      }
      updateSelectInput(session, "PCx", choices = unique(file_data$sds$PC))
      updateSelectInput(session, "PCy", choices = unique(file_data$sds$PC))

      # pca correlation
      output$pca <- renderPlot({
        samplyzer:::scatter(
          data = file_data$sds$df, x = input$PCx, y = input$PCy, strat = input$anno1,
          outliers = selectedOutliers(), primaryID = file_data$sds$primaryID)
      })

    }else{
      #qca correlation
      output$pca <- renderPlot({
        samplyzer:::scatter(
          data =  file_data$sds$df, x = input$qcMetr1, y = input$qcMetr2, strat = input$anno1,
          outliers = selectedOutliers(), primaryID = file_data$sds$primaryID)
      })
    }

  }
})
