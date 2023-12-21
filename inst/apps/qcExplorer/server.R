# Server
server <- shinyServer(function(input, output, session) {
  file_data <- reactiveValues(
    bamQcMetr = NULL,
    annotations = NULL,
    vcfQcMetr = NULL,
    samplePc = NULL,
    refpc = NULL,
    sds = NULL,
    loadedFiles = NULL
  )
  reactive_sds <- reactive({
    file_data$sds()
  })

  selectedAnno <- reactive({
    input$anno1
  })

  selectedQcMetrics <- reactive({
    input$qcMetr1
  })

  observeEvent(c(input$loadSampleData, input$bamQcMetrFile, input$annotationsFile, input$vcfQcMetrFile), {
    if ((!is.null(input$bamQcMetrFile) && !is.null(input$annotationsFile) && !is.null(input$vcfQcMetrFile))||(input$loadSampleData>0))  {
      if (input$loadSampleData>0){
        sampleDataPath <- "../../../vignettes/data"
        file_data$bamQcMetr <- read.csv(file.path(sampleDataPath, "bamQcMetr.tsv"), sep = '\t')
        file_data$annotations <- read.csv(file.path(sampleDataPath, "sampleAnnotations.tsv"), sep = '\t')
        file_data$vcfQcMetr <- read.csv(file.path(sampleDataPath, "vcfQcMetr.tsv"), sep = '\t')
        file_data$samplePc <- read.csv(file.path(sampleDataPath, "samplePCs.tsv"), sep = '\t')
        file_data$refPC <- read.csv(file.path(sampleDataPath, "refPCs.tsv"), sep = '\t')
        file_data$loadedFiles <- c("bamQcMetr.tsv", "sampleAnnotations.tsv", "vcfQcMetr.tsv", "samplePCs.tsv", "refPCs.tsv")
        output$loadedBamQcMetrFile <- renderText("bamQcMetr.tsv")
        output$loadedAnnotationsFile <- renderText("sampleAnnotations.tsv")
      }
      else{
        if (!is.null(input$samplePCsFile)) {
          file_data$samplePc <- read.csv(input$samplePCsFile$datapath, sep = '\t')
        }
        if (!is.null(input$refPCsFile)) {
          file_data$refPC <- read.csv(input$refPCsFile$datapath, sep = '\t')
        }
        file_data$bamQcMetr <- read.csv(input$bamQcMetrFile$datapath, sep = '\t', row.names = NULL)
        file_data$annotations <- read.csv(input$annotationsFile$datapath, sep = '\t', row.names = NULL)
        file_data$vcfQcMetr <- read.csv(input$vcfQcMetrFile$datapath, sep = '\t', row.names = NULL)
      }
      file_data$sds <- sampleDataset(
        bamQcInput = file_data$bamQcMetr,
        vcfQcInput = file_data$vcfQcMetr,
        annotInput = file_data$annotations,
        primaryID = 'SampleID'
      )
      updateSelectInput(session, "qcMetr1", choices = unique(file_data$sds$qcMetrics))
      updateSelectInput(session, "qcMetr2", choices = unique(file_data$sds$qcMetrics))
      updateSelectInput(session, "anno1", choices = unique(file_data$sds$annot))

      # QC scatterplot
      output$plot1 <- renderPlot({
        sampleQcPlot(
          file_data$sds, annot= selectedAnno(), qcMetrics = selectedQcMetrics(),
          geom= "scatter", outliers = input$outliers, show = T)
      })
      # QC violin plot
      output$plot2 <- renderPlot({
        sampleQcPlot(
          file_data$sds, annot= selectedAnno(), qcMetrics = selectedQcMetrics(),
          geom= "violin", outliers = input$outliers, show = T)
      })
      file_data$sds = setAttr(file_data$sds, attributes = 'PC', data = file_data$samplePc, primaryID = 'SampleID')
      file_data$sds <- inferAncestry(
        file_data$sds,
        trainSet = file_data$refPC[, grep("^PC", names(file_data$refPC))],
        knownAncestry =  file_data$refPC$group,
      )

      # pca correlation
      output$pca <- renderPlot({
        samplyzer:::scatter(
          data = file_data$sds$df, x = input$PCx, y = input$PCy, strat = input$anno1,
          outliers = input$outliers, primaryID = file_data$sds$primaryID)
      })

      observeEvent(input$plot_brush1, {
        sds_subset = subsetTable(input, file_data$sds, 1)
        # QC metrics correlation
        output$qcCorr <- renderPlot({
          samplyzer:::scatter(
            data = sds_subset, x = input$qcMetr1, y = input$qcMetr2, strat = input$anno1,
            outliers = input$outliers, primaryID = file_data$sds$primaryID
          )})

        output$table1 <- renderTable(sds_subset)
      })
    }
  })
  output$loadedFileList <- renderUI({
    if (!is.null(file_data$loadedFiles)) {
      do.call(tagList, lapply(file_data$loadedFiles, function(name) {
        tags$p(name)
      }))
    }
  })
})
