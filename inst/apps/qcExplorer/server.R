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
  reactive_sds <- reactive({1
    file_data$sds()
  })

  selectedAnno <- reactive({
    input$anno1
  })

  selectedQcMetrics <- reactive({
    input$qcMetr1
  })

  observeEvent(c(input$loadSampleData, input$runUploadedFiles), {
    if ((!is.null(input$annotationsFile) && (input$runUploadedFiles>0))||(input$loadSampleData>=0)) {
      if(input$loadSampleData==0 && input$runUploadedFiles==0){
        sampleDataPath <- "./data"
        file_data$bamQcMetr <- read.csv(file.path(sampleDataPath, "bamQcMetr.tsv"), sep = '\t')
        file_data$annotations <- read.csv(file.path(sampleDataPath, "sampleAnnotations.tsv"), sep = '\t')
        file_data$vcfQcMetr <- read.csv(file.path(sampleDataPath, "vcfQcMetr.tsv"), sep = '\t')
        file_data$samplePc <- read.csv(file.path(sampleDataPath, "samplePCs.tsv"), sep = '\t')
        file_data$refPC <- read.csv(file.path(sampleDataPath, "refPCs.tsv"), sep = '\t')
        file_data$loadedFiles <- c("bamQcMetr.tsv", "sampleAnnotations.tsv", "vcfQcMetr.tsv", "samplePCs.tsv", "refPCs.tsv")
      }
      else if (input$loadSampleData>0){
        sampleDataPath <- "./data"
        file_data$bamQcMetr <- read.csv(file.path(sampleDataPath, "bamQcMetr.tsv"), sep = '\t')
        file_data$annotations <- read.csv(file.path(sampleDataPath, "sampleAnnotations.tsv"), sep = '\t')
        file_data$vcfQcMetr <- read.csv(file.path(sampleDataPath, "vcfQcMetr.tsv"), sep = '\t')
        file_data$samplePc <- read.csv(file.path(sampleDataPath, "samplePCs.tsv"), sep = '\t')
        file_data$refPC <- read.csv(file.path(sampleDataPath, "refPCs.tsv"), sep = '\t')
        file_data$loadedFiles <- c("bamQcMetr.tsv", "sampleAnnotations.tsv", "vcfQcMetr.tsv", "samplePCs.tsv", "refPCs.tsv")
      }
      else{
        if (!is.null(input$samplePCsFile)) {
          file_data$samplePc <- read.csv(input$samplePCsFile$datapath, sep = '\t')
        }
        if (!is.null(input$refPCsFile)) {
          file_data$refPC <- read.csv(input$refPCsFile$datapath, sep = '\t')
        }
        if (!is.null(input$vcfQcMetrFile)) {
          file_data$vcfQcMetr <- read.csv(input$vcfQcMetrFile$datapath, sep = '\t')
        }
        if (!is.null(input$bamQcMetrFile)) {
          file_data$bamQcMetr <- read.csv(input$bamQcMetrFile$datapath, sep = '\t')
        }
        file_data$annotations <- read.csv(input$annotationsFile$datapath, sep = '\t', row.names = NULL)
        # file_data_list <- list()
        # if (!is.null(input$samplePCsFile)) {
        #   file_data_list$samplePc <- read.csv(input$samplePCsFile$datapath, sep = '\t')
        #   file_data_list$samplePCsColNames <- names(file_data_list$samplePc)
        # }

        # if (!is.null(input$refPCsFile)) {
        #   file_data_list$refPC <- read.csv(input$refPCsFile$datapath, sep = '\t')
        # }

        # if (!is.null(input$vcfQcMetrFile)) {
        #   file_data_list$vcfQcMetr <- read.csv(input$vcfQcMetrFile$datapath, sep = '\t')
        #   file_data_list$vcfQcColNames <- names(file_data_list$vcfQcMetr)
        # }

        # if (!is.null(input$bamQcMetrFile)) {
        #   file_data_list$bamQcMetr <- read.csv(input$bamQcMetrFile$datapath, sep = '\t')
        #   file_data_list$bamQcColNames <- names(file_data_list$bamQcMetr)
        # }

        # file_data_list$annotations <- read.csv(input$annotationsFile$datapath, sep = '\t', row.names = NULL)
        # file_data_list$annotColNames <- names(file_data_list$annotations)
        # colNameLists <- list(file_data_list$annotColNames, file_data_list$bamQcColNames, file_data_list$vcfQcColNames)
        # primaryID <- Reduce(intersect, colNameLists)
      }
      file_data$sds <- sampleDataset(
        annotInput = file_data$annotations,
        bamQcInput = file_data$bamQcMetr,
        vcfQcInput = file_data$vcfQcMetr,
        primaryID = input$primaryID
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
            outliers = input$outliers, primaryID = file_data$sds$primaryID)
        })

      }else{
        #qca correlation
        output$pca <- renderPlot({
          samplyzer:::scatter(
            data =  file_data$sds$df, x = input$qcMetr1, y = input$qcMetr2, strat = input$anno1,
            outliers = input$outliers, primaryID = file_data$sds$primaryID)
        })
      }


      # # 动态生成和显示 plotList 中的图形
      # plotList <- lapply(file_data$sds$annot, function(annot) {
      #   tryCatch({
      #     # 尝试生成图表
      #     plot = sampleQcPlot(sds = file_data$sds, qcMetrics = file_data$sds$bamQcMetr, annot = annot, geom = 'scatter', ncols = 2)
      #     return(plot)
      #   }, error = function(e) {
      #     message("Error in generating plot for annotation ", annot, ": ", e$message)
      #     return(NULL)
      #   })
      # })
      # plotList <- Filter(Negate(is.null), plotList)
      # output$dynamic_plots <- renderUI({
      #   plot_output_list <- lapply(1:length(plotList), function(i) {
      #     plotname <- paste("plot", i, sep="")
      #     print(plotname)
      #     plotOutput(plotname, height = "300px")
      #   })
      #   do.call(tagList, plot_output_list)
      # })

      # # 为每个图形创建一个 renderPlot 输出
      # for (i in 1:length(plotList)) {
      #   local({
      #     my_i <- i
      #     plotname <- paste("plot", my_i, sep="")
      #     output[[plotname]] <- renderPlot({
      #       grid.draw(plotList[[my_i]])
      #     })
      #   })
      # }

    }
  })
  output$loadedFileList <- renderUI({
    if (!is.null(file_data$loadedFiles)) {
      do.call(tagList, lapply(file_data$loadedFiles, function(name) {
        tags$p(name)
      }))
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
})
