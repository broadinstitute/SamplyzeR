# UI
ui <- shinyUI(pageWithSidebar(
  headerPanel("Sample Explorer"),
  sidebarPanel(
    tabsetPanel(
      tabPanel("Parameters",
               br(),
               selectInput("anno1", 'Subject Attributes', choices = NULL),
               textInput("outliers", "sample IDs", "Sample-001"),
               selectInput("qcMetr1", 'QC scatter plot - metricX', choices = NULL),
               selectInput('qcMetr2', 'QC scatter plot - metricY', choices = NULL),
               selectInput('PCx', 'PC scatter plot - X', choices = NULL),
               selectInput('PCy', 'PC scatter plot - Y', choices = NULL),
      ),
      tabPanel("Upload Files",
               br(),
               h4("Required Files"),
               fileInput("annotationsFile", "Upload sample annotations", accept = c('text/csv', 'text/comma-separated-values,text/plain')),
               fileInput("bamQcMetrFile", "Upload sample QC table", accept = c('text/csv', 'text/comma-separated-values,text/plain')),
               tagList(
                 div(id = "fileInputContainer",
                     h4("Optional Files"),
                     fileInput("samplePCsFile", "Upload samplePCs", accept = c('text/csv', 'text/comma-separated-values,text/plain')),
                     fileInput("refPCsFile", "Upload refPCs", accept = c('text/csv', 'text/comma-separated-values,text/plain')),
                     fileInput("vcfQcMetrFile", "Upload other Qc table", accept = c('text/csv', 'text/comma-separated-values,text/plain'))
                 )
               ),
               #actionButton("addFile", "Add File"),
               actionButton("runUploadedFiles", "Run Uploaded Files"),
      ),
      tabPanel("Example Files",
               br(),
               fluidRow(
                 column(4, h4("Annotations File:", style = "display: inline; padding-right: 10px;")),
                 column(4, downloadLink("downloadLink1", "sampleAnnotations.tsv", style = "display: inline"))
               ),
               br(),
               fluidRow(
                 column(4, h4("Sample QC File:", style = "display: inline; padding-right: 10px;")),
                 column(4, downloadLink("downloadLink2", "bamQcMetr.tsv", style = "display: inline"))
               ),
               br(),
               fluidRow(
                 column(4, h4("Sample PCs File:", style = "display: inline; padding-right: 10px;")),
                 column(4, downloadLink("downloadLink3", "samplePCs.tsv", style = "display: inline"))
               ),
               br(),
               fluidRow(
                 column(4, h4("Other PCs File:", style = "display: inline; padding-right: 10px;")),
                 column(4, downloadLink("downloadLink4", "refPCs.tsv", style = "display: inline"))
               ),
               br(),
               fluidRow(
                 column(4, h4("Other QC File:", style = "display: inline; padding-right: 10px;")),
                 column(4, downloadLink("downloadLink5", "vcfQcMetr.tsv", style = "display: inline"))
               ),
               br(),
               actionButton("loadSampleData", "Load Example Data"),
               #uiOutput("loadedFileList")
      )
    )
  ),
  mainPanel(
    fluidRow(
      column(6,
             plotOutput("plot1",brush = brushOpts(id = "plot_brush1")),
             plotOutput("pca")
      ),
      column(6,
             plotOutput("plot2"),
             plotOutput("qcCorr")
      )
    ),
    fluidRow(
      column(12,
             tableOutput("table1")
      )
    )
  )
))
