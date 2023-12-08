# UI
ui <- shinyUI(pageWithSidebar(
  headerPanel("Sample Explorer"),
  sidebarPanel(
    tabsetPanel(
      tabPanel("Upload Files",
               fileInput("bamQcMetrFile", "Upload sample QC table", accept = c('text/csv', 'text/comma-separated-values,text/plain')),
               fileInput("annotationsFile", "Upload sample annotations", accept = c('text/csv', 'text/comma-separated-values,text/plain')),
               fileInput("samplePCsFile", "Upload samplePCs", accept = c('text/csv', 'text/comma-separated-values,text/plain')),
               fileInput("refPCsFile", "Upload refPCs", accept = c('text/csv', 'text/comma-separated-values,text/plain')),
               fileInput("vcfQcMetrFile", "Upload vcfQcMetc", accept = c('text/csv', 'text/comma-separated-values,text/plain'))
      ),
      tabPanel("Parameters",
               selectInput("anno1", 'Subject Attributes', choices = NULL),
               textInput("outliers", "sample IDs", "Sample-001"),
               selectInput("qcMetr1", 'QC metrics', choices = NULL),
               selectInput('qcMetr2', 'QC metrics 2', choices = NULL),
               selectInput('PCx', 'first PC', paste('PC', 1:10, sep = '')),
               selectInput('PCy', 'second PC', paste('PC', 1:10, sep = ''))
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
