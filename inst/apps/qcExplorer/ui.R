library(shiny)

ui <- shinyUI(fluidPage(
  theme = shinytheme("cerulean"),#cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, readable, simplex, slate, spacelab, superhero, united, yeti.
  tags$head(
    tags$style(HTML('
          .header-container {
              background-color: #2176ae; /* 背景颜色为蓝色 */
              padding: 20px 40px; /* 内边距 */
              text-align: center; /* 文字居中 */
              border-bottom: 5px solid #1d5d90; /* 底部边框，更深的蓝色 */
              box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1); /* 阴影效果 */
          }
          .header-title {
              margin: 0;
              color: #fff !important;
              font-size: 30px;
              font-weight: bold;
              letter-spacing: 1px; /* 字符间距 */
          }
          .header-subtitle {
              color: #fff !important;
              margin-top: 5px;
              font-size: 18px;
              font-weight: normal;
          }
          .v-center{
              display: flex;
              align-items: center;
              justify-content: center;
              height: 100%;
          }
      '))
  ),
  # Header layout
  div(class = 'header-container',
      h1(class = 'header-title', 'Samplyzer Browser'),
      h2(class = 'header-subtitle', ' Sample Exployer')
  ),
  br(),
  sidebarPanel(
    h3("Upload Files"),
    h4("Required Files"),
    fluidRow(
      column(8, fileInput("annotationsFile", "Upload Sample Annotations", accept = c('text/csv', 'text/comma-separated-values,text/plain')), class = "full-height"),
      column(4, div(class = "v-center", downloadLink("downloadLink1", "Download Sample Annotations")),
             style = "display: flex; height: 80px;")
    ),
    fluidRow(
      column(8, fileInput("bamQcMetrFile", "Upload Sample QC Table*", accept = c('text/csv', 'text/comma-separated-values,text/plain'))),
      column(4, div(class = "v-center", downloadLink("downloadLink2", "Download Sample QC")),
             style = "display: flex; height: 80px;")
    ),
    h4("Optional Files"),
    fluidRow(
      column(8, fileInput("samplePCsFile", "Upload Sample PCs ", accept = c('text/csv', 'text/comma-separated-values,text/plain'))),
      column(4, div(class = "v-center", downloadLink("downloadLink3", "Download Sample PCs File")),
             style = "display: flex; height: 80px;")
    ),
    fluidRow(
      column(8, fileInput("refPCsFile", "Upload Ref PCs ", accept = c('text/csv', 'text/comma-separated-values,text/plain'))),
      column(4, div(class = "v-center", downloadLink("downloadLink4", "Download Ref PCs File")),
             style = "display: flex; height: 80px;")
    ),
    fluidRow(
      column(8, fileInput("vcfQcMetrFile", "Upload Other QC Table ", accept = c('text/csv', 'text/comma-separated-values,text/plain'))),
      column(4, div(class = "v-center", downloadLink("downloadLink5", "Download Other QC File")),
             style = "display: flex; height: 80px;")
    ),
    actionButton("runUploadedFiles", "Run Uploaded File"),
    actionButton("loadSampleData", "Load Example Data")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Tab 1 Title",br(),
               fluidRow(
                 column(2, selectInput("anno1", 'Subject Attributes', choices = NULL)),
                 column(2, textInput("outliers", "Sample IDs", "Sample-001")),
                 column(2, selectInput("qcMetr1", 'QC Scatter Plot - MetricX', choices = NULL)),
                 column(2, selectInput('qcMetr2', 'QC Scatter Plot - MetricY', choices = NULL)),
                 column(2, selectInput('PCx', 'PC Scatter Plot - X', choices = NULL)),
                 column(2, selectInput('PCy', 'PC Scatter Plot - Y', choices = NULL)),
                 column(2, textInput("primaryID", "primaryID", "SampleID")),
               ),
               fluidRow(
                 column(6, plotOutput("plot1", brush = brushOpts(id = "plot_brush1")), plotOutput("pca")),
                 column(6, plotOutput("plot2"), plotOutput("qcCorr"))
               ),
               fluidRow(
                 column(12, tableOutput("table1"))
               )
      ),
      tabPanel("Tab 2 Title",br(),
               h2("Content of Tab 2"),
               #uiOutput("dynamic_plots")
      )
    )
  )
))
