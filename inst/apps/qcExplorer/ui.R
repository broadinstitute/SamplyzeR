ui <- fluidPage(
  # Some custom CSS for a smaller font for preformatted text
  tags$head(tags$style(HTML("pre, table.table {font-size: smaller;}"))),
  titlePanel("Sample Explorer"),
  fluidRow(
    column(width = 2, wellPanel(
      fileInput("QC metrics", "Upload sample QC table",
                accept = c("text/csv", ".csv")),
      fileInput("Subject Annotations", "Upload sample annotatins",
                accept =c("text/csv",".csv")),
      selectInput('qcMetrics1', 'QC metrics', sds$qcMetrics),
      selectInput('anno1', 'Subject Attributes', sds$annotations),
      textInput("outliers","sample IDs","outliers")
    )),
    column(width = 5, plotOutput("plot1",
                                 brush = brushOpts(id = "plot_brush1"))),
    column(width = 5, plotOutput("violin"))
  ),
  fluidRow(
    column(width = 2, wellPanel(
      selectInput('qcMetr1', 'QC metrics 1', sds$qcMetrics),
      selectInput('qcMetr2', 'QC metrics 2', sds$qcMetrics),
      selectInput('attr', 'attributes', sds$annotations),
      selectInput('PCx', 'first PC', paste('PC', 1:10, sep = '')),
      selectInput('PCy', 'second PC', paste('PC', 1:10, sep = '')),
      selectInput('attr2', 'PC attributes', sds$annotations)
    )),
    column(width = 5, plotOutput("qcCorr")),
    column(width = 5, plotOutput("pca")),
    column(width = 3, tableOutput("table1"))
  )
)
