library("shiny")
library("markdown")
library("shinythemes")

ui <-shinyUI(
  navbarPage(
    title = p(HTML(paste('<img src="https://img2.imgtp.com/2024/02/26/IP94RMVp.png" alt="Image" style="width:30px; margin: -5px -5px 0px -5px;">',"Samplyzer Browser"))),
    id = "mainnavbar",
    inverse = FALSE,
    theme = shinytheme("flatly"),
    windowTitle = "🧬Samplyzer Browser",
    # tabPanel(
    #   title = "Home",
    #   fluidRow(
    #     column(
    #       width = 10, offset = 1,
    #       div(
    #         class = "jumbotron",
    #         h1(HTML(paste('<img src="https://img2.imgtp.com/2024/02/26/IP94RMVp.png" alt="Image" style="width:70px; margin: -15px -5px 0px -5px;">',"Samplyzer Browser"))),
    #         #h1("🧬Samplyzer Browser"),
    #         #h3("Samplyzer is an R package and Web Application that enables efficient exploration of sample level QC statistics."),
    #         br(),
    #         actionButton(
    #           "learnmore", "Learn More", icon("search"),
    #           class = "btn-primary btn-lg"
    #         )
    #       ),
    #       tags$blockquote(
    #         p(
    #           "Samplyzer is an R package and Web Application that enables efficient exploration of sample level QC statistics.",
    #           HTML("&emsp;"),
    #           a(
    #             tagList(icon("github"), "Samplyzer-Github"),
    #             href = "https://github.com/x-lab/samplyzer",
    #             target = "_blank"
    #           )
    #         )
    #       )
    #     )
    #     #，column(
    #     # width = 3, # Adjust the width for the image column
    #     #  img(
    #     #    src = "https://img2.imgtp.com/2024/02/26/J3vYzmos.png",
    #     #    style = "width:100%;"  # Adjust the image width as needed
    #     #  )
    #     #)
    #   ),
    #   fluidRow(
    #     column(
    #       width = 10, offset = 1,
    #       includeMarkdown("data/footer.md")
    #     )
    #   )
    # ),
    tabPanel(
      title = "Data",
      fluidRow(
        column(
          width = 10, offset = 1,
          sidebarPanel(
            width = 12,
            radioButtons("data_type",
                         label = "Use example data or upload your data:",
                         choices = list(
                           "Load example dataset" = "example",
                           "Upload your dataset" = "upload"
                         ),
                         selected = "example"
            ),
            conditionalPanel(
              condition = "input.data_type == 'upload'",
              #p("Please read our ", a("data privacy policy", href = "https://github.com/road2stat/hdnom-doc/blob/master/privacy.md", target = "_blank"), "before uploading any data."),
              #p("Read a detailed explanation about the ", a("upload data format", href = "https://github.com/road2stat/hdnom-doc/blob/master/upload.md", target = "_blank"), ". An example dataset is provided below."),
              h3("Required Files"),
              fileInput("annotationsFile", "Upload Sample Annotations*",
                        accept = c('text/csv', 'text/comma-separated-values,text/plain')),
              fileInput("bamQcMetrFile", "Upload Sample QC Table*",
                        accept = c('text/csv', 'text/comma-separated-values,text/plain')),
              textInput("primaryID", "primaryID*", "SampleID"),
              helpText("(primaryID is unique and consistent across all files)"),
              h3("Optional Files"),
              fileInput("samplePCsFile", "Upload Sample PCs",
                        accept = c('text/csv', 'text/comma-separated-values,text/plain')),
              fileInput("refPCsFile", "Upload Ref PCs",
                        accept = c('text/csv', 'text/comma-separated-values,text/plain')),
              fileInput("vcfQcMetrFile", "Upload Other QC Table",
                        accept = c('text/csv', 'text/comma-separated-values,text/plain')),
              br(),
              actionButton("runUploadedFiles", "Run Uploaded File"),
            )
          )
        ),
        column(
          width = 10, offset = 1,
          mainPanel(
            width = 12,
            conditionalPanel(
              condition = "input.data_type == 'example'",
              tabsetPanel(
                tabPanel(
                  "Example Data Table",
                  DT::dataTableOutput("table2"),
                  style = "overflow-x: auto;"
                ),
                tabPanel(
                  "Example Files",
                  verbatimTextOutput("summary_dataset"),
                  br(),
                  p(downloadLink("downloadLink1", "Sample Annotations File")),
                  p(downloadLink("downloadLink2", "Sample QC File")),
                  p(downloadLink("downloadLink3", "Sample PCs File")),
                  p(downloadLink("downloadLink4", "Ref PCs File")),
                  p(downloadLink("downloadLink5", "Other QC File")),
                )
              ),
              br(),
              actionButton("loadSampleData", "Load Example Data")
            )
          )
        )
      )
    ),
    tabPanel(
      title = "Sample Explorer",
      fluidRow(
        column(
          width = 10, offset = 1,
          column(2, selectInput("anno1", 'Subject Attributes', choices = NULL)),
          column(2, selectInput("qcMetr1", 'QC Scatter Plot - MetricX', choices = NULL)),
          column(2, selectInput('qcMetr2', 'QC Scatter Plot - MetricY', choices = NULL)),
          column(2, selectInput('PCx', 'PC Scatter Plot - X', choices = NULL)),
          column(2, selectInput('PCy', 'PC Scatter Plot - Y', choices = NULL)),
          column(2, selectInput("outliers", "Sample IDs", choices = NULL))
        )
      ),
      fluidRow(
        column(
          width = 3,
          sidebarPanel(
            width = 12,
            fluidRow(
              column(12,DT::dataTableOutput("table1"), style = "overflow-x: auto;")
            )
          )
        ),
        column(
          width = 9,
          mainPanel(
            width = 12,
            fluidRow(
              column(6,
                     div(plotOutput("plot1", brush = brushOpts(id = "plot_brush1"))),
                     div(plotOutput("pca"))
              ),
              column(6,
                     div(plotOutput("plot2")),
                     div(plotOutput("qcCorr")))
            )
          )
        )
      )
    ),
    tabPanel(
      title ="Panel",
      fluidRow(
        column(
          width = 10, offset = 1,
          column(3, selectInput("anno2", 'Subject Attributes', choices = NULL)),
          column(3, selectInput("ncol", 'Number of Columns', choices = c(2,3,4,5,6))),
          fluidRow(
            column(12, plotOutput("multPlot", height = "800px")
            ))
        )
      )
    ),
    tabPanel(
      title = "Tutorial",
      fluidRow(
        column(
          width = 10, offset = 2,
          includeMarkdown("data/help.md"),
          br()
        )
      )
    ),

    tabPanel(
      title = "README",
      fluidRow(
        column(
          width = 10, offset = 1,
          includeMarkdown("data/README.md")
        )
      )
    )
  )
)
