## webgui.R
##   - RGP Web GUI 
##
## RGP - a GP system for R
## 2010-13 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

require("shiny") # TODO add shiny as an RGP package dependency


dataPanel <- tabPanel("Data", value = "dataPanel",
  div(class = "row-fluid",
    sidebarPanel(
      tags$legend("CSV File Upload"),
      fileInput("csvFile", "CSV File",
                accept = c("text/csv", "text/comma-separated-values", "text/plain", ".csv")),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   c(Comma = ",",
                     Semicolon = ";",
                     Tab = "\t"),
                   "Comma"),
      radioButtons("quote", "Quote",
                   c(None="",
                     "Double Quote" = "\"",
                     "Single Quote" = "'"),
                     "Double Quote")),
    mainPanel(
      h4("Data Plot"),
      plotOutput("dataPlot"),
      h4("Data Table"),
      tableOutput("dataTable"))))
 
objectivePanel <- tabPanel("Objective", value = "objectivePanel",
  div(class = "row-fluid",
    sidebarPanel(
      tags$legend("Search Objective"),
      textInput("formula", "Formula"),
      textInput("buildingBlocks", "Building Blocks",
                value = 'c("+", "-", "*", "/", "sin", "cos", "exp", "log", "sqrt")'),
      selectInput("errorMeasure", "Error Measure", 
                  choices = c("SMSE", "SSSE", "RMSE", "SSE", "MAE")))))

runPanel <- tabPanel("Run", value = "runPanel",
  "TODO Run Content")

resultsPanel <- tabPanel("Results", value = "resultsPanel",
  "TODO Results Content")

ui <- bootstrapPage(
  div(class = "container-fluid",
    div(class = "row-fluid",
      headerPanel(list(img(src = "images/logo_rgp.png"), "RGP Symbolic Regression"),
                  windowTitle = "RGP")),
    div(class = "row-fluid",
      tabsetPanel(
        dataPanel,
        objectivePanel,
        runPanel,
        resultsPanel,
        id = "rgpPanels")),
  tags$footer(HTML("&copy; 2010-13"), a("rsymbolic.org", href = "http://rsymbolic.org"))))

server <- function(input, output, session) {
  dataFrame <- reactive({
    dataFile <- input$csvFile

    if (is.null(dataFile))
      return(NULL)
    
    dataFrame <- read.csv(dataFile$datapath, header = input$header, sep = input$sep, quote = input$quote)

    updateTextInput(session, "formula", value = nrow(dataFrame)) # TODO

    return(dataFrame)
  })

  output$dataPlot <- renderPlot({ if (is.null(dataFrame())) NULL else plot(dataFrame()) })

  output$dataTable <- renderTable({ dataFrame() })
}

rgpWebGui <- function(port = 1447) {
  addResourcePath("images", "./images") # TODO use system.file() to refer to folder in package
  runApp(list(ui = ui, server = server), port = port)
}

