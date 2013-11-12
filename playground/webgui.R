## webgui.R
##   - RGP Web GUI 
##
## RGP - a GP system for R
## 2010-13 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

require("shiny") # TODO add shiny as an RGP package dependency
require("parallel") # TODO add shiny as an RGP package dependency

RGP_PORT <- 6011

RGP_COLORS <- list(
  RED = "#D70026FF",
  GRAY = "#AAAAAAFF"
)

RGP_RUN_STATES <- list(PAUSED = 1, RUNNING = 2, RESET = 3, STOP = 4)

bootstrapButton <- function (inputId, label, class = "", icon = "") {
  tags$button(id = inputId, type = "button", class = paste("btn action-button", class), tags$i(class = icon), label)
}

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
      tabsetPanel(
        tabPanel("Table", tableOutput("dataTable")),
        tabPanel("Plot", plotOutput("dataPlot"))))))
 
objectivePanel <- tabPanel("Objective", value = "objectivePanel",
  div(class = "row-fluid",
    sidebarPanel(
      tags$legend("Search Objective"),
      selectInput("dependentVariable", "Dependent Variable",
                  choices = c("")),
      textInput("formula", "Formula"),
      textInput("buildingBlocks", "Building Blocks",
                value = 'c("+", "-", "*", "/", "sin", "cos", "exp", "log", "sqrt")'),
      selectInput("errorMeasure", "Error Measure", 
                  choices = c("SMSE", "SSSE", "RMSE", "SSE", "MAE")),
      checkboxInput("enableComplexityCriterion", "Enable Complexity Criterion",
                    value = TRUE)),
    mainPanel(
      h3("Dependent Variable Plot"),
      plotOutput("dependentVariablePlot"),
      selectInput("dependentVariablePlotAbscissa", "Abscissa",
                  choices = c("(Row Number)")))))

runPanel <- tabPanel("Run", value = "runPanel",
  div(class = "row-fluid",
    sidebarPanel(
      tags$legend("Run Parameters"),
      checkboxInput("enableAgeCriterion", "Enable Age Criterion",
                    value = TRUE),
      sliderInput("mu", "Mu (Population Size)", 
                  min = 2, max = 1000, value = 100, step = 1),
      sliderInput("lambda", "Lambda (Number of Children / Generation)", 
                  min = 2, max = 100, value = 50, step = 1),
      sliderInput("nu", "Nu (Number of New Individuals / Generation)", 
                  min = 2, max = 100, value = 50, step = 1),
      sliderInput("crossoverProbability", "Crossover Probability", 
                  min = 0, max = 1, value = .5, step = .01),
      sliderInput("subtreeMutationProbability", "Subtree Mutation Probability", 
                  min = 0, max = 1, value = 1, step = .01),
      sliderInput("functionMutationProbability", "Function Mutation Probability", 
                  min = 0, max = 1, value = 0, step = .01),
      sliderInput("constantMutationProbability", "Constant Mutation Probability", 
                  min = 0, max = 1, value = 0, step = .01),
      sliderInput("parentSelectionProbability", "Parent Selection Probability", 
                  min = 0, max = 1, value = 1, step = .01),
      selectInput("selectionFunction", "Selection Function", 
                  choices = c("Crowding Distance", "Hypervolume")),
      sliderInput("fitnessSubSamplingShare", "Fitness Subsampling Share", 
                  min = 0, max = 1, value = 1, step = .01),
      sliderInput("randomSeed", "Random Seed", 
                  min = 0, max = 1000, value = 1, step = 1),
      tags$legend("Run Control"),
      bootstrapButton("startRunButton", "Start Run", icon = "icon-play icon-white", class = "btn-primary btn-block"),
      bootstrapButton("pauseRunButton", "Pause Run", icon = "icon-pause", class = "btn-block"),
      tags$hr(),
      bootstrapButton("resetRunButton", "Reset Run", icon = "icon-eject icon-white", class = "btn-danger btn-block")),
    mainPanel(
      "TODO Run Content",
      plotOutput("workerProcessOutputPlot"))))

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
  
# TODO
workerProcessMain <- function() {
  stopJob <- FALSE
  serverConnection <- socketConnection(port = RGP_PORT, server = TRUE, open = "rwb", blocking = TRUE)
  command <- list(op = RGP_RUN_STATES$PAUSED)
  while (!stopJob) {
    Sys.sleep(0.5) # TODO
    if (socketSelect(list(serverConnection), timeout = 0)) {
      command <- unserialize(serverConnection)
      print(paste("job received command: ", command)) # TODO
    }
    if (RGP_RUN_STATES$PAUSED == command$op) {
      serialize(rep(0, 1e5), serverConnection)
    } else if (RGP_RUN_STATES$RUNNING == command$op) {
      serialize(cumsum(rnorm(1e5)), serverConnection)
    } else if (RGP_RUN_STATES$RESET == command$op) {
      serialize(1:1e5, serverConnection)
    } else if (RGP_RUN_STATES$STOP == command$op) {
      stopJob <- TRUE
    } else {
      warn("RGP background job: unknown command: ", command)
      stopJob <- TRUE
    }
  }
  close(serverConnection)
}

server <- function(input, output, session) {
  # TODO
  workerProcess <- mcparallel(workerProcessMain())
  Sys.sleep(1) # wait for background job to initialize 
  workerProcessConnection <- socketConnection(port = RGP_PORT, open = "rwb", blocking = TRUE) 

  independentVariables <- c()

  dataFrame <- reactive({
    dataFile <- input$csvFile

    if (is.null(dataFile))
      return (NULL)
    
    dataFrame <- read.csv(dataFile$datapath, header = input$header, sep = input$sep, quote = input$quote)

    updateSelectInput(session, "dependentVariable",
                      choices = colnames(dataFrame), selected = tail(colnames(dataFrame), 1))
    updateSelectInput(session, "dependentVariablePlotAbscissa",
                      choices = c("(Row Number)", colnames(dataFrame)), selected = "(Row Number)") 

    return (dataFrame)
  })

  observe({
    allVariables <- colnames(dataFrame())
    dependentVariable <- input$dependentVariable 
    independentVariables <- allVariables[allVariables != dependentVariable]
    formulaText <- paste(dependentVariable, "~", paste(independentVariables, collapse = " + "))
    updateTextInput(session, "formula", value = formulaText)
  })

  output$dataPlot <- renderPlot({
    if (is.null(dataFrame())) {
      NULL
    } else {
      plot(dataFrame(), col = RGP_COLORS$RED)
    }
  })

  output$dataTable <- renderTable({ dataFrame() })

  output$dependentVariablePlot <- renderPlot({
    if (is.null(dataFrame())) {
      NULL
    } else {
      if ("(Row Number)" == input$dependentVariablePlotAbscissa) {
        plot(dataFrame()[, input$dependentVariable], col = RGP_COLORS$RED, pch = 16,
             xlab = "Row Number", ylab = input$dependentVariable)
        lines(dataFrame()[, input$dependentVariable], col = RGP_COLORS$GRAY)
      } else {
        plot(x = dataFrame()[, input$dependentVariablePlotAbscissa],
             y = dataFrame()[, input$dependentVariable],
             col = RGP_COLORS$RED, pch = 16,
             xlab = input$dependentVariablePlotAbscissa, ylab = input$dependentVariable)
        lines(x = dataFrame()[, input$dependentVariablePlotAbscissa],
              y = dataFrame()[, input$dependentVariable],
              col = RGP_COLORS$GRAY)
      }
    }
  })

  # TODO
  runState <- RGP_RUN_STATES$PAUSED
  observe({ if (input$startRunButton > 0) {
    runState <<- RGP_RUN_STATES$RUNNING 
    serialize(list(op = runState), workerProcessConnection) 
  }})
  observe({ if (input$pauseRunButton > 0) {
    runState <<- RGP_RUN_STATES$PAUSED
    serialize(list(op = runState), workerProcessConnection) 
  }})
  observe({ if (input$resetRunButton > 0) {
    runState <<- RGP_RUN_STATES$RESET
    serialize(list(op = runState), workerProcessConnection) 
  }})

  workerProcessOutput <- reactive({
    invalidateLater(100, session)
    jobStatus <- if (socketSelect(list(workerProcessConnection), timeout = 1)) {
      unserialize(workerProcessConnection)
    } else {
      0
    }
    return (jobStatus)
  })

  # TODO
  output$workerProcessOutputPlot <- renderPlot({ plot(workerProcessOutput(), type = "l") })
}

rgpWebGui <- function(port = 1447) {
  addResourcePath("images", "./images") # TODO use system.file() to refer to folder in package
  runApp(list(ui = ui, server = server), port = port)
}

