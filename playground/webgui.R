## webgui.R
##   - RGP Web GUI 
##
## RGP - a GP system for R
## 2010-13 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

require("rgp")
require("shiny") # TODO add shiny as an RGP-UI package dependency
require("parallel") # TODO add shiny as an RGP-UI package dependency

RGP_PORT <- 6011

RGP_COLORS <- list(
  RED = "#D70026FF",
  YELLOW = "#E57600FF",
  BLUE = "#056D8FFF",
  GREEN = "#3BC500FF",
  GRAY = "#A0A0A0FF",
  DARK_GRAY = "#404040FF"
)

RGP_RUN_STATES <- list(PAUSED = 1, RUNNING = 2, RESET = 3, STOP = 4)
RGP_WORKER_MESSAGES <- list(PROGRESS = 1, NEWBEST = 2, STATISTICS = 3, RESULT = 4)
RGP_HISTORY_LENGTH <- 1000

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
      plotOutput("dependentVariablePlot"),
      selectInput("dependentVariablePlotAbscissa", "Abscissa",
                  choices = c("(Row Number)")))))

runPanel <- tabPanel("Run", value = "runPanel",
  div(class = "row-fluid",
    sidebarPanel(
      tags$legend("Run Parameters"),
      numericInput("mu", "Mu (Population Size)", 
                   min = 2, max = 1000, value = 100, step = 1),
      numericInput("lambda", "Lambda (Number of Children / Generation)", 
                   min = 2, max = 100, value = 50, step = 1),
      numericInput("nu", "Nu (Number of New Individuals / Generation)", 
                   min = 2, max = 100, value = 50, step = 1),
      sliderInput("crossoverProbability", "Crossover Probability", 
                  min = 0, max = 1, value = .5, step = .01),
      sliderInput("subtreeMutationProbability", "Subtree Mutation Probability", 
                  min = 0, max = 1, value = 1, step = .01),
      sliderInput("functionMutationProbability", "Function Mutation Probability", 
                  min = 0, max = 1, value = 0, step = .01),
      sliderInput("constantMutationProbability", "Constant Mutation Probability", 
                  min = 0, max = 1, value = 0, step = .01),
      checkboxInput("enableAgeCriterion", "Enable Age Criterion",
                    value = TRUE),
      sliderInput("parentSelectionProbability", "Parent Selection Probability", 
                  min = 0, max = 1, value = 1, step = .01),
      selectInput("selectionFunction", "Selection Function", 
                  choices = c("Crowding Distance", "Hypervolume")),
      sliderInput("fitnessSubSamplingShare", "Fitness Subsampling Share", 
                  min = 0, max = 1, value = 1, step = .01),
      numericInput("randomSeed", "Random Seed", 
                   min = 0, value = 1, step = 1),
      tags$legend("Run Control"),
      bootstrapButton("startRunButton", "Start Run", icon = "icon-play icon-white", class = "btn-primary btn-block"),
      bootstrapButton("pauseRunButton", "Pause Run", icon = "icon-pause", class = "btn-block"),
      tags$hr(),
      bootstrapButton("resetRunButton", "Reset Run", icon = "icon-eject icon-white", class = "btn-danger btn-block")),
    mainPanel(
      tabsetPanel(
        tabPanel("Progress", plotOutput("progressPlot", height = 1000)), 
        tabPanel("Pareto Front", plotOutput("paretoPlot", height = 768)), 
        tabPanel("Best Solution", plotOutput("bestSolutionPlot"),
                                  tableOutput("bestSolutionTable"))))))

resultsPanel <- tabPanel("Results", value = "resultsPanel",
  tags$head(tags$script(src = "scripts/jquery.sparkline.min.js")),
  tags$head(tags$script(type = "text/javascript", HTML("$(function() { $.extend($.fn.dataTable.defaults, { 'fnDrawCallback': function(oSettings) { $('.inlinesparkline').sparkline('html', { type: 'line', disableHiddenCheck: true, height: '40px', width: '200px' }); } }); });"))),
  div(class = "row-fluid",
    tabsetPanel(
      tabPanel("Pareto Front", 
               dataTableOutput("resultParetoFrontTable")))))

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
  
workerProcessMain <- function() {
  stopProcess <- FALSE
  serverConnection <- socketConnection(port = RGP_PORT, server = TRUE, open = "rwb", blocking = TRUE)
  command <- list(op = RGP_RUN_STATES$PAUSED)
  population <- NULL
  runStatistics <- NULL

  while (!stopProcess) {
    Sys.sleep(0.5)
    if (socketSelect(list(serverConnection), timeout = 0)) {
      command <- unserialize(serverConnection)
      message(paste("job received command: ", command)) # TODO
    }
    if (RGP_RUN_STATES$PAUSED == command$op) {
      # do nothing
    } else if (RGP_RUN_STATES$RUNNING == command$op) {
      runResult <- workerProcessRun(serverConnection, population, runStatistics, command$params)
      command <- runResult$command
      population <- runResult$population
      runStatistics <- runResult$runStatistics
    } else if (RGP_RUN_STATES$RESET == command$op) {
      # TODO implement reset
    } else if (RGP_RUN_STATES$STOP == command$op) {
      stopProcess <- TRUE
    } else {
      warn("RGP background job: unknown command: ", command)
      stopProcess <- TRUE
    }
  }
  close(serverConnection)
}

rescaleIndividual <- function(ind, srDataFrame, dependentVariable) {
  indX <- srDataFrame[, colnames(srDataFrame) != dependentVariable]
  indY <- if (is.data.frame(indX)) apply(indX, 1, function(x) do.call(ind, as.list(x))) else ind(indX)
  trueY <- srDataFrame[, dependentVariable]
  indY <- if (length(indY) == 1) rep(indY, length(trueY)) else indY
  b = cov(trueY, indY) / var(indY)
  a = mean(trueY) - b * mean(indY)
  rescaledInd <- function(...) a + b * ind(...)
  return (rescaledInd)
}

workerProcessRun <- function(serverConnection, population, runStatistics, params) {
  command <- list(op = RGP_RUN_STATES$RUNNING)
  
  set.seed(params$randomSeed) # TODO do not set seed when continuing a paused run

  srFormula <- as.formula(params$formulaText)
  srDataFrame <- params$dataFrame

  funSet <- do.call(functionSet, as.list(eval(parse(text = params$buildingBlocks))))
  inVarSet <- do.call(inputVariableSet, as.list(params$independentVariables))
  constSet <- numericConstantSet

  mutationFunction <- if (params$subtreeMutationProbability == 1 && params$functionMutationProbability == 0 && params$constantMutationProbability == 0) {
    function(ind) {
      subtreeMutantBody <- mutateSubtreeFast(body(ind), funSet, inVarSet, -10.0, 10.0, insertprob = 0.5, deleteprob = 0.5, subtreeprob = 1.0, constprob = 0.5, maxsubtreedepth = 8)
      makeClosure(subtreeMutantBody, inVarSet$all, envir = funSet$envir)
    }
  } else if (params$subtreeMutationProbability == 0 && params$functionMutationProbability == 1 && params$constantMutationProbability == 0) {
    function(ind) {
      functionMutantBody <- mutateFuncFast(body(ind), funSet, mutatefuncprob = 0.1)
      makeClosure(functionMutantBody, inVarSet$all, envir = funSet$envir)
    }
  } else if (params$subtreeMutationProbability == 0 && params$functionMutationProbability == 0 && params$constantMutationProbability == 1) {
    function(ind) {
      constantMutantBody <- mutateNumericConstFast(body(ind), mutateconstprob = 0.1, mu = 0.0, sigma = 1.0)
      makeClosure(constantMutantBody, inVarSet$all, envir = funSet$envir)
    }
  } else {
    function(ind) {
      mutantBody <- body(ind)
      runif3 <- runif(3)
      if (runif3[1] < params$subtreeMutationProbability) {
        mutantBody <- mutateSubtreeFast(mutantBody, funSet, inVarSet, -10.0, 10.0, insertprob = 0.5, deleteprob = 0.5, subtreeprob = 1.0, constprob = 0.5, maxsubtreedepth = 8)
      }
      if (runif3[2] < params$functionMutationProbability) {
        mutantBody <- mutateFuncFast(mutantBody, funSet, mutatefuncprob = 0.1)
      }
      if (runif3[3] < params$constantMutationProbability) {
        mutantBody <- mutateNumericConstFast(mutantBody, mutateconstprob = 0.1, mu = 0.0, sigma = 1.0)
      }
      makeClosure(mutantBody, inVarSet$all, envir = funSet$envir)
    }
  }

  populationFactory <- function(mu, funSet, inVarSet, maxfuncdepth, constMin, constMax) { 
    Map(function(i) makeClosure(.Call("initialize_expression_grow_R",
                                      as.list(funSet$nameStrings),
                                      as.integer(funSet$arities),
                                      as.list(inVarSet$nameStrings),
                                      constMin, constMax,
                                      0.8, 0.2,
                                      as.integer(maxfuncdepth)),
                                as.list(inVarSet$nameStrings)), 1:mu)
  }

  errorMeasure  <- switch(params$errorMeasure,
                          "SMSE" = smse,
                          "SSSE" = ssse,
                          "RMSE" = rmse,
                          "SSE" = sse,
                          "MAE" = mae,
                          stop("webUi: unkown error measure name: ", params$errorMeasure))

  ndsSelectionFunction <- switch(params$selectionFunction,
                                 "Crowding Distance" = nds_cd_selection,
                                 "Hypervolume" = nds_hv_selection,
                                 stop("webUi: unkown NDS selection function name: ", params$selectionFunction))

  searchHeuristic <- makeAgeFitnessComplexityParetoGpSearchHeuristic(lambda = params$lambda,
                                                                     crossoverProbability = params$crossoverProbability,
                                                                     newIndividualsPerGeneration = params$nu,
                                                                     enableComplexityCriterion = params$enableComplexityCriterion,
                                                                     enableAgeCriterion = params$enableAgeCriterion,
                                                                     ndsParentSelectionProbability = params$parentSelectionProbability,
                                                                     ndsSelectionFunction = ndsSelectionFunction)

  commandStopCondition <- function(pop, fitnessFunction, stepNumber, evaluationNumber, bestFitness, timeElapsed) {
    command$op == RGP_RUN_STATES$PAUSED
  }

  runStatistics <- if (is.null(runStatistics)) {
    list(startTime = Sys.time(),
         fitnessHistory = c(),
         complexityHistory = c(),
         ageHistory = c(),
         dominatedHypervolumeHistory = c(),
         generationsHistory = c(),
         lastBestFitness = Inf)
  } else {
    runStatistics
  }

  progressMonitor <- function(pop, objectiveVectors, fitnessFunction,
                              stepNumber, evaluationNumber, bestFitness, timeElapsed, indicesToRemove) {
    # TODO do not do this in every step
    if (socketSelect(list(serverConnection), timeout = 0)) {
      command <<- unserialize(serverConnection)
      message(paste("job received command: ", command)) # TODO
    }

    if (bestFitness < runStatistics$lastBestFitness) {
      alarm() # beep when a new best individual is found TODO transmit alarm to web client
      runStatistics$lastBestFitness <<- bestFitness
      bestIndividual <- pop[order(objectiveVectors$fitnessValues)][[1]] 
      rescaledBestIndividual <- if (params$errorMeasure == "SMSE" || params$errorMeasure == "SSSE") {
        rescaleIndividual(bestIndividual, srDataFrame, params$dependentVariable)
      } else {
        bestIndividual
      }
      message("NEW best solution (not rescaled):")
      message(sprintf(" %s", deparse(bestIndividual)))
      serialize(list(msg = RGP_WORKER_MESSAGES$NEWBEST,
                     params = list(bestIndividual = bestIndividual,
                                   rescaledBestIndividual = rescaledBestIndividual,
                                   stepNumber = stepNumber,
                                   evaluationNumber = evaluationNumber,
                                   bestFitness = bestFitness,
                                   timeElapsed = timeElapsed)),
                serverConnection)
    }

    if (stepNumber %% 10 == 0) { # every 10th generation...
      message(sprintf("evolution step %i, fitness evaluations: %i, best fitness: %f, time elapsed: %f",
                      stepNumber, evaluationNumber, bestFitness, timeElapsed))
      points <- rbind(objectiveVectors$fitnessValues, objectiveVectors$complexityValues, objectiveVectors$ageValues)
      finitePoints <- points[, !apply(is.infinite(points), 2, any)]
      bestFitnessIndex <- which.min(objectiveVectors$fitnessValues)
      historyIndexToReplace <- floor(runif(1, min = 1, max = RGP_HISTORY_LENGTH))
      runStatistics$fitnessHistory <<- c(if (length(runStatistics$fitnessHistory) <= RGP_HISTORY_LENGTH) runStatistics$fitnessHistory else runStatistics$fitnessHistory[-historyIndexToReplace], log(objectiveVectors$fitnessValues[bestFitnessIndex])) # cut-off at RGP_HISTORY_LENGTH number of entries
      runStatistics$complexityHistory <<- c(if (length(runStatistics$complexityHistory) <= RGP_HISTORY_LENGTH) runStatistics$complexityHistory else runStatistics$complexityHistory[-historyIndexToReplace], objectiveVectors$complexityValues[bestFitnessIndex])
      runStatistics$ageHistory <<- c(if (length(runStatistics$ageHistory) <= RGP_HISTORY_LENGTH) runStatistics$ageHistory else runStatistics$ageHistory[-historyIndexToReplace], objectiveVectors$ageValues[bestFitnessIndex])
      runStatistics$dominatedHypervolumeHistory <<- c(if (length(runStatistics$dominatedHypervolumeHistory) <= RGP_HISTORY_LENGTH) runStatistics$dominatedHypervolumeHistory else runStatistics$dominatedHypervolumeHistory[-historyIndexToReplace], dominated_hypervolume(finitePoints))
      runStatistics$generationsHistory <<- c(if (length(runStatistics$generationsHistory) <= RGP_HISTORY_LENGTH) runStatistics$generationsHistory else runStatistics$generationsHistory[-historyIndexToReplace], stepNumber)
      serialize(list(msg = RGP_WORKER_MESSAGES$PROGRESS,
                     params = list(generations = runStatistics$generationsHistory,
                                   fitnessHistory = runStatistics$fitnessHistory,
                                   complexityHistory = runStatistics$complexityHistory,
                                   ageHistory = runStatistics$ageHistory,
                                   dominatedHypervolumeHistory = runStatistics$dominatedHypervolumeHistory,
                                   poolFitnessValues = objectiveVectors$poolFitnessValues,
                                   poolComplexityValues = objectiveVectors$poolComplexityValues,
                                   poolAgeValues = objectiveVectors$poolAgeValues,
                                   poolIndicesToRemove = indicesToRemove)),
                serverConnection)
    }
  }
  
  tryCatch({
    population <- if (is.null(population)) {
      message("workerProcessRun: INITIALIZING population")
      populationFactory(params$mu, funSet, inVarSet, 8, -10.0, 10.0)
    } else {
      population
    }

    message("workerProcessRun: STARTING GP run")
    sr <- suppressWarnings(symbolicRegression(srFormula,
                                              data = srDataFrame,
                                              functionSet = funSet,
                                              errorMeasure = errorMeasure,
                                              stopCondition = commandStopCondition,
                                              #stopCondition = makeStepsStopCondition(250),
                                              #stopCondition = makeTimeStopCondition(10),
                                              population = population,
                                              populationSize = params$mu,
                                              individualSizeLimit = 128, # individuals with more than 128 nodes (inner and leafs) get fitness Inf
                                              #subSamplingShare = subSamplingShare,
                                              searchHeuristic = searchHeuristic,
                                              envir = environment(),
                                              verbose = FALSE,
                                              progressMonitor = progressMonitor))
    message("workerProcessRun: GP run done")
  }, error = function(e) { message("workerProcessRun: Catched error '", e, "'") }) # TODO
      
  # send results to server
  serialize(list(msg = RGP_WORKER_MESSAGES$RESULT,
                 params = list(result = sr,
                               data = srDataFrame,
                               dependentVariable = params$dependentVariable,
                               errorMeasure = params$errorMeasure)),
            serverConnection)

  return (list(command = command, result = sr, population = population, runStatistics = runStatistics))
}

server <- function(input, output, session) {
  runState <- RGP_RUN_STATES$PAUSED
  workerProcess <- mcparallel(workerProcessMain())
  Sys.sleep(1) # wait for background job to initialize 
  workerProcessConnection <- socketConnection(port = RGP_PORT, open = "rwb", blocking = TRUE) 

  independentVariables <- c()

  dataFrame <- reactive({
    dataFile <- input$csvFile

    if (is.null(dataFile))
      return (NULL)
    
    dataFrame <- read.csv(dataFile$datapath, header = input$header, sep = input$sep, quote = input$quote, colClasses = "numeric")

    updateSelectInput(session, "dependentVariable",
                      choices = colnames(dataFrame), selected = tail(colnames(dataFrame), 1))
    updateSelectInput(session, "dependentVariablePlotAbscissa",
                      choices = c("(Row Number)", colnames(dataFrame)), selected = "(Row Number)") 

    return (dataFrame)
  })

  observe({
    allVariables <- colnames(dataFrame())
    dependentVariable <- input$dependentVariable 
    independentVariables <<- allVariables[allVariables != dependentVariable]
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
             xlab = "Row Number", ylab = input$dependentVariable,
             main = "Dependent Variable Plot")
        lines(dataFrame()[, input$dependentVariable], col = RGP_COLORS$GRAY)
      } else {
        plot(x = dataFrame()[, input$dependentVariablePlotAbscissa],
             y = dataFrame()[, input$dependentVariable],
             col = RGP_COLORS$RED, pch = 16,
             xlab = input$dependentVariablePlotAbscissa, ylab = input$dependentVariable,
             main = "Dependent Variable Plot")
        lines(x = dataFrame()[, input$dependentVariablePlotAbscissa],
              y = dataFrame()[, input$dependentVariable],
              col = RGP_COLORS$GRAY)
      }
    }
  })

  observe({ if (input$startRunButton > 0) {
    runState <<- RGP_RUN_STATES$RUNNING 
    serialize(list(op = runState, params = list(buildingBlocks = isolate(input$buildingBlocks),
                                                independentVariables = independentVariables,
                                                dependentVariable = isolate(input$dependentVariable),
                                                mu = isolate(input$mu),
                                                lambda = isolate(input$lambda),
                                                nu = isolate(input$nu),
                                                crossoverProbability = isolate(input$crossoverProbability),
                                                subtreeMutationProbability = isolate(input$subtreeMutationProbability),
                                                functionMutationProbability = isolate(input$functionMutationProbability),
                                                constantMutationProbability = isolate(input$functionMutationProbability),
                                                enableAgeCriterion = isolate(input$enableAgeCriterion),
                                                enableComplexityCriterion = isolate(input$enableComplexityCriterion),
                                                parentSelectionProbability = isolate(input$parentSelectionProbability),
                                                selectionFunction = isolate(input$selectionFunction),
                                                fitnessSubSamplingShare = isolate(input$fitnessSubSamplingShare),
                                                errorMeasure = isolate(input$errorMeasure),
                                                randomSeed = isolate(input$randomSeed),
                                                formulaText = isolate(input$formula),
                                                dataFrame = dataFrame())), workerProcessConnection)
  }})
  observe({ if (input$pauseRunButton > 0) {
    runState <<- RGP_RUN_STATES$PAUSED
    serialize(list(op = runState), workerProcessConnection) 
  }})
  observe({ if (input$resetRunButton > 0) {
    runState <<- RGP_RUN_STATES$RESET
    serialize(list(op = runState), workerProcessConnection) 
  }})

  lastWorkerProcessMessages <- reactiveValues(progress = NULL, newBest = NULL, statistics = NULL, result = NULL)
  workerProcessMessage <- reactive({
    invalidateLater(100, session) # each 100 milliseconds
    return (if (socketSelect(list(workerProcessConnection), timeout = 0)) {
      unserialize(workerProcessConnection)
    } else {
      NULL
    })
  })
  observe({
    msg <- workerProcessMessage() # make sure that unserialize(workerProcessConnection) keeps called regularly
    if (!is.null(msg) && msg$msg == RGP_WORKER_MESSAGES$PROGRESS) {
      lastWorkerProcessMessages$progress <- msg
    } else if (!is.null(msg) && msg$msg == RGP_WORKER_MESSAGES$NEWBEST) {
      lastWorkerProcessMessages$newBest <- msg
    } else if (!is.null(msg) && msg$msg == RGP_WORKER_MESSAGES$STATISTICS) {
      lastWorkerProcessMessages$statistics <- msg
    } else if (!is.null(msg) && msg$msg == RGP_WORKER_MESSAGES$RESULT) {
      lastWorkerProcessMessages$result <- msg
    } else if (!is.null(msg)) {
      stop("webUi: unknown worker process message:", msg)
    } else {
      # ignore NULL messages
    }
  })

  output$progressPlot <- renderPlot({
    if (!is.null(lastWorkerProcessMessages$progress)) {
      params <- lastWorkerProcessMessages$progress$params 
      oldPar <- par(no.readonly = TRUE)
      layout(matrix(1:4, 4, 1, byrow = TRUE))
      plot(params$generations, params$fitnessHistory, type = "l",
           main = "Fittest Individual Fitness", xlab = "Generation", ylab = "log Fitness")
      plot(params$generations, params$complexityHistory, type = "l", col = "red",
           main = "Fittest Individual Complexity", xlab = "Generation", ylab = "Complexity (Visitation Length)")
      plot(params$generations, params$ageHistory, type = "l", col = "green",
           main = "Fittest Individual Age", xlab = "Generation", ylab = "Age (Generations)")
      plot(params$generations, params$dominatedHypervolumeHistory, type = "l", col = "gray",
           main = "Dominated Hypervolume", xlab = "Generation", ylab = "Hypervolume")
      par(oldPar)
    }
  })

  output$paretoPlot <- renderPlot({
    if (!is.null(lastWorkerProcessMessages$progress)) {
      params <- lastWorkerProcessMessages$progress$params 
      plotParetoFront(params$poolFitnessValues, params$poolComplexityValues, params$poolAgeValues,
                      params$poolIndicesToRemove,
                      main = sprintf("Selection Pool Fitness Pareto Plot (%d Individuals)", length(params$poolFitnessValues)),
                      xlab = "Fitness (Prediction Error)", ylab = "Complexity (Visitation Length)")
    }
  })

  output$bestSolutionPlot <- renderPlot({
    if (!is.null(lastWorkerProcessMessages$newBest)) {
      params <- lastWorkerProcessMessages$newBest$params 
      ind <- params$rescaledBestIndividual
      indX <- dataFrame()[, colnames(dataFrame()) != input$dependentVariable]
      indY <- if (is.data.frame(indX)) apply(indX, 1, function(x) do.call(ind, as.list(x))) else ind(indX)
      plot(dataFrame()[, input$dependentVariable], col = RGP_COLORS$RED, pch = 16,
           xlab = "Row Number", ylab = input$dependentVariable,
           main = "Best Solution Plot")
      lines(dataFrame()[, input$dependentVariable], col = RGP_COLORS$GRAY)
      points(indY, col = RGP_COLORS$BLUE, pch = 1)
      lines(indY, col = RGP_COLORS$DARK_GRAY)
      legend("bottomright", c("Data", "Best Solution"),
             bty = "n",
             pch = c(16, 1),
             col = c(RGP_COLORS$RED, RGP_COLORS$BLUE))
    }
  })

  output$bestSolutionTable <- renderTable({
    if (!is.null(lastWorkerProcessMessages$newBest)) {
      params <- lastWorkerProcessMessages$newBest$params 
      data.frame(list(Attribute = c("Formula", "Error", "Generation", "Evaluation Number", "Time Elapsed"),
                      Value = c(do.call(paste, c(as.list(deparse(params$bestIndividual)), sep = "")),
                                params$bestFitness,
                                params$stepNumber,
                                params$evaluationNumber,
                                formatSeconds(params$timeElapsed))))
    }
  }, include.rownames = FALSE)

  output$resultParetoFrontTable <- renderDataTable({
    if (!is.null(lastWorkerProcessMessages$result)) {
      params <- lastWorkerProcessMessages$result$params
      srResult <- params$result
      population <- srResult$population # TODO remove duplicates
      fitnessValues <- srResult$searchHeuristicResults$fitnessValues
      complexityValues <- srResult$searchHeuristicResults$complexityValues
      objectiveValues <- rbind(fitnessValues, complexityValues)
      ndsRanks <- nds_rank(objectiveValues)
      paretoFrontMask <- ndsRanks == 1
      uniqueMask <- !duplicated(population)
      mask <- paretoFrontMask & uniqueMask 
      deparseInd <- function(ind) do.call(paste, c(as.list(deparse(ind)), sep = ""))
      indToYstring <- function(ind) {
        rescaledInd <- if (params$errorMeasure == "SMSE" || params$errorMeasure == "SSSE") {
          rescaleIndividual(ind, params$data, params$dependentVariable)
        } else {
          ind 
        }
        indX <- params$data[, colnames(params$data) != params$dependentVariable]
        indY <- if (is.data.frame(indX)) apply(indX, 1, function(x) do.call(rescaledInd, as.list(x))) else rescaledInd(indX)
        indYString <- do.call(paste, c(as.list(indY), sep = ","))
        HTML(paste("<span class='inlinesparkline'>", indYString, "</span>", sep = ""))
      }
      paretoFrontFormulas <- as.character(Map(deparseInd, population[mask]))
      paretoFrontFitnessValues <- fitnessValues[mask]
      paretoFrontComplexityValues <- complexityValues[mask]
      paretoFrontPlots <- as.character(Map(indToYstring, population[mask]))

      data.frame(list(Formula = paretoFrontFormulas,
                      Error = paretoFrontFitnessValues,
                      Complexity = paretoFrontComplexityValues,
                      Plot = paretoFrontPlots)) 
    }
  }, options = list(iDisplayLength = 25)) # TODO
}

webUi <- function(port = 1447) {
  addResourcePath("images", "./images") # TODO use system.file() to refer to folder in package
  addResourcePath("scripts", "./scripts") # TODO use system.file() to refer to folder in package
  runApp(list(ui = ui, server = server), port = port)
}

