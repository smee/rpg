## gpdc.r
##
## Interactive Analysis of Genotype-Phenotype-Distance-Correlation in Genetic Programming
##
## 2011 Oliver Flasch
##

require("rgp")
require("snowfall")
require("twiddler")


## Cluster Initialization Functions...
## ---
initDummyCluster <- function()
  sfInit(parallel = FALSE)

initLocalCluster <- function(nodes = 2)
  sfInit(parallel = TRUE, cpus = nodes)

initMaanbsCluster <- function()
  sfInit(parallel = TRUE,
         socketHosts = c(rep("maanbs02.gm.fh-koeln.de", 16),
                         rep("maanbs03.gm.fh-koeln.de", 16),
                         rep("maanbs04.gm.fh-koeln.de", 16)))


## Sampling of Genotypic and Phenotypic Distances...
## ---
makeRandomIndividual <- function(funcset, inset, conset,
                                 treeDepth = 5, expressionFactory = randexprFull)
  randfunc(funcset = funcset, inset = inset, conset = conset,
           maxdepth = treeDepth, exprfactory = expressionFactory)

makeRandomIndividualTyped <- function(type, funcset, inset, conset,
                                      treeDepth = 5, expressionFactory = randexprTypedFull)
  randfuncTyped(type = type, funcset = funcset, inset = inset, conset = conset,
                maxdepth = treeDepth, exprfactory = expressionFactory)
# randfuncTyped(st("logical"), typedBooleanFunctionSet, inputVariableSet("x" %::% st("logical")), typedLogicalConstantSet)

univariateRmse <- function(f1, f2, xs = seq(1, 10, by = 0.1)) {
  ys1 <- Vectorize(f1)(xs)
  ys2 <- Vectorize(f2)(xs)
  rmse(ys1, ys2)
}

sampleMultivariateFunction <- function(dimensions, f, xs = seq(1, 10, by = 1)) {
  # grid sample f at points xs in every dimension
  enumerateArguments <- function(dimensions, xs)
    if (dimensions <= 1) xs else rgp:::intersperse(xs, enumerateArguments(dimensions - 1, xs),
                                                   pairConstructor = c)
  Map(function(x) do.call(f, as.list(x)), enumerateArguments(dimensions, xs))
}

multivariateRmse <- function(dimensions, f1, f2, xs = seq(1, 10, by = 1)) {
  ys1 <- as.numeric(sampleMultivariateFunction(dimensions, f1, xs = xs))
  ys2 <- as.numeric(sampleMultivariateFunction(dimensions, f2, xs = xs))
  rmse(ys1, ys2)
}

multivariateErrorCount <- function(dimensions, f1, f2) {
  ys1 <- as.logical(sampleMultivariateFunction(dimensions, f1, xs = c(FALSE, TRUE)))
  ys2 <- as.logical(sampleMultivariateFunction(dimensions, f2, xs = c(FALSE, TRUE)))
  sum(ys1 != ys2) # sum() also counts the number of TRUEs in boolean vector
}

sampleIndividualDistances <- function(n, funcset, inset, conset,
                                      treeDepth = 5,
                                      mutationSteps = 1,
                                      individualFactory = function()
                                        makeRandomIndividual(funcset, inset, conset, treeDepth = treeDepth),
                                      mutationFunction = function(ind)
                                        mutateChangeDeleteInsert(ind, funcset, inset, conset, iterations = mutationSteps),
                                      inputDimensions = length(inset$all),
                                      distanceMeasure = if (inputDimensions == 1)
                                                          univariateRmse
                                                        else
                                                          function(a, b) multivariateRmse(inputDimensions, a, b)) {
  rlen <- 0
  r <- c()
  while (rlen < n) {
    indA <- individualFactory()
    indB <- mutationFunction(indA)
    d <- distanceMeasure(indA, indB)
    if (!is.nan(d) && !is.infinite(d)) { # filter infinite and nan distances
      r <- c(r, d)
      rlen <- rlen + 1
    }
  }
  r
}

sampleMutations <- function(n, funcset, inset, conset,
                            treeDepth = 5,
                            mutationSteps = 1,
                            individualFactory = function()
                              makeRandomIndividual(funcset, inset, conset, treeDepth = treeDepth),
                            mutationFunction = function(ind)
                              mutateChangeDeleteInsert(ind, funcset, inset, conset, iterations = mutationSteps),
                            inputDimensions = length(inset$all),
                            distanceMeasure = if (inputDimensions == 1)
                                                univariateRmse
                                              else
                                                function(a, b) multivariateRmse(inputDimensions, a, b)) {
  population <- Map(function(i) individualFactory(), 1:n)
  mutatedPopulation <- if (mutationSteps > 0) Map(mutationFunction, population) else population
  distances <- Map(distanceMeasure, population, mutatedPopulation)
  list(population = population,
       mutatedPopulation = mutatedPopulation,
       distances = distances)
}

countIndividualFunctions <- function(individual) {
  res <- numeric()
  countExprFunctionsRec <- function(expr)
    if (is.call(expr)) {
      funcString <- as.character(expr[[1]])
      res[funcString] <<- if (is.na(res[funcString])) 1 else res[funcString] + 1
      Map(countExprFunctionsRec, expr[-1])
    }
  countExprFunctionsRec(body(individual))
  res
}

plotIndividual <- function(individual, circular = FALSE, structureOnly = TRUE, ...) {
  ig <- funcToIgraph(individual)
  vertexDepths <- shortest.paths(ig)[1,] + 1
  edgeDepths <- vertexDepths[get.edges(ig, E(ig))[,2] + 1]
  edgeWiths <- 20 / edgeDepths
  edgeColors <- rainbow(max(edgeDepths))[edgeDepths]
  if (structureOnly) {
    plot(ig, layout = layout.reingold.tilford(ig, circular = circular),
         vertex.shape = "none", vertex.size = 0, vertex.label = NA,
         edge.color = edgeColors, edge.width = edgeWiths, edge.arrow.mode = "-",
         ...)
  } else {
    plot(ig, layout = layout.reingold.tilford(ig, circular = circular),
         vertex.shape = "rectangle", vertex.color = "lightgray",
         vertex.label.color = "black", vertex.label.font = 1,
         edge.color = "black", edge.width = edgeWiths, edge.arrow.mode = "-",
         ...)
  }
}



## Search space definitions...
## ---
typedNumericConstantSet <- constantFactorySet((function() runif(1, -1, 1)) %::% (list() %->% st("numeric")))

typedLogicalConstantSet <- constantFactorySet((function() runif(1) > .5) %::% (list() %->% st("logical")))

typedArithmeticFunctionSet <- functionSet("+" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
                                          "-" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
                                          "*" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
                                          "/" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")))

typedExpLogFunctionSet <- functionSet("sqrt" %::% (list(st("numeric")) %->% st("numeric")),
                                      "exp" %::% (list(st("numeric")) %->% st("numeric")),
                                      "ln" %::% (list(st("numeric")) %->% st("numeric")))

typedTrigonometricFunctionSet <- functionSet("sin" %::% (list(st("numeric")) %->% st("numeric")),
                                             "cos" %::% (list(st("numeric")) %->% st("numeric")),
                                             "tan" %::% (list(st("numeric")) %->% st("numeric")))

typedMathFunctionSet <- c(typedArithmeticFunctionSet, typedExpLogFunctionSet, typedTrigonometricFunctionSet)

typedBooleanFunctionSet <- functionSet("&" %::% (list(st("logical"), st("logical")) %->% st("logical")),
                                       "|" %::% (list(st("logical"), st("logical")) %->% st("logical")),
                                       "!" %::% (list(st("logical")) %->% st("logical")))

typedLogicalFunctionSet <- c(typedBooleanFunctionSet,
                             functionSet("<" %::% (list(st("numeric"), st("numeric")) %->% st("logical")),
                                         ">" %::% (list(st("numeric"), st("numeric")) %->% st("logical")),
                                         "==" %::% (list(st("numeric"), st("numeric")) %->% st("logical")),
                                         "ifThenElse" %::% (list(st("logical"), st("numeric"), st("numeric")) %->% st("numeric"))))



## Experiments...
## ---
gpdcExperimentReal <- function(samples = 100,
                               functionNamesCode = 'c("+", "-", "*", "/", "sqrt", "exp", "log", "sin", "cos", "tan")',
                               inputVariableNamesCode = 'c("x1")',
                               treeDepth = 5,
                               mutationStepsIntervalCode = "1:3",
                               rmseIntervalCode = "seq(1, 10, by = 0.1)", 
                               plotType = "boxplot",
                               circular = FALSE, structureOnly = TRUE,
                               showOutliers = FALSE,
                               randomSeed = NA,
                               sfClusterInitializationFunction = initLocalCluster) {
  sfClusterInitializationFunction()
  if (!is.na(randomSeed)) {
    set.seed(randomSeed)
    sfClusterSetupRNG(seed = as.integer(randomSeed))
  }
  sfLibrary(rgp)
  funcset <- do.call(functionSet, as.list(eval(parse(text = functionNamesCode))))
  inset <- do.call(inputVariableSet, as.list(eval(parse(text = inputVariableNamesCode))))
  conset <- numericConstantSet
  rmseXs <- eval(parse(text = rmseIntervalCode))
  sfExportAll()
  phenotypicDistanceMeasure <- if (length(inset$all) > 1)
    function(f1, f2) multivariateRmse(length(inset$all), f1, f2, rmseXs)
  else function(f1, f2) univariateRmse(f1, f2, rmseXs)
  sfWorker <- function(mutationSteps) {
    sampleIndividualDistances(samples, funcset, inset, conset,
                              treeDepth = treeDepth, mutationSteps = mutationSteps,
                              distanceMeasure = phenotypicDistanceMeasure)
  }
  genotypicDistances <- eval(parse(text = mutationStepsIntervalCode))
  if (plotType == "population") {
    sampledMutations <- sampleMutations(samples, funcset, inset, conset,
                                        treeDepth = treeDepth, mutationSteps = max(genotypicDistances),
                                        distanceMeasure = phenotypicDistanceMeasure)
    phenotypicDistances <- sampledMutations$distances
    mutatedPopulation <- sampledMutations$mutatedPopulation
  } else {
    phenotypicDistances <- sfLapply(genotypicDistances, sfWorker)
    medianPhenotypicDistances <- sapply(phenotypicDistances, median)
    genotypicPhenotypicDistanceCorrelation <- cor(x = genotypicDistances, y = medianPhenotypicDistances)
  }
  sfStop()
  if (plotType == "boxplot") {
    boxplot(x = phenotypicDistances, outline = showOutliers,
            main = "Phenotypic Distance vs. Genotypic Distance",
            xlab = "Genotypic Distance (Atomic Mutation Steps)", ylab = "Phenotypic Distance (RMSE)")
    mtext(paste("Pearson Correlation: ", genotypicPhenotypicDistanceCorrelation, sep = ""))
  } else if (plotType == "medians") {
    plot(x = genotypicDistances, y = medianPhenotypicDistances,
         type = "b", pch = 1,
         main = "Phenotypic Distance vs. Genotypic Distance",
         xlab = "Genotypic Distance (Atomic Mutation Steps)", ylab = "Phenotypic Distance (RMSE)")
    mtext(paste("Pearson Correlation: ", genotypicPhenotypicDistanceCorrelation, sep = ""))
  } else if (plotType == "population") {
    oldPar <- par(no.readonly = TRUE)
    plotRows <- ceiling(sqrt(samples))
    par(mfrow = c(plotRows, plotRows), mar = c(1, 1, 1, 1))
    for (i in seq_along(mutatedPopulation)) {
      mutatedIndividual <- mutatedPopulation[[i]]
      mutationPhenotypicDistance <- phenotypicDistances[[i]]
      plotIndividual(mutatedIndividual, circular = circular, structureOnly = structureOnly,
                     main = mutationPhenotypicDistance)
    }
    par(oldPar)
  } else if (plotType == "none") {
    plot.new()
    mtext(paste("Pearson Correlation: ", genotypicPhenotypicDistanceCorrelation, sep = ""))
  } else stop("gpdcExperimentReal: Unvalid plotType: ", plotType, ".")
  list(genotypicDistances = genotypicDistances,
       phenotypicDistances = phenotypicDistances)
}

gpdcExperimentBoolean <- function(samples = 100,
                                  functionNamesCode = 'c("&", "|", "!")',
                                  inputDimension = 2,
                                  treeDepth = 5,
                                  mutationStepsIntervalCode = "1:3", 
                                  plotType = "boxplot",
                                  circular = FALSE, structureOnly = TRUE,
                                  showOutliers = FALSE,
                                  randomSeed = NA,
                                  sfClusterInitializationFunction = initLocalCluster) {
  sfClusterInitializationFunction()
  if (!is.na(randomSeed)) {
    set.seed(randomSeed)
    sfClusterSetupRNG(seed = as.integer(randomSeed))
  }
  sfLibrary(rgp)
  funcset <- do.call(functionSet, as.list(eval(parse(text = functionNamesCode))))
  inset <- do.call(inputVariableSet, as.list(paste("x", 1:inputDimension, sep = "")))
  conset <- typedLogicalConstantSet
  sfExportAll()
  phenotypicDistanceMeasure <- function(f1, f2) multivariateErrorCount(length(inset$all), f1, f2)
  sfWorker <- function(mutationSteps) {
    sampleIndividualDistances(samples, funcset, inset, conset,
                              treeDepth = treeDepth, mutationSteps = mutationSteps,
                              distanceMeasure = phenotypicDistanceMeasure)
  }
  genotypicDistances <- eval(parse(text = mutationStepsIntervalCode))
  if (plotType == "population") {
    sampledMutations <- sampleMutations(samples, funcset, inset, conset,
                                        treeDepth = treeDepth, mutationSteps = max(genotypicDistances),
                                        distanceMeasure = phenotypicDistanceMeasure)
    phenotypicDistances <- sampledMutations$distances
    mutatedPopulation <- sampledMutations$mutatedPopulation
  } else {
    phenotypicDistances <- sfLapply(genotypicDistances, sfWorker)
    medianPhenotypicDistances <- sapply(phenotypicDistances, median)
    genotypicPhenotypicDistanceCorrelation <- cor(x = genotypicDistances, y = medianPhenotypicDistances)
  }
  sfStop()
  if (plotType == "boxplot") {
    boxplot(x = phenotypicDistances, outline = showOutliers,
            main = "Phenotypic Distance vs. Genotypic Distance",
            xlab = "Genotypic Distance (Atomic Mutation Steps)", ylab = "Phenotypic Distance (# Errors)")
    mtext(paste("Pearson Correlation: ", genotypicPhenotypicDistanceCorrelation, sep = ""))
  } else if (plotType == "medians") {
    plot(x = genotypicDistances, y = medianPhenotypicDistances,
         type = "b", pch = 1,
         main = "Phenotypic Distance vs. Genotypic Distance",
         xlab = "Genotypic Distance (Atomic Mutation Steps)", ylab = "Phenotypic Distance (# Errors)")
    mtext(paste("Pearson Correlation: ", genotypicPhenotypicDistanceCorrelation, sep = ""))
  } else if (plotType == "population") {
    oldPar <- par(no.readonly = TRUE)
    plotRows <- ceiling(sqrt(samples))
    par(mfrow = c(plotRows, plotRows), mar = c(1, 1, 1, 1))
    for (i in seq_along(mutatedPopulation)) {
      mutatedIndividual <- mutatedPopulation[[i]]
      mutationPhenotypicDistance <- phenotypicDistances[[i]]
      plotIndividual(mutatedIndividual, circular = circular, structureOnly = structureOnly,
                     main = mutationPhenotypicDistance)
    }
    par(oldPar)
  } else if (plotType == "none") {
    plot.new()
    mtext(paste("Pearson Correlation: ", genotypicPhenotypicDistanceCorrelation, sep = ""))
  } else stop("gpdcExperimentBoolean: Unvalid plotType: ", plotType, ".")
  list(genotypicDistances = genotypicDistances,
       phenotypicDistances = phenotypicDistances)
}

gpdcExperimentRealInteractive <- function(sfClusterInitializationFunction = initLocalCluster) {
  invisible(twiddle(gpdcExperimentReal(samples, functionNamesCode, inputVariableNamesCode, treeDepth,
                                       mutationStepsIntervalCode, rmseIntervalCode,
                                       plotType, circular, structureOnly, showOutliers, randomSeed,
                                       sfClusterInitializationFunction),
                    samples = knob(label = "Samples", lim = c(1, 500), res = 1, ticks = 0,
                      default = 25),
                    functionNamesCode = entry(label = "Function Set", length = 26,
                      default = 'c("+", "-", "*", "/", "sqrt", "exp", "log", "sin", "cos", "tan")'),
                    inputVariableNamesCode = entry(label = "Input Variable Set", length = 26,
                      default = 'c("x1")'),
                    treeDepth = knob(label = "Tree Depth", lim = c(1, 10), res = 1, ticks = 0,
                      default = 5),
                    mutationStepsIntervalCode = entry(label = "Genotypic Dist.", length = 26,
                      default = "1:3"),
                    rmseIntervalCode = entry(label = "RMSE Domain", length = 26,
                      default = "seq(1, 10, by = 0.1)"),
                    plotType = combo("boxplot", "medians", "population", "none", label = "Plot Type"),
                    circular = toggle(label = "Circular Tree Population Plot",
                      default = FALSE),
                    structureOnly = toggle(label = "Simple Tree Population Plot",
                      default = TRUE),
                    showOutliers = toggle(label = "Show Outliers",
                      default = FALSE),
                    randomSeed = entry(label = "Random Seed", length = 26,
                      default = "1"),
                    eval = FALSE,
                    auto = FALSE))
}

gpdcExperimentBooleanInteractive <- function(sfClusterInitializationFunction = initLocalCluster) {
  invisible(twiddle(gpdcExperimentBoolean(samples, functionNamesCode, inputDimensions, treeDepth,
                                          mutationStepsIntervalCode,
                                          plotType, circular, structureOnly, showOutliers, randomSeed,
                                          sfClusterInitializationFunction),
                    samples = knob(label = "Samples", lim = c(1, 500), res = 1, ticks = 0,
                      default = 25),
                    functionNamesCode = entry(label = "Function Set", length = 26,
                      default = 'c("&", "|", "!")'),
                    inputDimensions = knob(label = "Input Dimension", lim = c(1, 10), res = 1, ticks = 0,
                      default = 2),
                    treeDepth = knob(label = "Tree Depth", lim = c(1, 10), res = 1, ticks = 0,
                      default = 5),
                    mutationStepsIntervalCode = entry(label = "Genotypic Dist.", length = 26,
                      default = "1:3"),
                    plotType = combo("boxplot", "medians", "population", "none", label = "Plot Type"),
                    circular = toggle(label = "Circular Tree Population Plot",
                      default = FALSE),
                    structureOnly = toggle(label = "Simple Tree Population Plot",
                      default = TRUE),
                    showOutliers = toggle(label = "Show Outliers",
                      default = FALSE),
                    randomSeed = entry(label = "Random Seed", length = 26,
                      default = "1"),
                    eval = FALSE,
                    auto = FALSE))
}


message("Type gpdcExperimentRealInteractive() or gpdcExperimentBooleanInteractive() to start...")
