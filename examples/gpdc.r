## gpdc.r
##
## Interactive Analysis of Genotype-Phenotype-Distance-Correlation in Genetic Programming
##
## 2011 Oliver Flasch
##

require("rgp")
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

sampleIndividualDistances <- function(n, funcset, inset, conset,
                                      treeDepth = 5,
                                      mutationSteps = 1,
                                      individualFactory = function()
                                        makeRandomIndividual(funcset, inset, conset, treeDepth = treeDepth),
                                      mutationFunction = function(ind)
                                        mutateChangeDeleteInsert(ind, funcset, inset, conset, iterations = mutationSteps),
                                      distanceMeasure = univariateRmse) {
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


## Search space definitions...
## ---

# TODO
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
                               treeDepth = 5,
                               mutationStepsIntervalCode = "1:2",
                               rmseIntervalCode = "seq(1, 10, by = 0.1)", 
                               plotType = "boxplot",
                               showOutliers = FALSE,
                               randomSeed = NA,
                               sfClusterInitializationFunction = initLocalCluster) {
  sfClusterInitializationFunction()
  if (!is.na(randomSeed))
    sfClusterSetupRNG(seed = as.integer(randomSeed))
  sfLibrary(rgp)
  funcset <- do.call(functionSet, as.list(eval(parse(text = functionNamesCode))))
  inset <- inputVariableSet("x")
  conset <- numericConstantSet
  rmseXs <- eval(parse(text = rmseIntervalCode))
  sfExportAll()
  phenotypicDistanceMeasure <- function(f1, f2) univariateRmse(f1, f2, rmseXs)
  sfWorker <- function(mutationSteps) {
    sampleIndividualDistances(samples, funcset, inset, conset,
                              treeDepth = treeDepth, mutationSteps = mutationSteps,
                              distanceMeasure = phenotypicDistanceMeasure)
  }
  genotypicDistances <- eval(parse(text = mutationStepsIntervalCode))
  phenotypicDistances <- sfLapply(genotypicDistances, sfWorker)
  sfStop()
  medianPhenotypicDistances <- sapply(phenotypicDistances, median)
  genotypicPhenotypicDistanceCorrelation <- cor(x = genotypicDistances, y = medianPhenotypicDistances)
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
  } else if (plotType == "none") {
    plot.new()
    mtext(paste("Pearson Correlation: ", genotypicPhenotypicDistanceCorrelation, sep = ""))
  } else stop("gpdcExperimentReal: Unvalid plotType: ", plotType, ".")
  list(genotypicDistances = genotypicDistances,
       phenotypicDistances = phenotypicDistances)
}

gpdcExperimentRealInteractive <- function(sfClusterInitializationFunction = initLocalCluster) {
  invisible(twiddle(gpdcExperimentReal(samples, functionNamesCode, treeDepth,
                                       mutationStepsIntervalCode, rmseIntervalCode,
                                       plotType, showOutliers, randomSeed,
                                       sfClusterInitializationFunction),
                    samples = knob(label = "Samples", lim = c(1, 500), res = 1, ticks = 0,
                      default = 20),
                    functionNamesCode = entry(label = "Function Set", length = 26,
                      default = 'c("+", "-", "*", "/", "sqrt", "exp", "log", "sin", "cos", "tan")'),
                    treeDepth = knob(label = "Tree Depth", lim = c(1, 10), res = 1, ticks = 0,
                      default = 5),
                    mutationStepsIntervalCode = entry(label = "Genotypic Dist.", length = 26,
                      default = "1:3"),
                    rmseIntervalCode = entry(label = "RMSE Domain", length = 26,
                      default = "seq(1, 10, by = 0.1)"),
                    plotType = combo("boxplot", "medians", "none", label = "Plot Type"),
                    showOutliers = toggle(label = "Show Outliers",
                      default = FALSE),
                    randomSeed = entry(label = "Random Seed", length = 26,
                      default = "1"),
                    eval = FALSE,
                    auto = FALSE))
}

message("Type gpdcExperimentRealInteractive() or gpdcExperimentBooleanInteractive() to start...")
