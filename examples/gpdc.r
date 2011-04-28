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

univariateRmse <- function(f1, f2, xs = seq(1, 10, by = 0.1)) {
  ys1 <- Vectorize(f1)(xs)
  ys2 <- Vectorize(f2)(xs)
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

## Experiments...
## ---
gpdcExperiment <- function(samples = 100,
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
  } else if (plotType == "none") {
    ## draw no plot at all
  } else stop("gpdcExperiment: Unvalid plotType: ", plotType, ".")
  list(genotypicDistances = genotypicDistances,
       phenotypicDistances = phenotypicDistances)
}

gpdcExperimentInteractive <- function(sfClusterInitializationFunction = initLocalCluster) {
  invisible(twiddle(gpdcExperiment(samples, functionNamesCode, treeDepth,
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
                    plotType = combo("boxplot", "none", label = "Plot Type"),
                    showOutliers = toggle(label = "Show Outliers",
                      default = FALSE),
                    randomSeed = entry(label = "Random Seed", length = 26,
                      default = "1"),
                    eval = FALSE,
                    auto = FALSE))
}

message("Type gpdcExperimentInteractive() to start...")
