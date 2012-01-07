library("rgp")
#source("astOps.r")
source("evalSsa.r")
source("fitnessSsa.r")
source("helpingFunctions.r")
source("mutateSsa.r")
source("ssaEvolution.r")
#source("ssaGp.r")


ssaSymbolicRegression <- function(formula, data,
                               stopCondition = makeTimeStopCondition(5),
                               population = NULL,
                               populationSize = 100,
                               eliteSize = ceiling(0.1 * populationSize),
                               elite = list(),
                               # extinctionPrevention not supported
                               # archive not supported
                               # genealogy not supported
                               individualSizeLimit = 64,
                               penalizeGenotypeConstantIndividuals = FALSE,
                               functionSet = mathFunctionSet,
                               constantSet = numericConstantSet,
                               selectionFunction = fitnessProportionalSelectionSsa,
                               # crossoverFunction not supported
                               mutationFunction = ssaMutate,
                               # restartCondition not supported
                               # restartStrategy not supported
                               breedingFitness = function(individual) TRUE,
                               breedingTries = 50,
                               progressMonitor = NULL,
                               verbose = TRUE,
							   mutStrength = 1,
							   mutConstprob = 0.4,
							   initPopConstprob = 0.3,
							   subtreeDepth = 2) {
  ## Match variables in formula to those in data or parent.frame() and
  ## return them in a new data frame. This also expands any '.'
  ## arguments in the formula.  
  mf <- model.frame(formula, data)
  ## Extract list of terms (rhs of ~) in expanded formula
  variableNames <- attr(terms(formula(mf)), "term.labels")
  ## Create inputVariableSet
  inVarSet <- inputVariableSet(list=as.list(variableNames))
  fitFunc <- makeSsaRegressionFitnessFunction(formula(mf), mf, errormeasure = rmse, indsizelimit = individualSizeLimit)
		
  gpModel <- geneticSsaProgramming(fitFunc,
                                stopCondition = stopCondition,
                                population = population,
                                populationSize = populationSize,
                                eliteSize = eliteSize,
                                elite = elite,
                                functionSet = functionSet,
                                inputVariables = inVarSet,
                                constantSet = constantSet,
                                selectionFunction = selectionFunction,
                                # crossoverFunction not supported
                                mutationFunction = mutationFunction,
                                # restartCondition not supported
                                # restartStrategy not supported
                                breedingFitness = breedingFitness,
                                breedingTries = breedingTries,
                                # extinctionPrevention not supported
                                # archive not supported
                                # genealogy not supported
                                progressMonitor = progressMonitor,
                                verbose = verbose,
								mutStrength = mutStrength,
								mutConstprob = mutConstprob,
								initPopConstprob = initPopConstprob,
								subtreeDepth = subtreeDepth)
  
  structure(append(gpModel, list(formula = formula(mf))),
                   class = c("symbolicRegressionModel", "geneticProgrammingResult"))
}

##' a little modified version of geneticProgramming. For docs see evolution.r/geneticProgramming by Oliver Flasch.
##' Added parameter individualFactory
geneticSsaProgramming <- function(fitnessFunction,
                               stopCondition = makeTimeStopCondition(5),
                               population = NULL,
                               populationSize = 100,
                               eliteSize = ceiling(0.1 * populationSize),
                               elite = list(),
                               functionSet = mathFunctionSet,
                               inputVariables = inputVariableSet("x"),
                               constantSet = numericConstantSet,
                               selectionFunction = fitnessProportionalSelectionSsa,
                               # crossoverFunction not supported
                               mutationFunction = NULL,
                               # restartCondition not supported
                               # restartStrategy not supported
                               breedingFitness = function(individual) TRUE,
                               breedingTries = 50,
							   # extinctionPrevention not supported
                               # archive not supported
                               # genealogy not supported
                               progressMonitor = NULL,
                               verbose = TRUE,
							   mutStrength = 1,
							   mutConstprob = 0.4,
							   initPopConstprob = 0.3,
							   subtreeDepth = subtreeDepth,
							   individualFactory = function() randSsaIndividual(functionSet, inputVariables, constantSet,
								maxfuncdepth=8, constprob = initPopConstprob)) {
  ## Provide default parameters and initialize GP run...
  logmsg <- function(msg, ...) {
    if (verbose)
      message(sprintf(msg, ...))
  }
  progmon <-
    if (verbose) {
      function(pop, fitnessFunction, stepNumber, evaluationNumber, bestFitness, timeElapsed) {
        if (!is.null(progressMonitor))
          progressMonitor(pop, fitnessFunction, stepNumber, evaluationNumber, bestFitness, timeElapsed)
        if (stepNumber %% 100 == 0)
          logmsg("evolution step %i, fitness evaluations: %i, best fitness: %f, time elapsed: %s",
                 stepNumber, evaluationNumber, bestFitness, formatSeconds(timeElapsed))
      }
    } else if (is.null(progressMonitor)) {
      function(pop, fitnessFunction, stepNumber, evaluationNumber, bestFitness, timeElapsed) NULL # verbose == FALSE, do not show progress
    } else
      progressMonitor
  mutatefunc <-
    if (is.null(mutationFunction)) {
      stop("mutate Function must be given")
    } else
      mutationFunction
  pop <-
    if (is.null(population))
      makePopulation(populationSize, functionSet, inputVariables, constantSet,
                     extinctionPrevention = FALSE,
                     breedingFitness = breedingFitness, breedingTries = breedingTries,
					 funcfactory=individualFactory)
    else
      population
  stepNumber <- 1
  evaluationNumber <- 0
  startTime <- proc.time()["elapsed"]
  timeElapsed <- 0
  bestFitness <- Inf # best fitness value seen in this run, if multi-criterial, only the first component counts

  
  ## Execute GP run...
  logmsg("STARTING standard genetic programming evolution run...")
  while (!stopCondition(pop = pop, fitnessFunction = fitnessFunction, stepNumber = stepNumber,
                        evaluationNumber = evaluationNumber, bestFitness = bestFitness, timeElapsed = timeElapsed)) {
    
	# Create new population through mutation of the old population...
	for(i in 1:length(pop)) {
		pop[[i]] <- list(ind=mutatefunc(pop[[i]]$ind, functionSet, inputVariables, constantSet, mutStrength, mutConstprob))
	}
	
	###test
	#replace 1 individual with best from eliteset
	#if(length(elite)>0) pop[[1]] <- elite[[1]]
	
	# select the individuals chosen to create offpring
    selection <- selectionFunction(pop, fitnessFunction)  
	
	# remember fitness for each individual
	pop <- selection$population
	
	bestFitness <- min(c(bestFitness,unlist(sapply(selection$population[1:length(pop)],"[","fitness"))))
   
	pop <- pop[selection$selectedIds]
	
	elite <- joinSsaElites(pop, elite, eliteSize, fitnessFunction) # insert pop into elite at end of run  
	
    timeElapsed <- proc.time()["elapsed"] - startTime
    stepNumber <- 1 + stepNumber
    evaluationNumber <- selection$numberOfFitnessEvaluations + evaluationNumber
    progmon(pop = pop, fitnessFunction = fitnessFunction, stepNumber = stepNumber,
            evaluationNumber = evaluationNumber, bestFitness = bestFitness, timeElapsed = timeElapsed)
  }
  elite <- joinSsaElites(pop, elite, eliteSize, fitnessFunction) # insert pop into elite at end of run
  logmsg("Standard genetic programming evolution run FINISHED after %i evolution steps, %i fitness evaluations and %s.",
         stepNumber, evaluationNumber, formatSeconds(timeElapsed))

  ## Return GP run result...
  structure(list(fitnessFunction = fitnessFunction,
                 stopCondition = stopCondition,
                 timeElapsed = timeElapsed,
                 stepNumber = stepNumber,
                 evaluationNumber = evaluationNumber,
                 bestFitness = bestFitness,
                 population = pop,
                 elite = elite,
                 functionSet = functionSet,
                 constantSet = constantSet,
                 mutationFunction = mutatefunc,              
                 breedingFitness = breedingFitness,
                 breedingTries = breedingTries), class = "geneticProgrammingResult")
}

randSsaIndividual <- function(functionSet, inputVariables, constantSet,
								maxfuncdepth=8, constprob = 0.2) {
	ssaInd <- ssaMutateInsertSubtree(NULL, functionSet, inputVariables, constantSet, strength =  maxfuncdepth - 1, constprob = constprob)
	list(ind=ssaInd)
}

##' returns a list of ids which represent the index of the indivudials choosen to create one offspring.
##' individuals can be in this list multiple times and should later create as many offspring as their index is counted in this list
fitnessProportionalSelectionSsa <- function(population, fitnessFunction=makeSsaRegressionFitnessFunction(formula(mydf3), mydf3, errormeasure = rmse,
																			indsizelimit = 64) ) {
	popLen <- length(population)	
	propVector <- vector()
	for(i in 1:popLen) {
		fitness <- fitnessFunction(population[[i]]$ind)
		selectionPropability <- 1/fitness
		propVector <- c(propVector, selectionPropability)
		population[[i]] <- list(ind=population[[i]]$ind, fitness=fitness, selectionPropability=selectionPropability)
	}
	candidates <- list(population=population, selectedIds=sample(1:popLen,popLen,replace=TRUE, propVector), numberOfFitnessEvaluations=popLen)
	return(candidates)	
}

##' sorts the population by its $fitness attribute
sortPopulationByFitness <- function(population) {
	population[order(as.numeric(sapply(population,"[","fitness")))]
}

##' updates the elite set
##' fitness function calls in this function are not counted because they're not essentially needed for the evolving of the population
joinSsaElites <- function(individuals, elite, eliteSize, fitnessFunction) {
	allIndividuals <- c(individuals, elite)
	allIndividualsSorted <- sortPopulationByFitness(allIndividuals)
	newElite <- if (length(allIndividualsSorted) > eliteSize)
		allIndividualsSorted[1:eliteSize]
	else
		allIndividualsSorted
	newElite
}

