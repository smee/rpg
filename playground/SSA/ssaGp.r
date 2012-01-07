##' ***Beispieldaten:***
##'
##'

# dataframe für formel 10*sqrt(a) + b^2
e <- 11
a <- 1
b <- 1
mydf3 <- data.frame(e, a,b)	
mydf3 <- rbind(mydf3, list( 14, 1, 2 ))
mydf3 <- rbind(mydf3, list( 19, 1, 3 ))
mydf3 <- rbind(mydf3, list( 26, 1, 4 ))
mydf3 <- rbind(mydf3, list( 35, 1, 5 ))
mydf3 <- rbind(mydf3, list( 15.1421356, 2, 1 ))
mydf3 <- rbind(mydf3, list( 18.3205081, 3, 1 ))
mydf3 <- rbind(mydf3, list( 21, 4, 1 ))
mydf3 <- rbind(mydf3, list( 23.3606798, 5, 1 ))
mydf3 <- rbind(mydf3, list( 32.6227766, 10, 1 ))

#beispiel fitnessfunktion zu mydf3
fitFunc <- makeSsaRegressionFitnessFunction(formula(mydf3), mydf3, errormeasure = rmse,
                                           indsizelimit = 64)

# Beispielaufruf von ssaSymbilicRegression als vereinfachte funktion (siehe includes.txt für alle in R einzulesenden Dateien)
ssaSr <- function(runs = 50, populationSize = 100) ssaAltSymbolicRegression(formula(mydf3), mydf3, runs = runs, populationSize = populationSize)
##'
##'



ssaAltSymbolicRegression <- function(formula, data, functionSet = mathFunctionSet, 
							constantFactory = numericConstantSet,individualSizeLimit = 64, populationSize = 100, runs = 500) {
	mf <- model.frame(formula, data)
	variableNames <- attr(terms(formula(mf)), "term.labels")
	inVarSet <- inputVariableSet(list=as.list(variableNames)) # (machs jetzt aus kompatibilitätsgründen so wie oliver) # as.list(variableNames)
	fitFunc <- makeSsaRegressionFitnessFunction(formula(mf), mf, errormeasure = rmse,
                                           indsizelimit = individualSizeLimit)
	gpresult <- ssaGeneticProgramming(fitFunc, functionSet, inVarSet, constantFactory, populationSize = populationSize, runs = runs)
	
}

ssaGeneticProgramming <- function(fitnessFunction, functionSet, inVarSet, constantFactory, populationSize = 100, runs = 500) {
	#print("anfang ssaGeneticProgramming") ### testcode
	population <- makeSsaPopulationWithInitialFitness(fitnessFunction, functionSet, inVarSet, constantFactory, populationSize)
	population <- sortPopulationByFitness(population) 
	doEvolutionaryRun(population, fitnessFunction, functionSet, inVarSet, constantFactory, populationSize, runs)
}

makeSsaPopulationWithInitialFitness <- function(fitnessFunction, functionSet, inVarSet, constantFactory, populationSize) {
	population <- list()
	for(i in 1:populationSize) {
		population <- c(population, list(ssaMutateInsertSubtree(NULL, functionSet, inVarSet, constantFactory, strength = 1)))		
		population[[i]] <- list(ind=population[[i]],fitness = fitnessFunction(population[[i]]))
	}
	population
}

sortPopulationByFitness <- function(population) {
	population[order(as.numeric(sapply(population,"[","fitness")))]
}

doEvolutionaryRun <- function(population, fitnessFunction, functionSet, inVarSet, constantFactory, populationSize = 100, runs = 500) {
	#print("anfang evolutionary run") ### testcode
	borderList <- calculateBorders(populationSize)
	for(i in 1:runs) {
		population <- mutatePopulationAndDetermineFitness(population, borderList, fitnessFunction, functionSet, inVarSet, constantFactory, populationSize)
		population <- sortPopulationByFitness(population)
	}
	population
}

##' The Population is divided into sections which the indivuduals are treat different in.
##' This function determines the borders of these different sections in the population list depending on the size of the population.
##' 
##' return a list with the keys border10, border25 and border50 where e.g. border10 seperates the fittest 10% of the population from the weaker rest. The border10 index is included in the fittest 10% section
calculateBorders <- function(populationSize) {
	border10 <- ceiling(populationSize * 0.1)
	border25 <- ceiling(populationSize * 0.25)
	border50 <- ceiling(populationSize * 0.5)
	list(border10=border10, border25=border25, border50=border50)
}

mutatePopulationAndDetermineFitness <- function(population, borderList, fitnessFunction, functionSet, inVarSet, constantFactory, populationSize) {
	population <- mutateTopTenToBottomTen(population, borderList, fitnessFunction, functionSet, inVarSet, constantFactory, populationSize)
	population <- mutateUpperAndLowerSectionAndOverrideEveryFourthFromLowerSectionWithUpperSectionChild(population, borderList, fitnessFunction, functionSet, inVarSet, constantFactory, populationSize)
	#population <- mutateMidSectionAndOverrideEveryTenthFromLowerSectionWithUpperSectionChild(population, borderList, fitnessFunction, functionSet, inVarSet, constantFactory, populationSize)
	population
}

mutateTopTenToBottomTen <- function(population, borderList, fitnessFunction, functionSet, inVarSet, constantFactory, populationSize) {
	#print("anfang mutate top 10") ### testcode
	for(i in 1:borderList$border10) {
		population <- overrideLowerIndividualWithUpperChildAndAssignFitness(population, i, fitnessFunction, functionSet, inVarSet, constantFactory, populationSize)
	}
	population
}

overrideLowerIndividualWithUpperChildAndAssignFitness <- function(population, index, fitnessFunction, functionSet, inVarSet, constantFactory, populationSize) {
	population[[populationSize - index + 1]] <- ssaMutate(population[[index]]$ind, functionSet, inVarSet, constantFactory)
	population[[populationSize - index + 1]] <- list(ind=population[[populationSize - index + 1]], fitness=fitnessFunction(population[[populationSize - index + 1]]))
	population
}

mutateUpperAndLowerSectionAndOverrideEveryFourthFromLowerSectionWithUpperSectionChild <- function(population, borderList, fitnessFunction, functionSet, inVarSet, constantFactory, populationSize) {
	#print("anfang mutate upper and lower") ### testcode
	for(i in (borderList$border10 + 1):borderList$border25) {		
		if(i%%4==0) 
			population <- overrideLowerIndividualWithUpperChildAndAssignFitness(population, i, fitnessFunction, functionSet, inVarSet, constantFactory, populationSize)			
		else 
			population <- mutateThisIndividualAndAssignFitness(population, populationSize - i + 1, fitnessFunction, functionSet, inVarSet, constantFactory)
		population <- mutateThisIndividualAndAssignFitness(population, i, fitnessFunction, functionSet, inVarSet, constantFactory)
	}
	population
}

##' mutates the individual at the given index
##' return the whole population with the new mutatet individual and assigned fitness value at the index location
mutateThisIndividualAndAssignFitness <- function(population, index, fitnessFunction, functionSet, inVarSet, constantFactory) {
	population[[index]] <- ssaMutate(population[[index]]$ind, functionSet, inVarSet, constantFactory)
	population[[index]] <- list(ind=population[[index]], fitness=fitnessFunction(population[[index]]))
	population
}

mutateMidSectionAndOverrideEveryTenthFromLowerSectionWithUpperSectionChild <- function(population, borderList, fitnessFunction, functionSet, inVarSet, constantFactory, populationSize) {
	#print("anfang mutate mid section") ### testcode
	for(i in (borderList$border25 + 1):borderList$border50) {
		if(i%%10==0)
			population <- overrideLowerIndividualWithUpperChildAndAssignFitness(population, i, fitnessFunction, functionSet, inVarSet, constantFactory, populationSize)			
		else
			population <- mutateThisIndividualAndAssignFitness(population, populationSize - i + 1, fitnessFunction, functionSet, inVarSet, constantFactory)
		population <- mutateThisIndividualAndAssignFitness(population, i, fitnessFunction, functionSet, inVarSet, constantFactory)
	}
	population
}

