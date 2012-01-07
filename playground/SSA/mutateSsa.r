ssaMutate <- function(ssaFunc, funcset, inset, conset, strength = 1, constprob = 0.4) {
	rand <- runif(1)
	if(rand <= 1/3) 
		ssaMutateDeleteSubtree(ssaFunc, inset, conset, strength, constprob)
	else if(rand <= 2/3)
		ssaMutateInsertSubtree(ssaFunc, funcset, inset, conset, strength, constprob)
	else 
		ssaMutateChangeLabel(ssaFunc, funcset, inset, conset, strength, constprob)
}

mut <- function(theSsa) {
	ssaMutate(theSsa, list("+", "-", "*", "/", "sin"), list("ah", "beh", "ceh"), list(1337, 42))
}

ssaMutateDeleteSubtree <- function(ssaFunc, inset, conset,
                                strength = 1,
                                constprob = 0.4) { 
    if(strength>0) { 	
		### TODO: ssaGetAllSmallSubtrees von linearer auf logarithmische laufzeit bringen, indem der baum mit zufallsrichtungen durchlaufen wird und wenn 
		### er ganz unten ankommt hat der parent entweder tiefe von 1 oder fuehrt in einer anderen richtung noch weiter nach unten. nach abschluss des algs 
		### hat man ein zufaellig ausgewaehlten subtree der hoehe 2. zufallsauswahl mit randelt also nichtmehr noetig. implementierung nach fittnesauswertung.	
		candidate <- randelt(ssaGetAllSmallSubtrees(ssaFunc))
		if(!is.null(candidate)) {
			#print("delete label") ### testcode
			arit <- length(ssaFunc[[candidate]]) - 1		
			for(i in 1:arit) 
				ssaFunc[[candidate - i + 1]] <- NULL 
			constfactory <- randelt(conset$all, prob = attr(conset$all, "probabilityWeight"))
			newLeaf <- if(runif(1)<=constprob) constfactory() else toName(randelt(inset$all, prob = attr(inset$all, "probabilityWeight")))
			ssaFunc <- correctRegisterValues(ssaFunc, - arit, candidate - arit)
			ssaFunc[[candidate - arit]] <- newLeaf
			ssaMutateDeleteSubtree(ssaFunc, inset, conset, strength - 1, constprob)
		}
		else {
			#print("kein delete, zu kurz") ### testcode
			ssaFunc
		}
	}
	else
		ssaFunc
}
	
mds <- function(theSsa) {
	ssaMutateDeleteSubtree(theSsa, list("ah", "beh", "ceh"), list(1337, 42), 1)
}		
ssaMutateInsertSubtree <- function(ssaFunc, funcset, inset, conset,
                                strength = 1,
								constprob = 0.4) {									
	if(strength>0) {	
		if(ssaFuncSize(ssaFunc)>0) {
			#print("insert label") ### testcode 
			### TODO: aehnlich wie bei ssaGetAllSmallSubtrees (siehe TODO) den alg von linearer auf logarithmische laufzeit bringen. gleiches prinzip, nur ganz bis nach unten gehen. 
			candidate <- randelt(ssaGetAllFuncLeaves(ssaFunc))
			newSubtree <- toName(randelt(funcset$all))
			arit <- arity(newSubtree)
			for(i in 1:arit) {
				if(runif(1)<=constprob) {
					constfactory <- randelt(conset$all, prob = attr(conset$all, "probabilityWeight"))
					newSubtree <- append(constfactory(), newSubtree)
				}
				else
					newSubtree <- append(toName(randelt(inset$all, prob = attr(inset$all, "probabilityWeight"))), newSubtree)
			}
			newSubtree[[arit + 1]] <- append(newSubtree[[arit + 1]], 1:arit + candidate - 1)
			ssaFunc <- correctRegisterValues(ssaFunc, length(newSubtree) - 1, candidate) 
			ssaFunc[[candidate]] <- NULL
			ssaFunc <- append(ssaFunc, newSubtree, candidate - 1)
			ssaMutateInsertSubtree(ssaFunc, funcset, inset, conset, strength - 1, constprob)
		}
		else {
			#print("create label") ### testcode
			newSubtree <- toName(randelt(funcset$all))
			arit <- arity(newSubtree)
			for(i in 1:arit) {
				if(runif(1)<=constprob) {				
					constfactory <- randelt(conset$all, prob = attr(conset$all, "probabilityWeight"))
					newSubtree <- append(constfactory(), newSubtree)					
				}
				else
					newSubtree <- append(toName(randelt(inset$all, prob = attr(inset$all, "probabilityWeight"))), newSubtree)
					
			}
			newSubtree[[arit + 1]] <- append(newSubtree[[arit + 1]], 1:arit) 
			ssaMutateInsertSubtree(newSubtree, funcset, inset, conset, strength - 1, constprob)
		}
	}
	else
		ssaFunc
}		

mis <- function(theSsa) {
	ssaMutateInsertSubtree(theSsa, list("+", "-", "*", "/", "sin"), list("ah", "beh", "ceh"), list(1337, 42))
}	
					
ssaMutateChangeLabel <- function(ssaFunc, funcset, inset, conset,
								strength = 1, constprob) {												
	if(strength>0) {	
		#print("change label") ### testcode 	
		candidate <- randelt(ssaFuncSize(ssaFunc))
		if(is.list(ssaFunc[[candidate]])) {
			oldfunc <- ssaFunc[[candidate]][[1]]
			newfunccandidate <- toName(randelt(funcset$all))
			ssaFunc[[candidate]][[1]] <- if (arity(newfunccandidate) == arity(oldfunc)) newfunccandidate 
				else {	
					#print("arity fail") ### testcode
					oldfunc 
				} 
		}
		else {
			prob <- runif(1)
			ssaFunc[[candidate]] <- if(prob > 0.4) { ## change it with something of the same type
				if(is.numeric(ssaFunc[[candidate]])) {
					ssaFunc[[candidate]] + rnorm(1)
				}
				else if(is.name(ssaFunc[[candidate]])) {
					toName(randelt(inset$all, prob = attr(inset$all, "probabilityWeight")))
				}
				else if(is.logical(ssaFunc[[candidate]])) {
					as.logical(rbinom(1, 1, 0.5))
				}
			}
			else { ## change it with a random type
				if(prob <= constprob) {
					constfactory <- randelt(conset$all, prob = attr(conset$all, "probabilityWeight"))
					constfactory()
				}
				else
					toName(randelt(inset$all, prob = attr(inset$all, "probabilityWeight")))
			}
		}
		ssaMutateChangeLabel(ssaFunc, funcset, inset, conset, strength - 1)
	}
	else
		ssaFunc
}

mcl <- function(theSsa) {
	ssaMutateChangeLabel(theSsa, list("+", "-", "*", "/", "sin"), list("ah", "beh", "ceh"), list(1337, 42))
}

