ssaFuncSize <- function(ssaFunc) {
	length(ssaFunc) 
}
##' returns all trees with depth of 2.
ssaGetAllSmallSubtrees <- function(ssaFunc) {
	retList <- list()	
	arit <- 99
	funcPos <- 0
	for(i in length(ssaFunc):1) {
		if(is.list(ssaFunc[[i]])) {
			arit <- length(ssaFunc[[i]])
			funcPos <- i
		}
		if(arit == 1)
			retList <- append(funcPos, retList)
		arit <- arit - 1
	}
	retList
}

ssaGetAllFuncLeaves <- function(ssaFunc) {
	retList <- list()
	for(i in 1:length(ssaFunc)) {
		if(!is.list(ssaFunc[[i]])) 
			retList <- append(retList, i)	
	}
	retList
}

ssaFuncLeaves <- function(ssaFunc) {
	if(length(ssaFunc)>0) {
		if(length(ssaFunc[[1]])==1)
			1 + ssaFuncLeaves(rest(ssaFunc))
		else if(length(ssaFunc[[1]])>1)
			ssaFuncLeaves(rest(ssaFunc))
	}
	else 0
}

ssaExprCount <- function(expr, subtreeDepth) {
	ssaExprCount2(expr, subtreeDepth, expr[[length(expr)]][-1])
}  
  
ssaExprCount2 <- function(expr, subtreeDepth, children) {
	isCall <- (length(expr[[1]])>1)
	if(is.null(children)) 
		matchesPredicate <- (subtreeDepth==1)
	else
		matchesPredicate <- if(ssaExprDepth(expr, children)==subtreeDepth) TRUE else FALSE
	if (length(children)>0) {
		v1 <- vector()
		for(i in 1:length(children)) {
			if(length(expr[[children[[i]]]])>1) 
				v1 <- c(v1, ssaExprCount2(expr, subtreeDepth, expr[[children[[i]]]][-1]))
			else 
				v1 <- c(v1, ssaExprCount2(expr, subtreeDepth, NULL))
		}
		if(matchesPredicate)
			sum(v1) + 1
		else
			sum(v1)
	} else if(matchesPredicate) 1 else 0
}

ssaExprDepth2 <- function(expr, children) {
	if (length(children)>0) {
		v1 <- vector()
		for(i in 1:length(children)) {
			if(length(expr[[children[[i]]]])>1) 
				v1 <- c(v1, ssaExprDepth2(expr,expr[[children[[i]]]][-1]))
			else 
				v1 <- c(v1, ssaExprDepth2(expr,NULL))
		}
		max(v1) + 1
	} else 1
}

ssaExprDepth <- function(expr, children = NULL) {
	if(is.null(children))
		ssaExprDepth2(expr, expr[[length(expr)]][-1])
	else
		ssaExprDepth2(expr, children)
}

randSsaExprFull <- function(funcset, inset, conset, maxdepth, constprob = 0.2, 
							subtreeprob = 0.5, currentNode = 0, curdepth = 1) {
	if(runif(1) <= subtreeprob && curdepth < maxdepth) {
		funcName <- toName(randelt(funcset$all, prob = attr(funcset$all, "probabilityWeight")))
		funcArity <- arity(funcName)
		l <- list()
		l2 <- list()
		curLen <- 0
		for(i in 1:funcArity) {
			rExpr <- randSsaExprFull(funcset, inset, conset, maxdepth, constprob, subtreeprob, currentNode + curLen, curdepth + 1)
			l <- append(l,rExpr)
			l2 <- append(l2, curLen - currentNode + 1) 
			curLen <- curLen + length(rExpr)	
		}
		len <- length(l)
		l3 <- list()
		while(length(l2)>0) {
			l3 <- append(len - l2[[1]], l3)
			l2 <- rest(l2)
		}
		append(l, list(append(funcName, l3)))
	}
	else {
		if(runif(1) <= constprob) {
			constfactory <- randelt(conset$all, prob = attr(conset$all, "probabilityWeight"))
			constfactory()
		}
		else
			toName(randelt(inset$all, prob = attr(inset$all, "probabilityWeight")))		
	}
}

deleteArgumentsOfOldNode <- function(ssaExpr, currentNode, arguments, stopper = 10) {
	if(stopper>0)for(arg in length(arguments):1) {
		oldLen <- length(ssaExpr)
		if(length(ssaExpr[[arguments[[arg]]]])>1) 
			ssaExpr <- deleteArgumentsOfOldNode(ssaExpr, arguments[[arg]], ssaExpr[[arguments[[arg]]]][-1], stopper - 1)
		diffr <- oldLen - length(ssaExpr)
		ssaExpr[[arguments[[arg]]-diffr]] <- NULL
		currentNode <- currentNode - 1 - diffr
		ssaExpr <- correctRegisterValues(ssaExpr, -1, currentNode)
	}	
	ssaExpr
}


correctRegisterValuesOld <- function(ssaExpr, correction) {
	rtrnExpr <- list()
	while(length(ssaExpr)>0) {	
		if(length(ssaExpr[[1]])>1) {
			ltmp <- list()
			for(i in 1:length(ssaExpr[[1]])) {
				if(i==1) 
					ltmp <- append(ltmp, ssaExpr[[1]][[i]])
				else
					ltmp <- append(ltmp, ssaExpr[[1]][[i]] + correction)
			}
			rtrnExpr <- append(rtrnExpr, list(ltmp))
		}
		else
			rtrnExpr <- append(rtrnExpr, ssaExpr[[1]])
		ssaExpr <- rest(ssaExpr)
	}
	rtrnExpr
}

correctRegisterValues <- function(ssaExpr, correction, position) {
	for(i in position:length(ssaExpr)) {
		for(arg in 1:length(ssaExpr[[i]])) {
			if(arg > 1 && ssaExpr[[i]][[arg]] >= position)
				ssaExpr[[i]][[arg]] <- ssaExpr[[i]][[arg]] + correction
		}
	}
	ssaExpr
}

