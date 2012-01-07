evalSsa2 <- function(ssa, pos, envList) {
	if(is.list(ssa[[pos]])) {
		children <- ssa[[pos]][-1]
		return(do.call(as.character(ssa[[pos]][[1]]), Map(evalSsa2, list(ssa), children, list(envList))))
	}
	else {
		if(is.name(ssa[[pos]]))
			envList[[as.character(ssa[[pos]])]]
		else
			ssa[[pos]]
	}
}

evalSsa <- function(ssa, envList=NULL) {
	if(is.null(envList))
		NULL
	else
		evalSsa2(ssa, length(ssa), envList)
}

evalSsaAsAst <- function(ssa, env1) {
	return(eval(ssaToAst(ssa), envir = env1))
}

