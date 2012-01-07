ast1 <- quote(a + b * 3)
ast2 <- quote(7-sin(5)+b*3)

op <- function(opcode, ...) list(opcode, ...) 
prog <- list

astToSsa <- function(ast) {
	program <- prog()
	return(astToSsa2(ast,program))
}

astToSsa2 <- function(ast, program) {
	if(is.call(ast) || class(ast)=="(") { # if the class of ast is "call" or "(" than recursive calls follow
		r2 <- 0 # initialized with 0 to be able to decide whether the variable got a new value
		program <- astToSsa2(ast[[2]],program) # recursive call with the first argument 
		r1 <- length(program) # remember the position of the first argument in the ssa-list
		if(length(ast)>=3) { 
			program <- astToSsa2(ast[[3]],program) # recursive call with the second argument if it exists
			r2 <- length(program) # remember the position of the second argument in the ssa-list
		}	
		if(r2==0) # depending on the number of arguments
			program <- c(program, list(list(ast[[1]],r1)))
		else 
			program <- c(program, list(list(ast[[1]],r1, r2)))
	} 
	else { # in this case ast is a terminal number or variable. it is concatenated with the end of the list.
		program <- c(program, ast)
	}
	return(program)
}

ssa1 <- astToSsa(ast1)
ssa2 <- astToSsa(ast2)

ssaToAst <- function(ssa) {
	if(class(ssa[[1]])!="list")
		astList <- list(ssa[[1]])
	else 
		return(null) # unvalid ssa form. at least 1 value must be read at the beginning 
	ssa <- ssa[-1] # delete the first element of the ssa list 
	return(ssaToAst2(ssa, astList))
}

ssaToAst2 <- function(ssa, astList) {
	if(length(ssa)==0) # stop condition
		return(astList[[1]])

	if(class(ssa[[1]])!="list") # its a terminal concatenated with the temporarily ast list
		astList <- c(astList, ssa[[1]])
	else {	
		tmp1 <- astList[[length(astList)]]
		if(length(ssa[[1]])>2) { # the function's got more than 1 argument
			tmp2 <- astList[[length(astList)-1]]
			astList[length(astList)] <- NULL # the last to list elemtns are removed
			astList[length(astList)] <- NULL
			astList <- c(astList, call(as.character(ssa[[1]][[1]]), tmp2, tmp1)) # and replaced by the call whose arguments they are

		}
		else { # the function's got 1 arg
			astList[length(astList)] <- NULL # the last list element is removed
			astList <- c(astList, call(as.character(ssa[[1]][[1]]), tmp1)) # and replaced by the call whose argument it is
		}				
	}
	ssa <- ssa[-1] # the first element of the ssa-list is removed
	return(ssaToAst2(ssa,astList))
} 

