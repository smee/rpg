dfToLists2 <- function(df, resultList) {
	if(length(df[,1])>0) {
		resultList <- c(resultList, list(as.list(df[1,-1])))
		df <- df[-1,]
		dfToLists2(df, resultList)
	}
	else 
		resultList
}

dfToLists <- function(df) {
	resultList <- list()
	dfToLists2(df, resultList)
}


makeSsaRegressionFitnessFunction <- function(formula, data, errormeasure = rmse,
                                          indsizelimit = NA) {
  data <- if (any(is.na(data))) {
    dataWithoutNAs <- na.omit(data)
    warning(sprintf("removed %i data rows containing NA values",
                    length(attr(dataWithoutNAs, "na.action"))))
    dataWithoutNAs
  } else data
  formulaVars <- as.list(attr(terms(formula), "variables")[-1])
  responseVariable <- formulaVars[[1]]
  explanatoryVariables <- formulaVars[-1]
  trueResponse <- eval(responseVariable, envir=data)
  explanatories <- dfToLists(data)
  function(ind) { 
    ysind <- mapply(evalSsa, list(ind), explanatories)
    errorind <- errormeasure(trueResponse, ysind)    
    if (!is.na(indsizelimit) && ssaFuncSize(ind) > indsizelimit)
      Inf # individual size limit exceeded
    else if (is.na(errorind) || is.nan(errorind))
      Inf # error value is NA or NaN
    else errorind
  }
}

