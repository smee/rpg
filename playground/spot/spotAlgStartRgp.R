###################################################################################################
#' RGP target function for SPOT
#'
#' This target function is used by \code{\link{spotAlgStartRgp}} to start rgp with different population or 
#' tournament sizes.
#'
#' @param populationSize the \code{populationSize} parameter, as used by \code{symbolicRegression()} in \code{rgp}
#' @param tournamentSize the \code{tournamentSize} parameter, as used by \code{makeTournamentSelection()} in \code{rgp}
#'
#' @return this function returns the RMSE (fitness) of the best individual in the population
#'
#' @references  \code{\link{spotAlgStartRgp}}
###################################################################################################
spotCRgpTargetFunction <- function(populationSize = 200, selectionSize = 20, maxDepth = 7, debugOutput = TRUE) {
  if (debugOutput) message("spotCRgpTargetFunction: populationSize= ", populationSize,
                           " selectionSize= ", selectionSize,
                           " maxDepth= ", maxDepth)
  Salutowicz1d <- function(x) exp(-1*x)*x*x*x*sin(x)*cos(x)*(sin(x)*sin(x)*cos(x)-1)
  xSalutowicz1d <- seq(from= 0, to= 12, length= 64)
  ySalutowicz1d <- Salutowicz1d(xSalutowicz1d)

  rmse <- .Call("evolutionRun", steps = 9999999,
    populationSize= populationSize, selectionSize= selectionSize,
    actualParameters= xSalutowicz1d, targetValues= ySalutowicz1d,
    funcset= c("+","-","*","/","sin","cos","sqrt","exp","log"),
    inset= c("x"),
    maxDepth= maxDepth,
    maxleafs= 9999, maxNodes= 9999,
    constprob= 0.3, constScaling= 2, subtreeprob= 0.7,
    rmselimit= 0, Returnrmse= 1,
    silent= 1,
    #runtime= 10)
    runtime= 720) # choose runtime limit (seconds) here
  if (debugOutput) message("rmse= ", rmse)
  rmse
}

###################################################################################################
#' RGP function call for SPOT
#'
#' SPOT uses this function for some demos to call the \code{\link{symbolicRegression}} function
#' with the RGP.
#' This function is needed as an interface, to ensure the right information
#' are passed from SPOT to the target algorithm (i.e. RGP) and vice versa.
#'
#' @param spotConfig Contains the list of spot configurations, results of the algorithm can be passed to this list instead of the .res file.
#'		  spotConfig defaults to "NA", and will only be passed to the Algorithm if spotConfig$spot.fileMode=FALSE. See also: \code{\link{spotGetOptions}}
#'			Items used are: \cr \cr
#'			alg.currentDesign: data frame holding the design points that will be evaluated \cr
#'			io.apdFileName: name of the apd file \cr
#'			io.desFileName: name of the des file \cr
#'			io.resFileName: name of the res file, for logging results (if spotConfig$spot.fileMode==TRUE)\cr
#'			spot.fileMode: boolean, if selected with true the results will also be written to the res file, otherwise it will only be saved in the spotConfig returned by this function\cr
#' @return this function returns the \code{spotConfig} list with the results in spotConfig$alg.currentResult
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{demo}}
###################################################################################################
spotAlgStartRgp <- function(spotConfig){
	#spotInstAndLoadPackages("rgp") #installs and requires() rgp
        dyn.load("~/repos/rgp/playground/evolution.so")
	io.apdFileName=spotConfig$io.apdFileName;
	io.desFileName=spotConfig$io.desFileName;
	io.resFileName=spotConfig$io.resFileName;	
	#default Values that can be changed with apd file
	populationSize <- 200;
	selectionSizeFactor <- 0.1;
        maxDepth <- 7;
	f <- "salutowicz1d"; ## TODO set name of test function here!
	## read problem design file
	if(file.exists(io.apdFileName)) {
		source(io.apdFileName,local=TRUE)
	}
	else{
		spotWriteLines(spotConfig$io.verbosity,1,"apd File not found, defaults used");
	}
	if (spotConfig$spot.fileMode) { ##Check if spotConfig was passed to the algorithm, if yes the spot.fileMode is chosen with False wich means results have to be passed to spotConfig and not to res file.
		spotWriteLines(spotConfig$io.verbosity,1,paste("Loading design file data from::",  io.desFileName), con=stderr());
		## read doe/dace etc settings:
		des <- read.table( io.desFileName, sep=" ", header = TRUE);	
	}else{
		des <- spotConfig$alg.currentDesign; ##The if/else should not be necessary anymore, since des will always be written into the spotConfig
	}			
	pNames <- names(des);	
	config<-nrow(des);	
	for (k in 1:config){
		for (i in 1:des$REPEATS[k]){
			if (is.element("POPULATIONSIZE", pNames)){
				populationSize <- des$POPULATIONSIZE[k]
			}
			if (is.element("SELECTIONSIZEFACTOR", pNames)){
			        selectionSizeFactor <- des$SELECTIONSIZEFACTOR[k]
			}
			if (is.element("MAXDEPTH", pNames)){
			        maxDepth <- des$MAXDEPTH[k]
			}
			conf <- k
			if (is.element("CONFIG", pNames)){
				conf <- des$CONFIG[k]
			}
			spotStep<-NA
			if (is.element("STEP", pNames)){
				spotStep <- des$STEP[k]
			}			
			seed <- des$SEED[k]+i-1	
			set.seed(seed)
      ## force valid parameterization (repair)...
      quantize <- function(n, q) n - (n %% q)
      populationSize <- as.integer(populationSize) ## populationSize must be a natural number > 0
      populationSize <- if (populationSize <= 1000L) populationSize else 1000L
      populationSize <- if (populationSize >= 20L) populationSize else 20L
      selectionSize <- as.integer(selectionSizeFactor * populationSize)
      selectionSize <- quantize(selectionSize, 4L) ## selectionSize must be a multiple of 4
      maxDepth <- as.integer(maxDepth)
			y <- spotCRgpTargetFunction(populationSize, selectionSize, maxDepth)
			res <- NULL
			res <- list(Y=y, POPULATIONSIZE=populationSize, SELECTIONSIZEFACTOR=selectionSizeFactor, MAXDEPTH=maxDepth, FUNCTION=f, SEED=seed, CONFIG=conf)
			if (is.element("STEP", pNames)){
				res=c(res,STEP=spotStep)
			} 
			res <-data.frame(res)
			if (spotConfig$spot.fileMode){ ##Log the result in the .res file, only if user didnt set fileMode==FALSE
				colNames = TRUE
				if (file.exists(io.resFileName)){
					colNames = FALSE
				}				
				write.table(res, file = io.resFileName, row.names = FALSE, 
					col.names = colNames, sep = " ", append = !colNames, quote = FALSE);
				colNames = FALSE					
			}
			spotConfig$alg.currentResult=rbind(spotConfig$alg.currentResult,res);#always log the results in spotConfig			
		}
	}	
	return(spotConfig)
}
