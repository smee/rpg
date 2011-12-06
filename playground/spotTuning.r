# spotTuning.r

dyn.load("evolution.so")


#globally define fitness cases for 1D test-function
Salutowicz1d <- function(x) exp(-1*x)*x*x*x*sin(x)*cos(x)*(sin(x)*sin(x)*cos(x)-1)
x <- seq(from=0, to=12, length= 64)
y <- Salutowicz1d(x)


testGPRun <- function(seed, popsize, selectionsize, maxdepth) {
  set.seed(seed) 
  rmse <- .Call("evolutionRun", stepss = 9999999, popsize= popsize,selectionsize = selectionsize, actualParameters= x, targetValues= y, funcset= c("+","-","*","/","sin","cos","sqrt","exp","log"),inset=c("x"),maxdepth= maxdepth, maxleafs= 9999, maxNodes= 9999, constprob= 0.3, constScaling= 2, subtreeprob= 0.7, rmselimit= 0, Returnrmse= 1, silent= 0, runtime= 720)
  rmse
}


