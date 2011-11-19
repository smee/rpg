#This functions are for benching the RGPC system with different functions.

dyn.load("evolution.so")

rgpc1D <- function(regressionfunction, rmselimit = 0.8, lowlimit= 0, uplimit= 10, samples= 101, Runs = 10000, popsize= 100, selectionsize = 20, funcset= c("+","-","*","/","sin","exp"),inset=c("x"),maxdepth= 7, maxleafs= 10, maxnodes= 10, constprob= 0.3, constScaling= 5, subtreeprob= 0.5, silent= 1) { 
xs <- seq(lowlimit, uplimit, lenght= samples)
ys <- regressionfunction(xs)
rmse <- .Call("evolutionRun", Runs, popsize ,selectionsize, actualParameters= x1, targetValues= y, funcset, inset ,maxdepth, maxleafs, maxnodes, constprob, constScaling, subtreeprob, rmselimit, returnRMSE= 1, silent)
rmse
}

bench1d <- function(steps, regressionfunction, rmselimit) {
system.time( for (i in 1:steps) {
rgpc1D(regressionfunction, rmselimit) } )
}

#require(rgp)

#data1 <- {
#  x1 <- seq(0, 4*pi, length.out=201)
#  y <- sin(x1) + cos(2*x1)
#  data.frame(y=y, x1=x1)
#}

#mdl <- symbolicRegression(y ~ x1, data=data1,
#                          functionSet=mathFunctionSet,
#                          stopCondition=makeTimeStopCondition(120))
