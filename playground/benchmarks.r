#This functions are for benching the RGPC system with different functions.

dyn.load("evolution.so")

rgpc1D <- function(regressionfunction, rmselimit = 0.8, lowlimit= 0, uplimit= 10, samples= 101, Runs = 10000, popsize= 100, selectionsize = 20, funcset= c("+","-","*","/","sin","cos","tan","sqrt","exp","ln"),inset=c("x"),maxdepth= 7, maxleafs= 20, maxnodes= 20, constprob= 0.3, constScaling= 5, subtreeprob= 0.5, silent= 1) { 
xs <- seq(lowlimit, uplimit, lenght= samples)
ys <- regressionfunction(xs)
rmse <- .Call("evolutionRun", Runs, popsize ,selectionsize, actualParameters= x1, targetValues= y, funcset, inset ,maxdepth, maxleafs, maxnodes, constprob, constScaling, subtreeprob, rmselimit, returnRMSE= 1, silent)
rmse
}

#1d
pop1 <- .Call("evolutionRun", Runs = 15000, popsize= 200,selectionsize = 20, actualParameters= x, targetValues= y, funcset= c("+","-","*","/","sin","cos","tan","sqrt","exp","log"),inset=c("x"),maxdepth= 7, maxleafs= 25, maxNodes= 25, constprob= 0.3, constScaling= 5, subtreeprob= 0.7, rmselimit= 0, Returnrmse= 0, silent= 0)

#2d
pop1 <- .Call("evolutionRun", Runs = 15000, popsize= 200,selectionsize = 20, actualParameters= xM, targetValues= z, funcset= c("+","-","*","/","sin","cos","tan","sqrt","exp","log"),inset=c("x1","x2"),maxdepth= 7, maxleafs= 25, maxNodes= 25, constprob= 0.3, constScaling= 5, subtreeprob= 0.7, rmselimit= 0, Returnrmse= 0, silent= 0)


test <- function(seed) {
set.seed(seed) 
rmse <- .Call("evolutionRun", Runs = 15000, popsize= 200,selectionsize = 20, actualParameters= x, targetValues= y, funcset= c("+","-","*","/","sin","cos","tan","sqrt","exp","log"),inset=c("x"),maxdepth= 7, maxleafs= 25, maxNodes= 25, constprob= 0.3, constScaling= 5, subtreeprob= 0.7, rmselimit= 0, Returnrmse= 1, silent= 0)
rmse
}

result <- sapply(1:10,test)


bench1d <- function(steps, regressionfunction, rmselimit) {
system.time( for (i in 1:steps) {
rgpc1D(regressionfunction, rmselimit) } )
}
#1D test-functions

DampedOscillator1d <- function(x) (3/2)*exp(-x/2)*sin(pi*x + pi)
x <- seq(from=1, to=4*pi, length= 512)
y <- DampedOscillator1d(x)

Salutowicz1d <- function(x) exp(-1*x)*x*x*x*sin(x)*cos(x)*(sin(x)*sin(x)*cos(x)-1)
x <- seq(from=0, to=12, length= 512)
y <- Salutowicz1d(x)

UnwrappedBall1d <- function(x) 10/((x - 3)*(x - 3) + 5)
x <- seq(from=-2, to=8, length= 512)
y <- UnwrappedBall1d(x)


# 2D test-functions

Kotanchek2d <- function(x1, x2 ,A = -1, B = -2.5, C = 3.2) (exp (-1*(x1 + A)*(x1 + A))) / ((x2 + B)*(x2 + B) + C) 
x1 <- seq(from=0, to=4, length= 50)
x2 <- seq(from=0, to=4, length= 50)
z <-outer(x1,x2,Kotanchek2d)

persp(x1,x2,z)


SineCosine2d<- function(x1,x2) 6*sin(x1)*cos(x2)
x1 <- seq(from=0, to=6, length= 50)
x2 <- seq(from=0, to=6, length= 50)
z <-outer(x1,x2,SineCosine2d)

persp(x1,x2,z)

# sampleMatrix for 2D function inputVector generation


sampleMatrix <- function(dim, xs = seq(from = -1, to = 1, by = 0.1)) {
  xl <- length(xs)
  Ml <- xl ^ dim
  M <- matrix(ncol = dim, nrow = Ml)
  for (j in 1:dim) {
    replicates <- xl ^ (j - 1)
    for (i in 1:Ml) {
      index <- (ceiling(i / replicates) - 1) %% xl + 1
      #M[i, dim - j + 1] <- xs[index]
      M[i, j] <- xs[index]
    }
  } 
  colnames(M) <- paste("x", 1:dim, sep = "")
  return (M)
}


.Call("changeNode",foo,funcset= c("+","-","*","/","sin"),inset=c("x"),constprob= 0.2, subtreeProb= 0.5, maxDepth_ext = 7, constScaling= 10)

require(rgp)

data1 <- {
  x <- seq(from=1, to=4*pi, length= 512)
  y <- (3/2)*exp(-x/2)*sin(pi*x + pi)
  data.frame(y=y, x=x)
}

system.time(mdl <- symbolicRegression(y ~ x, data=data1,
                          functionSet=mathFunctionSet,
                         stopCondition=makeStepsStopCondition(15000)))
