# Kotanchek - Funktion:

Kotanchek <- function(x1, x2 ,A = -1, B = -2.5, C = 3.2) (exp (-1*(x1 + A)*(x1 + A))) / ((x2 + B)*(x2 + B) + 3.2) 

x1 <- seq(from=0, to=4, length= 50)
x2 <- seq(from=0, to=4, length= 50)
z <-outer(x1,x2,Kotanchek)

persp(x1,x2,z)

# Salustowicz 1d - Function:

Salutowicz1d <- function(x) exp(-1*x)*x*x*x*sin(x)*cos(x)*(sin(x)*sin(x)*cos(x)-1)

x <- seq(from=-1, to=12, length= 500)

plot(x,Salutowicz1d(x))

# Salustowicz 2d - Function:

Salutowicz2d <- function(x1,x2) exp(-1*x1)*x1*x1*x1*(x2-5)*sin(x1)*cos(x1)*(sin(x1)*sin(x1)*cos(x1)-1)

x1 <- seq(from=0, to=10, length= 50)
x2 <- seq(from=0, to=10, length= 50)
z <-outer(x1,x2,Salutowicz2d)

persp(x1,x2,z)

# UnwrappedBall 1d - Function:

UnwrappedBall1d <- function(x) 10/((x - 3)*(x - 3) + 5)

x <- seq(from=-4, to=10, length= 500)

plot(x,UnwrappedBall1d(x))

# UnwrappedBallXd - TODO X-dimensional, dynamisch machbar?

#Rational polynomial 3d - function  Plot?!

RationalPolynom3d <- function(x1,x2,x3= 2) (30*(x1 - 1)*(x3 -1)) / ((x1 -10)*x2*x2)

x1 <- seq(from=0, to=2, length= 50)
x1 <- seq(from=0, to=2, length= 50)
z <-outer(x1,x2,RationalPolynom3d)

#SineCosine function 2d

SineCosine2d<- function(x1,x2) 6*sin(x1)*cos(x2)

x1 <- seq(from=0, to=6, length= 50)
x2 <- seq(from=0, to=6, length= 50)
z <-outer(x1,x2,SineCosine2d)

persp(x1,x2,z)

#Ripple function 2d 

Ripple2d <- function(x1,x2) (x1-3)*(x2-3) + 2*sin((x1-4)*(x2-4))

x1 <- seq(from=0, to=6, length= 50)
x2 <- seq(from=0, to=6, length= 50)
z <-outer(x1,x2,Ripple2d)

persp(x1,x2,z)

#RationalPolynomial2 2d

RatPol2d <- function(x1,x2) ((x1-3)*(x1-3)*(x1-3)*(x1-3) + (x2-3)*(x2-3)*(x2-3) -x2 + 3) / ((x2-2)*(x2-2)*(x2-2)*(x2-2)+ 10)

x1 <- seq(from=0, to=6, length= 50)
x2 <- seq(from=0, to=6, length= 50)
z <-outer(x1,x2,RatPol2d)

persp(x1,x2,z)
