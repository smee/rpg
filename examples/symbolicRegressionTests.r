# symbolicRegressionTests1.r
#

#zaefferer1d = function(x) (x-1)**3 * (150 + (x - 40) * 7.679999)
#zaefferer1dInterval = 41:76

salustowicz1d <- function(x) exp(-x)*x^3*sin(x)*cos(x)*(sin(x)*sin(x)*cos(x)-1)
unwrappedBall1d <- function(x) 10 / ((x - 3)^2 + 5)
dampedOscillator1d <- function(x) 1.5 * exp(-0.5 * x) * sin(pi * x + pi)

df.sin <- data.frame(x=seq(from=-2*pi, to=2*pi, length.out=512), y=sin(seq(from=-2*pi, to=2*pi, length.out=512)))
df.salustowicz1d <- data.frame(x=seq(from=0, to=12, length.out=512), y=salustowicz1d(seq(from=0, to=12, length.out=512)))
df.unwrappedBall1d <- data.frame(x=seq(from=-2, to=8, length.out=512), y=unwrappedBall1d(seq(from=-2, to=8, length.out=512)))
df.dampedOscillator1d <- data.frame(x=seq(from=1, to=4*pi, length.out=512), y=dampedOscillator1d(seq(from=1, to=4*pi, length.out=512)))
