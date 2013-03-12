#!/usr/bin/env Rscript
#
# spurious_variable_test_function_generator.R 
# Scalable regression test functions
# 2013 SPOTSeven group
#

require("rgp")
require("soobench")
require("twiddler")
 

new.alist <- function(fargs) {
  alistargs <- Reduce(function(a,b) paste(a,b,sep="=,") , fargs, "", right = TRUE)
  alistargslen <- nchar(alistargs)
  if (alistargslen != 0)
    alistargs <- substr(alistargs, 1, alistargslen-1)
  eval(parse(text = paste("alist(", alistargs, ")", sep="")), baseenv())
}

makeSpuriousVariableTestFuntion <- function(f, numberOfSpuriousVariables = 1, shuffle = FALSE) {
  if (numberOfSpuriousVariables <= 0) return(f)
  spuriousF <- f
  newFormals <- c(formals(f), new.alist(paste("spuriousX", 1:numberOfSpuriousVariables, sep = "")))
  if (shuffle) newFormals <- sample(newFormals, length(newFormals))
  formals(spuriousF) <- newFormals
  return (spuriousF)
}


# demo code

baseFunction <- branin_function()
#baseFunction <- rastrigin_function(2)

twiddleTestFunctionGenerator <- function(n = 1, shuffle = FALSE) {
  print(makeSpuriousVariableTestFuntion(baseFunction, numberOfSpuriousVariables = n, shuffle = shuffle))
}
 
twiddle(twiddleTestFunctionGenerator(n, shuffle), eval = TRUE, 
  n = knob(c(0, 10), res = 1, default = 1),
  shuffle = toggle(label = "shuffle", default = FALSE))

