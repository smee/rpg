gcinfo(TRUE)

leaks <- function(r, n, funs = NA) {
  if (is.na(funs)) funs <- as.list(1:n)
  for (i in 1:r) {
    funs <- makeFun(funs, n)
    #rIdx <- 1+round(runif(1) * n) # Inlining the call to makeFun seems to remove the leak...
    #funs[[rIdx]] <- function(x) x*(2+x)/sqrt(x) # ... .
  }
  funs 
}

makeFun <- function(funs, n) {
  rIdx <- 1+round(runif(1) * n)
  fun <- function(x) x*(2+x)/sqrt(x)
  environment(fun) <- globalenv() # !!! This fixes the leak. !!!
  funs[[rIdx]] <- fun # This leaks memory.
  #for (i in 1:n) funs[[i]] <- fun # This does not leak!!??
  funs 
}

# example calls...
#f <- leaks(10000, 500)
#gc(reset=TRUE)
