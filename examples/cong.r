# cong.r
# Continous Genotype Genetic Programming
# 2010 Oliver Flasch and Markus Friese
#

require(rgp)
require(twiddler)


# cong implementation
#
congType <- function(m = 2,
                     funcSet = c("+", "-", "*", "/", "exp"),
                     funcArities = c(2, 2, 2, 2, 1),
                     inSet = c("x"))
  structure(list(m = m, funcSet = funcSet, funcArities = funcArities, inSet = inSet),
            class = c("congType", "list"))

funcToCong <- function(func, congType, d = exprDepth(body(func)))
  exprToCong(body(func), congType, d)

congToFunc <- function(cong, congType, leaf = as.symbol(congType$inSet[[1]]),
                       scale = as.symbol("*"), join = as.symbol("sum")) {
  func <- new.function()
  body(func) <- congToExpr(cong, congType, leaf, scale, join)
  formals(func) <- new.alist(congType$inSet)
  func
}

# The continous genotype (cong) of an R expression expr of max function arity m
# consists of m ^ exprDepth(expr) - 1 codons. Each codon is of the following format:
# |- function bases -|- inVar bases -|- const base -|
exprToCong <- function(expr, congType, d = exprDepth(expr)) {
  funcSetLen <- length(congType$funcSet)
  inSetLen <- length(congType$inSet)
  codonLen <- funcSetLen + inSetLen + 1
  exprNodes <- congType$m ^ d - 1
  cong <- rep(0, codonLen * exprNodes)
  buildCong <- function(subExpr, n) {
    codonStart <- n * codonLen
    codonInSetStart <- codonStart + funcSetLen
    codonConSetStart <- codonInSetStart + inSetLen
    if (is.symbol(subExpr)) {
      cong[codonInSetStart + which(subExpr == congType$inSet)] <<- 1
    } else if (is.language(subExpr)) {
      cong[codonStart + which(subExpr[[1]] == congType$funcSet)] <<- 1
    } else {
      cong[codonConSetStart + 1] <<- subExpr
    }
  }
  applyExprFullLevelOrder(buildCong, expr, congType$m) 
  class(cong) <- c("cong", class(cong))
  cong
}

congToExpr <- function(cong, congType, leaf = as.symbol(congType$inSet[[1]]),
                       scale = as.symbol("*"), join = as.symbol("sum")) {
  congLen <- length(cong)
  funcSetLen <- length(congType$funcSet)
  inSetLen <- length(congType$inSet)
  codonLen <- funcSetLen + inSetLen + 1
  codons <- congLen / codonLen
  congToExprRec <- function(cong, n = 0) {
    if (n >= codons) {
      leaf
    } else {
      codonStart <- n * codonLen
      children <- Map(function(k) congToExprRec(cong, n * congType$m + k), 1:congType$m)
      bases <- Filter(function(base) !is.null(base),
                      Map(function(i) if (cong[codonStart + i] == 0) {
                                        NULL 
                                      } else if (i <= funcSetLen) {
                                        if (cong[codonStart + i] == 1) {
                                          as.call(c(as.symbol(congType$funcSet[i]),
                                                    children[1:congType$funcArities[i]]))
                                        } else {
                                          as.call(list(scale, cong[codonStart + i],
                                                  as.call(c(as.symbol(congType$funcSet[i]),
                                                            children[1:congType$funcArities[i]]))))
                                        }
                                      } else if (i <= funcSetLen + inSetLen) {
                                        if (cong[codonStart + i] == 1) {
                                          as.symbol(congType$inSet[i - funcSetLen])
                                        } else {
                                          as.call(list(scale, cong[codonStart + i],
                                                  as.symbol(congType$inSet[i - funcSetLen])))
                                        }
                                      } else {
                                        cong[codonStart + i]
                                      },
                          1:codonLen))
      if (length(bases) == 1) bases[[1]] else as.call(c(join, bases))
    }
  }
  congToExprRec(cong)
}

congToMatrix <- function(cong, congType) {
  congMatrix <- t(matrix(cong, nrow = length(congType$funcSet) + length(congType$inSet) + 1))
  colnames(congMatrix) <- c(congType$funcSet, congType$inSet, "C")
  congMatrix
}

applyExprFullLevelOrder <- function(f, expr, m, n = 0) {
  f(expr, n)
  if (!is.symbol(expr) && is.language(expr)) {
    nm <- n * m
    for (i in 1:(length(expr) - 1)) Recall(f, expr[[i + 1]], m, nm + i)
  }
}

levelOrderChildIndex <- function(n, k, m) n * m + k


# fun stuff for testing
#
makeCongMorpher <- function(cong1, cong2) {
  if (length(cong1) != length(cong2)) stop("morphCongs: cong1 and cong2 must be of the same length")
  function(pos) {
    if (pos < 0 || pos > 1) stop("morphCongs: pos must be in [0, 1]")
    (1 - pos) * cong1 + pos * cong2
  }
}

makeCongFuncMorpher <- function(func1, func2, congType) {
  cong1 <- funcToCong(func1, congType)
  cong2 <- funcToCong(func2, congType)
  if (length(cong1) != length(cong2)) {
    maxLen <- max(length(cong1), length(cong2))
    codonLen <- length(congType$funcSet) + length(congType$inSet) + 1
    maxNodes <- maxLen / codonLen
    d <- log(maxNodes + 1, base = congType$m)
    cong1sl <- funcToCong(func1, congType, d)
    cong2sl <- funcToCong(func2, congType, d)
    congMorpher <- makeCongMorpher(cong1sl, cong2sl) 
    function(pos) congToFunc(congMorpher(pos), congType)
  } else {
    congMorpher <- makeCongMorpher(cong1, cong2)
    function(pos) congToFunc(congMorpher(pos), congType)
  }
}

makeSimpleFuncMorpher <- function(func1, func2)
  function(pos) function(...) (1 - pos) * func1(...) + pos * func2(...)

plotFunc <- function(func, samples = 512, xlim = c(0, 1),
                     xlab = "x", ylab = "f(x)",
                     type = "l", main = deparse(func), ...) {
  xs <- seq(from = xlim[1], to = xlim[2], length.out = samples)
  ys <- Vectorize(func)(xs)
  plot(xs, ys, xlim = xlim, xlab = xlab, ylab = ylab, type = type, main = main, ...) 
}

morphCongPlotFuncs <- function(func1, func2, congType, ...) {
  congFuncMorpher <- makeCongFuncMorpher(func1, func2, congType)
  funcPlotter <- function(pos) plotFunc(congFuncMorpher(pos), ...)
  twiddle(funcPlotter(pos)) 
}

morphSimplePlotFuncs <- function(func1, func2, ...) {
  simpleFuncMorpher <- makeSimpleFuncMorpher(func1, func2)
  funcPlotter <- function(pos) plotFunc(simpleFuncMorpher(pos), ...)
  twiddle(funcPlotter(pos))
}

#ct1 <- congType()
#morphCongPlotFuncs(function(x) 2*x*x, function(x) 3+x, ct1, xlim = c(-10, 10), ylim = c(-10, 10))
#morphSimplePlotFuncs(function(x) 2*x*x, function(x) 3+x, xlim = c(-10, 10), ylim = c(-10, 10))
#morphCongPlotFuncs(function(x) x*x, function(x) x*x*x, ct1, xlim = c(-10, 10), ylim = c(-10, 10))
#morphSimplePlotFuncs(function(x) x*x, function(x) x*x*x, xlim = c(-10, 10), ylim = c(-10, 10))
#morphCongPlotFuncs(function(x) exp(x), function(x) x*x*x, ct1, xlim = c(-10, 10), ylim = c(-10, 10))
#morphSimplePlotFuncs(function(x) exp(x), function(x) x*x*x, xlim = c(-10, 10), ylim = c(-10, 10))


# TODO remove these tool functions before adding cong.r to the RGP package!
#
new.function <- function() {
  # The following line has to be inside this function to prevent capturing the lexical
  # environment, which would cause a hard to find memory leak:
  fun <- function() NULL
  # If you use the last line outside of this function, by shure to initialize the
  # environment of the generated function object to the global environment, like so:
  # environment(fun) <- globalenv()
  fun
}

new.alist <- function(fargs) {
  alistargs <- Reduce(function(a,b) paste(a,b,sep="=,") , fargs, "", right = TRUE)
  alistargslen <- nchar(alistargs)
  if (alistargslen != 0)
    alistargs <- substr(alistargs, 1, alistargslen-1)
  eval(parse(text = paste("alist(", alistargs, ")", sep="")), baseenv())
}
