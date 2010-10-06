# cong.r
# Continous Genotype Genetic Programming
# 2010 Oliver Flasch and Markus Friese
#

require(rgp)


congType <- function(m = 2,
                     funcSet = c("+", "-", "*", "/", "exp", "log"),
                     inSet = c("x"))
  structure(list(m = m, funcSet = funcSet, inSet = inSet), class = c("congType", "list"))

# The continous genotype (cong) of an R expression expr of max function arity m
# consists of m ^ exprDepth(expr) - 1 codons. Each codon is of the following format:
# |- function genes -|- inVar genes -|- const gene -|
exprToCong <- function(expr, congType) {
  funcSetLen <- length(congType$funcSet)
  inSetLen <- length(congType$inSet)
  codonLen <- funcSetLen + inSetLen + 1
  exprNodes <- congType$m ^ exprDepth(expr) - 1
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

congToExpr <- function(cong)
  NULL

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
