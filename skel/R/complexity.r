## complexity.r
##   - Complexity measures for GP individuals and estimators of GP search space size
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Complexity measures for R functions and expressions
##'
##' \code{exprDepth} returns the depth of the tree representation ("exression tree") of an R expression.
##' \code{funcDepth} returns the tree depth of the body expression of an R function.
##' \code{exprSize} returns the number of nodes in the tree of an R expression.
##' \code{funcSize} returns the number of nodes in the body expression tree of an R function.
##' \code{exprVisitationLength} returns the visitation length of the tree of an R expression.
##' The visitation length is the total number of nodes in all possible subtrees of a tree.
##' \code{funcVisitationLength} returns the visitation length of the body expression tree of an R function.
##' The visitation length can be interpreted as the size of the expression obtained by substituting all
##' inner functions by their function bodies (see "Crossover Bias in Genetic Programming", Maarten Keijzer
##' and James Foster).
##'
##' @rdname expressionComplexityMeasures
##' @export
exprDepth <- function(expr)
  if (is.call(expr)) {
    max(as.vector(Map(exprDepth, rest(as.list(expr))), mode = "integer")) + 1
  } else 1

##' @rdname expressionComplexityMeasures
##' @export
funcDepth <- function(func) exprDepth(body(func))

##' @rdname expressionComplexityMeasures
##' @export
exprSize <- function(expr)
  if (is.call(expr)) {
    sum(as.vector(Map(exprSize, rest(as.list(expr))), mode = "integer")) + 1
  } else 1

##' @rdname expressionComplexityMeasures
##' @export
funcSize <- function(func) exprSize(body(func))

##' @rdname expressionComplexityMeasures
##' @export
exprVisitationLength <- function(expr)
  exprVisitationLengthRecursive(expr)[[2]]

##' @rdname expressionComplexityMeasures
exprVisitationLengthRecursive <- function(expr)
  ## The visitaion length of a tree T is the sum of the number of nodes of all subtrees of T...
  if (is.call(expr)) {
    childrenResults <- lapply(rest(expr), exprVisitationLengthRecursive)
    childrenSums <- Reduce(function(a,b) c(a[1] + b[1], a[2] + b[2]), childrenResults, c(0,0))
    childrenSizesSum <- childrenSums[1]
    childrenVisitationLengthsSum <- childrenSums[2]
    thisTreeSize <- 1 + childrenSizesSum
    thisTreeVisitationLength <- thisTreeSize + childrenVisitationLengthsSum
    c(thisTreeSize, thisTreeVisitationLength)
  } else c(1, 1)

##' @rdname expressionComplexityMeasures
##' @export
funcVisitationLength <- function(func) exprVisitationLength(body(func))

##' Upper bounds for expression tree search space sizes
##'
##' These functions return the number of structurally different expressions or expression shapes of a given
##' depth or size that can be build from a fixed function- and input-variable set. Here, "expression shape"
##' means the shape of an expression tree, not taking any node labels into account.
##' \code{exprShapesOfDepth} returns the number of structurally different expression shapes of a depth
##' exactly equal to \code{n}.
##' \code{exprShapesOfMaxDepth} returns the number of structurally different expression shapes of a depth
##' less or equal to \code{n}.
##' \code{exprsOfDepth} returns the number of structurally different expressions of a depth exactly equal to
##' \code{n}. Note that constants are handled by conceptually substiting them with a fresh input variable.
##' \code{exprShapesOfMaxDepth} returns the number of structurally different expressions of a depth
##' less or equal to \code{n}. Note that constants are handled by conceptually substiting them with a fresh
##' input variable.
##' \code{exprShapesOfSize}, \code{exprShapesOfMaxSize}, \code{exprsOfSize}, \code{exprsOfMaxSize} are
##' equivalents that regard expression tree size (number of nodes) instead of expression tree depth.
##'
##' @param funcset The function set.
##' @param inset The set of input variables.
##' @param n The fixed size or depth.
##'
##' @rdname expressionCounts
##' @export
exprShapesOfDepth <- function(funcset, n)
  if (n < 1) {
    0
  } else if (n == 1) {
    1 # there is only one expression shape of depth 1
  } else {
    exprShapesOfDepthNminusOne <- exprShapesOfDepth(funcset, n - 1)
    uniqueArities <- unique(as.vector(Map(arity, funcset$all), mode = "integer"))
    exprsWithFixedRootArity <- exprShapesOfDepthNminusOne ^ uniqueArities
    sum(exprsWithFixedRootArity)
  }

##' @rdname expressionCounts
##' @export
exprShapesOfMaxDepth <- function(funcset, n)
  if (n < 1) {
    0
  } else if (n == 1) {
    1 # there is only one expression shape of max depth 1
  } else {
    exprShapesOfDepthNminusOne <- exprShapesOfMaxDepth(funcset, n - 1)
    uniqueArities <- unique(as.vector(Map(arity, funcset$all), mode = "integer"))
    exprsWithFixedRootArity <- exprShapesOfDepthNminusOne ^ uniqueArities
    sum(exprsWithFixedRootArity) + 1
  }

##' @rdname expressionCounts
##' @export
exprsOfDepth <- function(funcset, inset, n)
  if (n < 1) {
    0
  } else if (n == 1) {
    1 + length(inset) # an expression of depth 1 is either a constant or an input variable
  } else {
    exprsOfDepthNminusOne <- exprsOfDepth(funcset, inset, n - 1)
    arities <- as.vector(Map(arity, funcset$all), mode = "integer")
    exprsWithFixedRoot <- exprsOfDepthNminusOne ^ arities
    sum(exprsWithFixedRoot)
  }

##' @rdname expressionCounts
##' @export
exprsOfMaxDepth <- function(funcset, inset, n)
  if (n < 1) {
    0
  } else if (n == 1) {
    1 + length(inset) # an expression of depth 1 is either a constant or an input variable
  } else {
    exprsOfMaxDepthNminusOne <- exprsOfMaxDepth(funcset, inset, n - 1)
    arities <- as.vector(Map(arity, funcset$all), mode = "integer")
    exprsWithFixedRoot <- exprsOfMaxDepthNminusOne ^ arities
    sum(exprsWithFixedRoot) + 1 + length(inset)
  }

##' @rdname expressionCounts
##' @export
exprShapesOfSize <- function(funcset, n) NA # TODO

##' @rdname expressionCounts
##' @export
exprShapesOfMaxSize <- function(funcset, n) NA # TODO

##' @rdname expressionCounts
##' @export
exprsOfSize <- function(funcset, inset, n) NA # TODO

##' @rdname expressionCounts
##' @export
exprsOfMaxSize <- function(funcset, inset, n) NA # TODO

##' Similarity and distance measures for R functions and expressions
##'
##' These functions implement several similarity and distance measures for R functions
##' (i.e. their body expressions).
##' TODO check and document measure-theoretic properties of each measure defined here
##' \code{commonSubexpressions} returns the set of common subexpressions of \code{expr1}
##' and \code{expr2}. This is not a measure itself, but can be used to implement
##' several subtree-based similarity measures.
## '\code{numberOfcommonSubexpressions} returns the number of common subexpressions
##' of \code{expr1} and \code{expr2}.
##' \code{sizeWeightedNumberOfcommonSubexpressions} returns the number of common
##' subexpressions of \code{expr1} and \code{expr2}, weighting the size of each common
##' subexpression. Note that for every expression \emph{e},
##' \code{sizeWeightedNumberOfcommonSubexpressions(} \emph{e} \code{, } \emph{e}
##' \code{) == exprVisitationLength(} \emph{e} \code{)}.
##' \code{normalizedNumberOfCommonSubexpressions} returns the ratio of the number of
##' common subexpressions of \code{expr1} and \code{expr2} in relation to the number
##' of subexpression in the larger expression of \code{expr1} and \code{expr2}.
##' \code{normalizedSizeWeightedNumberOfcommonSubexpressions} returns the ratio of
##' the size-weighted number of common subexpressions of \code{expr1} and \code{expr2}
##' in relation to the visitation length of the larger expression of \code{expr1} and
##' \code{expr2}.
##' \code{NCSdist} and \code{SNCSdist} are distance measures derived from
##' \code{normalizedNumberOfCommonSubexpressions} and
##' \code{normalizedSizeWeightedNumberOfCommonSubexpressions} respectively.
##' \code{differingSubexpressions}, and code{numberOfDifferingSubexpressions}
##' are duals of the functions described above, based on counting the number of
##' differing subexpressions of \code{expr1} and \code{expr2}. The possible functions
##' "normalizedNumberOfDifferingSubexpressions" and
##' "normalizedSizeWeightedNumberOfDifferingSubexpressions" where ommited because they
##' are always equal to \code{NCSdist} and \code{SNCSdist} by definition.
##'
##' @param expr1 An R expression.
##' @param expr2 An R expression.
##'
##' @rdname expressionSimilarityMeasures
##' @export
commonSubexpressions <- function(expr1, expr2)
  intersect(subexpressions(expr1), subexpressions(expr2))

##' @rdname expressionSimilarityMeasures
##' @export
numberOfCommonSubexpressions <- function(expr1, expr2)
  length(commonSubexpressions(expr1, expr2))

##' @rdname expressionSimilarityMeasures
##' @export
normalizedNumberOfCommonSubexpressions <- function(expr1, expr2)
  numberOfCommonSubexpressions(expr1, expr2) / max(exprSize(expr1), exprSize(expr2))

##' @rdname expressionSimilarityMeasures
##' @export
NCSdist <- function(expr1, expr2)
  1 - normalizedNumberOfCommonSubexpressions(expr1, expr2)

##' @rdname expressionSimilarityMeasures
##' @export
sizeWeightedNumberOfCommonSubexpressions <- function(expr1, expr2) {
  weightedCommonSubexpressions <- sapply(commonSubexpressions(expr1, expr2), exprSize)
  if (identical(weightedCommonSubexpressions, list())) 0 else sum(weightedCommonSubexpressions)
}

##' @rdname expressionSimilarityMeasures
##' @export
normalizedSizeWeightedNumberOfCommonSubexpressions <- function(expr1, expr2) {
  largerExpr <- if (exprSize(expr1) >= exprSize(expr2)) expr1 else expr2
  sizeWeightedNumberOfCommonSubexpressions(expr1, expr2) / exprVisitationLength(largerExpr)
}

##' @rdname expressionSimilarityMeasures
##' @export
SNCSdist <- function(expr1, expr2)
  1 - normalizedSizeWeightedNumberOfCommonSubexpressions(expr1, expr2)

##' @rdname expressionSimilarityMeasures
##' @export
differingSubexpressions <- function(expr1, expr2) {
  expr1subs <- subexpressions(expr1)
  expr2subs <- subexpressions(expr2)
  setdiff(union(expr1subs, expr2subs), intersect(expr1subs, expr2subs))
}

##' @rdname expressionSimilarityMeasures
##' @export
numberOfDifferingSubexpressions <- function(expr1, expr2)
  length(differingSubexpressions(expr1, expr2))

##' @rdname expressionSimilarityMeasures
##' @export
sizeWeightedNumberOfDifferingSubexpressions <- function(expr1, expr2) {
  weightedDifferingSubexpressions <- sapply(differingSubexpressions(expr1, expr2), exprSize)
  if (identical(weightedDifferingSubexpressions, list())) 0 else sum(weightedDifferingSubexpressions)
}
