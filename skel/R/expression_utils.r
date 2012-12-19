## expression_utils.R
##   - Utility functions for R expressions
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Common higher-order functions for transforming R expressions
##'
##' \code{MapExpressionNodes} transforms an expression \code{expr} by
##' replacing every node in the tree with the result of applying a function
##' \code{f}. The parameters \code{functions}, \code{inners}, and \code{leafs}
##' control if \code{f} should be applied to the function symbols, inner
##' subtrees, and leafs of \code{expr}, respectively.
##' \code{MapExpressionLeafs} and \code{MapExpressionSubtrees} are shorthands
##' for calls to \code{MapExpressionNodes}.
##  \code{FlattenExpression} returns a list of all nodes in an expression
##' \code{expr}.
##' \code{AllExpressionNodes} checks if all nodes in the tree of \code{expr}
##' satisfy the predicate \code{p} (\code{p} returns \code{TRUE} for every node).
##' This function short-cuts returning \code{FALSE} as soon as a node that
##' does not satisfy \code{p} is encountered.
##' \code{AnyExpressionNode} checks if any node in the tree of \code{expr}
##' satisfies the predicate \code{p}. This function short-cuts returning
##' \code{TRUE} as soon as a node that satisfies \code{p} is encountered.
##' \code{subtreeAt} returns the subtree at \code{index}.
##' \code{replaceSubtreeAt} replaces the subtree at \code{index} with
##' \code{replacement} and returns the result. 
##'
##' @param f The function to apply.
##' @param functions Whether to apply \code{f} to the function symbols
##'   of \code{expr}. Defaults to \code{TRUE}.
##' @param inners Whether to apply \code{f} to the inner subtrees of
##'   \code{expr}. Defaults to \code{FALSE}.
##' @param leafs Wheter to apply \code{f} to the leafs of \code{expr}.
##'   Defaults to \code{TRUE}.
##' @param p The predicate to check.
##' @param expr The expression to transform.
##' @param index An in-order subtree index starting from \code{0} (the root).
##' @param replacement An expression. 
##' @return The transformed expression.
##'
##' @rdname expressionTransformation
MapExpressionNodes <- function(f, expr, functions = TRUE, inners = FALSE, leafs = TRUE) {
  if (is.call(expr)) {
    oldfunc <- expr[[1]]
    newfunc <- if (functions) f(oldfunc) else oldfunc
    newcall <- as.call(append(newfunc, Map(function(e) MapExpressionNodes(f, e, functions, inners, leafs), expr[-1])))
    if (inners) f(newcall) else newcall
  } else {
    if (leafs) f(expr) else expr
  }
}

##' @rdname expressionTransformation
MapExpressionLeafs <- function(f, expr) MapExpressionNodes(f, expr, FALSE, FALSE, TRUE)

##' @rdname expressionTransformation
MapExpressionSubtrees <- function(f, expr) MapExpressionNodes(f, expr, TRUE, TRUE, TRUE)

##' @rdname expressionTransformation
FlattenExpression <- function(expr) {
  if (is.call(expr)) {
    func <- expr[[1]]
    c(list(func), Map(FlattenExpression, expr[-1]), recursive = TRUE)
  } else {
    list(expr)
  }
}

##' @rdname expressionTransformation
AllExpressionNodes <- function(p, expr) {
  if (is.call(expr)) {
    if (!p(expr[[1]])) return(FALSE) # check function
    if (length(expr) >= 2) { # check arguments recursively...
      for (i in 2:length(expr)) {
        if (!AllExpressionNodes(p, expr[[i]])) return(FALSE) # short-cut
      }
    }
    TRUE
  } else {
    p(expr)
  }
}

##' @rdname expressionTransformation
AnyExpressionNode <- function(p, expr) {
  if (is.call(expr)) {
    if (p(expr[[1]])) return(TRUE) # check function
    if (length(expr) >= 2) { # check arguments recursively...
      for (i in 2:length(expr)) {
        if (AnyExpressionNode(p, expr[[i]])) return(TRUE) # short-cut
      }
    }
    FALSE
  } else {
    p(expr)
  }
}

##' @rdname expressionTransformation
subtreeAt <- function(expr, index) subtreeAtRecursive(expr, index)

subtreeAtRecursive <- function(expr, index, currentIndex = 0)
  if (currentIndex == index) {
    expr
  } else if (is.call(expr)) { 
    for (i in 2:length(expr)) {
      result <- subtreeAtRecursive(expr[[i]], index, currentIndex + (i - 1))
      if (!is.null(result)) {
        return (result)
      }
    }
    NULL # NULL := subtree not found
  } else {
    NULL # NULL := subtree not found
  }

##' @rdname expressionTransformation
replaceSubtreeAt <- function(expr, index, replacement) replaceSubtreeAtRecursive(expr, index, replacement)

replaceSubtreeAtRecursive <- function(expr, index, replacement, currentIndex = 0)
  if (currentIndex == index) {
    replacement 
  } else if (is.call(expr)) { 
    as.call(append(expr[[1]],
                   Map(function(e, i) if (index == i) {
                                        replacement
                                      } else {
                                        replaceSubtreeAtRecursive(e, index, replacement, currentIndex + i)
                                      }, expr[-1], 1:(length(expr) - 1))))
  } else {
    expr 
  }

##' Functions for decomposing and recombining R expressions
##'
##' \code{subExpressions} returns a list of all subexpressions (subtrees) of an
##' expression \code{expr}.
##'
##' @param expr An R expression.
##' @return The decomposed or recombined expression.
##'
##' @rdname expressionComposing
subexpressions <- function(expr)
  if (is.call(expr)) {
    c(expr, Map(subexpressions, expr[-1]), recursive = TRUE)
  } else expr

##' Functions for handling R symbols / names
##'
##' \code{toName} converts a character string \code{x} to an R symbol / name,
##' while copying all attributes iff \code{copyAttributes} is \code{TRUE}.
##' In the case that \code{x} is not a character string, a copy of the object
##' is returned as-is.
##' \code{extractLeafSymbols} returns the set of symbols (names) at the leafs
##' of an expression \code{expr}. The symbols are returned as character strings.
##'
##' @param x The object to operate on.
##' @param expr An R expression.
##' @param copyAttributes Whether to copy all attributes of \code{x} to the
##'   result object.
##' @return The result.
##'
##' @rdname expressionNames
toName <- function(x, copyAttributes = TRUE)
  if (is.null(x)) {
    NULL
  } else if (is.character(x)) {
    xAsName <- as.name(x)
    if (copyAttributes) mostattributes(xAsName) <- attributes(x)
    xAsName
  } else {
    xCopy <- x
    if (copyAttributes) mostattributes(xCopy) <- attributes(x)
    xCopy
  }

##' @rdname expressionNames
extractLeafSymbols <- function(expr) {
  leafSymbols <- list()
  MapExpressionLeafs(function(n)
                       if (is.symbol(n)) leafSymbols <<- c(leafSymbols, as.character(n)),
                     expr)
  unique(leafSymbols)
}
