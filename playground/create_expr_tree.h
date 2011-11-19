#ifndef CREATE_EXPR_TREE_H
#define CREATE_EXPR_TREE_H

#include <R.h>
#include <Rinternals.h>

struct RandExprGrowContext {
  const char ** functions;
  int * arities;
  int nFunctions;
  double probSubtree;
  const char ** variables;
  int nVariables;
  double constProb;
  int maxDepth;
  double constScaling;
};


SEXP randExprGrow(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP randFuncGrow(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

void getArities(const char **, int *, int);

int randIndex(int);

SEXP randomNumber();

SEXP randExprGrowRecursive(struct RandExprGrowContext *, int);

#endif 

