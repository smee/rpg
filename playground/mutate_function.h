#ifndef MUTATE_FUNCTION_H
#define MUTATE_FUNCTION_H

#include <R.h>
#include <Rinternals.h>


SEXP crossover(SEXP, SEXP);

SEXP mutateConstants(SEXP);

SEXP deleteInsertSubtree(SEXP, SEXP, SEXP, SEXP);

#endif

