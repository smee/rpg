#ifndef MUTATE_FUNCTION_H
#define MUTATE_FUNCTION_H

#include <R.h>
#include <Rinternals.h>


SEXP crossover(SEXP, SEXP);
SEXP crossoverRandomNode(SEXP, SEXP);
SEXP crossoverDepthProof(SEXP, SEXP, SEXP);

SEXP mutateConstants(SEXP, SEXP);

SEXP deleteInsertChangeSubtree(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

#endif

