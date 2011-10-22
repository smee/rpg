#ifndef EVAL_VECTORIZED_H
#define EVAL_VECTORIZED_H

#include <R.h>
#include <Rinternals.h>


struct EvalVectorizedContext {
    int arity;
    R_len_t samples;
    SEXP formalParameters;
    double *actualParameters;
};

SEXP evalVectorizedRmse(SEXP, SEXP, SEXP);

#endif

