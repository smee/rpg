/* eval_vectorized.h 
 *
 */

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

/* eval_vectorized
 *
 */
SEXP eval_vectorized(SEXP rFunction, SEXP actualParameters);

/* eval_vectorized_rmse
 *
 */
SEXP eval_vectorized_rmse(SEXP, SEXP, SEXP, double *);

#endif

