/* initialization.h
 *
 */

#ifndef INITIALIZATION_H
#define INITIALIZATION_H

#include <R.h>
#include <Rinternals.h>


/* initialize_ 
 *
 */
SEXP mutate_subtrees(SEXP sexp, double p, double mu, double sigma, SEXP input_variable_list);
SEXP mutate_subtrees_R(SEXP sexp, SEXP p, SEXP mu, SEXP sigma, SEXP input_variable_list);

#endif

