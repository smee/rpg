/* mutation.h
 *
 */

#ifndef MUTATION_H
#define MUTATION_H

#include <R.h>
#include <Rinternals.h>


/* mutate_constants_normal
 *
 */
SEXP mutate_constants_normal(SEXP sexp, double p, double mu, double sigma);
SEXP mutate_constants_normal_R(SEXP sexp, SEXP p, SEXP mu, SEXP sigma);

/* mutate_subtrees
 *
 */
SEXP mutate_subtrees(SEXP sexp, double p, double mu, double sigma, SEXP input_variable_list);
SEXP mutate_subtrees_R(SEXP sexp, SEXP p, SEXP mu, SEXP sigma, SEXP input_variable_list);

#endif

