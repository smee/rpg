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

#endif

