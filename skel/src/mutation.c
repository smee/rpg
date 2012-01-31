/* mutation.c
 *
 */

#include "mutation.h"
#include <Rmath.h>


SEXP mutate_constants_normal(SEXP sexp, double p, double mu, double sigma) {
  SEXP c;
  switch (TYPEOF(sexp)) { // switch for speed
  case REALSXP:
    if (unif_rand() < p) { // mutate constant with probability p
      PROTECT(c = allocVector(REALSXP, 1));
      REAL(c)[0] = REAL(sexp)[0] + rnorm(mu, sigma);
      UNPROTECT(1);
      return c;
    } else {
      return sexp;
    }
  case NILSXP:
    return sexp; // do nothing with nils
  case LANGSXP: // fall-through to next case
  case LISTSXP:
    return LCONS(mutate_constants_normal(CAR(sexp), p, mu, sigma),
                 mutate_constants_normal(CDR(sexp), p, mu, sigma)); // do nothing with inner nodes, recurse
  default: // base case
    return sexp; // map leafs
  }
}

SEXP mutate_constants_normal_R(SEXP sexp, SEXP p, SEXP mu, SEXP sigma) {
  SEXP result;
  GetRNGstate();
  PROTECT(result = mutate_constants_normal(sexp, REAL(p)[0], REAL(mu)[0], REAL(sigma)[0]));
  PutRNGstate();
  UNPROTECT(1);
  return result;
}

SEXP mutate_subtrees(SEXP sexp, double p, double mu, double sigma, SEXP input_variable_list) {
  // TODO
  int number_of_input_variables = length(input_variable_list);
  for (int i = 0; i < number_of_input_variables; i++) {
    SEXP input_variable = VECTOR_ELT(input_variable_list, i);
    Rprintf(".");
  }
  return R_NilValue; // TODO
}

SEXP mutate_subtrees_R(SEXP sexp, SEXP p, SEXP mu, SEXP sigma, SEXP input_variable_list) {
  SEXP result;
  GetRNGstate();
  PROTECT(result = mutate_subtrees(sexp, REAL(p)[0], REAL(mu)[0], REAL(sigma)[0], input_variable_list));
  PutRNGstate();
  UNPROTECT(1);
  return result;
}

