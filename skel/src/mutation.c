/* mutation.c
 *
 */

#include "mutation.h"
#include <Rmath.h>


SEXP mutate_constants_normal(SEXP sexp, double p, double mu, double sigma) {
  SEXP c;
  switch (TYPEOF(sexp)) { // switch for speed
  case REALSXP:
    PROTECT(c = allocVector(REALSXP, 1));
    REAL(c)[0] = REAL(sexp)[0] + rnorm(mu, sigma);
    UNPROTECT(1);
    return c;
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

