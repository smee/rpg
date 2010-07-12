/* sexp_utils.c
 *
 */

#include "sexp_utils.h"


SEXP map_sexp(const SEXP sexp, SEXP (*const f)(SEXP)) {
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return sexp; // do nothing with nils
  case LANGSXP: // fall-through to next case
  case LISTSXP:
    return LCONS(map_sexp(CAR(sexp), f), map_sexp(CDR(sexp), f)); // recurse
  default: // base case
    return f(sexp); // apply f here
  }
}
