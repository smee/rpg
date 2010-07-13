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
    return f(LCONS(map_sexp(CAR(sexp), f),
                   map_sexp(CDR(sexp), f))); // map inner nodes, recurse
  default: // base case
    return f(sexp); // map leafs
  }
}

SEXP map_sexp_leafs(const SEXP sexp, SEXP (*const f)(SEXP)) {
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return sexp; // do nothing with nils
  case LANGSXP: // fall-through to next case
  case LISTSXP:
    return LCONS(map_sexp_leafs(CAR(sexp), f),
                 map_sexp_leafs(CDR(sexp), f)); // do nothing with inner nodes, recurse
  default: // base case
    return f(sexp); // map leafs
  }
}

SEXP map_sexp_inner_nodes(const SEXP sexp, SEXP (*const f)(SEXP)) {
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return sexp; // do nothing with nils
  case LANGSXP: // fall-through to next case
  case LISTSXP:
    return f(LCONS(map_sexp_inner_nodes(CAR(sexp), f),
                   map_sexp_inner_nodes(CDR(sexp), f))); // map inner nodes, recurse
  default: // base case
    return sexp; // do noting with leafs
  }
}
