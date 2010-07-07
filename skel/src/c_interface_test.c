#include <stdio.h>
#include <R.h>
#include <Rinternals.h>

#define CHECK_ARG_IS_FUNCTION(A) \
  if (!isFunction(A)) \
    error("Argument '" #A "' is not a function.");


SEXP test_hello_world(const SEXP arg) {
  printf("Hello, C, this is R!\n");
  return R_NilValue;
}

/* map_sexp
 * Apply a function from SEXP to SEXP to each leaf along the tree structure of
 * an SEXP.
 */
SEXP map_sexp(const SEXP, SEXP (*const)(SEXP));

SEXP print_sexp(const SEXP sexp) {
  // if this code was serious, it would use an ugly hairy switch statement for speed...
  if (isSymbol(sexp)) {
    printf("symbol '%s'\n", CHAR(PRINTNAME(sexp)));
  } else if (isNumeric(sexp)) {
    if (isInteger(sexp)) {
      printf("integer '%d'\n", *INTEGER(sexp));
    } else if (isReal(sexp)) {
      printf("real '%f'\n", *REAL(sexp));
    } else {
      printf("numeric\n");
    }
  } else if (isLogical(sexp)) {
    printf("logical\n"); // TODO
  } else if (isString(sexp)) {
    printf("string\n"); // TODO
  } else {
    printf("something else (TYPE %d)\n", TYPEOF(sexp));
  }
  return sexp;
}

/* Call this from R via .Call("test_function_manipulation", f, PACKAGE = "rgp"),
 * where f is a R function.
 * Example: .Call("test_function_manipulation", function(x) x + 1 + sin(2))
 */
SEXP test_function_manipulation(SEXP f) {
  CHECK_ARG_IS_FUNCTION(f);
  const SEXP f_formals = FORMALS(f);
  const SEXP f_body = BODY(f);

  map_sexp(f_body, print_sexp);

  return f_body;
}

SEXP map_sexp(const SEXP sexp, SEXP (*const f)(SEXP)) {
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return sexp; // do nothing with nils
  case LANGSXP: // fall-through to next case
  case LISTSXP:
    map_sexp(CAR(sexp), f); // recurse on head...
    map_sexp(CDR(sexp), f); // ..and tail of list
    break;
  default: // base case
    return f(sexp); // apply f here
  }
}
