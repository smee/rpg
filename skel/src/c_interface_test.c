#include <R.h>
#include <Rinternals.h>

#define CHECK_ARG_IS_FUNCTION(A) \
  if (!isFunction(A)) \
    error("Argument '" #A "' is not a function.");
#define CHECK_ARG_IS_NUMERIC(A) \
  if (!isNumeric(A)) \
    error("Argument '" #A "' is not numeric.");


SEXP test_hello_world(const SEXP arg) {
  Rprintf("Hello, C, this is R!\n");
  return R_NilValue;
}

SEXP test_call_function(const SEXP f, const SEXP reps) {
  CHECK_ARG_IS_FUNCTION(f);
  CHECK_ARG_IS_NUMERIC(reps);
  const int repeats = (int) *REAL(reps);
  const SEXP f_call = PROTECT(lang1(f));
  SEXP last_result = R_NilValue;
  for (int i = 0; i < repeats; i++) last_result = eval(f_call, R_GlobalEnv); // could also be R_BaseEnv or a parameter

  UNPROTECT(1);
  return last_result;
}

/* test_make_function_sexp
 * Just for fun, build the R function "function(x) 1 / sin(x)" and return it.
 */
SEXP test_make_function_sexp() {
  const SEXP one = PROTECT(allocVector(REALSXP, 1)); // the number "1.0" as an R real vector
  REAL(one)[0] = 1.0; // "
  const SEXP x = PROTECT(install("x")); // the symbol "x"
  const SEXP f_body = PROTECT(LCONS(install("/"), LCONS(one, LCONS(LCONS(install("sin"), LCONS(x, R_NilValue)), R_NilValue)))); // in Lisp syntax, this just means "(/ 1.0 (sin x))"

  const SEXP f_formals = PROTECT(CONS(R_MissingArg, R_NilValue)); // formals in R are quite funny creatures
  SET_TAG(f_formals, x); // "

  const SEXP f = PROTECT(allocSExp(CLOSXP)); // create an empty function (closure),...
  SET_CLOENV(f, R_GlobalEnv); // ...its closure environment (could also be R_BaseEnv or given as a parameter),...
  SET_FORMALS(f, f_formals); // ...its formal arguments,...
  SET_BODY(f, f_body); // ...as well as its body

  UNPROTECT(5);
  return f;
}

/* map_sexp
 * Apply a function from SEXP to SEXP to each leaf along the tree structure of
 * an SEXP.
 */
SEXP map_sexp(const SEXP, SEXP (*const)(SEXP));

SEXP print_sexp(const SEXP sexp) {
  // if this code was serious, it would use an ugly hairy switch statement for speed...
  if (isSymbol(sexp)) {
    Rprintf("symbol '%s'\n", CHAR(PRINTNAME(sexp)));
  } else if (isNumeric(sexp)) {
    if (isInteger(sexp)) {
      Rprintf("integer '%d'\n", *INTEGER(sexp));
    } else if (isReal(sexp)) {
      Rprintf("real '%f'\n", *REAL(sexp));
    } else {
      Rprintf("numeric\n");
    }
  } else if (isLogical(sexp)) {
    Rprintf("logical\n"); // TODO
  } else if (isString(sexp)) {
    Rprintf("string\n"); // TODO
  } else {
    Rprintf("something else (TYPE %d)\n", TYPEOF(sexp));
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

  return map_sexp(f_body, print_sexp);
}

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
