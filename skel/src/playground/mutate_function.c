
#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <math.h>


void mutateConstantsRecursive(SEXP rExpr) {
  if(isNumeric(rExpr)) { // numeric constant...
    Rprintf("Value: %f ", REAL(rExpr)[0]);
    REAL(rExpr)[0] = REAL(rExpr)[0] + (unif_rand() * 2) - 1;
    Rprintf(" NewValue: %f \n", REAL(rExpr)[0]);
  } else if (!isNull(CADR(rExpr))) { // composite...
      mutateConstantsRecursive(CADR(rExpr));
      if (!isNull(CADDR(rExpr))) {
        mutateConstantsRecursive(CADDR(rExpr)); 
      }
    }
}

SEXP mutateConstants(SEXP rExpr) {
  GetRNGstate();
  mutateConstantsRecursive(BODY(rExpr));
  PutRNGstate();
  return rExpr; // TODO return NULL!
}

/*
mutateFunction <- function(func1) {
  .Call("MutateFunctionWrapper",func1)
  attr(func1, "source") <- deparse(func1) # TODO do this step in C!
  func1
}
*/

void removeSubtreeRecursive(SEXP rExpr) {
  if(isNumeric(rExpr)) { // numeric constant...
    return; // nothing to do
  } else if (isLanguage(rExpr)) { // composite...
    for (SEXP child = CDR(rExpr); !isNull(child); child = CDR(child)) {
      if (isLanguage(CAR(child))) {
        Rprintf("L\n");
        SEXP replacementConstant = allocVector(REALSXP, 1);
        REAL(replacementConstant)[0] = 42.0;
        SETCAR(child, replacementConstant); // TODO de-allocate memory first?
      } else {
        Rprintf("C\n");
      }
    }
    return;
  }
}

SEXP removeSubtree(SEXP rExpr) {
  removeSubtreeRecursive(BODY(rExpr));
  return rExpr; // TODO return NULL!
}



