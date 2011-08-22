/* mutate_function.c
 *
 */

#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <math.h>


enum RandomDistributionKind {
  NORMAL,
  UNIFORM
};


static R_INLINE void recreateSourceAttribute(SEXP rFunc) {
  // TODO This is not perfect, as R seems to use slightly different arguments to
  // deparse() when initially setting the source attribute.
  setAttrib(rFunc, R_SourceSymbol, eval(lang2(install("deparse"), rFunc), R_BaseEnv));
}

void mutateConstantsRecursive(SEXP rExpr, enum RandomDistributionKind rDistKind) {
  if(isNumeric(rExpr)) { // numeric constant...
    //Rprintf("Value: %f ", REAL(rExpr)[0]);
    switch (rDistKind) {
    case NORMAL:
      REAL(rExpr)[0] = REAL(rExpr)[0] + norm_rand(); // random normal mutation
      break;
    case UNIFORM:
      REAL(rExpr)[0] = REAL(rExpr)[0] + (unif_rand() * 2) - 1; // random uniform mutation
      break;
    }
    //Rprintf(" NewValue: %f \n", REAL(rExpr)[0]);
  } else if (!isNull(CADR(rExpr))) { // composite...
    mutateConstantsRecursive(CADR(rExpr), rDistKind);
    if (!isNull(CADDR(rExpr))) {
      mutateConstantsRecursive(CADDR(rExpr), rDistKind); 
    }
  }
}

SEXP mutateConstants(SEXP rFunc) { // TODO add parameter for RandomDistributionKind
  GetRNGstate();
  mutateConstantsRecursive(BODY(rFunc), NORMAL);
  PutRNGstate();
  recreateSourceAttribute(rFunc);
  return R_NilValue;
}


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

SEXP removeSubtree(SEXP rFunc) {
  removeSubtreeRecursive(BODY(rFunc));
  recreateSourceAttribute(rFunc);
  return R_NilValue;
}

