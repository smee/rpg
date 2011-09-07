/* mutate_function.c
 *
 */

#include "create_expr_tree.h"
#include <string.h>
#include <math.h>


enum RandomDistributionKind {
  NORMAL,
  UNIFORM
};

static int rand_number(int a)
{
int x = a * unif_rand();
if(x==a) 
{return x;}
return x + 1;
}


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
void countSubtrees(SEXP rExpr, int* counter) { // count matching Subtrees for random selection
if(isNumeric(rExpr)) { // numeric constant...
    return;
  } else if(isSymbol(rExpr)) { // 
    return; 
  } else if (isLanguage(rExpr)) {// composite...
    for (SEXP child = CDR(rExpr); !isNull(child); child = CDR(child)) {
      if (isLanguage(CAR(child))){
        if (!isLanguage(CADR(CAR(child))) && !isLanguage(CADDR(CAR(child)))) {
	  *counter = *counter + 1;
          }
        }
      }
    countSubtrees(CADR(rExpr),counter);
    countSubtrees(CADDR(rExpr),counter);
  }
}

void removeSubtreeRecursive(SEXP rExpr, int subtreeNumber, int i) {
double probConstant= 0.2; //TODO probconstant
if(isNumeric(rExpr)) { // numeric constant...
    return; // nothing to do
  } else if(isSymbol(rExpr)) { // 
    return; // nothing to do
  } else if (isLanguage(rExpr)) {// composite...
    for (SEXP child = CDR(rExpr); !isNull(child); child = CDR(child)) {
      if (isLanguage(CAR(child))){
        if (!isLanguage(CADR(CAR(child))) && !isLanguage(CADDR(CAR(child)))) {
          i++;
            if(i == subtreeNumber) { // selected subtree is found
              if (unif_rand() <= probConstant) { 
                SEXP replacement = allocVector(REALSXP, 1);
                REAL(replacement)[0] = (unif_rand() * 2) - 1;
              }
              SEXP replacement= install("x"); //TODO different variables
              SETCAR(child, replacement);  
            }
          }
        }
      }
    removeSubtreeRecursive(CADR(rExpr),subtreeNumber, i);
    removeSubtreeRecursive(CADDR(rExpr),subtreeNumber, i);
  }
}


SEXP removeSubtree(SEXP rFunc) {
  int rnumber,counter= 0;
  GetRNGstate();
  countSubtrees(BODY(rFunc), &counter);
  rnumber= rand_number(counter); // get random subtree-number
  if(counter >= 1) {
    removeSubtreeRecursive(BODY(rFunc),rnumber,0);
    }
  PutRNGstate();
  recreateSourceAttribute(rFunc);
  return R_NilValue;
}




