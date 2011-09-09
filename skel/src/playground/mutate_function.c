/* mutate_function.c
 *
 */

#include "create_expr_tree.h"
#include <string.h>
#include <math.h>


struct deleteSubtreeContext {
  const char ** variables;
  int nVariables;
  double constProb;
  int subtreeCounter;
  int rSubtree;
};

enum RandomDistributionKind {
  NORMAL,
  UNIFORM
};

static int rand_number(int a)
{
int x= a * unif_rand();
if(x == a) 
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
      REAL(rExpr)[0]= REAL(rExpr)[0] + norm_rand(); // random normal mutation
      break;
    case UNIFORM:
      REAL(rExpr)[0]= REAL(rExpr)[0] + (unif_rand() * 2) - 1; // random uniform mutation
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
	  *counter= *counter + 1;
          }
        }
      }
    countSubtrees(CADR(rExpr),counter);
    if(!isNull(CADDR(rExpr))){  // for functions with arity one (sin, cos...)
      countSubtrees(CADDR(rExpr),counter);
    }
  }
}

void removeSubtreeRecursive(SEXP rExpr, struct deleteSubtreeContext *delSubCon, int * counter) {
SEXP replacement;
if(isNumeric(rExpr)) { // numeric constant...
    return; // nothing to do
  } else if(isSymbol(rExpr)) { // 
    return; // nothing to do
  } else if (isLanguage(rExpr)) {// composite...
    for (SEXP child = CDR(rExpr); !isNull(child); child = CDR(child)) {
      if (isLanguage(CAR(child))){
        if (!isLanguage(CADR(CAR(child))) && !isLanguage(CADDR(CAR(child))) ) {
          *counter= *counter + 1;
            if(*counter == delSubCon->rSubtree) { // selected subtree is found
              if (unif_rand() <= delSubCon->constProb) { 
                replacement= allocVector(REALSXP, 1);
                REAL(replacement)[0]= (unif_rand() * 2) - 1;
              } else {
                int varIdx= randIndex(delSubCon->nVariables); //create variable
                replacement= install(delSubCon->variables[varIdx]);
              }
              SETCAR(child, replacement); 
            }
          }
        }
      }
    removeSubtreeRecursive(CADR(rExpr),delSubCon, counter);
    if(!isNull(CADDR(rExpr))){ // for functions with arity one (sin, cos...)
      removeSubtreeRecursive(CADDR(rExpr),delSubCon, counter);
    }
  }
}

SEXP removeSubtree(SEXP rFunc, SEXP inSet, SEXP constProb_ext)
{
  struct deleteSubtreeContext delSubCon;
 
    // Variables
  delSubCon.nVariables= LENGTH(inSet);
  const char *arrayOfVariables[delSubCon.nVariables];
  for (int i= 0; i < delSubCon.nVariables; i++) {
    arrayOfVariables[i]= CHAR(STRING_ELT(inSet,i));
  }
  delSubCon.variables = arrayOfVariables;
  
    // Constant Prob	
  constProb_ext = coerceVector(constProb_ext, REALSXP);
  delSubCon.constProb= REAL(constProb_ext)[0];

  int counter= 0;
  delSubCon.subtreeCounter= 0;
  delSubCon.rSubtree= 0;
  GetRNGstate();
  countSubtrees(BODY(rFunc), &delSubCon.subtreeCounter);
  delSubCon.rSubtree= rand_number(delSubCon.subtreeCounter); // get random subtree-number
  if(delSubCon.subtreeCounter >= 1) {
    removeSubtreeRecursive(BODY(rFunc), &delSubCon, &counter);
    }
  PutRNGstate();
  recreateSourceAttribute(rFunc);
  return R_NilValue;
} 


void countLeafs(SEXP rExpr, int *counter)  {

  if(isNumeric(rExpr)) { // numeric constant...
      return;
  } else if(isSymbol(rExpr)) { // 
      return;
  } else if (isLanguage(rExpr)){
     for (SEXP child= CDR(rExpr); !isNull(child); child = CDR(child)) {
       if (isNumeric(CAR(child)) || isSymbol(CAR(child))) {
         *counter= *counter + 1;
          }
        }
      }
      countLeafs(CADR(rExpr), counter);
      if(!isNull(CADDR(rExpr))){ // for functions with arity one (sin, cos...)
        countLeafs(CADDR(rExpr), counter);
    }
}

void insertSubtreeRecursive(SEXP rExpr, struct RandExprGrowContext* TreeParams, int *counter, int rLeaf)  {

  SEXP replacement;
 
  if(isNumeric(rExpr)) { // numeric constant...
      return;
  } else if(isSymbol(rExpr)) { // 
      return;
  } else if (isLanguage(rExpr)){
     for (SEXP child= CDR(rExpr); !isNull(child); child = CDR(child)) {
       if (isNumeric(CAR(child)) || isSymbol(CAR(child))) {
         *counter= *counter + 1;
          if(*counter == rLeaf){
            GetRNGstate();
            replacement= randExprGrowRecursive(TreeParams, 1);
            PutRNGstate();
            SETCAR(child, replacement);
          }
        }
      }
      insertSubtreeRecursive(CADR(rExpr),TreeParams, counter, rLeaf);
      if(!isNull(CADDR(rExpr))){ // for functions with arity one (sin, cos...)
        insertSubtreeRecursive(CADDR(rExpr),TreeParams, counter, rLeaf);
    }
  }
}

SEXP insertSubtree(SEXP rFunc, SEXP funcSet, SEXP inSet, SEXP constProb_ext)
{
 
  struct RandExprGrowContext TreeParams;
  TreeParams.probSubtree= 1; //100% chance for subtree
  TreeParams.maxDepth= 2;

  TreeParams.nFunctions= LENGTH(funcSet);
  const char *arrayOfFunctions[TreeParams.nFunctions];
  int arrayOfArities[TreeParams.nFunctions];
  TreeParams.arities= arrayOfArities;
  
  for (int i= 0; i < TreeParams.nFunctions; i++) {
    arrayOfFunctions[i]= CHAR(STRING_ELT(funcSet,i));
  }
  getArities(arrayOfFunctions,TreeParams.arities, TreeParams.nFunctions);
  TreeParams.functions= arrayOfFunctions;

    // Variables
  TreeParams.nVariables= LENGTH(inSet);
  const char *arrayOfVariables[TreeParams.nVariables];
  for (int i= 0; i < TreeParams.nVariables; i++) {
    arrayOfVariables[i]= CHAR(STRING_ELT(inSet,i));
  }
  TreeParams.variables= arrayOfVariables;
  
    // Constant Prob	
  constProb_ext= coerceVector(constProb_ext, REALSXP);
  TreeParams.constProb= REAL(constProb_ext)[0];

  int rLeaf, leafCounter= 0, counter= 0;
  countLeafs(BODY(rFunc), &leafCounter);  
  if(leafCounter > 0) {
    GetRNGstate();
    rLeaf= rand_number(leafCounter);
    insertSubtreeRecursive(BODY(rFunc), &TreeParams, &counter, rLeaf);
    recreateSourceAttribute(rFunc);
    PutRNGstate();
  }

return R_NilValue;
} 

// .Call("insertSubtree",func1,c("+","-","*","/"),c("x","y","z"),0.2)

SEXP deleteInsertSubtree(SEXP rFunc, SEXP funcSet, SEXP inSet, SEXP constProb_ext)
{  
  removeSubtree(rFunc, inSet, constProb_ext);
  insertSubtree(rFunc, funcSet, inSet, constProb_ext);
  return R_NilValue;
}











