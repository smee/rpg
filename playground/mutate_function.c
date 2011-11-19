/* mutate_function.c
 *
 */

#include "create_expr_tree.h"
#include "mutate_function.h"
#include <string.h>
#include <math.h>


struct deleteSubtreeContext {
  const char ** variables;
  int nVariables;
  double constProb;
  int subtreeCounter;
  int rSubtree;
  double constScaling;
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


SEXP recreateSourceAttribute(SEXP rFunc) {
  // TODO This is not perfect, as R seems to use slightly different arguments to
  // deparse() when initially setting the source attribute.
  setAttrib(rFunc, R_SourceSymbol, eval(lang2(install("deparse"), rFunc), R_BaseEnv));
  return rFunc;
}

void mutateFullConstantsRecursive(SEXP rExpr, enum RandomDistributionKind rDistKind, double constScaling) {
  if(isNumeric(rExpr)) { // numeric constant...
    //Rprintf("Value: %f ", REAL(rExpr)[0]);
    if (unif_rand() <= 0.5) {
    switch (rDistKind) {
    case NORMAL:
      REAL(rExpr)[0]= REAL(rExpr)[0] + (constScaling/10) * norm_rand(); // random normal mutation
      break;
    case UNIFORM:
      
        REAL(rExpr)[0]= REAL(rExpr)[0] + (constScaling/10) *((unif_rand() * 2) - 1); // random uniform mutation
        break;
    } }
    //Rprintf(" NewValue: %f \n", REAL(rExpr)[0]);
  } else if (!isNull(CADR(rExpr))) { // composite...
    mutateFullConstantsRecursive(CADR(rExpr), rDistKind, constScaling);
    if (!isNull(CADDR(rExpr))) {
      mutateFullConstantsRecursive(CADDR(rExpr), rDistKind, constScaling); 
    }
  }
}

SEXP mutateConstants(SEXP rFunc, SEXP constScaling_ext) { // TODO add parameter for RandomDistributionKind
  
  PROTECT(constScaling_ext = coerceVector(constScaling_ext, REALSXP));
  double constScaling= REAL(constScaling_ext)[0];

  GetRNGstate();
    mutateFullConstantsRecursive(BODY(rFunc), UNIFORM, constScaling);
  PutRNGstate();
  //PROTECT(recreateSourceAttribute(rFunc));
  UNPROTECT(1);
  return rFunc;
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
                REAL(replacement)[0]= delSubCon->constScaling * ((unif_rand() * 2) - 1);
              } else {
                int varIdx= randIndex(delSubCon->nVariables); //create variable
                replacement= install(delSubCon->variables[varIdx]);
              }
              SETCAR(child, replacement); 
            }
          }
        }
      }
    if(*counter <= delSubCon->rSubtree) { 
      removeSubtreeRecursive(CADR(rExpr),delSubCon, counter);
      if(!isNull(CADDR(rExpr))){ // for functions with arity one (sin, cos...)
        removeSubtreeRecursive(CADDR(rExpr),delSubCon, counter);
      }
    }
  }
}

SEXP removeSubtree(SEXP rFunc, SEXP inSet, SEXP constProb_ext, SEXP constScaling_ext)
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
  PROTECT(constProb_ext = coerceVector(constProb_ext, REALSXP));
  delSubCon.constProb= REAL(constProb_ext)[0];

  PROTECT(constScaling_ext = coerceVector(constScaling_ext, REALSXP));
  delSubCon.constScaling= REAL(constScaling_ext)[0];

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
  UNPROTECT(2);
  return rFunc;
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

int countDepth(SEXP rExpr, int depth)  {
  depth= depth +1;
  if(isNumeric(rExpr)) { // numeric constant...
      return depth;
  } else if(isSymbol(rExpr)) { // 
      return depth;
  } else if (isLanguage(rExpr)){
     
      }int ldepth, rdepth = 0;
      ldepth= countDepth(CADR(rExpr), depth);
      if(!isNull(CADDR(rExpr))){ // for functions with arity one (sin, cos...)
      rdepth= countDepth(CADDR(rExpr), depth);
    }
 if (rdepth >= ldepth) {
   return rdepth; }
  else {
   return ldepth; }
}

void insertSubtreeRecursive(SEXP rExpr, struct RandExprGrowContext* TreeParams, int *counter, int rLeaf, int depth)  {
 
  SEXP replacement;
  depth= depth +1;
  
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
            if(depth < TreeParams->maxDepth ) {
              TreeParams->maxDepth= 2;
              PROTECT(replacement= randExprGrowRecursive(TreeParams, 1)); }
            else{
              TreeParams->maxDepth= 1;
              PROTECT(replacement= randExprGrowRecursive(TreeParams, 1)); }       
            PutRNGstate();
            SETCAR(child, replacement);
            UNPROTECT(1);
          }
        }
      }
      if(*counter <= rLeaf)  {
        insertSubtreeRecursive(CADR(rExpr),TreeParams, counter, rLeaf, depth);
        if(!isNull(CADDR(rExpr))){ // for functions with arity one (sin, cos...)
          insertSubtreeRecursive(CADDR(rExpr),TreeParams, counter, rLeaf, depth);
        }
      }
  }
}

SEXP insertSubtree(SEXP rFunc, SEXP funcSet, SEXP inSet, SEXP constProb_ext, SEXP subtreeProb_ext, SEXP maxDepth_ext, SEXP maxLeafs_ext, SEXP constScaling_ext)
{
  
  struct RandExprGrowContext TreeParams;
  TreeParams.probSubtree= 1; //100% chance for subtree
 
  PROTECT(maxDepth_ext = coerceVector(maxDepth_ext, INTSXP));
  TreeParams.maxDepth= INTEGER(maxDepth_ext)[0];
  
  TreeParams.nFunctions= LENGTH(funcSet);
  const char *arrayOfFunctions[TreeParams.nFunctions];
  int arrayOfArities[TreeParams.nFunctions];
  TreeParams.arities= arrayOfArities;
  
  for (int i= 0; i < TreeParams.nFunctions; i++) {
    arrayOfFunctions[i]= CHAR(STRING_ELT(funcSet,i));
  }
  getArities(arrayOfFunctions,TreeParams.arities, TreeParams.nFunctions);
  TreeParams.functions= arrayOfFunctions;

  PROTECT(constScaling_ext = coerceVector(constScaling_ext, REALSXP));
  TreeParams.constScaling= REAL(constScaling_ext)[0];

    // Variables
  TreeParams.nVariables= LENGTH(inSet);
  const char *arrayOfVariables[TreeParams.nVariables];
  for (int i= 0; i < TreeParams.nVariables; i++) {
    arrayOfVariables[i]= CHAR(STRING_ELT(inSet,i));
  }
  TreeParams.variables= arrayOfVariables;
  
    // Constant Prob	
  PROTECT(constProb_ext= coerceVector(constProb_ext, REALSXP));
  TreeParams.constProb= REAL(constProb_ext)[0];

  PROTECT(maxLeafs_ext= coerceVector(maxLeafs_ext, INTSXP));
  int maxLeafs= INTEGER(maxLeafs_ext)[0];
  
  int rLeaf, leafCounter= 0, counter= 0, depth= 0;
  countLeafs(BODY(rFunc), &leafCounter); 
  //depth= countDepth(BODY(rFunc), depth);
  // Rprintf("depth %d", depth);
  if((leafCounter > 1) && (leafCounter <= maxLeafs))  {
    GetRNGstate();
    rLeaf= rand_number(leafCounter);
     insertSubtreeRecursive(BODY(rFunc), &TreeParams, &counter, rLeaf, depth); 
    PutRNGstate(); //Todo Else for symbols and constants 
  } else { //restart if trees get too big or small
    SEXP rExpr;
    PROTECT(subtreeProb_ext = coerceVector(subtreeProb_ext, REALSXP));
    TreeParams.probSubtree= REAL(subtreeProb_ext)[0];
    GetRNGstate();
     PROTECT(rExpr= randExprGrowRecursive(&TreeParams, 1));
    PutRNGstate();
    SET_BODY(rFunc, rExpr);
    UNPROTECT(2);
    }
UNPROTECT(4);
return rFunc;;
} 

// .Call("insertSubtree",func1,c("+","-","*","/"),c("x","y","z"),0.2)

SEXP deleteInsertSubtree(SEXP rFunc, SEXP funcSet, SEXP inSet, SEXP constProb_ext, SEXP subtreeProb_ext, SEXP maxDepth_ext, SEXP maxLeafs_ext, SEXP constScaling_ext)
{ 
  GetRNGstate();
  int mutateop= rand_number(3);
  PutRNGstate();
  if (mutateop == 1) {
    PROTECT(rFunc= removeSubtree(rFunc, inSet, constProb_ext, constScaling_ext));
    UNPROTECT(1); } 
  if (mutateop == 2) {
    PROTECT(rFunc= insertSubtree(rFunc, funcSet, inSet, constProb_ext, subtreeProb_ext, maxDepth_ext, maxLeafs_ext, constScaling_ext));
    UNPROTECT(1); }
  if (mutateop == 3) {
    PROTECT(rFunc= mutateConstants(rFunc, constScaling_ext));
    UNPROTECT(1); }
  //PROTECT(recreateSourceAttribute(rFunc));
  return rFunc;
}

// Crossover functions


void selectSubtreeRecursive(SEXP rExpr, SEXP * returnExpr, int subtree, int * counter) {
if(isNumeric(rExpr)) { // numeric constant...
    return; // nothing to do
  } else if(isSymbol(rExpr)) { // 
    return; // nothing to do
  } else if (isLanguage(rExpr)) {// composite...
    for (SEXP child = CDR(rExpr); !isNull(child); child = CDR(child)) {
      if (isLanguage(CAR(child))){
        if (!isLanguage(CADR(CAR(child))) && !isLanguage(CADDR(CAR(child))) ) {
          *counter= *counter + 1;
            if(*counter == subtree) { // selected subtree is found
               *returnExpr= CAR(child); 
            }
          }
        }
      }
    if(*counter <= subtree) {
      selectSubtreeRecursive(CADR(rExpr), returnExpr, subtree, counter);
      if(!isNull(CADDR(rExpr))){ // for functions with arity one (sin, cos...)
        selectSubtreeRecursive(CADDR(rExpr),returnExpr, subtree, counter);
      }
    }
  }
}

void crossSubtreeRecursive(SEXP rExpr, SEXP rExpr2, int subtree, int * counter) {
if(isNumeric(rExpr)) { // numeric constant...
    return; // nothing to do
  } else if(isSymbol(rExpr)) { // 
    return; // nothing to do
  } else if (isLanguage(rExpr)) {// composite...
    for (SEXP child = CDR(rExpr); !isNull(child); child = CDR(child)) {
      if (isLanguage(CAR(child))){
        if (!isLanguage(CADR(CAR(child))) && !isLanguage(CADDR(CAR(child))) ) {
          *counter= *counter + 1;
            if(*counter == subtree) { // selected subtree is found
               SETCAR(child, rExpr2);  
            }
          }
        }
      }
    if(*counter <= subtree) {
      crossSubtreeRecursive(CADR(rExpr),rExpr2 , subtree, counter);
      if(!isNull(CADDR(rExpr))){ // for functions with arity one (sin, cos...)
        crossSubtreeRecursive(CADDR(rExpr), rExpr2 , subtree, counter);
      }
    }
  }
}

void selectInsertSubtreeRecursive(SEXP rExpr, SEXP rExpr2,  SEXP * returnExpr,  int subtree, int * counter) {
if(isNumeric(rExpr)) { // numeric constant...
    return; // nothing to do
  } else if(isSymbol(rExpr)) { // 
    return; // nothing to do
  } else if (isLanguage(rExpr)) {// composite...
    for (SEXP child = CDR(rExpr); !isNull(child); child = CDR(child)) {
      if (isLanguage(CAR(child))){
        if (!isLanguage(CADR(CAR(child))) && !isLanguage(CADDR(CAR(child))) ) {
          *counter= *counter + 1;
            if(*counter == subtree) { // selected subtree is found
               *returnExpr= CAR(child); 
               SETCAR(child, rExpr2); 
            }
          }
        }
      }
    if(*counter <= subtree) {
      selectInsertSubtreeRecursive(CADR(rExpr), rExpr2, returnExpr, subtree, counter);
      if(!isNull(CADDR(rExpr))){ // for functions with arity one (sin, cos...)
        selectInsertSubtreeRecursive(CADDR(rExpr), rExpr2, returnExpr, subtree, counter);
      }
    }
  }
}

SEXP crossover(SEXP rFunc1, SEXP rFunc2) {

int subtreeFunc1= 0, subtreeFunc2= 0, rSubtree1= 0, rSubtree2= 0, counter= 0, counter2= 0, counter3 = 0;
countSubtrees(BODY(rFunc1), &subtreeFunc1);
countSubtrees(BODY(rFunc2), &subtreeFunc2);
  if(subtreeFunc1 >= 1 && subtreeFunc2 >= 1 ) {
    GetRNGstate();
    rSubtree1= rand_number(subtreeFunc1); // get random subtree-number
    rSubtree2= rand_number(subtreeFunc2);
    PutRNGstate(); 
    SEXP exprPointer1;
    SEXP exprPointer2;
    selectSubtreeRecursive(BODY(rFunc1), &exprPointer1, rSubtree1, &counter);
    selectInsertSubtreeRecursive(BODY(rFunc2), exprPointer1,  &exprPointer2,  rSubtree2, &counter2);
    crossSubtreeRecursive(BODY(rFunc1), exprPointer2, rSubtree1, &counter3);
    //PROTECT(recreateSourceAttribute(rFunc1));
    //PROTECT(recreateSourceAttribute(rFunc2));
    //UNPROTECT(2);
    }

return R_NilValue;
}




 
 












