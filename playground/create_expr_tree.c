
#include "create_expr_tree.h"
/* create_expr_tree.c
 *
 */



#include <R.h>
#include <Rinternals.h>

SEXP randomNumber(double constScaling) {
  SEXP Rval;
  PROTECT(Rval = allocVector(REALSXP, 1));
  REAL(Rval)[0] = constScaling * ((unif_rand() * 2) - 1);
  UNPROTECT(1);
  return Rval;
}

int randIndex(int a) {
  int x = a * unif_rand();
  if (x == a) {
    return x - 1;
  }
  return x;
}

SEXP randExprGrowRecursive(struct RandExprGrowContext * TreeParams, int currentDepth) {
if ((unif_rand() <= TreeParams->probSubtree)&&(currentDepth < TreeParams->maxDepth))
{
  const int funIdx= randIndex(TreeParams->nFunctions);
  const int arity= TreeParams->arities[funIdx];
  SEXP expr;
  PROTECT(expr= R_NilValue);
  for ( int i=0; i < arity; i++ ) {
    SEXP newParameter;
    PROTECT(newParameter = randExprGrowRecursive(TreeParams, currentDepth+1));
    PROTECT(expr= LCONS(newParameter, expr));
    }
  PROTECT(expr= LCONS(install(TreeParams->functions[funIdx]), expr));
  UNPROTECT(2 + 2*arity);
  return expr;
  }
  else if (unif_rand() <= TreeParams->constProb){ //create constant
    unif_rand(); // TODO constant factory
    return randomNumber(TreeParams->constScaling);
  } else {
    const int varIdx= randIndex(TreeParams->nVariables); //create variable
    SEXP expr2;
    PROTECT(expr2= install(TreeParams->variables[varIdx]));
    UNPROTECT(1);
    return expr2;
  }
}

void getArities(const char ** arrayOfFunctions, int * arrayOfArities, int nFunctions){
  for (int i=0; i < nFunctions; i++){
    if (!strcmp(arrayOfFunctions[i], "+")) {
      arrayOfArities[i] = 2; 
      }
    else if (!strcmp(arrayOfFunctions[i], "-")) {
      arrayOfArities[i] = 2; 
      }
    else if (!strcmp(arrayOfFunctions[i], "/")) {
      arrayOfArities[i] = 2;
      }
    else if (!strcmp(arrayOfFunctions[i], "*")) {
      arrayOfArities[i] = 2;
      }
    else if (!strcmp(arrayOfFunctions[i], "sin")) {
      arrayOfArities[i] = 1;
      }
    else if (!strcmp(arrayOfFunctions[i], "cos")) {
      arrayOfArities[i] = 1;
      }
    else if (!strcmp(arrayOfFunctions[i], "tan")) {
      arrayOfArities[i] = 1;
      }
    else if (!strcmp(arrayOfFunctions[i], "exp")) {
      arrayOfArities[i] = 1;
      }
    else if (!strcmp(arrayOfFunctions[i], "log")) {
      arrayOfArities[i] = 1;
      }
    else if (!strcmp(arrayOfFunctions[i], "sqrt")) {
      arrayOfArities[i] = 1;
      }
    else if (!strcmp(arrayOfFunctions[i], "abs")) {
      arrayOfArities[i] = 1;
      }
   
   }
}
 
//TODO error not functional
void initRandExprGrowContext(SEXP funcSet, SEXP inSet, SEXP maxDepth_ext, SEXP constProb_ext, SEXP subtreeProb_ext, struct RandExprGrowContext * TreeParams)
{
    // Functions
  TreeParams->nFunctions= LENGTH(funcSet);
  const char *arrayOfFunctions[TreeParams->nFunctions];
  int arrayOfArities[TreeParams->nFunctions];
  TreeParams->arities = arrayOfArities;
  
  for (int i= 0; i < TreeParams->nFunctions; i++) {
    arrayOfFunctions[i]= CHAR(STRING_ELT(funcSet,i));
  }
  getArities(arrayOfFunctions,TreeParams->arities, TreeParams->nFunctions);
  TreeParams->functions = arrayOfFunctions;

    // Variables
  TreeParams->nVariables= LENGTH(inSet);
  const char *arrayOfVariables[TreeParams->nVariables];
  for (int i= 0; i < TreeParams->nVariables; i++) {
    arrayOfVariables[i]= CHAR(STRING_ELT(inSet,i));
  }
  TreeParams->variables = arrayOfVariables;
  
  PROTECT(maxDepth_ext = coerceVector(maxDepth_ext, INTSXP));
  TreeParams->maxDepth= INTEGER(maxDepth_ext)[0];
    
    // Constant Prob	
  PROTECT(constProb_ext = coerceVector(constProb_ext, REALSXP));
  TreeParams->constProb= REAL(constProb_ext)[0];

    //	Subtree Prob
  PROTECT(subtreeProb_ext = coerceVector(subtreeProb_ext, REALSXP));
  TreeParams->probSubtree= REAL(subtreeProb_ext)[0];

  UNPROTECT(3);
} 

     
SEXP randExprGrow(SEXP funcSet, SEXP inSet, SEXP maxDepth_ext, SEXP constProb_ext, SEXP subtreeProb_ext, SEXP constScaling_ext)
{
  SEXP rfun;
 
  struct RandExprGrowContext TreeParams;
  //initRandExprGrowContext(funcSet, inSet, maxDepth_ext, constProb_ext, subtreeProb_ext, &TreeParams); //TODO
    // Functions
  TreeParams.nFunctions= LENGTH(funcSet);
  const char *arrayOfFunctions[TreeParams.nFunctions];
  int arrayOfArities[TreeParams.nFunctions];
  TreeParams.arities = arrayOfArities;
  
  for (int i= 0; i < TreeParams.nFunctions; i++) {
    arrayOfFunctions[i]= CHAR(STRING_ELT(funcSet,i));
  }
  getArities(arrayOfFunctions,TreeParams.arities, TreeParams.nFunctions);
  TreeParams.functions = arrayOfFunctions;

    // Variables
  TreeParams.nVariables= LENGTH(inSet);
  const char *arrayOfVariables[TreeParams.nVariables];
  for (int i= 0; i < TreeParams.nVariables; i++) {
    arrayOfVariables[i]= CHAR(STRING_ELT(inSet,i));
  }
  TreeParams.variables = arrayOfVariables;
  
  PROTECT(maxDepth_ext = coerceVector(maxDepth_ext, INTSXP));
  TreeParams.maxDepth= INTEGER(maxDepth_ext)[0];
    
    // Constant Prob	
  PROTECT(constProb_ext = coerceVector(constProb_ext, REALSXP));
  TreeParams.constProb= REAL(constProb_ext)[0];

    //	Subtree Prob
  PROTECT(subtreeProb_ext = coerceVector(subtreeProb_ext, REALSXP));
  TreeParams.probSubtree= REAL(subtreeProb_ext)[0];
  
  PROTECT(constScaling_ext = coerceVector(constScaling_ext, REALSXP));
  TreeParams.constScaling= REAL(constScaling_ext)[0];
 
 int currentDepth= 0;
  GetRNGstate();
    PROTECT(rfun= randExprGrowRecursive(&TreeParams, currentDepth));
  PutRNGstate();
  UNPROTECT(5);
  return rfun;
} 

//TODO

/* SEXP randExprGrow(SEXP funcSet, SEXP inSet, SEXP maxDepth_ext, SEXP constProb_ext, SEXP subtreeProb_ext)
{
  SEXP rfun;
  
  struct RandExprGrowContext TreeParams;
  initRandExprGrowContext(funcSet, inSet, maxDepth_ext, constProb_ext, subtreeProb_ext, &TreeParams);

  int currentDepth= 1;
  GetRNGstate();
    rfun= randExprGrowRecursive(&TreeParams, currentDepth);
  PutRNGstate();
  return rfun;
} */ 

SEXP exprToFunction(int nVariables, const char **vaList, SEXP rExpr)  {
  PROTECT(rExpr);
  SEXP charList, rChar, pl;
  SEXP rFunc;
  PROTECT(rFunc= allocSExp(CLOSXP));
  SET_CLOENV(rFunc, R_GlobalEnv);
  int i = 0, warn= 0, n= 0;
  if(nVariables > 0) {
  PROTECT(charList = allocVector(STRSXP, nVariables));
  
  for(int i=0; i < nVariables; i++){ //TODO STRSXP fill
    PROTECT(rChar= mkChar(vaList[i]));
    SET_STRING_ELT(charList, i, rChar);
    UNPROTECT(1);
  }
  PROTECT(charList= VectorToPairList(charList));
  n= length(charList);
  if(n > 0) { 
    PROTECT(pl = allocList(n));
    if(n == 1) {
      SET_TAG(pl, CreateTag(CAR(charList)));
      SETCAR(pl, R_MissingArg);
               }
     else
     { SET_TAG(pl, CreateTag(CAR(charList)));
       SETCAR(pl, R_MissingArg);
       SEXP nextpl= CDR(pl);
       SEXP nextChar= CDR(charList);
       for (i= 1; i < n; i++, nextpl = CDR(nextpl), nextChar = CDR(nextChar)) {
        SET_TAG(nextpl, CreateTag(CAR(nextChar)));
	SETCAR(nextpl, R_MissingArg);
        }
     }  
   } }
  SET_FORMALS(rFunc, pl); 
  SET_BODY(rFunc, rExpr);
  //setAttrib(rFunc, R_SourceSymbol, eval(lang2(install("deparse"), rFunc), R_BaseEnv)); // TODO: Deparse not necessary
  if(n > 0) {UNPROTECT(1);}
  UNPROTECT(4); 
  return rFunc;
}

SEXP randFuncGrow(SEXP funcSet, SEXP inSet, SEXP maxDepth_ext, SEXP constProb_ext, SEXP subtreeProb_ext, SEXP constScaling_ext)
{
  SEXP rfun;
 
  struct RandExprGrowContext TreeParams;
  //initRandExprGrowContext(funcSet, inSet, maxDepth_ext, constProb_ext, subtreeProb_ext, &TreeParams); //TODO
    // Functions
  TreeParams.nFunctions= LENGTH(funcSet);
  const char *arrayOfFunctions[TreeParams.nFunctions];
  int arrayOfArities[TreeParams.nFunctions];
  TreeParams.arities = arrayOfArities;
  
  for (int i= 0; i < TreeParams.nFunctions; i++) {
    arrayOfFunctions[i]= CHAR(STRING_ELT(funcSet,i));
  }
  getArities(arrayOfFunctions,TreeParams.arities, TreeParams.nFunctions);
  TreeParams.functions = arrayOfFunctions;

    // Variables
  TreeParams.nVariables= LENGTH(inSet);
  const char *arrayOfVariables[TreeParams.nVariables];
  for (int i= 0; i < TreeParams.nVariables; i++) {
    arrayOfVariables[i]= CHAR(STRING_ELT(inSet,i));
  }
  TreeParams.variables = arrayOfVariables;
  
  PROTECT(maxDepth_ext = coerceVector(maxDepth_ext, INTSXP));
  TreeParams.maxDepth= INTEGER(maxDepth_ext)[0];
    
    // Constant Prob	
  PROTECT(constProb_ext = coerceVector(constProb_ext, REALSXP));
  TreeParams.constProb= REAL(constProb_ext)[0];

    //	Subtree Prob
  PROTECT(subtreeProb_ext = coerceVector(subtreeProb_ext, REALSXP));
  TreeParams.probSubtree= REAL(subtreeProb_ext)[0];

  PROTECT(constScaling_ext = coerceVector(constScaling_ext, REALSXP));
  TreeParams.constScaling= REAL(constScaling_ext)[0];
 
 int currentDepth= 0;
  GetRNGstate();
    PROTECT(rfun= randExprGrowRecursive(&TreeParams, currentDepth));
  PutRNGstate();
  PROTECT(rfun= exprToFunction(TreeParams.nVariables, TreeParams.variables, rfun));
  UNPROTECT(6);
  return rfun;
} 



















