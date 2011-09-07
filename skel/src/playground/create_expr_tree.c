#include "create_expr_tree.h"

// test12 <- function(n) { .Call("test_wrapper_full",n) }



struct RandExprGrowContext {
  const char ** functions;
  int * arities;
  int nFunctions;
  double probSubtree;
  const char ** variables;
  int nVariables;
  double probConstant;
  int maxDepth;
};

SEXP randomNumber() {
  SEXP Rval;
  PROTECT(Rval = allocVector(REALSXP, 1));
  REAL(Rval)[0] = (unif_rand() * 2) - 1;
  UNPROTECT(1);

  return Rval;
}

static int randIndex(int a) {
  int x = a * unif_rand();
  if (x == a) {
    return x - 1;
  }
  return x;
}

static R_INLINE SEXP randExprGrowRecursive(struct RandExprGrowContext * TreeParams, int currentDepth) {
if ((unif_rand() <= TreeParams->probSubtree)&&(currentDepth < TreeParams->maxDepth))
{
  const int funIdx= randIndex(TreeParams->nFunctions);
  const int arity= TreeParams->arities[funIdx];
  SEXP expr= R_NilValue;
  for ( int i=0; i < arity; i++ ) {
    SEXP newParameter;
    PROTECT(newParameter = randExprGrowRecursive(TreeParams, currentDepth+1));
    UNPROTECT(1);
    expr= LCONS(newParameter, expr);
    }
  expr= LCONS(install(TreeParams->functions[funIdx]), expr);
  return expr;
  }
  else if (unif_rand() <= TreeParams->probConstant){ //create constant
    unif_rand(); // TODO constant factory
    return randomNumber();
  } else {
    const int varIdx= randIndex(TreeParams->nVariables); //create variable
    const SEXP expr2= install(TreeParams->variables[varIdx]);
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
   }
}
      
SEXP randExprGrow(SEXP funcSet, SEXP InSet, SEXP maxDepth_ext, SEXP constProb_ext, SEXP subtreeProb_ext)
{
  SEXP rfun;
  struct RandExprGrowContext TreeParams;

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
  TreeParams.nVariables= LENGTH(InSet);
  const char *arrayOfVariables[TreeParams.nVariables];
  for (int i= 0; i < TreeParams.nVariables; i++) {
    arrayOfVariables[i]= CHAR(STRING_ELT(InSet,i));
  }
  TreeParams.variables = arrayOfVariables;
  
  maxDepth_ext = coerceVector(maxDepth_ext, INTSXP);
  TreeParams.maxDepth= INTEGER(maxDepth_ext)[0];
    
    // Constant Prob	
  constProb_ext = coerceVector(constProb_ext, REALSXP);
  TreeParams.probConstant= REAL(constProb_ext)[0];

    //	Subtree Prob
  subtreeProb_ext = coerceVector(subtreeProb_ext, REALSXP);
  TreeParams.probSubtree= REAL(subtreeProb_ext)[0];
    
    
  int currentDepth= 1;
  GetRNGstate();
    rfun= randExprGrowRecursive(&TreeParams, currentDepth);
  PutRNGstate();
  return rfun;
} 

