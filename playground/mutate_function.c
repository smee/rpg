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

int randIndex2(int a) {
  int x = a * unif_rand();
  if (x == a) {
    return x - 1;
  }
  return x;
}

static int rand_number(int a)
{
int x= a * unif_rand();
if(x == a) 
{return x;}
return x + 1;
}

int expression_arity(SEXP expr) {
    int arity = 0;
    while (!isNull(CDR(expr))) {
        expr = CDR(expr);
        ++arity;
    }
    return arity;
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
    if (unif_rand() <= 0.3) {
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

void countNodes(SEXP rExpr, int *counter)  {
 
  if(isNumeric(rExpr)) { // numeric constant...
      return;
  } else if(isSymbol(rExpr)) { // 
      return;
  } else if (isLanguage(rExpr)){
         *counter= *counter + 1; } 
      countNodes(CADR(rExpr), counter);
      if(!isNull(CADDR(rExpr))){ // for functions with arity one (sin, cos...)
        countNodes(CADDR(rExpr), counter);
    }
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

void changeNodesRekursive(SEXP rExpr, struct RandExprGrowContext* TreeParams)  {
 
  if(isNumeric(rExpr)) { // numeric constant...
      return;
  } else if(isSymbol(rExpr)) { // 
      return;
  } else if ((isLanguage(rExpr)) && (unif_rand() <= 0.3)) {
     int oldArity= expression_arity(rExpr);
     SEXP replacement;
     int arity, funIdx;
        while(arity != oldArity) {
        funIdx= randIndex2(TreeParams->nFunctions);
        arity= TreeParams->arities[funIdx];
        }
        PROTECT(replacement= install(TreeParams->functions[funIdx]));
        SETCAR(rExpr, replacement);
        UNPROTECT(1);
          }
        changeNodesRekursive(CADR(rExpr),TreeParams);
        if(!isNull(CADDR(rExpr))){ // for functions with arity one (sin, cos...)
          changeNodesRekursive(CADDR(rExpr),TreeParams); }
}

SEXP insertSubtree(SEXP rFunc, SEXP funcSet, SEXP inSet, SEXP constProb_ext, SEXP subtreeProb_ext, SEXP maxDepth_ext, SEXP maxLeafs_ext, SEXP maxNodes_ext, SEXP constScaling_ext)
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

  PROTECT(maxNodes_ext= coerceVector(maxNodes_ext, INTSXP));
  int maxNodes= INTEGER(maxNodes_ext)[0];
  
  int rLeaf, leafCounter= 0, counter= 0, depth= 0, nodeCounter= 0;
  countLeafs(BODY(rFunc), &leafCounter); 
  countNodes(BODY(rFunc), &nodeCounter); 
  //Rprintf(" %d", nodeCounter);
  //depth= countDepth(BODY(rFunc), depth);
  // Rprintf("depth %d", depth);
  if((leafCounter > 1) && (leafCounter <= maxLeafs) && (nodeCounter <= maxNodes))  {
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
UNPROTECT(5);
return rFunc;;
} 

SEXP changeNodes(SEXP rFunc, SEXP funcSet, SEXP inSet, SEXP constProb_ext, SEXP subtreeProb_ext, SEXP maxDepth_ext, SEXP constScaling_ext)
{
  
  struct RandExprGrowContext TreeParams;
  TreeParams.probSubtree= 1;
 
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

    GetRNGstate();
     changeNodesRekursive(BODY(rFunc), &TreeParams); 
    PutRNGstate(); 
UNPROTECT(3);
return rFunc;
} 

// .Call("insertSubtree",func1,c("+","-","*","/"),c("x","y","z"),0.2)

SEXP deleteInsertChangeSubtree(SEXP rFunc, SEXP funcSet, SEXP inSet, SEXP constProb_ext, SEXP subtreeProb_ext, SEXP maxDepth_ext, SEXP maxLeafs_ext, SEXP maxNodes_ext, SEXP constScaling_ext)
{ 
  GetRNGstate();
  int mutateop= rand_number(4);
  PutRNGstate();
  if (mutateop == 1) {
    PROTECT(rFunc= removeSubtree(rFunc, inSet, constProb_ext, constScaling_ext));
    UNPROTECT(1); } 
  else if (mutateop == 2) {
    PROTECT(rFunc= insertSubtree(rFunc, funcSet, inSet, constProb_ext, subtreeProb_ext, maxDepth_ext, maxLeafs_ext, maxNodes_ext, constScaling_ext));
    UNPROTECT(1); }
  else if (mutateop == 3) {
    PROTECT(rFunc= mutateConstants(rFunc, constScaling_ext));
    UNPROTECT(1); }
  else if (mutateop == 4) {
    PROTECT(rFunc= changeNodes(rFunc, funcSet, inSet, constProb_ext, subtreeProb_ext, maxDepth_ext, constScaling_ext));
    UNPROTECT(1); }
  //PROTECT(recreateSourceAttribute(rFunc));
  return rFunc;
}

// Crossover functions
// Single point crossover Subtreedepth=2

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

// Crossover random node, no depth control! do not use! 

void selectNodeRecursive(SEXP rExpr, SEXP * returnExpr, int node, int * counter) {
if(isNumeric(rExpr)) { // numeric constant...
    return; // nothing to do
  } else if(isSymbol(rExpr)) { // 
    return; // nothing to do
  } else if (isLanguage(rExpr)) {// composite...
          *counter= *counter + 1;
          if(*counter == node) { // selected subtree is found
           *returnExpr= CADR(rExpr); 
  
      }
    if(*counter <= node) {
      selectNodeRecursive(CADR(rExpr), returnExpr, node, counter);
      if(!isNull(CADDR(rExpr))){ // for functions with arity one (sin, cos...)
        selectNodeRecursive(CADDR(rExpr),returnExpr, node, counter);
      }
    }
  }
}

void crossNodeRecursive(SEXP rExpr, SEXP rExpr2, int node, int * counter) {
if(isNumeric(rExpr)) { // numeric constant...
    return; // nothing to do
  } else if(isSymbol(rExpr)) { // 
    return; // nothing to do
  } else if (isLanguage(rExpr)) {// composite...
          *counter= *counter + 1;
            if(*counter == node) { // selected subtree is found
               SETCAR(CDR(rExpr), rExpr2);  
         
      }
    if(*counter <= node) {
      crossNodeRecursive(CADR(rExpr),rExpr2 , node, counter);
      if(!isNull(CADDR(rExpr))){ // for functions with arity one (sin, cos...)
        crossNodeRecursive(CADDR(rExpr), rExpr2 , node, counter);
      }
    }
  }
}

void selectInsertNodeRecursive(SEXP rExpr, SEXP rExpr2,  SEXP * returnExpr,  int node, int * counter) {
if(isNumeric(rExpr)) { // numeric constant...
    return; // nothing to do
  } else if(isSymbol(rExpr)) { // 
    return; // nothing to do
  } else if (isLanguage(rExpr)) {// composite...
          *counter= *counter + 1;
            if(*counter == node) { // selected subtree is found
               *returnExpr= CADR(rExpr); 
               SETCAR(CDR(rExpr), rExpr2); 
      }
    if(*counter <= node) {
      selectInsertNodeRecursive(CADR(rExpr), rExpr2, returnExpr, node, counter);
      if(!isNull(CADDR(rExpr))){ // for functions with arity one (sin, cos...)
        selectInsertNodeRecursive(CADDR(rExpr), rExpr2, returnExpr, node, counter);
      }
    }
  }
}

SEXP crossoverRandomNode(SEXP rFunc1, SEXP rFunc2) {

int nodeFunc1= 0, nodeFunc2= 0, rSubtree1= 0, rSubtree2= 0, counter= 0, counter2= 0, counter3 = 0, depth1=0, depth2=0;
countNodes(BODY(rFunc1), &nodeFunc1);
countNodes(BODY(rFunc2), &nodeFunc2);
depth1= countDepth(BODY(rFunc1), depth1);
depth2= countDepth(BODY(rFunc1), depth2);

//Rprintf(" %d %d ", depth1, depth2);
  if(nodeFunc1 >= 1 && nodeFunc2 >= 1 && (depth1 < 7 || depth2 < 7) ) {
    GetRNGstate();
    rSubtree1= rand_number(nodeFunc1); // get random subtree-number
    rSubtree2= rand_number(nodeFunc2);
    //Rprintf("rS1: %d rS2: %d", rSubtree1, rSubtree2);
    PutRNGstate(); 
    SEXP exprPointer1;
    SEXP exprPointer2;
    PROTECT(exprPointer1);
    PROTECT(exprPointer2);
    selectNodeRecursive(BODY(rFunc1), &exprPointer1, rSubtree1, &counter);
    selectInsertNodeRecursive(BODY(rFunc2), exprPointer1,  &exprPointer2,  rSubtree2, &counter2);
    crossNodeRecursive(BODY(rFunc1), exprPointer2, rSubtree1, &counter3);
    //PROTECT(recreateSourceAttribute(rFunc1));
    //PROTECT(recreateSourceAttribute(rFunc2));
    //Rprintf("suc");
    UNPROTECT(2);
    } else {
    crossover(rFunc1, rFunc2); }

return R_NilValue;
}

//Cross with regard to treesize 

void selectDPTRecursive(SEXP rExpr, SEXP * returnExpr, int node, int * counter, int depth, int * rdepth) {
depth= depth+1;
if(isNumeric(rExpr)) { // numeric constant...
    return; // nothing to do
  } else if(isSymbol(rExpr)) { // 
    return; // nothing to do
  } else if (isLanguage(rExpr)) {// composite...
          *counter= *counter + 1;
          if(*counter == node) { // selected subtree is found
           *returnExpr= CADR(rExpr); 
           *rdepth= depth;
  
      }
    if(*counter <= node) {
      selectDPTRecursive(CADR(rExpr), returnExpr, node, counter, depth, rdepth);
      if(!isNull(CADDR(rExpr))){ // for functions with arity one (sin, cos...)
        selectDPTRecursive(CADDR(rExpr),returnExpr, node, counter, depth, rdepth);
      }
    }
  }
}

void crossDPTRecursive(SEXP rExpr, SEXP rExpr2, int node, int * counter) {
if(isNumeric(rExpr)) { // numeric constant...
    return; // nothing to do
  } else if(isSymbol(rExpr)) { // 
    return; // nothing to do
  } else if (isLanguage(rExpr)) {// composite...
          *counter= *counter + 1;
            if(*counter == node) { // selected subtree is found
               SETCAR(CDR(rExpr), rExpr2);  
         
      }
    if(*counter <= node) {
      crossDPTRecursive(CADR(rExpr),rExpr2 , node, counter);
      if(!isNull(CADDR(rExpr))){ // for functions with arity one (sin, cos...)
        crossDPTRecursive(CADDR(rExpr), rExpr2 , node, counter);
      }
    }
  }
}

void selectInsertDPTRecursive(SEXP rExpr, SEXP rExpr2,  SEXP * returnExpr,  int node, int * counter, int maxSubtreeDepth, int depthcounter, int maximaldepth) {
depthcounter= depthcounter+1;
if(isNumeric(rExpr)) { // numeric constant...
    return; // nothing to do
  } else if(isSymbol(rExpr)) { // 
    return; // nothing to do
  } else if (isLanguage(rExpr)) {// composite...
      if(depthcounter <= maximaldepth) {
      int subtreedepth= countDepth(CADR(rExpr), subtreedepth);
      if(subtreedepth <= maxSubtreeDepth) {
          *counter= *counter + 1;
            if(*counter == node) { // selected subtree is found
               *returnExpr= CADR(rExpr); 
               SETCAR(CDR(rExpr), rExpr2); 
               
      } } }
    if(*counter <= node) {
      selectInsertDPTRecursive(CADR(rExpr), rExpr2, returnExpr, node, counter, maxSubtreeDepth, depthcounter, maximaldepth);
      if(!isNull(CADDR(rExpr))){ // for functions with arity one (sin, cos...)
        selectInsertDPTRecursive(CADDR(rExpr), rExpr2, returnExpr, node, counter, maxSubtreeDepth, depthcounter, maximaldepth);
      }
    }
  }
}

void countCrossPoints(SEXP rExpr, int *counter, int maxSubtreeDepth, int depthcounter, int maximaldepth)  {
  depthcounter= depthcounter+1;
  if(isNumeric(rExpr)) { // numeric constant...
      return;
  } else if(isSymbol(rExpr)) { // 
      return;
  } else if (isLanguage(rExpr)){
    if(depthcounter <= maximaldepth) {
      int subtreedepth= countDepth(CADR(rExpr), subtreedepth);
      if( subtreedepth <= maxSubtreeDepth )  {
         *counter= *counter + 1; }  } }
      countCrossPoints(CADR(rExpr), counter, maxSubtreeDepth, depthcounter, maximaldepth);
      if(!isNull(CADDR(rExpr))){ // for functions with arity one (sin, cos...)
        countCrossPoints(CADDR(rExpr), counter, maxSubtreeDepth, depthcounter, maximaldepth);
    }
}


SEXP crossoverDepthProof(SEXP rFunc1, SEXP rFunc2, SEXP maxDepth_ext) {

int nodeFunc1= 0, nodeFunc2= 0, selectNode1= 0, selectNode2= 0, countNode= 0, counter= 0, counter2= 0, counter3 = 0;
countNodes(BODY(rFunc1), &nodeFunc1);
countNodes(BODY(rFunc2), &nodeFunc2);
//Rprintf(" %d %d ", depth1, depth2);
  if(nodeFunc1 >= 1 && nodeFunc2 >= 1) {

    PROTECT(maxDepth_ext = coerceVector(maxDepth_ext, INTSXP));
    int maxTreeDepth= INTEGER(maxDepth_ext)[0] + 1;

    GetRNGstate();
    selectNode1= rand_number(nodeFunc1); // get random subtree-number
    //Rprintf("rS1: %d rS2: %d", rSubtree1, rSubtree2);
    PutRNGstate(); 
    SEXP exprPointer1;
    SEXP exprPointer2;
    PROTECT(exprPointer1);
    PROTECT(exprPointer2);
    
    int subtreedepth= 0, depthcounter= 0, CrossPointDepth= 0, maxSubtreeDepth, maximalPointDepth;
    selectDPTRecursive(BODY(rFunc1), &exprPointer1, selectNode1, &counter, depthcounter, &CrossPointDepth);
    subtreedepth= countDepth(exprPointer1, subtreedepth);
    maxSubtreeDepth= maxTreeDepth - CrossPointDepth;
    maximalPointDepth= maxTreeDepth - subtreedepth;
   // Rprintf(" maxSubtreeDepth %d, CrossPointDepth %d, maximalPointDepth %d, subtreedepth %d ", maxSubtreeDepth, CrossPointDepth, maximalPointDepth, subtreedepth);
      depthcounter= 0;
    countCrossPoints(BODY(rFunc2), &countNode, maxSubtreeDepth, depthcounter, maximalPointDepth);
    selectNode2= rand_number(countNode);
      depthcounter= 0;
    selectInsertDPTRecursive(BODY(rFunc2), exprPointer1,  &exprPointer2,  selectNode2, &counter2, maxSubtreeDepth, depthcounter, maximalPointDepth);
    crossDPTRecursive(BODY(rFunc1), exprPointer2, selectNode1, &counter3);
    //PROTECT(recreateSourceAttribute(rFunc1));
    //PROTECT(recreateSourceAttribute(rFunc2));
    //Rprintf("suc");
    UNPROTECT(3);
    }

return R_NilValue;
}


/*
dyn.load("evolution.so")
test1 <- function(x) 1+2+3+4+5+6
test2 <- function(x) 1*2*3*4*5*6
.Call("crossoverDepthProof",test1,test2) 
*/



 
 












