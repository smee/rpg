#include "eval_vectorized.h"
#include "mutate_function.h"
#include "selection.h"
#include "population.h"
#include "create_expr_tree.h"

void summary(SEXP population,SEXP actualParameters, SEXP targetValues) {

int popSize= LENGTH(population);
SEXP rmseVectorSum;
PROTECT(rmseVectorSum= allocVector(VECSXP, popSize));

for(int i= 0; i < popSize; i++) {
SET_VECTOR_ELT(rmseVectorSum, i, evalVectorizedRmse(VECTOR_ELT(population, i), actualParameters, targetValues)); 
}
int *numbersRMSE= Calloc(popSize * sizeof(int), int);
int *sortedNumbersRMSE= Calloc(popSize * sizeof(int), int);
for(int i=0; i < popSize; i++) {
numbersRMSE[i]=i + 1;
}

sortByRmse(numbersRMSE, sortedNumbersRMSE, popSize, rmseVectorSum);

PROTECT(rmseVectorSum= coerceVector(rmseVectorSum, REALSXP));
double rmseArray[popSize];
for(int i= 0; i < popSize; i++) {
  rmseArray[i]= REAL(rmseVectorSum)[i];
}
for(int i= 0; i < popSize; i++) {
  int temp=sortedNumbersRMSE[i] - 1;
  printf("\n Number: %d, RMSE: %f ",sortedNumbersRMSE[i], rmseArray[temp]);
}
UNPROTECT(2);
}


SEXP evolutionRun(SEXP numberOfRuns_ext, SEXP popSize_ext, SEXP sampleSize_ext, SEXP actualParameters, SEXP targetValues, SEXP funcSet, SEXP inSet, SEXP maxDepth_ext, SEXP constProb_ext, SEXP subtreeProb_ext) {

SEXP population;

  PROTECT(population= createPopulation(popSize_ext, funcSet, inSet, maxDepth_ext, constProb_ext, subtreeProb_ext));
  PROTECT(numberOfRuns_ext= coerceVector(numberOfRuns_ext, INTSXP));

  int numberOfRuns= INTEGER(numberOfRuns_ext)[0];
for(int i=0; i < numberOfRuns; i++) {
  Rprintf(" \n StepNumber: %d", i+1);
    PROTECT(population= selection(population, sampleSize_ext, actualParameters, targetValues, funcSet, inSet, maxDepth_ext, constProb_ext, subtreeProb_ext));
    UNPROTECT(1);
  }
summary(population, actualParameters, targetValues);
UNPROTECT(2);
return population;
}





//   pop1 <- .Call("evolutionRun", Runs = 1000, popsize= 40, samples= 20, actualParameters= c(1,2,3,4,5,6), targetValues= c(1.1,4.1,9.1,16.1,25.1,36.1), funcset= c("+","-","*","/"),inset=c("x"),maxdepth= 7,constprob= 0.2,subtreeprob= 0.5) 
