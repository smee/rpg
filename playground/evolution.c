#include "eval_vectorized.h"
#include "mutate_function.h"
#include "selection.h"
#include "population.h"
#include "create_expr_tree.h"

SEXP evolutionRun(SEXP numberOfRuns_ext, SEXP popSize_ext, SEXP sampleSize_ext, SEXP actualParameters, SEXP targetValues, SEXP funcSet, SEXP inSet, SEXP maxDepth_ext, SEXP constProb_ext, SEXP subtreeProb_ext) {

  numberOfRuns_ext= coerceVector(numberOfRuns_ext, INTSXP);
  int numberOfRuns= INTEGER(numberOfRuns_ext)[0];

SEXP population;
PROTECT(population= createPopulation(popSize_ext, funcSet, inSet, maxDepth_ext, constProb_ext, subtreeProb_ext));

for(int i=0; i < numberOfRuns; i++) {
  Rprintf(" \n StepNumber: %d", i);
    population= selection(population, sampleSize_ext, actualParameters, targetValues, funcSet, inSet, maxDepth_ext, constProb_ext, subtreeProb_ext);
  }
UNPROTECT(1);
return population;
}





//  .Call("evolution", Runs = 100, popsize= 10, samples= 4, actualParameters= c(1,2,3,4,5), targetValues= c(2,3,4,5,6), funcset= c("+","-","*","/"),inset=("x"),maxdepth= 8,constprob= 0.2,subtreeprob= 0.5)
