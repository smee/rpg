#include "eval_vectorized.h"
#include "mutate_function.h"
#include "selection.h"
#include "create_expr_tree.h"
#include "population.h"
/*
functionSet1 <- functionSet("+", "*", "-", "/")
inputVariableSet1 <- inputVariableSet("x")
numericConstantSet <- constantFactorySet(function() runif(1, -1, 1))
pop1 <- makePopulation(10, functionSet1, inputVariableSet1, numericConstantSet)  
*/


static void snr(int k, int n, int *y, int *x)
{
    int i, j;
    for (i = 0; i < n; i++)
	x[i] = i;
    for (i = 0; i < k; i++) {
	j = n * unif_rand();
	y[i] = x[j] + 1;
	x[j] = x[--n];
    }
}

int compare_doubles (const void *a, const void *b) //TODO
     {
       const double *da = (const double *) a;
       const double *db = (const double *) b;
     
       return (*da > *db) - (*da < *db);
     }

void sortByRmse(int * sampleNumbers, int * sortedNumbers, int sampleSize, SEXP rmseVector) //TODO sort isnt working
{ 
  rmseVector= coerceVector(rmseVector, REALSXP);
  double rmseArray[sampleSize];
  double rmseArraySort[sampleSize][2];
  for(int i=0; i < sampleSize; i++) {
    rmseArray[i]= REAL(rmseVector)[i];
    rmseArraySort[i][0]= REAL(rmseVector)[i];
    rmseArraySort[i][1]= (double)sampleNumbers[i];
   Rprintf("\n before: Numbers: %f RMSE: %f ",rmseArraySort[i][1], rmseArraySort[i][0]);
  }

 qsort(rmseArraySort, sampleSize, 2 * sizeof(double), compare_doubles); //TODO needs to sort 2-dimensional Array, not working
  
 for(int i=0; i < sampleSize; i++) {
   sortedNumbers[i] = (int)rmseArraySort[i][1];
   Rprintf(" \n RmseSorted: %f, Number: %d", rmseArraySort[i][0], sortedNumbers[i]);
  }
   
}

SEXP selection(SEXP population, SEXP sampleSize_ext, SEXP actualParameters, SEXP targetValues, SEXP funcSet, SEXP inSet,SEXP maxDepth_ext, SEXP constProb_ext, SEXP subtreeProb_ext) {

  SEXP rmseVectorA, rmseVectorB;
  sampleSize_ext = coerceVector(sampleSize_ext, INTSXP);
  int sampleSize= INTEGER(sampleSize_ext)[0];
  if(sampleSize % 4) {
   error("sampleSize cant be an odd number and has to be divisible by 4");
  }

  int popSize= LENGTH(population);
  int *x = Calloc(popSize*sizeof(int), int);
  int *y = Calloc(sampleSize*sizeof(int), int);
  int *sampleNumbersA= Calloc((sampleSize/2)*sizeof(int), int);
  int *sampleNumbersB= Calloc((sampleSize/2)*sizeof(int), int);

  GetRNGstate();
  snr(sampleSize, (popSize), y, x);
  PutRNGstate();
  for(int i=0; i < (sampleSize) ; i++) {
  //Rprintf(" %d", y[i]);
  }
  
  for(int i=0; i < (sampleSize / 2) ; i++) {
    sampleNumbersA[i]=y[i];
    }

  for(int i= (sampleSize / 2) ; i < sampleSize; i++) {
    int j= i - (sampleSize / 2);
    sampleNumbersB[j]=y[i];
    }
 
  PROTECT(rmseVectorA= allocVector(VECSXP, (sampleSize/2)));
  PROTECT(rmseVectorB= allocVector(VECSXP, (sampleSize/2)));
  for(int i=0; i < (sampleSize / 2) ; i++) {
    SET_VECTOR_ELT(rmseVectorA, i, evalVectorizedRmse(VECTOR_ELT(population, (sampleNumbersA[i]-1)), actualParameters, targetValues)); 
    SET_VECTOR_ELT(rmseVectorB, i, evalVectorizedRmse(VECTOR_ELT(population, (sampleNumbersB[i]-1)), actualParameters, targetValues)); } 

  int *sortedNumbersA= Calloc((sampleSize/2)*sizeof(int), int);
  int *sortedNumbersB= Calloc((sampleSize/2)*sizeof(int), int);
  sortByRmse(sampleNumbersA, sortedNumbersA, (sampleSize/2), rmseVectorA);
  sortByRmse(sampleNumbersB, sortedNumbersB, (sampleSize/2), rmseVectorB);
  for(int i=0; i < (sampleSize/2); i++) {
  Rprintf(" SortA: %d, SortB %d", sortedNumbersA[i] - 1, sortedNumbersB[i] - 1);
  }

 SEXP winnerA,winnerB,loserA,loserB;
 PROTECT(winnerA= allocVector(VECSXP, (sampleSize/4)));
 PROTECT(winnerB= allocVector(VECSXP, (sampleSize/4)));
 PROTECT(loserA= allocVector(VECSXP, (sampleSize/4)));
 PROTECT(loserB= allocVector(VECSXP, (sampleSize/4)));
 for(int i=0; i < (sampleSize/4); i++) {
   SET_VECTOR_ELT(winnerA, i, VECTOR_ELT(population, sortedNumbersA[i] - 1));
   SET_VECTOR_ELT(winnerB, i, VECTOR_ELT(population, sortedNumbersB[i] - 1));
   SET_VECTOR_ELT(loserA, i, VECTOR_ELT(population, sortedNumbersA[i + (sampleSize/4)] - 1));
   SET_VECTOR_ELT(loserB, i, VECTOR_ELT(population, sortedNumbersB[i + (sampleSize/4)] - 1));
   }

  for(int i= 0; i < (sampleSize/4); i++) { 
    // crossover(VECTOR_ELT(winnerA, i),(VECTOR_ELT(winnerB, i))); 
    SET_VECTOR_ELT(loserA, i, deleteInsertSubtree(VECTOR_ELT(loserA, i), funcSet, inSet, constProb_ext));
    SET_VECTOR_ELT(loserB, i, deleteInsertSubtree(VECTOR_ELT(loserB, i), funcSet, inSet, constProb_ext));
  } 
    
  

 //for(int i=(sampleSize/4); i < (sampleSize/2); i++) {
 //  SET_VECTOR_ELT(population, (sortedNumbersA[i]), VECTOR_ELT(winnerA, i - (sampleSize/4)));
 //  SET_VECTOR_ELT(population, (sortedNumbersB[i]), VECTOR_ELT(winnerB, i - (sampleSize/4)));
 // } 
/* PROTECT(loserPop= createPopulation(sampleSize_ext, funcSet, inSet, maxDepth_ext, constProb_ext, subtreeProb_ext));
for(int i=(sampleSize/4); i < (sampleSize/2); i++) {
Rprintf(" sortedNumbersA: %d, sortedNumbersB: %d",(sortedNumbersA[i]),(sortedNumbersB[i]));
SET_VECTOR_ELT(population, (sortedNumbersA[i]),VECTOR_ELT(loserPop,i));
SET_VECTOR_ELT(population, (sortedNumbersB[i]),VECTOR_ELT(loserPop,i));
} */

  Free(x);
  Free(y);
  Free(sampleNumbersA);
  Free(sampleNumbersB);
  Free(sortedNumbersA);
  Free(sortedNumbersB);
  UNPROTECT(6);

return population;

}





