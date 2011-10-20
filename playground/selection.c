#include <R.h>
#include <Rinternals.h>
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

SEXP sample(SEXP population, SEXP sampleSize_ext) {

  sampleSize_ext = coerceVector(sampleSize_ext, INTSXP);
  int sampleSize= INTEGER(sampleSize_ext)[0];

  SEXP samplePop = allocVector(VECSXP, sampleSize);
  
  int popSize= LENGTH(population);

  int *x = malloc(popSize*sizeof(int));
  int *y = malloc(sampleSize*sizeof(int));

  GetRNGstate();
  snr(sampleSize, popSize, y, x);
  PutRNGstate();
  
  

  for(int i=0; i < sampleSize; i++) {
    Rprintf("%d ",y[i]);
    SET_VECTOR_ELT(samplePop, i, VECTOR_ELT(population, (y[i]-1))); 
  }

  free(x);
  free(y);

  return samplePop; 
}

void quicksort(double *a,int l,int r, int n)
{
     double t;
     if(l>=r)
        return;
     int i=l;
     int j=r+1;
     double pivot=a[l];
     while(1)
     {
   do
   {i++;}while(a[i]<pivot && i<n);
   do
   {j--;}while(a[j]>pivot && j<n);
   if(i>=j)
   break;
   t=a[i],a[i]=a[j],a[j]=t;
      }
      a[l]=a[j];
      a[j]=pivot;
      quicksort(a,l,j-1, n);
      quicksort(a,j+1,r, n);
}

SEXP sortByRmse(SEXP samplePop, int sampleSize, SEXP rmseVector) //TODO not perfect
{ SEXP samplePopSorted;
  double rmseArray[sampleSize];
  double rmseArraySort[sampleSize];
  PROTECT(samplePopSorted= allocVector(VECSXP, sampleSize));

  for(int i=0; i < sampleSize; i++) {
    rmseArray[i]= REAL(rmseVector)[i];
    rmseArraySort[i]= REAL(rmseVector)[i];
  }

  quicksort(rmseArray, 0, sampleSize, sampleSize);

  for(int i=0; i < ( sampleSize / 2 ); i++) {
    for(int j=0; j < sampleSize; j++) {
      if(rmseArraySort[i] == rmseArray[j]) {
        SET_VECTOR_ELT(samplePopSorted, i, VECTOR_ELT(samplePop, j));
      }
    }
  }

  UNPROTECT(1);       
  return samplePopSorted;
}

SEXP selection(SEXP population, SEXP sampleSize_ext, SEXP actualParameters, SEXP targetValues) {

  SEXP rmseVectorA, rmseVectorB, winnerPop;
  sampleSize_ext = coerceVector(sampleSize_ext, INTSXP);
  int sampleSize= INTEGER(sampleSize_ext)[0];

  SEXP selectionPopA = sample(population, sampleSize_ext);
  SEXP selectionPopB = sample(population, sampleSize_ext);
  PROTECT(rmseVectorA= allocVector(REALSXP, sampleSize));
  PROTECT(rmseVectorB= allocVector(REALSXP, sampleSize));

  for (int i=0; i < sampleSize; i++) {

    SET_VECTOR_ELT(rmseVectorA, i, evalVectorizedRmse(VECTOR_ELT(selectionPopA,i), actualParameters, targetValues)); //TODO eval
    SET_VECTOR_ELT(rmseVectorB, i, evalVectorizedRmse(VECTOR_ELT(selectionPopB,i), actualParameters, targetValues)); 
  }

  sortByRmse(selectionPopA, sampleSize, rmseVectorA);
  sortByRmse(selectionPopB, sampleSize, rmseVectorB);

  PROTECT(winnerPop= allocVector(VECSXP, sampleSize));

  for(int i=0; i < (sampleSize / 2); i++) { //TODO crossover
    SET_VECTOR_ELT(winnerPop, i, VECTOR_ELT(selectionPopA,i));
    SET_VECTOR_ELT(winnerPop, (i + ((sampleSize / 2))), VECTOR_ELT(selectionPopB,i));
  }


  UNPROTECT(3);
  return winnerPop;

}





