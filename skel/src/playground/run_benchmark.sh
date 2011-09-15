#!/bin/bash

## Revision of evaluator to which we wish to compare (only uncomment one):
BASE_REVISION=7fbb1174760d7a2824be9bc04df090895be90d78 # vectorized evaluator without optimizations

rm -f eval_vectorized.o eval_vectorized.so
cd ../../..; make m4; cd skel/src/playground

git show $BASE_REVISION:skel/src/playground/eval_vectorized.c > eval_vectorized_orig.c

R CMD SHLIB eval_vectorized.c
R CMD SHLIB eval_vectorized_orig.c

echo "Benchmarking eval_vectorized.so. This may take several minutes..."
Rscript benchmark_eval.R
echo "...Benchmark DONE."
