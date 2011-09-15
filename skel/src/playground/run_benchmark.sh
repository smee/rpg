#!/bin/bash

## Revision of evaluator to which we wish to compare:
BASE_REVISION=7fbb1174760d7a2824be9bc04df090895be90d78

rm -f eval_vectorized.o eval_vectorized.so
m4 evaluate_language_expression.m4 > evaluate_language_expression.h

git show $BASE_REVISION:skel/src/playground/eval_vectorized.c > eval_vectorized_orig.c

R CMD SHLIB eval_vectorized.c
R CMD SHLIB eval_vectorized_orig.c

Rscript benchmark_eval.R
