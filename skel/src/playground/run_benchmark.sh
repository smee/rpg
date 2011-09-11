#!/bin/bash

rm -f eval_vectorized.o eval_vectorized.so
gm4 evaluate_language_expression.m4 > evaluate_language_expression.h
R CMD SHLIB eval_vectorized.c
R CMD SHLIB eval_vectorized_orig.c

Rscript benchmark_eval.R
