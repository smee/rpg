#ifndef SELECTION_H
#define SELECTION_H

#include <R.h>
#include <Rinternals.h>


SEXP selection(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

void sortByRmse(int *, int *, int, SEXP);

#endif
