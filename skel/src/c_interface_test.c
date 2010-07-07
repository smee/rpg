#include <stdio.h>
#include <R.h>
#include <Rinternals.h>

SEXP do_test(SEXP arg) {
  SEXP s_res;
  printf("Hello, World of C!");
  return R_NilValue;
}
