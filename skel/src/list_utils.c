/* list_utils.c
 *
 */

#include "list_utils.h"


SEXP map_list(SEXP (*const f)(SEXP), const SEXP list) {
  return R_NilValue; // TODO
}

SEXP make_alist() {
  return allocList(0); // TODO is this equal to R_NilValue???
}

SEXP add_alist(const SEXP key, const SEXP value, const SEXP alist) {
  const SEXP augmented_alist = CONS(value, alist);
  SET_TAG(augmented_alist, key);
  return augmented_alist;
}

SEXP get_alist(const SEXP key, const SEXP alist) {
  for (SEXP rest = alist; rest != R_NilValue; rest = CDR(rest)) {
    if (R_compute_identical(key, TAG(rest), TRUE, TRUE, TRUE)) return CAR(rest);
  }
  error("get_alist: No such key.");
  return R_NilValue;
}

Rboolean contains_alist(const SEXP key, const SEXP alist) {
  for (SEXP rest = alist; rest != R_NilValue; rest = CDR(rest)) {
    if (R_compute_identical(key, TAG(rest), TRUE, TRUE, TRUE)) return TRUE;
  }
  return FALSE;
}
