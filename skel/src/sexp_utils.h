/* sexp_utils.h
 *
 */

#ifndef SEXP_UTILS_H
#define SEXP_UTILS_H

#include <R.h>
#include <Rinternals.h>


/* deep_copy_closxp
 *
 */
SEXP deep_copy_closxp(SEXP sexp);

/* map_sexp
 *
 */
SEXP map_sexp(SEXP (*const f)(SEXP), SEXP sexp);

/* map_sexp_leafs
 *
 */
SEXP map_sexp_leafs(SEXP (*const f)(SEXP), SEXP sexp);

/* map_sexp_inner_nodes
 *
 */
SEXP map_sexp_inner_nodes(SEXP (*const f)(SEXP), SEXP sexp);

/* modify_sexp_shortcut
 *
 */
void modify_sexp_shortcut(void (*const f)(SEXP), SEXP sexp);

/* sexp_size
 *
 */
int sexp_size(SEXP sexp);
SEXP sexp_size_R(SEXP sexp);

// TODO

#endif
