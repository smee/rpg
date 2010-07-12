/* sexp_utils.h
 *
 */

#ifndef SEXP_UTILS_H
#define SEXP_UTILS_H

#include <R.h>
#include <Rinternals.h>


/* map_sexp
 *
 */
SEXP map_sexp(const SEXP, SEXP (*const)(SEXP));

#endif
