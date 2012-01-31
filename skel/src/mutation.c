/* mutation.c
 *
 */

#include "mutation.h"
#include <Rmath.h>
#include "initialization.h"
#include "random_utils.h"

#define MAX_FUNCTION_SYMBOLS 64


SEXP random_function_symbol_of_arity(int arity, SEXP function_symbol_list, SEXP function_arities);

SEXP mutate_constants_normal(SEXP sexp, double p, double mu, double sigma) {
  SEXP c;
  switch (TYPEOF(sexp)) { // switch for speed
  case REALSXP:
    if (unif_rand() < p) { // mutate constant with probability p
      PROTECT(c = allocVector(REALSXP, 1));
      REAL(c)[0] = REAL(sexp)[0] + rnorm(mu, sigma);
      UNPROTECT(1);
      return c;
    } else {
      return sexp;
    }
  case NILSXP:
    return sexp; // do nothing with nils
  case LANGSXP: // fall-through to next case
  case LISTSXP:
    return LCONS(mutate_constants_normal(CAR(sexp), p, mu, sigma),
                 mutate_constants_normal(CDR(sexp), p, mu, sigma)); // do nothing with inner nodes, recurse
  default: // base case
    return sexp; // do nothing
  }
}

SEXP mutate_constants_normal_R(SEXP sexp, SEXP p, SEXP mu, SEXP sigma) {
  SEXP result;
  GetRNGstate();
  PROTECT(result = mutate_constants_normal(sexp, REAL(p)[0], REAL(mu)[0], REAL(sigma)[0]));
  PutRNGstate();
  UNPROTECT(1);
  return result;
}

SEXP mutate_subtrees(SEXP sexp,
                     double p, double p_insert_delete,
                     SEXP function_symbol_list,
                     SEXP function_arities,
                     SEXP input_variable_list,
                     double constant_min, double constant_max,
                     double p_subtree, double p_constant,
                     int depth_max) {
  switch (TYPEOF(sexp)) { // switch for speed
  case NILSXP:
    return sexp; // do nothing with nils
  case LANGSXP: // fall-through to next case
    if (unif_rand() < p) { // mutate inner node with probability p
      if (unif_rand() < p_insert_delete) { // replace with new subtree (insert)
        return initialize_expression_grow(function_symbol_list, function_arities,
                                          input_variable_list,
                                          constant_min, constant_max,
                                          p_subtree, p_constant,
                                          depth_max);
      } else { // replace with new leaf (delete)
        return initialize_expression_grow(function_symbol_list, function_arities,
                                          input_variable_list,
                                          constant_min, constant_max,
                                          p_subtree, p_constant,
                                          0);
      }
    } else {
      return LCONS(CAR(sexp),
                   mutate_subtrees(CDR(sexp), p, p_insert_delete,
                                   function_symbol_list, function_arities,
                                   input_variable_list,
                                   constant_min, constant_max,
                                   p_subtree, p_constant,
                                   depth_max)); // recurse on parameters
    }
  case LISTSXP:
    return LCONS(mutate_subtrees(CAR(sexp), p, p_insert_delete,
                                 function_symbol_list, function_arities,
                                 input_variable_list,
                                 constant_min, constant_max,
                                 p_subtree, p_constant,
                                 depth_max),
                 mutate_subtrees(CDR(sexp), p, p_insert_delete,
                                 function_symbol_list, function_arities,
                                 input_variable_list,
                                 constant_min, constant_max,
                                 p_subtree, p_constant,
                                 depth_max)); // recurse on parameters
  default: // base case
    if (unif_rand() < p) { // mutate leaf with probability p
      if (unif_rand() < p_insert_delete) { // replace with new subtree (insert)
        return initialize_expression_grow(function_symbol_list, function_arities,
                                          input_variable_list,
                                          constant_min, constant_max,
                                          p_subtree, p_constant,
                                          depth_max);
      } else { // replace with new leaf (delete)
        return initialize_expression_grow(function_symbol_list, function_arities,
                                          input_variable_list,
                                          constant_min, constant_max,
                                          p_subtree, p_constant,
                                          0);
      }
    } else {
      return sexp; // do nothing
    }
  }
}

SEXP mutate_subtrees_R(SEXP sexp,
                       SEXP p, SEXP p_insert_delete,
                       SEXP function_symbol_list,
                       SEXP function_arities,
                       SEXP input_variable_list,
                       SEXP constant_min, SEXP constant_max,
                       SEXP p_subtree, SEXP p_constant,
                       SEXP depth_max) {
  SEXP result;
  GetRNGstate();
  PROTECT(result = mutate_subtrees(sexp, REAL(p)[0], REAL(p_insert_delete)[0],
                                   function_symbol_list, function_arities,
                                   input_variable_list,
                                   REAL(constant_min)[0], REAL(constant_max)[0],
                                   REAL(p_subtree)[0], REAL(p_constant)[0],
                                   INTEGER(depth_max)[0]));
  PutRNGstate();
  UNPROTECT(1);
  return result;
}

SEXP mutate_functions(SEXP sexp, double p, SEXP function_symbol_list, SEXP function_arities) {
  int arity;
  switch (TYPEOF(sexp)) { // switch for speed
  case LANGSXP:
    if (unif_rand() < p) { // mutate function symbol with probability p
      arity = length(sexp) - 1;
      SEXP new_function_symbol = install(CHAR(STRING_ELT(random_function_symbol_of_arity(arity, function_symbol_list, function_arities), 0)));
      return LCONS(new_function_symbol,
                   mutate_functions(CDR(sexp), p, function_symbol_list, function_arities)); // recurse on parameters
    } else {
      return LCONS(CAR(sexp),
                   mutate_functions(CDR(sexp), p, function_symbol_list, function_arities)); // recurse on parameters
    }
  case LISTSXP:
    return LCONS(mutate_functions(CAR(sexp), p, function_symbol_list, function_arities),
                 mutate_functions(CDR(sexp), p, function_symbol_list, function_arities)); // do nothing with parameter lists, just recurse
  default: // base case
    return sexp; // do nothing
  }
}

SEXP mutate_functions_R(SEXP sexp, SEXP p, SEXP function_symbol_list, SEXP function_arities) {
  SEXP result;
  GetRNGstate();
  PROTECT(result = mutate_functions(sexp, REAL(p)[0], function_symbol_list, function_arities));
  PutRNGstate();
  UNPROTECT(1);
  return result;
}

SEXP random_function_symbol_of_arity(int arity, SEXP function_symbol_list, SEXP function_arities) {
  int matching_function_symbols[MAX_FUNCTION_SYMBOLS];
  int number_of_matching_function_symbols = 0;
  for (int i = 0; i < length(function_arities); i++) {
    if (arity == INTEGER(function_arities)[i]) {
      matching_function_symbols[number_of_matching_function_symbols] = i;
      number_of_matching_function_symbols++;
    }
  }
  if (number_of_matching_function_symbols == 0) {
    error("random_function_symbol_of_arity: no function symbol of arity %d", arity);
  }
  int selected_function_symbol = matching_function_symbols[random_index(number_of_matching_function_symbols)];
  return VECTOR_ELT(function_symbol_list, selected_function_symbol);
}

