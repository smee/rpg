dnl -*- mode:m4 -*-
dnl
define(`forloop', `ifelse(eval(`($2) <= ($3)'), `1',
  `pushdef(`$1')_$0(`$1', eval(`$2'),
    eval(`$3'), `$4')popdef(`$1')')')
define(`_forloop',
  `define(`$1', `$2')$4`'ifelse(`$2', `$3', `',
  `$0(`$1', incr(`$2'), `$3', `$4')')')
#include <R.h>
#include <math.h>
dnl
define(`matches',`dnl
ifelse(len($1), 1,dnl
(arity == $2) && (symbol_len == 1) && symbol[0] == "$1"[0],dnl
(arity == $2) && (symbol_len == len($1)) && !strncmp(symbol, "$1", len(`$1')))')dnl
dnl
dnl binary_function(_SYMBOL, _DEFINITION) - define a binary function
dnl
dnl Use $1 to access the argument in the _DEFINITION. _DEFINITION
dnl should be free of side-effects!
dnl
dnl Example:
dnl
dnl   unary_function(sin, `sin($1)')
dnl
dnl Implementation:
dnl
dnl The generated code checks if $1 is a constant and avoid repeated
dnl evaluation of _DEFINITION in that case. 
define(`unary_function',`dnl
define(`_SYMBOL', `$1')dnl
define(`_DEFINITION', $2)dnl
    if (matches(``$1'', 1)) {
        SEXP s_arg = CADR(s_expr);
        if (isNumeric(s_arg)) {
            const double value = _DEFINITION(REAL(s_arg)[0]);
            for (int i = 0; i < samples; i++) 
                result[i] = value;
        } else if (isSymbol(s_arg)) {
            const R_len_t index = function_argument_index(s_arg, context);
            const double *value = context->actualParameters + (index * samples);
            for (R_len_t i = 0; i < samples; ++i) 
                result[i] = _DEFINITION(value[i]);
        } else if (isLanguage(s_arg)) {
            evalVectorizedRecursive(s_arg, context, result);
            for (R_len_t i = 0; i < samples; ++i) 
                result[i] = _DEFINITION(result[i]);
        } else {
            error("`unary_function'(_SYMBOL):  Unhandled argument type combination");
        }
        return;
    }')dnl
dnl
dnl binary_function(_SYMBOL, _DEFINITION) - define a binary function
dnl
dnl Use $1 and $2 to access the two arguments of in
dnl _DEFINITION. _DEFINITION should be free of any side-effects!
dnl
dnl Example:
dnl
dnl   binary_function(+, `$1 + $2')
dnl
dnl Implementation:
dnl
dnl The generated code tries hard to avoid allocating any extra
dnl memory. It does this by inspecting the arguments for their types
dnl and special cases execution in case they are constants. This
dnl avoids allocating a vector of size "samples" and filling it with a
dnl constant. 
define(binary_function,`dnl
define(`_DEFINITION', $2)dnl
    if (matches(`$1', 2)) {
        SEXP s_arg1 = CADR(s_expr);
        SEXP s_arg2 = CADDR(s_expr);

        if (isNumeric(s_arg1) && isNumeric(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const double value2 = REAL(s_arg2)[0];
            const double value = _DEFINITION(value1, value2);
            for (R_len_t i = 0; i < samples; ++i) 
                result[i] = value;
        } else if (isNumeric(s_arg1) && isSymbol(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            for (R_len_t i = 0; i < samples; ++i) 
                result[i] = _DEFINITION(value1, value2[i]);
        } else if (isNumeric(s_arg1) && isLanguage(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            evalVectorizedRecursive(s_arg2, context, result);
            for (R_len_t i = 0; i < samples; ++i) 
                result[i] = _DEFINITION(value1, result[i]);
        } else if (isSymbol(s_arg1) && isNumeric(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const double value2 = REAL(s_arg2)[0];
            for (R_len_t i = 0; i < samples; ++i) 
                result[i] = _DEFINITION(value1[i], value2);
        } else if (isSymbol(s_arg1) && isSymbol(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
dnl Explicit unrolling here to facilitate vectorization:
            R_len_t i;
            const R_len_t n_unrolled = samples / 4;
            for (i = 0; i < n_unrolled; i += 4) {
                forloop(`j', 0, 3,`result[i + j] = _DEFINITION(value1[i + j], value2[i + j]);
')
            }
            for (; i < samples; ++i) 
                result[i] = _DEFINITION(value1[i], value2[i]);
        } else if (isSymbol(s_arg1) && isLanguage(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            evalVectorizedRecursive(s_arg2, context, result);

            for (R_len_t i = 0; i < samples; ++i) 
                result[i] = _DEFINITION(value1[i], result[i]);
        } else if (isLanguage(s_arg1) && isNumeric(s_arg2)) {
            evalVectorizedRecursive(s_arg1, context, result);
            const double value2 = REAL(s_arg2)[0];
            for (R_len_t i = 0; i < samples; ++i) 
                result[i] = _DEFINITION(result[i], value2);
        } else if (isLanguage(s_arg1) && isSymbol(s_arg2)) {
            evalVectorizedRecursive(s_arg1, context, result);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            for (R_len_t i = 0; i < samples; ++i) 
                result[i] = _DEFINITION(result[i], value2[i]);
        } else if (isLanguage(s_arg1) && isLanguage(s_arg2)) {
            /* NOTE: Do _not_ use stack allocation here because this
             * function is recursive and "samples" might be large
             * (>100000). This will overrun the (limited) C stack and 
             * crash R.
             */
            double *tmp = malloc(sizeof(double) * samples);
            evalVectorizedRecursive(s_arg1, context, result);
            evalVectorizedRecursive(s_arg2, context, tmp);
            for (int i = 0; i < samples; i++) 
                result[i] = _DEFINITION(result[i], tmp[i]);
            free(tmp);
        } else {
            error("`binary_function'($1): Unhandled argument type combination");
        }
        return;
    }')dnl

static R_INLINE R_len_t expression_arity(SEXP s_expr) {
    R_len_t arity = 0;
    while (!isNull(CDR(s_expr))) {
        s_expr = CDR(s_expr);
        ++arity;
    }
    return arity;
}

static R_INLINE R_len_t function_argument_index(SEXP s_arg,
                                                struct EvalVectorizedContext *context) {
    const char *argument_name = CHAR(PRINTNAME(s_arg));
    for (R_len_t i = 0; i < context->arity; i++) {
        if (!strcmp(argument_name, CHAR(STRING_ELT(context->formalParameters, i)))) {
            return i;
        }
    }
    error("evalVectorizedRecursive: undefined symbol");
    return -1; /* Make compiler happy. */
}

static R_INLINE void evaluate_language_expression(SEXP s_expr,
                                                  struct EvalVectorizedContext *context,
                                                  double *result) {
    const char *symbol = CHAR(PRINTNAME(CAR(s_expr)));
    const int symbol_len = strlen(symbol);
    const R_len_t arity = expression_arity(s_expr);
    const R_len_t samples = context->samples;
    
include(`function_definitions.m4')dnl
    evalVectorizedFallback(s_expr, context, result);
    return;
}
