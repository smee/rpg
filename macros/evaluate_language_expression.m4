dnl -*- mode:m4 -*-
dnl
dnl ------------------------------------------------------------- utility macros
define(`forloop', `ifelse(eval(`($2) <= ($3)'), `1',
  `pushdef(`$1')_$0(`$1', eval(`$2'),
    eval(`$3'), `$4')popdef(`$1')')')dnl
define(`_forloop',
  `define(`$1', `$2')$4`'ifelse(`$2', `$3', `',
  `$0(`$1', incr(`$2'), `$3', `$4')')')dnl
dnl
dnl ---------------------------------------------------- language SEXP evaluator
/* !!! Automatically generated from macros/evaluate_language_expression.m4.
 * !!! Do not modify this generated code, as all changes will be overwritten.
 * !!! START OF GENERATED CODE
 */
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
define(`_DEFINITION', `$2')dnl
dnl double qoute to avoid a problem with "(" as a unary function symbol
    if (matches(``$1'', 1)) {
        SEXP s_arg = CADR(s_expr);
        if (isNumeric(s_arg)) {
            const double value = _DEFINITION(REAL(s_arg)[0]);
            for (int i = 0; i < samples; ++i)
                out_result[i] = value;
            if (context->keepIntermediateResults) {
                SEXP intermediateResult;
                PROTECT(intermediateResult = allocVector(REALSXP, 1));
                double *intermediateResultData = REAL(intermediateResult);
                intermediateResultData[0] = REAL(s_arg)[0];
                PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                //Rprintf("3.1) intermediate result: ");
                //for (int i = 0; i < samples; i++)
                //    Rprintf("%f ", REAL(s_arg)[0]);
                //Rprintf("\n");
                PROTECT(intermediateResult = allocVector(REALSXP, samples));
                intermediateResultData = REAL(intermediateResult);
                memcpy(intermediateResultData, out_result, samples * sizeof(double));
                PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                //Rprintf("3.2) intermediate result: ");
                //for (int i = 0; i < samples; i++)
                //    Rprintf("%f ", out_result[i]);
                //Rprintf("\n");
            }
        } else if (isSymbol(s_arg)) {
            const R_len_t index = function_argument_index(s_arg, context);
            const double *value = context->actualParameters + (index * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = _DEFINITION(value[i]);
            if (context->keepIntermediateResults) {
                SEXP intermediateResult;
                PROTECT(intermediateResult = allocVector(REALSXP, samples));
                double *intermediateResultData = REAL(intermediateResult);
                memcpy(intermediateResultData, value, samples * sizeof(double));
                PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                //Rprintf("4.1) intermediate result: ");
                //for (int i = 0; i < samples; i++)
                //    Rprintf("%f ", value[i]);
                //Rprintf("\n");
                PROTECT(intermediateResult = allocVector(REALSXP, samples));
                intermediateResultData = REAL(intermediateResult);
                memcpy(intermediateResultData, out_result, samples * sizeof(double));
                PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                //Rprintf("4.2) intermediate result: ");
                //for (int i = 0; i < samples; i++)
                //    Rprintf("%f ", out_result[i]);
                //Rprintf("\n");
            }
        } else if (isLanguage(s_arg)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = _DEFINITION(out_result[0]);
                *out_is_scalar_result = 1;
                if (context->keepIntermediateResults) {
                    SEXP intermediateResult;
                    PROTECT(intermediateResult = allocVector(REALSXP, 1));
                    double *intermediateResultData = REAL(intermediateResult);
                    intermediateResultData[0] = out_result[0];
                    PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                    //Rprintf("5) intermediate result: %f\n", out_result[0]);
                }
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = _DEFINITION(out_result[i]);
                if (context->keepIntermediateResults) {
                    SEXP intermediateResult;
                    PROTECT(intermediateResult = allocVector(REALSXP, samples));
                    double *intermediateResultData = REAL(intermediateResult);
                    memcpy(intermediateResultData, out_result, samples * sizeof(double));
                    PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                    //Rprintf("6) intermediate result: ");
                    //for (int i = 0; i < samples; i++)
                    //    Rprintf("%f ", out_result[i]);
                    //Rprintf("\n");
                }
            }
        } else {
            error("`unary_function'(\"_SYMBOL\"):  Unhandled argument type combination");
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
define(`_SYMBOL', `$1')dnl
define(`_DEFINITION', `$2')dnl
    if (matches(_SYMBOL, 2)) {
        SEXP s_arg1 = CADR(s_expr);
        SEXP s_arg2 = CADDR(s_expr);

        if (isNumeric(s_arg1) && isNumeric(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const double value2 = REAL(s_arg2)[0];
            const double value = _DEFINITION(value1, value2);
            out_result[0] = value;
            *out_is_scalar_result = 1;
            if (context->keepIntermediateResults) {
                SEXP intermediateResult;
                PROTECT(intermediateResult = allocVector(REALSXP, 1));
                double *intermediateResultData = REAL(intermediateResult);
                intermediateResultData[0] = value1;
                PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                //Rprintf("7.1) intermediate result: %f\n", value1);
                PROTECT(intermediateResult = allocVector(REALSXP, 1));
                intermediateResultData = REAL(intermediateResult);
                intermediateResultData[0] = value2;
                PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                //Rprintf("7.2) intermediate result: %f\n", value2);
                PROTECT(intermediateResult = allocVector(REALSXP, 1));
                intermediateResultData = REAL(intermediateResult);
                intermediateResultData[0] = out_result[0];
                PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                //Rprintf("7.3) intermediate result: %f\n", out_result[0]);
            }
        } else if (isNumeric(s_arg1) && isSymbol(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = _DEFINITION(value1, value2[i]);
            if (context->keepIntermediateResults) {
                SEXP intermediateResult;
                PROTECT(intermediateResult = allocVector(REALSXP, 1));
                double *intermediateResultData = REAL(intermediateResult);
                intermediateResultData[0] = value1;
                PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                //Rprintf("8.1) intermediate result: %f\n", value1);
                PROTECT(intermediateResult = allocVector(REALSXP, samples));
                intermediateResultData = REAL(intermediateResult);
                memcpy(intermediateResultData, value2, samples * sizeof(double));
                PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                //Rprintf("8.2) intermediate result: ");
                //for (int i = 0; i < samples; i++)
                //    Rprintf("%f ", value2[i]);
                //Rprintf("\n");
                PROTECT(intermediateResult = allocVector(REALSXP, samples));
                intermediateResultData = REAL(intermediateResult);
                memcpy(intermediateResultData, out_result, samples * sizeof(double));
                PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                //Rprintf("8.3) intermediate result: ");
                //for (int i = 0; i < samples; i++)
                //    Rprintf("%f ", out_result[i]);
                //Rprintf("\n");
            }
        } else if (isNumeric(s_arg1) && isLanguage(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            int is_scalar_result;
            evaluate_language_expression(s_arg2, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = _DEFINITION(value1, out_result[0]);
                *out_is_scalar_result = 1;
                if (context->keepIntermediateResults) {
                    SEXP intermediateResult;
                    PROTECT(intermediateResult = allocVector(REALSXP, 1));
                    double *intermediateResultData = REAL(intermediateResult);
                    intermediateResultData[0] = value1;
                    PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                    //Rprintf("9.1) intermediate result: %f\n", value1);
                    PROTECT(intermediateResult = allocVector(REALSXP, 1));
                    intermediateResultData = REAL(intermediateResult);
                    intermediateResultData[0] = out_result[0];
                    PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                    //Rprintf("9.2) intermediate result: %f\n", out_result[0]);
                }
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = _DEFINITION(value1, out_result[i]);
                if (context->keepIntermediateResults) {
                    SEXP intermediateResult;
                    PROTECT(intermediateResult = allocVector(REALSXP, 1));
                    double *intermediateResultData = REAL(intermediateResult);
                    intermediateResultData[0] = value1;
                    PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                    //Rprintf("10.1) intermediate result: %f\n", value1);
                    PROTECT(intermediateResult = allocVector(REALSXP, samples));
                    intermediateResultData = REAL(intermediateResult);
                    memcpy(intermediateResultData, out_result, samples * sizeof(double));
                    PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                    //Rprintf("10.2) intermediate result: ");
                    //for (int i = 0; i < samples; i++)
                    //    Rprintf("%f ", out_result[i]);
                    //Rprintf("\n");
                }
            }
        } else if (isSymbol(s_arg1) && isNumeric(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const double value2 = REAL(s_arg2)[0];
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = _DEFINITION(value1[i], value2);
            if (context->keepIntermediateResults) {
                SEXP intermediateResult;
                PROTECT(intermediateResult = allocVector(REALSXP, samples));
                double *intermediateResultData = REAL(intermediateResult);
                memcpy(intermediateResultData, value1, samples * sizeof(double));
                PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                //Rprintf("11.1) intermediate result: ");
                //for (int i = 0; i < samples; i++)
                //    Rprintf("%f ", value1[i]);
                //Rprintf("\n");
                PROTECT(intermediateResult = allocVector(REALSXP, 1));
                intermediateResultData = REAL(intermediateResult);
                intermediateResultData[0] = value2;
                PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                //Rprintf("11.2) intermediate result: %f\n", value2);
                PROTECT(intermediateResult = allocVector(REALSXP, samples));
                intermediateResultData = REAL(intermediateResult);
                memcpy(intermediateResultData, out_result, samples * sizeof(double));
                PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                //Rprintf("11.3) intermediate result: ");
                //for (int i = 0; i < samples; i++)
                //    Rprintf("%f ", out_result[i]);
                //Rprintf("\n");
            }
        } else if (isSymbol(s_arg1) && isSymbol(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
dnl Explicit unrolling here to facilitate automatic vectorization by modern C compilers:
            R_len_t i;
            const R_len_t n_unrolled = samples / 4;
            for (i = 0; i < n_unrolled; i += 4) {
                forloop(`j', 0, 3,`out_result[i + j] = _DEFINITION(value1[i + j], value2[i + j]);
')
            }
            for (; i < samples; ++i)
                out_result[i] = _DEFINITION(value1[i], value2[i]);
            if (context->keepIntermediateResults) {
                SEXP intermediateResult;
                PROTECT(intermediateResult = allocVector(REALSXP, samples));
                double *intermediateResultData = REAL(intermediateResult);
                memcpy(intermediateResultData, value1, samples * sizeof(double));
                PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                //Rprintf("12.1) intermediate result: ");
                //for (int i = 0; i < samples; i++)
                //    Rprintf("%f ", value1[i]);
                //Rprintf("\n");
                PROTECT(intermediateResult = allocVector(REALSXP, samples));
                intermediateResultData = REAL(intermediateResult);
                memcpy(intermediateResultData, value2, samples * sizeof(double));
                PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                //Rprintf("12.2) intermediate result: ");
                //for (int i = 0; i < samples; i++)
                //    Rprintf("%f ", value2[i]);
                //Rprintf("\n");
                PROTECT(intermediateResult = allocVector(REALSXP, samples));
                intermediateResultData = REAL(intermediateResult);
                memcpy(intermediateResultData, out_result, samples * sizeof(double));
                PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                //Rprintf("12.3) intermediate result: ");
                //for (int i = 0; i < samples; i++)
                //    Rprintf("%f ", out_result[i]);
                //Rprintf("\n");
            }
        } else if (isSymbol(s_arg1) && isLanguage(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            int is_scalar_result;
            evaluate_language_expression(s_arg2, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = _DEFINITION(value1[i], out_result[1]);
                if (context->keepIntermediateResults) {
                    SEXP intermediateResult;
                    PROTECT(intermediateResult = allocVector(REALSXP, samples));
                    double *intermediateResultData = REAL(intermediateResult);
                    memcpy(intermediateResultData, value1, samples * sizeof(double));
                    PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                    //Rprintf("13.1) intermediate result: ");
                    //for (int i = 0; i < samples; i++)
                    //    Rprintf("%f ", value1[i]);
                    //Rprintf("\n");
                    PROTECT(intermediateResult = allocVector(REALSXP, samples));
                    intermediateResultData = REAL(intermediateResult);
                    memcpy(intermediateResultData, out_result, samples * sizeof(double));
                    PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                    //Rprintf("13.2) intermediate result: ");
                    //for (int i = 0; i < samples; i++)
                    //    Rprintf("%f ", out_result[i]);
                    //Rprintf("\n");
                }
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = _DEFINITION(value1[i], out_result[i]);
                if (context->keepIntermediateResults) {
                    SEXP intermediateResult;
                    PROTECT(intermediateResult = allocVector(REALSXP, samples));
                    double *intermediateResultData = REAL(intermediateResult);
                    memcpy(intermediateResultData, value1, samples * sizeof(double));
                    PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                    //Rprintf("14.1) intermediate result: ");
                    //for (int i = 0; i < samples; i++)
                    //    Rprintf("%f ", value1[i]);
                    //Rprintf("\n");
                    PROTECT(intermediateResult = allocVector(REALSXP, samples));
                    intermediateResultData = REAL(intermediateResult);
                    memcpy(intermediateResultData, out_result, samples * sizeof(double));
                    PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                    //Rprintf("14.2) intermediate result: ");
                    //for (int i = 0; i < samples; i++)
                    //    Rprintf("%f ", out_result[i]);
                    //Rprintf("\n");
                }
            }
        } else if (isLanguage(s_arg1) && isNumeric(s_arg2)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg1, context, out_result, &is_scalar_result);
            const double value2 = REAL(s_arg2)[0];
            if (is_scalar_result) {
                out_result[0] = _DEFINITION(out_result[0], value2);
                *out_is_scalar_result = 1;
                if (context->keepIntermediateResults) {
                    SEXP intermediateResult;
                    PROTECT(intermediateResult = allocVector(REALSXP, 1));
                    double *intermediateResultData = REAL(intermediateResult);
                    intermediateResultData[0] = out_result[0];
                    PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                    //Rprintf("15.1) intermediate result: %f\n", out_result[0]);
                    PROTECT(intermediateResult = allocVector(REALSXP, 1));
                    intermediateResultData = REAL(intermediateResult);
                    intermediateResultData[0] = value2;
                    PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                    //Rprintf("15.2) intermediate result: %f\n", value2);
                }
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = _DEFINITION(out_result[i], value2);
                if (context->keepIntermediateResults) {
                    SEXP intermediateResult;
                    PROTECT(intermediateResult = allocVector(REALSXP, samples));
                    double *intermediateResultData = REAL(intermediateResult);
                    memcpy(intermediateResultData, out_result, samples * sizeof(double));
                    PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                    //Rprintf("16.1) intermediate result: ");
                    //for (int i = 0; i < samples; i++)
                    //    Rprintf("%f ", out_result[i]);
                    //Rprintf("\n");
                    PROTECT(intermediateResult = allocVector(REALSXP, 1));
                    intermediateResultData = REAL(intermediateResult);
                    intermediateResultData[0] = value2;
                    PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                    //Rprintf("16.2) intermediate result: %f\n", value2);
                }
            }
        } else if (isLanguage(s_arg1) && isSymbol(s_arg2)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg1, context, out_result, &is_scalar_result);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            if (is_scalar_result) {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = _DEFINITION(out_result[0], value2[i]);
                if (context->keepIntermediateResults) {
                    SEXP intermediateResult;
                    PROTECT(intermediateResult = allocVector(REALSXP, samples));
                    double *intermediateResultData = REAL(intermediateResult);
                    memcpy(intermediateResultData, out_result, samples * sizeof(double));
                    PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                    //Rprintf("17.1) intermediate result: ");
                    //for (int i = 0; i < samples; i++)
                    //    Rprintf("%f ", out_result[i]);
                    //Rprintf("\n");
                    PROTECT(intermediateResult = allocVector(REALSXP, samples));
                    intermediateResultData = REAL(intermediateResult);
                    memcpy(intermediateResultData, value2, samples * sizeof(double));
                    PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                    //Rprintf("17.2) intermediate result: ");
                    //for (int i = 0; i < samples; i++)
                    //    Rprintf("%f ", value2[i]);
                    //Rprintf("\n");
                }
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = _DEFINITION(out_result[i], value2[i]);
                if (context->keepIntermediateResults) {
                    SEXP intermediateResult;
                    PROTECT(intermediateResult = allocVector(REALSXP, samples));
                    double *intermediateResultData = REAL(intermediateResult);
                    memcpy(intermediateResultData, out_result, samples * sizeof(double));
                    PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                    //Rprintf("18.1) intermediate result: ");
                    //for (int i = 0; i < samples; i++)
                    //    Rprintf("%f ", out_result[i]);
                    //Rprintf("\n");
                    PROTECT(intermediateResult = allocVector(REALSXP, samples));
                    intermediateResultData = REAL(intermediateResult);
                    memcpy(intermediateResultData, value2, samples * sizeof(double));
                    PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                    //Rprintf("18.2) intermediate result: ");
                    //for (int i = 0; i < samples; i++)
                    //    Rprintf("%f ", value2[i]);
                    //Rprintf("\n");
                }
            }
        } else if (isLanguage(s_arg1) && isLanguage(s_arg2)) {
            /* NOTE: Do _not_ use stack allocation here because this
             * function is recursive and "samples" might be large
             * (>100000). This will overrun the (limited) C stack and 
             * crash R.
             */
            int is_scalar_result, is_scalar_tmp;
            evaluate_language_expression(s_arg1, context, out_result, &is_scalar_result);
            if (!is_scalar_result) {
                double *tmp = malloc(sizeof(double) * samples);
                evaluate_language_expression(s_arg2, context, tmp, &is_scalar_tmp);
                if (!is_scalar_tmp) {
                    for (int i = 0; i < samples; ++i)
                        out_result[i] = _DEFINITION(out_result[i], tmp[i]);
                    *out_is_scalar_result = 0;
                    if (context->keepIntermediateResults) {
                        SEXP intermediateResult;
                        PROTECT(intermediateResult = allocVector(REALSXP, samples));
                        double *intermediateResultData = REAL(intermediateResult);
                        memcpy(intermediateResultData, out_result, samples * sizeof(double));
                        PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                        //Rprintf("19) intermediate result: ");
                        //for (int i = 0; i < samples; i++)
                        //    Rprintf("%f ", out_result[i]);
                        //Rprintf("\n");
                    }
                } else {
                    const double value2 = tmp[0];
                    for (int i = 0; i < samples; ++i)
                        out_result[i] = _DEFINITION(out_result[i], value2);
                    *out_is_scalar_result = 0;
                    if (context->keepIntermediateResults) {
                        SEXP intermediateResult;
                        PROTECT(intermediateResult = allocVector(REALSXP, samples));
                        double *intermediateResultData = REAL(intermediateResult);
                        memcpy(intermediateResultData, out_result, samples * sizeof(double));
                        PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                        //Rprintf("20) intermediate result: ");
                        //for (int i = 0; i < samples; i++)
                        //    Rprintf("%f ", out_result[i]);
                        //Rprintf("\n");
                    }
                }
                free(tmp);
            } else {
                const double value1 = out_result[0];
                evaluate_language_expression(s_arg2, context, out_result, &is_scalar_tmp);
                if (!is_scalar_tmp) {
                    for (int i = 0; i < samples; ++i)
                        out_result[i] = _DEFINITION(value1, out_result[i]);
                    *out_is_scalar_result = 0;
                    if (context->keepIntermediateResults) {
                        SEXP intermediateResult;
                        PROTECT(intermediateResult = allocVector(REALSXP, samples));
                        double *intermediateResultData = REAL(intermediateResult);
                        memcpy(intermediateResultData, out_result, samples * sizeof(double));
                        PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                        //Rprintf("21) intermediate result: ");
                        //for (int i = 0; i < samples; i++)
                        //    Rprintf("%f ", out_result[i]);
                        //Rprintf("\n");
                    }
                } else {
                    out_result[0] = _DEFINITION(value1, out_result[0]);
                    *out_is_scalar_result = 1;
                    if (context->keepIntermediateResults) {
                        SEXP intermediateResult;
                        PROTECT(intermediateResult = allocVector(REALSXP, 1));
                        double *intermediateResultData = REAL(intermediateResult);
                        intermediateResultData[0] = out_result[0];
                        PROTECT(context->outIntermediateResults = CONS(intermediateResult, context->outIntermediateResults));
                        //Rprintf("22) intermediate result: %f\n", out_result[0]);
                    }
                }
            }
        } else {
            error("`binary_function'(\"_SYMBOL\"): Unhandled argument type combination");
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
    for (R_len_t i = 0; i < context->arity; ++i) {
        if (!strcmp(argument_name, CHAR(STRING_ELT(context->formalParameters, i)))) {
            return i;
        }
    }
    error("eval_vectorized_recursive: undefined symbol");
    return -1; /* Make compiler happy. */
}

static R_INLINE void evaluate_language_expression(SEXP s_expr,
                                                  struct EvalVectorizedContext *context,
                                                  double *out_result,
                                                  int *out_is_scalar_result) {
    const char *symbol = CHAR(PRINTNAME(CAR(s_expr)));
    const int symbol_len = strlen(symbol);
    const R_len_t arity = expression_arity(s_expr);
    const R_len_t samples = context->samples;

    *out_is_scalar_result = 0; // out_result is a vector by default

include(`macros/function_definitions.m4')dnl
    eval_vectorized_fallback(s_expr, context, out_result);
    return;
}
/* !!! END OF GENERATED CODE */
