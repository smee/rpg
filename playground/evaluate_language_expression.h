/* !!! Automatically generated from codegen/evaluate_language_expression.m4.
 * !!! Do not modify this generated code, as all changes will be overwritten.
 * !!! START OF GENERATED CODE
 */

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
    error("evalVectorizedRecursive: undefined symbol");
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
    
    if ((arity == 2) && (symbol_len == 1) && symbol[0] == "+"[0]) {
        SEXP s_arg1 = CADR(s_expr);
        SEXP s_arg2 = CADDR(s_expr);

        if (isNumeric(s_arg1) && isNumeric(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const double value2 = REAL(s_arg2)[0];
            const double value = value1 + value2;
            out_result[0] = value;
            *out_is_scalar_result = 1;
        } else if (isNumeric(s_arg1) && isSymbol(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = value1 + value2[i];
        } else if (isNumeric(s_arg1) && isLanguage(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            int is_scalar_result;
            evaluate_language_expression(s_arg2, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = value1 + out_result[0];
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = value1 + out_result[i];
            }
        } else if (isSymbol(s_arg1) && isNumeric(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const double value2 = REAL(s_arg2)[0];
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = value1[i] + value2;
        } else if (isSymbol(s_arg1) && isSymbol(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            R_len_t i;
            const R_len_t n_unrolled = samples / 4;
            for (i = 0; i < n_unrolled; i += 4) {
                out_result[i + 0] = value1[i + 0] + value2[i + 0];
out_result[i + 1] = value1[i + 1] + value2[i + 1];
out_result[i + 2] = value1[i + 2] + value2[i + 2];
out_result[i + 3] = value1[i + 3] + value2[i + 3];

            }
            for (; i < samples; ++i)
                out_result[i] = value1[i] + value2[i];
        } else if (isSymbol(s_arg1) && isLanguage(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            int is_scalar_result;
            evaluate_language_expression(s_arg2, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = value1[i] + out_result[1];
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = value1[i] + out_result[i];
            }
        } else if (isLanguage(s_arg1) && isNumeric(s_arg2)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg1, context, out_result, &is_scalar_result);
            const double value2 = REAL(s_arg2)[0];
            if (is_scalar_result) {
                out_result[0] = out_result[0] + value2;
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = out_result[i] + value2;
            }
        } else if (isLanguage(s_arg1) && isSymbol(s_arg2)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg1, context, out_result, &is_scalar_result);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            if (is_scalar_result) {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = out_result[0] + value2[i];
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = out_result[i] + value2[i];
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
                        out_result[i] = out_result[i] + tmp[i];
                    *out_is_scalar_result = 0;
                } else {
                    const double value2 = tmp[0];
                    for (int i = 0; i < samples; ++i)
                        out_result[i] = out_result[i] + value2;
                    *out_is_scalar_result = 0;
                }
                free(tmp);
            } else {
                const double value1 = out_result[0];
                evaluate_language_expression(s_arg2, context, out_result, &is_scalar_tmp);
                if (!is_scalar_tmp) {
                    for (int i = 0; i < samples; ++i)
                        out_result[i] = value1 + out_result[i];
                    *out_is_scalar_result = 0;
                } else {
                    out_result[0] = value1 + out_result[0];
                    *out_is_scalar_result = 1;
                }
            }
        } else {
            error("binary_function(\"+\"): Unhandled argument type combination");
        }
        return;
    }
    if ((arity == 2) && (symbol_len == 1) && symbol[0] == "-"[0]) {
        SEXP s_arg1 = CADR(s_expr);
        SEXP s_arg2 = CADDR(s_expr);

        if (isNumeric(s_arg1) && isNumeric(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const double value2 = REAL(s_arg2)[0];
            const double value = value1 - value2;
            out_result[0] = value;
            *out_is_scalar_result = 1;
        } else if (isNumeric(s_arg1) && isSymbol(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = value1 - value2[i];
        } else if (isNumeric(s_arg1) && isLanguage(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            int is_scalar_result;
            evaluate_language_expression(s_arg2, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = value1 - out_result[0];
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = value1 - out_result[i];
            }
        } else if (isSymbol(s_arg1) && isNumeric(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const double value2 = REAL(s_arg2)[0];
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = value1[i] - value2;
        } else if (isSymbol(s_arg1) && isSymbol(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            R_len_t i;
            const R_len_t n_unrolled = samples / 4;
            for (i = 0; i < n_unrolled; i += 4) {
                out_result[i + 0] = value1[i + 0] - value2[i + 0];
out_result[i + 1] = value1[i + 1] - value2[i + 1];
out_result[i + 2] = value1[i + 2] - value2[i + 2];
out_result[i + 3] = value1[i + 3] - value2[i + 3];

            }
            for (; i < samples; ++i)
                out_result[i] = value1[i] - value2[i];
        } else if (isSymbol(s_arg1) && isLanguage(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            int is_scalar_result;
            evaluate_language_expression(s_arg2, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = value1[i] - out_result[1];
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = value1[i] - out_result[i];
            }
        } else if (isLanguage(s_arg1) && isNumeric(s_arg2)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg1, context, out_result, &is_scalar_result);
            const double value2 = REAL(s_arg2)[0];
            if (is_scalar_result) {
                out_result[0] = out_result[0] - value2;
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = out_result[i] - value2;
            }
        } else if (isLanguage(s_arg1) && isSymbol(s_arg2)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg1, context, out_result, &is_scalar_result);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            if (is_scalar_result) {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = out_result[0] - value2[i];
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = out_result[i] - value2[i];
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
                        out_result[i] = out_result[i] - tmp[i];
                    *out_is_scalar_result = 0;
                } else {
                    const double value2 = tmp[0];
                    for (int i = 0; i < samples; ++i)
                        out_result[i] = out_result[i] - value2;
                    *out_is_scalar_result = 0;
                }
                free(tmp);
            } else {
                const double value1 = out_result[0];
                evaluate_language_expression(s_arg2, context, out_result, &is_scalar_tmp);
                if (!is_scalar_tmp) {
                    for (int i = 0; i < samples; ++i)
                        out_result[i] = value1 - out_result[i];
                    *out_is_scalar_result = 0;
                } else {
                    out_result[0] = value1 - out_result[0];
                    *out_is_scalar_result = 1;
                }
            }
        } else {
            error("binary_function(\"-\"): Unhandled argument type combination");
        }
        return;
    }
    if ((arity == 2) && (symbol_len == 1) && symbol[0] == "*"[0]) {
        SEXP s_arg1 = CADR(s_expr);
        SEXP s_arg2 = CADDR(s_expr);

        if (isNumeric(s_arg1) && isNumeric(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const double value2 = REAL(s_arg2)[0];
            const double value = value1 * value2;
            out_result[0] = value;
            *out_is_scalar_result = 1;
        } else if (isNumeric(s_arg1) && isSymbol(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = value1 * value2[i];
        } else if (isNumeric(s_arg1) && isLanguage(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            int is_scalar_result;
            evaluate_language_expression(s_arg2, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = value1 * out_result[0];
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = value1 * out_result[i];
            }
        } else if (isSymbol(s_arg1) && isNumeric(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const double value2 = REAL(s_arg2)[0];
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = value1[i] * value2;
        } else if (isSymbol(s_arg1) && isSymbol(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            R_len_t i;
            const R_len_t n_unrolled = samples / 4;
            for (i = 0; i < n_unrolled; i += 4) {
                out_result[i + 0] = value1[i + 0] * value2[i + 0];
out_result[i + 1] = value1[i + 1] * value2[i + 1];
out_result[i + 2] = value1[i + 2] * value2[i + 2];
out_result[i + 3] = value1[i + 3] * value2[i + 3];

            }
            for (; i < samples; ++i)
                out_result[i] = value1[i] * value2[i];
        } else if (isSymbol(s_arg1) && isLanguage(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            int is_scalar_result;
            evaluate_language_expression(s_arg2, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = value1[i] * out_result[1];
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = value1[i] * out_result[i];
            }
        } else if (isLanguage(s_arg1) && isNumeric(s_arg2)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg1, context, out_result, &is_scalar_result);
            const double value2 = REAL(s_arg2)[0];
            if (is_scalar_result) {
                out_result[0] = out_result[0] * value2;
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = out_result[i] * value2;
            }
        } else if (isLanguage(s_arg1) && isSymbol(s_arg2)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg1, context, out_result, &is_scalar_result);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            if (is_scalar_result) {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = out_result[0] * value2[i];
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = out_result[i] * value2[i];
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
                        out_result[i] = out_result[i] * tmp[i];
                    *out_is_scalar_result = 0;
                } else {
                    const double value2 = tmp[0];
                    for (int i = 0; i < samples; ++i)
                        out_result[i] = out_result[i] * value2;
                    *out_is_scalar_result = 0;
                }
                free(tmp);
            } else {
                const double value1 = out_result[0];
                evaluate_language_expression(s_arg2, context, out_result, &is_scalar_tmp);
                if (!is_scalar_tmp) {
                    for (int i = 0; i < samples; ++i)
                        out_result[i] = value1 * out_result[i];
                    *out_is_scalar_result = 0;
                } else {
                    out_result[0] = value1 * out_result[0];
                    *out_is_scalar_result = 1;
                }
            }
        } else {
            error("binary_function(\"*\"): Unhandled argument type combination");
        }
        return;
    }
    if ((arity == 2) && (symbol_len == 1) && symbol[0] == "/"[0]) {
        SEXP s_arg1 = CADR(s_expr);
        SEXP s_arg2 = CADDR(s_expr);

        if (isNumeric(s_arg1) && isNumeric(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const double value2 = REAL(s_arg2)[0];
            const double value = (value2 != 0.0) ? (value1 / value2) : ((value1 < 0.0) ? R_NegInf : R_PosInf);
            out_result[0] = value;
            *out_is_scalar_result = 1;
        } else if (isNumeric(s_arg1) && isSymbol(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = (value2[i] != 0.0) ? (value1 / value2[i]) : ((value1 < 0.0) ? R_NegInf : R_PosInf);
        } else if (isNumeric(s_arg1) && isLanguage(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            int is_scalar_result;
            evaluate_language_expression(s_arg2, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = (out_result[0] != 0.0) ? (value1 / out_result[0]) : ((value1 < 0.0) ? R_NegInf : R_PosInf);
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (out_result[i] != 0.0) ? (value1 / out_result[i]) : ((value1 < 0.0) ? R_NegInf : R_PosInf);
            }
        } else if (isSymbol(s_arg1) && isNumeric(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const double value2 = REAL(s_arg2)[0];
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = (value2 != 0.0) ? (value1[i] / value2) : ((value1[i] < 0.0) ? R_NegInf : R_PosInf);
        } else if (isSymbol(s_arg1) && isSymbol(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            R_len_t i;
            const R_len_t n_unrolled = samples / 4;
            for (i = 0; i < n_unrolled; i += 4) {
                out_result[i + 0] = (value2[i + 0] != 0.0) ? (value1[i + 0] / value2[i + 0]) : ((value1[i + 0] < 0.0) ? R_NegInf : R_PosInf);
out_result[i + 1] = (value2[i + 1] != 0.0) ? (value1[i + 1] / value2[i + 1]) : ((value1[i + 1] < 0.0) ? R_NegInf : R_PosInf);
out_result[i + 2] = (value2[i + 2] != 0.0) ? (value1[i + 2] / value2[i + 2]) : ((value1[i + 2] < 0.0) ? R_NegInf : R_PosInf);
out_result[i + 3] = (value2[i + 3] != 0.0) ? (value1[i + 3] / value2[i + 3]) : ((value1[i + 3] < 0.0) ? R_NegInf : R_PosInf);

            }
            for (; i < samples; ++i)
                out_result[i] = (value2[i] != 0.0) ? (value1[i] / value2[i]) : ((value1[i] < 0.0) ? R_NegInf : R_PosInf);
        } else if (isSymbol(s_arg1) && isLanguage(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            int is_scalar_result;
            evaluate_language_expression(s_arg2, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (out_result[1] != 0.0) ? (value1[i] / out_result[1]) : ((value1[i] < 0.0) ? R_NegInf : R_PosInf);
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (out_result[i] != 0.0) ? (value1[i] / out_result[i]) : ((value1[i] < 0.0) ? R_NegInf : R_PosInf);
            }
        } else if (isLanguage(s_arg1) && isNumeric(s_arg2)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg1, context, out_result, &is_scalar_result);
            const double value2 = REAL(s_arg2)[0];
            if (is_scalar_result) {
                out_result[0] = (value2 != 0.0) ? (out_result[0] / value2) : ((out_result[0] < 0.0) ? R_NegInf : R_PosInf);
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (value2 != 0.0) ? (out_result[i] / value2) : ((out_result[i] < 0.0) ? R_NegInf : R_PosInf);
            }
        } else if (isLanguage(s_arg1) && isSymbol(s_arg2)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg1, context, out_result, &is_scalar_result);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            if (is_scalar_result) {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (value2[i] != 0.0) ? (out_result[0] / value2[i]) : ((out_result[0] < 0.0) ? R_NegInf : R_PosInf);
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (value2[i] != 0.0) ? (out_result[i] / value2[i]) : ((out_result[i] < 0.0) ? R_NegInf : R_PosInf);
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
                        out_result[i] = (tmp[i] != 0.0) ? (out_result[i] / tmp[i]) : ((out_result[i] < 0.0) ? R_NegInf : R_PosInf);
                    *out_is_scalar_result = 0;
                } else {
                    const double value2 = tmp[0];
                    for (int i = 0; i < samples; ++i)
                        out_result[i] = (value2 != 0.0) ? (out_result[i] / value2) : ((out_result[i] < 0.0) ? R_NegInf : R_PosInf);
                    *out_is_scalar_result = 0;
                }
                free(tmp);
            } else {
                const double value1 = out_result[0];
                evaluate_language_expression(s_arg2, context, out_result, &is_scalar_tmp);
                if (!is_scalar_tmp) {
                    for (int i = 0; i < samples; ++i)
                        out_result[i] = (out_result[i] != 0.0) ? (value1 / out_result[i]) : ((value1 < 0.0) ? R_NegInf : R_PosInf);
                    *out_is_scalar_result = 0;
                } else {
                    out_result[0] = (out_result[0] != 0.0) ? (value1 / out_result[0]) : ((value1 < 0.0) ? R_NegInf : R_PosInf);
                    *out_is_scalar_result = 1;
                }
            }
        } else {
            error("binary_function(\"/\"): Unhandled argument type combination");
        }
        return;
    }
    if ((arity == 2) && (symbol_len == 1) && symbol[0] == "^"[0]) {
        SEXP s_arg1 = CADR(s_expr);
        SEXP s_arg2 = CADDR(s_expr);

        if (isNumeric(s_arg1) && isNumeric(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const double value2 = REAL(s_arg2)[0];
            const double value = (value2 == 2.0) ? value1 * value1 : pow(value1, value2);
            out_result[0] = value;
            *out_is_scalar_result = 1;
        } else if (isNumeric(s_arg1) && isSymbol(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = (value2[i] == 2.0) ? value1 * value1 : pow(value1, value2[i]);
        } else if (isNumeric(s_arg1) && isLanguage(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            int is_scalar_result;
            evaluate_language_expression(s_arg2, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = (out_result[0] == 2.0) ? value1 * value1 : pow(value1, out_result[0]);
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (out_result[i] == 2.0) ? value1 * value1 : pow(value1, out_result[i]);
            }
        } else if (isSymbol(s_arg1) && isNumeric(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const double value2 = REAL(s_arg2)[0];
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = (value2 == 2.0) ? value1[i] * value1[i] : pow(value1[i], value2);
        } else if (isSymbol(s_arg1) && isSymbol(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            R_len_t i;
            const R_len_t n_unrolled = samples / 4;
            for (i = 0; i < n_unrolled; i += 4) {
                out_result[i + 0] = (value2[i + 0] == 2.0) ? value1[i + 0] * value1[i + 0] : pow(value1[i + 0], value2[i + 0]);
out_result[i + 1] = (value2[i + 1] == 2.0) ? value1[i + 1] * value1[i + 1] : pow(value1[i + 1], value2[i + 1]);
out_result[i + 2] = (value2[i + 2] == 2.0) ? value1[i + 2] * value1[i + 2] : pow(value1[i + 2], value2[i + 2]);
out_result[i + 3] = (value2[i + 3] == 2.0) ? value1[i + 3] * value1[i + 3] : pow(value1[i + 3], value2[i + 3]);

            }
            for (; i < samples; ++i)
                out_result[i] = (value2[i] == 2.0) ? value1[i] * value1[i] : pow(value1[i], value2[i]);
        } else if (isSymbol(s_arg1) && isLanguage(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            int is_scalar_result;
            evaluate_language_expression(s_arg2, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (out_result[1] == 2.0) ? value1[i] * value1[i] : pow(value1[i], out_result[1]);
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (out_result[i] == 2.0) ? value1[i] * value1[i] : pow(value1[i], out_result[i]);
            }
        } else if (isLanguage(s_arg1) && isNumeric(s_arg2)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg1, context, out_result, &is_scalar_result);
            const double value2 = REAL(s_arg2)[0];
            if (is_scalar_result) {
                out_result[0] = (value2 == 2.0) ? out_result[0] * out_result[0] : pow(out_result[0], value2);
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (value2 == 2.0) ? out_result[i] * out_result[i] : pow(out_result[i], value2);
            }
        } else if (isLanguage(s_arg1) && isSymbol(s_arg2)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg1, context, out_result, &is_scalar_result);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            if (is_scalar_result) {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (value2[i] == 2.0) ? out_result[0] * out_result[0] : pow(out_result[0], value2[i]);
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (value2[i] == 2.0) ? out_result[i] * out_result[i] : pow(out_result[i], value2[i]);
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
                        out_result[i] = (tmp[i] == 2.0) ? out_result[i] * out_result[i] : pow(out_result[i], tmp[i]);
                    *out_is_scalar_result = 0;
                } else {
                    const double value2 = tmp[0];
                    for (int i = 0; i < samples; ++i)
                        out_result[i] = (value2 == 2.0) ? out_result[i] * out_result[i] : pow(out_result[i], value2);
                    *out_is_scalar_result = 0;
                }
                free(tmp);
            } else {
                const double value1 = out_result[0];
                evaluate_language_expression(s_arg2, context, out_result, &is_scalar_tmp);
                if (!is_scalar_tmp) {
                    for (int i = 0; i < samples; ++i)
                        out_result[i] = (out_result[i] == 2.0) ? value1 * value1 : pow(value1, out_result[i]);
                    *out_is_scalar_result = 0;
                } else {
                    out_result[0] = (out_result[0] == 2.0) ? value1 * value1 : pow(value1, out_result[0]);
                    *out_is_scalar_result = 1;
                }
            }
        } else {
            error("binary_function(\"^\"): Unhandled argument type combination");
        }
        return;
    }
    if ((arity == 2) && (symbol_len == 2) && !strncmp(symbol, "**", 2)) {
        SEXP s_arg1 = CADR(s_expr);
        SEXP s_arg2 = CADDR(s_expr);

        if (isNumeric(s_arg1) && isNumeric(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const double value2 = REAL(s_arg2)[0];
            const double value = (value2 == 2.0) ? value1 * value1 : pow(value1, value2);
            out_result[0] = value;
            *out_is_scalar_result = 1;
        } else if (isNumeric(s_arg1) && isSymbol(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = (value2[i] == 2.0) ? value1 * value1 : pow(value1, value2[i]);
        } else if (isNumeric(s_arg1) && isLanguage(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            int is_scalar_result;
            evaluate_language_expression(s_arg2, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = (out_result[0] == 2.0) ? value1 * value1 : pow(value1, out_result[0]);
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (out_result[i] == 2.0) ? value1 * value1 : pow(value1, out_result[i]);
            }
        } else if (isSymbol(s_arg1) && isNumeric(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const double value2 = REAL(s_arg2)[0];
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = (value2 == 2.0) ? value1[i] * value1[i] : pow(value1[i], value2);
        } else if (isSymbol(s_arg1) && isSymbol(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            R_len_t i;
            const R_len_t n_unrolled = samples / 4;
            for (i = 0; i < n_unrolled; i += 4) {
                out_result[i + 0] = (value2[i + 0] == 2.0) ? value1[i + 0] * value1[i + 0] : pow(value1[i + 0], value2[i + 0]);
out_result[i + 1] = (value2[i + 1] == 2.0) ? value1[i + 1] * value1[i + 1] : pow(value1[i + 1], value2[i + 1]);
out_result[i + 2] = (value2[i + 2] == 2.0) ? value1[i + 2] * value1[i + 2] : pow(value1[i + 2], value2[i + 2]);
out_result[i + 3] = (value2[i + 3] == 2.0) ? value1[i + 3] * value1[i + 3] : pow(value1[i + 3], value2[i + 3]);

            }
            for (; i < samples; ++i)
                out_result[i] = (value2[i] == 2.0) ? value1[i] * value1[i] : pow(value1[i], value2[i]);
        } else if (isSymbol(s_arg1) && isLanguage(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            int is_scalar_result;
            evaluate_language_expression(s_arg2, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (out_result[1] == 2.0) ? value1[i] * value1[i] : pow(value1[i], out_result[1]);
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (out_result[i] == 2.0) ? value1[i] * value1[i] : pow(value1[i], out_result[i]);
            }
        } else if (isLanguage(s_arg1) && isNumeric(s_arg2)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg1, context, out_result, &is_scalar_result);
            const double value2 = REAL(s_arg2)[0];
            if (is_scalar_result) {
                out_result[0] = (value2 == 2.0) ? out_result[0] * out_result[0] : pow(out_result[0], value2);
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (value2 == 2.0) ? out_result[i] * out_result[i] : pow(out_result[i], value2);
            }
        } else if (isLanguage(s_arg1) && isSymbol(s_arg2)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg1, context, out_result, &is_scalar_result);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            if (is_scalar_result) {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (value2[i] == 2.0) ? out_result[0] * out_result[0] : pow(out_result[0], value2[i]);
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (value2[i] == 2.0) ? out_result[i] * out_result[i] : pow(out_result[i], value2[i]);
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
                        out_result[i] = (tmp[i] == 2.0) ? out_result[i] * out_result[i] : pow(out_result[i], tmp[i]);
                    *out_is_scalar_result = 0;
                } else {
                    const double value2 = tmp[0];
                    for (int i = 0; i < samples; ++i)
                        out_result[i] = (value2 == 2.0) ? out_result[i] * out_result[i] : pow(out_result[i], value2);
                    *out_is_scalar_result = 0;
                }
                free(tmp);
            } else {
                const double value1 = out_result[0];
                evaluate_language_expression(s_arg2, context, out_result, &is_scalar_tmp);
                if (!is_scalar_tmp) {
                    for (int i = 0; i < samples; ++i)
                        out_result[i] = (out_result[i] == 2.0) ? value1 * value1 : pow(value1, out_result[i]);
                    *out_is_scalar_result = 0;
                } else {
                    out_result[0] = (out_result[0] == 2.0) ? value1 * value1 : pow(value1, out_result[0]);
                    *out_is_scalar_result = 1;
                }
            }
        } else {
            error("binary_function(\"**\"): Unhandled argument type combination");
        }
        return;
    }
    if ((arity == 2) && (symbol_len == 3) && !strncmp(symbol, "min", 3)) {
        SEXP s_arg1 = CADR(s_expr);
        SEXP s_arg2 = CADDR(s_expr);

        if (isNumeric(s_arg1) && isNumeric(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const double value2 = REAL(s_arg2)[0];
            const double value = (value1 < value2) ? value1 : value2;
            out_result[0] = value;
            *out_is_scalar_result = 1;
        } else if (isNumeric(s_arg1) && isSymbol(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = (value1 < value2[i]) ? value1 : value2[i];
        } else if (isNumeric(s_arg1) && isLanguage(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            int is_scalar_result;
            evaluate_language_expression(s_arg2, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = (value1 < out_result[0]) ? value1 : out_result[0];
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (value1 < out_result[i]) ? value1 : out_result[i];
            }
        } else if (isSymbol(s_arg1) && isNumeric(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const double value2 = REAL(s_arg2)[0];
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = (value1[i] < value2) ? value1[i] : value2;
        } else if (isSymbol(s_arg1) && isSymbol(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            R_len_t i;
            const R_len_t n_unrolled = samples / 4;
            for (i = 0; i < n_unrolled; i += 4) {
                out_result[i + 0] = (value1[i + 0] < value2[i + 0]) ? value1[i + 0] : value2[i + 0];
out_result[i + 1] = (value1[i + 1] < value2[i + 1]) ? value1[i + 1] : value2[i + 1];
out_result[i + 2] = (value1[i + 2] < value2[i + 2]) ? value1[i + 2] : value2[i + 2];
out_result[i + 3] = (value1[i + 3] < value2[i + 3]) ? value1[i + 3] : value2[i + 3];

            }
            for (; i < samples; ++i)
                out_result[i] = (value1[i] < value2[i]) ? value1[i] : value2[i];
        } else if (isSymbol(s_arg1) && isLanguage(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            int is_scalar_result;
            evaluate_language_expression(s_arg2, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (value1[i] < out_result[1]) ? value1[i] : out_result[1];
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (value1[i] < out_result[i]) ? value1[i] : out_result[i];
            }
        } else if (isLanguage(s_arg1) && isNumeric(s_arg2)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg1, context, out_result, &is_scalar_result);
            const double value2 = REAL(s_arg2)[0];
            if (is_scalar_result) {
                out_result[0] = (out_result[0] < value2) ? out_result[0] : value2;
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (out_result[i] < value2) ? out_result[i] : value2;
            }
        } else if (isLanguage(s_arg1) && isSymbol(s_arg2)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg1, context, out_result, &is_scalar_result);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            if (is_scalar_result) {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (out_result[0] < value2[i]) ? out_result[0] : value2[i];
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (out_result[i] < value2[i]) ? out_result[i] : value2[i];
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
                        out_result[i] = (out_result[i] < tmp[i]) ? out_result[i] : tmp[i];
                    *out_is_scalar_result = 0;
                } else {
                    const double value2 = tmp[0];
                    for (int i = 0; i < samples; ++i)
                        out_result[i] = (out_result[i] < value2) ? out_result[i] : value2;
                    *out_is_scalar_result = 0;
                }
                free(tmp);
            } else {
                const double value1 = out_result[0];
                evaluate_language_expression(s_arg2, context, out_result, &is_scalar_tmp);
                if (!is_scalar_tmp) {
                    for (int i = 0; i < samples; ++i)
                        out_result[i] = (value1 < out_result[i]) ? value1 : out_result[i];
                    *out_is_scalar_result = 0;
                } else {
                    out_result[0] = (value1 < out_result[0]) ? value1 : out_result[0];
                    *out_is_scalar_result = 1;
                }
            }
        } else {
            error("binary_function(\"min\"): Unhandled argument type combination");
        }
        return;
    }
    if ((arity == 2) && (symbol_len == 3) && !strncmp(symbol, "max", 3)) {
        SEXP s_arg1 = CADR(s_expr);
        SEXP s_arg2 = CADDR(s_expr);

        if (isNumeric(s_arg1) && isNumeric(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const double value2 = REAL(s_arg2)[0];
            const double value = (value1 > value2) ? value1 : value2;
            out_result[0] = value;
            *out_is_scalar_result = 1;
        } else if (isNumeric(s_arg1) && isSymbol(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = (value1 > value2[i]) ? value1 : value2[i];
        } else if (isNumeric(s_arg1) && isLanguage(s_arg2)) {
            const double value1 = REAL(s_arg1)[0];
            int is_scalar_result;
            evaluate_language_expression(s_arg2, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = (value1 > out_result[0]) ? value1 : out_result[0];
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (value1 > out_result[i]) ? value1 : out_result[i];
            }
        } else if (isSymbol(s_arg1) && isNumeric(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const double value2 = REAL(s_arg2)[0];
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = (value1[i] > value2) ? value1[i] : value2;
        } else if (isSymbol(s_arg1) && isSymbol(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            R_len_t i;
            const R_len_t n_unrolled = samples / 4;
            for (i = 0; i < n_unrolled; i += 4) {
                out_result[i + 0] = (value1[i + 0] > value2[i + 0]) ? value1[i + 0] : value2[i + 0];
out_result[i + 1] = (value1[i + 1] > value2[i + 1]) ? value1[i + 1] : value2[i + 1];
out_result[i + 2] = (value1[i + 2] > value2[i + 2]) ? value1[i + 2] : value2[i + 2];
out_result[i + 3] = (value1[i + 3] > value2[i + 3]) ? value1[i + 3] : value2[i + 3];

            }
            for (; i < samples; ++i)
                out_result[i] = (value1[i] > value2[i]) ? value1[i] : value2[i];
        } else if (isSymbol(s_arg1) && isLanguage(s_arg2)) {
            const R_len_t index1 = function_argument_index(s_arg1, context);
            const double *value1 = context->actualParameters + (index1 * samples);
            int is_scalar_result;
            evaluate_language_expression(s_arg2, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (value1[i] > out_result[1]) ? value1[i] : out_result[1];
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (value1[i] > out_result[i]) ? value1[i] : out_result[i];
            }
        } else if (isLanguage(s_arg1) && isNumeric(s_arg2)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg1, context, out_result, &is_scalar_result);
            const double value2 = REAL(s_arg2)[0];
            if (is_scalar_result) {
                out_result[0] = (out_result[0] > value2) ? out_result[0] : value2;
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (out_result[i] > value2) ? out_result[i] : value2;
            }
        } else if (isLanguage(s_arg1) && isSymbol(s_arg2)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg1, context, out_result, &is_scalar_result);
            const R_len_t index2 = function_argument_index(s_arg2, context);
            const double *value2 = context->actualParameters + (index2 * samples);
            if (is_scalar_result) {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (out_result[0] > value2[i]) ? out_result[0] : value2[i];
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (out_result[i] > value2[i]) ? out_result[i] : value2[i];
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
                        out_result[i] = (out_result[i] > tmp[i]) ? out_result[i] : tmp[i];
                    *out_is_scalar_result = 0;
                } else {
                    const double value2 = tmp[0];
                    for (int i = 0; i < samples; ++i)
                        out_result[i] = (out_result[i] > value2) ? out_result[i] : value2;
                    *out_is_scalar_result = 0;
                }
                free(tmp);
            } else {
                const double value1 = out_result[0];
                evaluate_language_expression(s_arg2, context, out_result, &is_scalar_tmp);
                if (!is_scalar_tmp) {
                    for (int i = 0; i < samples; ++i)
                        out_result[i] = (value1 > out_result[i]) ? value1 : out_result[i];
                    *out_is_scalar_result = 0;
                } else {
                    out_result[0] = (value1 > out_result[0]) ? value1 : out_result[0];
                    *out_is_scalar_result = 1;
                }
            }
        } else {
            error("binary_function(\"max\"): Unhandled argument type combination");
        }
        return;
    }

    if ((arity == 1) && (symbol_len == 3) && !strncmp(symbol, "sin", 5)) {
        SEXP s_arg = CADR(s_expr);
        if (isNumeric(s_arg)) {
            const double value = sin(REAL(s_arg)[0]);
            for (int i = 0; i < samples; ++i)
                out_result[i] = value;
        } else if (isSymbol(s_arg)) {
            const R_len_t index = function_argument_index(s_arg, context);
            const double *value = context->actualParameters + (index * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = sin(value[i]);
        } else if (isLanguage(s_arg)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = sin(out_result[0]);
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = sin(out_result[i]);
            }
        } else {
            error("unary_function(\"sin\"):  Unhandled argument type combination");
        }
        return;
    }
    if ((arity == 1) && (symbol_len == 3) && !strncmp(symbol, "cos", 5)) {
        SEXP s_arg = CADR(s_expr);
        if (isNumeric(s_arg)) {
            const double value = cos(REAL(s_arg)[0]);
            for (int i = 0; i < samples; ++i)
                out_result[i] = value;
        } else if (isSymbol(s_arg)) {
            const R_len_t index = function_argument_index(s_arg, context);
            const double *value = context->actualParameters + (index * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = cos(value[i]);
        } else if (isLanguage(s_arg)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = cos(out_result[0]);
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = cos(out_result[i]);
            }
        } else {
            error("unary_function(\"cos\"):  Unhandled argument type combination");
        }
        return;
    }
    if ((arity == 1) && (symbol_len == 3) && !strncmp(symbol, "tan", 5)) {
        SEXP s_arg = CADR(s_expr);
        if (isNumeric(s_arg)) {
            const double value = tan(REAL(s_arg)[0]);
            for (int i = 0; i < samples; ++i)
                out_result[i] = value;
        } else if (isSymbol(s_arg)) {
            const R_len_t index = function_argument_index(s_arg, context);
            const double *value = context->actualParameters + (index * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = tan(value[i]);
        } else if (isLanguage(s_arg)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = tan(out_result[0]);
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = tan(out_result[i]);
            }
        } else {
            error("unary_function(\"tan\"):  Unhandled argument type combination");
        }
        return;
    }
    if ((arity == 1) && (symbol_len == 3) && !strncmp(symbol, "exp", 5)) {
        SEXP s_arg = CADR(s_expr);
        if (isNumeric(s_arg)) {
            const double value = exp(REAL(s_arg)[0]);
            for (int i = 0; i < samples; ++i)
                out_result[i] = value;
        } else if (isSymbol(s_arg)) {
            const R_len_t index = function_argument_index(s_arg, context);
            const double *value = context->actualParameters + (index * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = exp(value[i]);
        } else if (isLanguage(s_arg)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = exp(out_result[0]);
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = exp(out_result[i]);
            }
        } else {
            error("unary_function(\"exp\"):  Unhandled argument type combination");
        }
        return;
    }
    if ((arity == 1) && (symbol_len == 3) && !strncmp(symbol, "log", 5)) {
        SEXP s_arg = CADR(s_expr);
        if (isNumeric(s_arg)) {
            const double value = log(REAL(s_arg)[0]);
            for (int i = 0; i < samples; ++i)
                out_result[i] = value;
        } else if (isSymbol(s_arg)) {
            const R_len_t index = function_argument_index(s_arg, context);
            const double *value = context->actualParameters + (index * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = log(value[i]);
        } else if (isLanguage(s_arg)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = log(out_result[0]);
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = log(out_result[i]);
            }
        } else {
            error("unary_function(\"log\"):  Unhandled argument type combination");
        }
        return;
    }
    if ((arity == 1) && (symbol_len == 4) && !strncmp(symbol, "sqrt", 6)) {
        SEXP s_arg = CADR(s_expr);
        if (isNumeric(s_arg)) {
            const double value = sqrt(REAL(s_arg)[0]);
            for (int i = 0; i < samples; ++i)
                out_result[i] = value;
        } else if (isSymbol(s_arg)) {
            const R_len_t index = function_argument_index(s_arg, context);
            const double *value = context->actualParameters + (index * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = sqrt(value[i]);
        } else if (isLanguage(s_arg)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = sqrt(out_result[0]);
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = sqrt(out_result[i]);
            }
        } else {
            error("unary_function(\"sqrt\"):  Unhandled argument type combination");
        }
        return;
    }
    if ((arity == 1) && (symbol_len == 1) && symbol[0] == "-"[0]) {
        SEXP s_arg = CADR(s_expr);
        if (isNumeric(s_arg)) {
            const double value = -REAL(s_arg)[0];
            for (int i = 0; i < samples; ++i)
                out_result[i] = value;
        } else if (isSymbol(s_arg)) {
            const R_len_t index = function_argument_index(s_arg, context);
            const double *value = context->actualParameters + (index * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = -value[i];
        } else if (isLanguage(s_arg)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = -out_result[0];
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = -out_result[i];
            }
        } else {
            error("unary_function(\"-\"):  Unhandled argument type combination");
        }
        return;
    }
    if ((arity == 1) && (symbol_len == 1) && symbol[0] == "("[0]) {
        SEXP s_arg = CADR(s_expr);
        if (isNumeric(s_arg)) {
            const double value = REAL(s_arg)[0];
            for (int i = 0; i < samples; ++i)
                out_result[i] = value;
        } else if (isSymbol(s_arg)) {
            const R_len_t index = function_argument_index(s_arg, context);
            const double *value = context->actualParameters + (index * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = value[i];
        } else if (isLanguage(s_arg)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = out_result[0];
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = out_result[i];
            }
        } else {
            error("unary_function(\"(\"):  Unhandled argument type combination");
        }
        return;
    }
    if ((arity == 1) && (symbol_len == 3) && !strncmp(symbol, "abs", 5)) {
        SEXP s_arg = CADR(s_expr);
        if (isNumeric(s_arg)) {
            const double value = fabs(REAL(s_arg)[0]);
            for (int i = 0; i < samples; ++i)
                out_result[i] = value;
        } else if (isSymbol(s_arg)) {
            const R_len_t index = function_argument_index(s_arg, context);
            const double *value = context->actualParameters + (index * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = fabs(value[i]);
        } else if (isLanguage(s_arg)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = fabs(out_result[0]);
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = fabs(out_result[i]);
            }
        } else {
            error("unary_function(\"abs\"):  Unhandled argument type combination");
        }
        return;
    }
    if ((arity == 1) && (symbol_len == 4) && !strncmp(symbol, "sign", 6)) {
        SEXP s_arg = CADR(s_expr);
        if (isNumeric(s_arg)) {
            const double value = (REAL(s_arg)[0] < 0.0) ? -1.0 : ((REAL(s_arg)[0] > 0.0) ? 1.0 : 0.0);
            for (int i = 0; i < samples; ++i)
                out_result[i] = value;
        } else if (isSymbol(s_arg)) {
            const R_len_t index = function_argument_index(s_arg, context);
            const double *value = context->actualParameters + (index * samples);
            for (R_len_t i = 0; i < samples; ++i)
                out_result[i] = (value[i] < 0.0) ? -1.0 : ((value[i] > 0.0) ? 1.0 : 0.0);
        } else if (isLanguage(s_arg)) {
            int is_scalar_result;
            evaluate_language_expression(s_arg, context, out_result, &is_scalar_result);
            if (is_scalar_result) {
                out_result[0] = (out_result[0] < 0.0) ? -1.0 : ((out_result[0] > 0.0) ? 1.0 : 0.0);
                *out_is_scalar_result = 1;
            } else {
                for (R_len_t i = 0; i < samples; ++i)
                    out_result[i] = (out_result[i] < 0.0) ? -1.0 : ((out_result[i] > 0.0) ? 1.0 : 0.0);
            }
        } else {
            error("unary_function(\"sign\"):  Unhandled argument type combination");
        }
        return;
    }
    evalVectorizedFallback(s_expr, context, out_result);
    return;
}
/* !!! END OF GENERATED CODE */
