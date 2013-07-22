/* eval_vectorized.c
 *
 */

#include "eval_vectorized.h"
#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <math.h>

// TODO move this function to environment_tools.c
static R_INLINE SEXP make_environment(SEXP enclosingEnvironment) { 
  SEXP env;
  PROTECT(env = allocSExp(ENVSXP));
  SET_FRAME(env, R_NilValue);
  SET_ENCLOS(env, (enclosingEnvironment)? enclosingEnvironment : R_GlobalEnv);
  SET_HASHTAB(env, R_NilValue);
  SET_ATTRIB(env, R_NilValue);
  UNPROTECT(1);
  return env; 
}

// TODO move this function to symbol_tools.c
static R_INLINE SEXP make_fresh_symbol(int idx, SEXP env) {
  // TODO this function must create a symbol that is fresh (unbound) in env
  const int freshStringSize = 8 + (int) log10(idx + 1) + 1;
  char freshString[freshStringSize];
  sprintf(freshString, "__arg%d__", idx);
  return install(freshString);
}

void eval_vectorized_recursive(SEXP rExpr, 
                               struct EvalVectorizedContext *context, 
                               double *out_result);

static R_INLINE void eval_vectorized_fallback(SEXP rExpr, 
                                              struct EvalVectorizedContext *context, 
                                              double *out_result) {
  //PROTECT(rExpr= coerceVector(rExpr, VECSXP));
  //const int arity = LENGTH(rExpr) - 1;
  const int samples = context->samples;
  //Rprintf("fallback to eval for function of arity %d\n", arity); // TODO
  int argIdx; // argument index
  SEXP argItor; // argument iterator
  SEXP call; // call object, initialized with function name
  SEXP callRev, env;
  PROTECT(callRev = lang1(CAR(rExpr))); // call is build in reverse
  PROTECT(env = make_environment(R_GlobalEnv)); // fresh R environment for holding argument values
  
  // evaluate the arguments with eval_vectorized_recursive...
  for (argItor = CDR(rExpr), argIdx = 0; !isNull(argItor); argItor = CDR(argItor), argIdx++) {
      SEXP argName;
      PROTECT(argName = make_fresh_symbol(argIdx, env));
      SEXP argVal;
      PROTECT(argVal = allocVector(REALSXP, samples));
      eval_vectorized_recursive(CAR(argItor), context, REAL(argVal));
      defineVar(argName, argVal, env);
      callRev = LCONS(argName, callRev); // add argument name to function call object
  }
  for (call = R_NilValue; !isNull(callRev); callRev = CDR(callRev)) { // reverse callRev into call
      call = LCONS(CAR(callRev), call);
  }
  
  // evaluate rExpr via R's evaluator...
  SEXP result = PROTECT(eval(call, env));
  for (int i = 0; i < samples; i++) {
      /* TODO this will of course fail miserably for results of other
       * type than real */
      out_result[i] = REAL(result)[i]; 
  }
  UNPROTECT(2 + 1);//arity);
  return;
}

#include "evaluate_language_expression.h"

void eval_vectorized_recursive(SEXP rExpr, 
                               struct EvalVectorizedContext *context, 
                               double *out_result) {
    const int samples = context->samples;
    /* Composite R expression: */
    if (isLanguage(rExpr)) {
        int out_is_scalar_result;
        evaluate_language_expression(rExpr, context, out_result, &out_is_scalar_result);
        if (out_is_scalar_result) {
          /* expand scalar result (to fill up the entire result vector) */
          const double value = out_result[0];
          for (R_len_t i = 0; i < samples; ++i) 
              out_result[i] = value;
        }
    } else  {
        /* The following two case should only be exercised if the
         * top-level rExpr is a Symbol or
         * Numeric. evaluate_language_expression() internally avoids
         * dispatching on these two types. Instead they are special
         * cased for efficiency.
         */
        if (isNumeric(rExpr)) {
            const double value = REAL(rExpr)[0];
            for (int i = 0; i < samples; i++) 
                out_result[i] = value;
            return;
        } else if (isSymbol(rExpr)) {
            const char *rSymbol = CHAR(PRINTNAME(rExpr));
            /* find matching actualParameter vector and return it... */
            for (int i = 0; i < context->arity; i++) {
                if (!strcmp(rSymbol, CHAR(STRING_ELT(context->formalParameters, i)))) {
                    const R_len_t offset = i * samples;
                    for (int j = 0; j < samples; j++) {
                        out_result[j] = context->actualParameters[offset + j];
                    }
                    return;
                }
            }
            error("eval_vectorized_recursive: undefined symbol '%s'.", rSymbol);
        } else
            error("eval_vectorized_recursive: unsupported R expression");
    }
}

void initialize_eval_vectorized_context(SEXP rFunction, 
                                        SEXP actualParameters, 
                                        struct EvalVectorizedContext *contextOut) {
    SEXP rFormals, rFormalNames;
    
    PROTECT(rFormals = FORMALS(rFunction));
    PROTECT(rFormals= coerceVector(rFormals, VECSXP));
    int arity = LENGTH(rFormals);
    contextOut->arity = arity;
    
    PROTECT(rFormalNames = getAttrib(rFormals, R_NamesSymbol));
    contextOut->formalParameters = rFormalNames;
    if (arity) {
        contextOut->samples = LENGTH(actualParameters) / arity;
    } else {
        contextOut->samples = 1;
    }
    
    PROTECT(actualParameters = coerceVector(actualParameters, REALSXP));
    contextOut->actualParameters = REAL(actualParameters);
}

SEXP eval_vectorized(SEXP rFunction, SEXP actualParameters) {
  struct EvalVectorizedContext context;
  initialize_eval_vectorized_context(rFunction, actualParameters, &context);
  
  SEXP rResult;
  PROTECT(rResult = allocVector(REALSXP, context.samples));  
  double *result = REAL(rResult);
  eval_vectorized_recursive(BODY(rFunction), &context, result);
  UNPROTECT(1 + 4); // 4 are PROTECTed in "initialize_eval_vectorized_context"
  return rResult;
}

SEXP eval_vectorized_rmse(SEXP rFunction, SEXP actualParameters, SEXP targetValues, double * bestRMSE) {
  struct EvalVectorizedContext context;
  initialize_eval_vectorized_context(rFunction, actualParameters, &context);
  double result[context.samples];
  
  eval_vectorized_recursive(BODY(rFunction), &context, result);
  
  // calculate RMSE...
  double diff, total = 0.0, rmse;
  PROTECT(targetValues = coerceVector(targetValues, REALSXP));
  
  for (int i = 0; i < context.samples; i++) {
    diff = result[i] - REAL(targetValues)[i];
    total += diff * diff;
  }
  rmse = sqrt(total / (double) context.samples);
  if(*bestRMSE > rmse) {
    *bestRMSE= rmse; }
  SEXP rResult;
  PROTECT(rResult = allocVector(REALSXP, 1));
  
  if(!isnan(rmse)) {  //nan-exception error handling
  REAL(rResult)[0] = rmse;
  UNPROTECT(6);
  return rResult; } else { 
    REAL(rResult)[0] = 10000000000000; //large number TODO: set to inf
    UNPROTECT(6);
    return rResult;
    
  }
}


