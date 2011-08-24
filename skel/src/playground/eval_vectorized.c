/* eval_vectorized.c
 *
 */

#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <math.h>


// TODO move structs to eval_vectorized.h
struct EvalVectorizedContext {
  int arity;
  int samples;
  SEXP formalParameters;
  double *actualParameters;
};


// TODO move this function to environment_tools.c
static R_INLINE SEXP makeEnvironment(SEXP enclosingEnvironment) { 
  SEXP env;
  PROTECT(env = allocSExp(ENVSXP));
  SET_FRAME(env, R_NilValue);
  SET_ENCLOS(env, (enclosingEnvironment)? enclosingEnvironment : R_GlobalEnv);
  SET_HASHTAB(env, R_NilValue);
  SET_ATTRIB(env, R_NilValue);
  UNPROTECT(1);
  return env; 
}

// TODO move forward declarations to eval_vectorized.h
static void evalVectorizedFallback(SEXP, struct EvalVectorizedContext *, double *);

void evalVectorizedRecursive(SEXP rExpr, struct EvalVectorizedContext *context, double *resultOut) {
  const char *rFuncName;
  const char *rSymbol;
  const int samples = context->samples;

  // recurse along the structure of rExpr and evaluate...
  if (isNumeric(rExpr)) { // numeric constant...
    for (int i = 0; i < samples; i++) { // return a vector "c(a, a, ..., a)" where "a" is the constant
      resultOut[i] = REAL(rExpr)[0];
    }
    return;
  }
  else if (isSymbol(rExpr)) { // input variable...
    rSymbol = CHAR(PRINTNAME(rExpr));
    //Rprintf("SYMBOL '%s'\n", rSymbol);

    //Rprintf("arity: %i\n", context->arity);
    //for (int i = 0; i < context->arity; i++) Rprintf("formal %i: %s\n", i, CHAR(STRING_ELT(context->formalParameters, i)));
    for (int i = 0; i < context->arity; i++) { // find matching actualParameter vector and return it...
      if (!strcmp(rSymbol, CHAR(STRING_ELT(context->formalParameters, i)))) {
        for (int j = 0; j < samples; j++) {
          resultOut[j] = context->actualParameters[i * samples + j];
        }
        return;
      }
    }
    error("evalVectorizedRecursive: undefined symbol");
  }
  else if (isLanguage(rExpr)) { // composite R expression....
    rFuncName = CHAR(PRINTNAME(CAR(rExpr)));
    //Rprintf("COMPOSITE %s\n", rFuncName);

    if (!strcmp(rFuncName, "+")) {
      double lhs[samples], rhs[samples];
      evalVectorizedRecursive(CADR(rExpr), context, lhs);
      evalVectorizedRecursive(CADDR(rExpr), context, rhs);
      for (int i = 0; i < samples; i++) {
        resultOut[i] = lhs[i] + rhs[i];
      }
      return;
    }
    else if (!strcmp(rFuncName, "-")) {
      if (!isNull(CADDR(rExpr))) { // support for "handmade" (parsed) functions, GP created function dont need this exception
        double lhs[samples], rhs[samples];
        evalVectorizedRecursive(CADR(rExpr), context, lhs);
        evalVectorizedRecursive(CADDR(rExpr), context, rhs);
        for (int i = 0; i < samples; i++) {
          resultOut[i] = lhs[i] - rhs[i];
        } 
      } else {
        double lhs[samples];
        evalVectorizedRecursive(CADR(rExpr), context, lhs);
        for (int i = 0; i < samples; i++) {
          resultOut[i] = -lhs[i];
        }
      }
      return;
    }
    else if (!strcmp(rFuncName, "*")) {
      double lhs[samples], rhs[samples];
      evalVectorizedRecursive(CADR(rExpr), context, lhs);
      evalVectorizedRecursive(CADDR(rExpr), context, rhs);
      for (int i = 0; i < samples; i++) {
        resultOut[i] = lhs[i] * rhs[i];
      }
      return;
    }
    else if (!strcmp(rFuncName, "/")) {
      double lhs[samples], rhs[samples];
      evalVectorizedRecursive(CADR(rExpr), context, lhs);
      evalVectorizedRecursive(CADDR(rExpr), context, rhs);
      for (int i = 0; i < samples; i++) {
        if (rhs[i]) {
          resultOut[i] =  lhs[i] / rhs[i];
        } else error("evalVectorizedRecursive: division by zero");
      }
      return;
    }
    else if (!strcmp(rFuncName, "sin")) {
      double lhs[samples];
      evalVectorizedRecursive(CADR(rExpr), context, lhs);
      for (int i = 0; i < samples; i++) {
        resultOut[i] = sin(lhs[i]);
      }
      return;
    }
    else if (!strcmp(rFuncName, "cos")) {
      double lhs[samples];
      evalVectorizedRecursive(CADR(rExpr), context, lhs);
      for (int i = 0; i < samples; i++) {
        resultOut[i] = cos(lhs[i]);
      }
      return;
    }
    else if (!strcmp(rFuncName, "tan")) {
      double lhs[samples];
      evalVectorizedRecursive(CADR(rExpr), context, lhs);
      for (int i = 0; i < samples; i++) {
        resultOut[i] = tan(lhs[i]);
      }
      return;
    }
    else if (!strcmp(rFuncName, "(")) { // just skip parenthesis...
      evalVectorizedRecursive(CADR(rExpr), context, resultOut);
      return;
    }
    else {
      evalVectorizedFallback(rExpr, context, resultOut);
      return;
    }
  }
  else error("evalVectorizedRecursive: unsupported R expression");
}

// TODO move this function to symbol_tools.c
static R_INLINE SEXP makeFreshSymbol(int idx, SEXP env) {
  // TODO this function must create a symbol that is fresh (unbound) in env
  const int freshStringSize = 8 + (int) log10(idx + 1) + 1;
  char freshString[freshStringSize];
  sprintf(freshString, "__arg%d__", idx);
  return install(freshString);
}

static R_INLINE void evalVectorizedFallback(SEXP rExpr, struct EvalVectorizedContext *context, double *resultOut) {
  const int arity = LENGTH(coerceVector(rExpr, VECSXP)) - 1;
  const int samples = context->samples;
  //Rprintf("fallback to eval for function of arity %d\n", arity); // TODO
  int argIdx; // argument index
  SEXP argItor; // argument iterator
  SEXP call; // call object, initialized with function name
  SEXP callRev = lang1(CAR(rExpr)); // call is build in reverse
  SEXP env = makeEnvironment(R_GlobalEnv); // fresh R environment for holding argument values
  
  // evaluate the arguments with evalVectorizedRecursive...
  for (argItor = CDR(rExpr), argIdx = 0; !isNull(argItor); argItor = CDR(argItor), argIdx++) {
    SEXP argName = makeFreshSymbol(argIdx, env);
    SEXP argVal = PROTECT(allocVector(REALSXP, samples));
    evalVectorizedRecursive(CAR(argItor), context, REAL(argVal));
    defineVar(argName, argVal, env);
    callRev = LCONS(argName, callRev); // add argument name to function call object
  }
  for (call = R_NilValue; !isNull(callRev); callRev = CDR(callRev)) { // reverse callRev into call
    call = LCONS(CAR(callRev), call);
  }
  
  // evaluate rExpr via R's evaluator...
  SEXP result = PROTECT(eval(call, env));
  for (int i = 0; i < samples; i++) {
    resultOut[i] = REAL(result)[i]; // TODO this will of course fail miserably for results of other type than real
  }
  UNPROTECT(1 + arity);
  return;
}

void initializeEvalVectorizedContext(SEXP rFunction, SEXP actualParameters, struct EvalVectorizedContext *contextOut) {
  SEXP rFormals, rFormalNames, strFormals;

  rFormals = FORMALS(rFunction);
  int arity = LENGTH(coerceVector(rFormals, VECSXP));
  contextOut->arity = arity;

  rFormalNames = getAttrib(rFormals, R_NamesSymbol);
  contextOut->formalParameters = rFormalNames;

  if (arity) {
    contextOut->samples = LENGTH(actualParameters) / arity;
  } else {
    contextOut->samples = 1;
  }
  //Rprintf("samples: %i \n", contextOut->samples);

  actualParameters = coerceVector(actualParameters, REALSXP);
  contextOut->actualParameters = REAL(actualParameters);
}

SEXP evalVectorized(SEXP rFunction, SEXP actualParameters) {
  struct EvalVectorizedContext context;
  initializeEvalVectorizedContext(rFunction, actualParameters, &context);

  double result[context.samples];
  evalVectorizedRecursive(BODY(rFunction), &context, result);

  SEXP rResult;
  PROTECT(rResult = allocVector(REALSXP, context.samples));
  for (int i= 0; i < context.samples; i++) {
    //Rprintf("%f\n", result[i]);
    REAL(rResult)[i] = result[i];
  }
  UNPROTECT(1);
  return rResult;
}

SEXP evalVectorizedRmse(SEXP rFunction, SEXP actualParameters, SEXP targetValues) {
  struct EvalVectorizedContext context;
  initializeEvalVectorizedContext(rFunction, actualParameters, &context);

  double result[context.samples];
  evalVectorizedRecursive(BODY(rFunction), &context, result);

  // calculate RMSE...
  double diff, total, rmse;
  targetValues = coerceVector(targetValues, REALSXP);

  for (int i = 0; i < context.samples; i++) {
    diff = result[i] - REAL(targetValues)[i];
    total += diff * diff;
  }
  rmse = sqrt(total / (double) context.samples);

  SEXP rResult;
  PROTECT(rResult = allocVector(REALSXP, 1));
  REAL(rResult)[0] = rmse;
  UNPROTECT(1);
  return rResult;
}
