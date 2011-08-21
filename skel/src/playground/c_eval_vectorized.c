/* c_eval_vectorized.c
 *
 */

#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <math.h>


struct EvalVectorizedContext {
  int arity;
  int samples;
  const char **formalParameters;
  double *actualParameters;
};

void evalVectorizedRecursive(SEXP rExpr, struct EvalVectorizedContext *context, double *resultOut) {
  SEXP coercedRExpr;
  const char *rChar;
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
    PROTECT(coercedRExpr = coerceVector(rExpr, STRSXP));
    rSymbol = CHAR(STRING_ELT(coercedRExpr, 0));
    UNPROTECT(1);
    //Rprintf("SYMBOL '%s'\n", rSymbol);

    for (int i = 0; i < context->arity; i++) { // find matching actualParameter vector and return it...
      if (!strcmp(rSymbol, context->formalParameters[i])) {
        for (int j = 0; j < samples; j++) {
          resultOut[j] = context->actualParameters[i * samples + j];
        }
        return;
      }
    }
    error("evalVectorizedRecursive: undefined symbol");
  }
  else { // composite R expression....
    PROTECT(coercedRExpr = coerceVector(CAR(rExpr), STRSXP));
    rChar = CHAR(STRING_ELT(coercedRExpr, 0));
    UNPROTECT(1);
    //Rprintf("COMPOSITE %s\n", rChar);

    if (!strcmp(rChar, "+")) {
      double lhs[samples], rhs[samples];
      evalVectorizedRecursive(CADR(rExpr), context, lhs);
      evalVectorizedRecursive(CADDR(rExpr), context, rhs);
      for (int i = 0; i < samples; i++) {
        resultOut[i] = lhs[i] + rhs[i];
      }
      return;
    }
    else if (!strcmp(rChar, "-")) {
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
    else if (!strcmp(rChar, "*")) {
      double lhs[samples], rhs[samples];
      evalVectorizedRecursive(CADR(rExpr), context, lhs);
      evalVectorizedRecursive(CADDR(rExpr), context, rhs);
      for (int i = 0; i < samples; i++) {
        resultOut[i] = lhs[i] * rhs[i];
      }
      return;
    }
    else if (!strcmp(rChar, "/")) {
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
    else if (!strcmp(rChar, "sin")) {
      double lhs[samples];
      evalVectorizedRecursive(CADR(rExpr), context, lhs);
      for (int i = 0; i < samples; i++) {
        resultOut[i] = sin(lhs[i]);
      }
      return;
    }
    else if (!strcmp(rChar, "cos")) {
      double lhs[samples];
      evalVectorizedRecursive(CADR(rExpr), context, lhs);
      for (int i = 0; i < samples; i++) {
        resultOut[i] = cos(lhs[i]);
      }
      return;
    }
    else if (!strcmp(rChar, "tan")) {
      double lhs[samples];
      evalVectorizedRecursive(CADR(rExpr), context, lhs);
      for (int i = 0; i < samples; i++) {
        resultOut[i] = tan(lhs[i]);
      }
      return;
    }
    else if (!strcmp(rChar, "(")) { // just skip parenthesis...
      evalVectorizedRecursive(CADR(rExpr), context, resultOut);
      return;
    }
    else error("evalVectorizedRecursive: unsupported composite R expression");
  }
}

void makeEvalVectorizedContext(SEXP rFunction, SEXP actualParameters, struct EvalVectorizedContext *contextOut) {
  SEXP rFormals, rFormalNames, strFormals;

  rFormals = FORMALS(rFunction);
  int arity = LENGTH(coerceVector(rFormals, VECSXP));
  contextOut->arity = arity;
  const char *formalParameters[arity];

  PROTECT(rFormalNames = getAttrib(rFormals, R_NamesSymbol));
  PROTECT(strFormals = coerceVector(rFormalNames, STRSXP));
  for(int i = 0; i < arity; i++) {
    formalParameters[i] = CHAR(STRING_ELT(strFormals,i));
    //Rprintf("formals no %d: %s \n", i + 1, formalParameters[i]);
  }
  contextOut->formalParameters = formalParameters;

  if(arity) {
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
  makeEvalVectorizedContext(rFunction, actualParameters, &context);

  double result[context.samples];
  evalVectorizedRecursive(BODY(rFunction), &context, result);

  SEXP rResult;
  PROTECT(rResult = allocVector(REALSXP, context.samples));
  for(int i= 0; i < context.samples; i++) {
    //Rprintf("%f\n", result[i]);
    REAL(rResult)[i] = result[i];
  }
  UNPROTECT(3);
  return rResult;
}

SEXP evalVectorizedRmse(SEXP rFunction, SEXP actualParameters, SEXP targetValues) {
  struct EvalVectorizedContext context;
  makeEvalVectorizedContext(rFunction, actualParameters, &context);

  double result[context.samples];
  evalVectorizedRecursive(BODY(rFunction), &context, result);

  // calculate RMSE...
  double diff, total, rmse;
  targetValues = coerceVector(targetValues, REALSXP);

  for(int i = 0; i < context.samples; i++) {
    diff = result[i] - REAL(targetValues)[i];
    total += diff * diff;
  }
  rmse = sqrt(total / (double) context.samples);

  SEXP rResult;
  PROTECT(rResult = allocVector(REALSXP, 1));
  REAL(rResult)[0] = rmse;
  UNPROTECT(3);
  return rResult;
}
