/* eval_vectorized_omp.c
 *
 */

#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <math.h>


struct EvalVectorizedOmpContext {
  int arity;
  int samples;
  SEXP formalParameters;
  double *actualParameters;
};

void evalVectorizedOmpRecursive(SEXP rExpr, struct EvalVectorizedOmpContext *context, double *resultOut) {
  SEXP coercedRExpr;
  const char *rChar;
  const char *rSymbol;
  const int samples = context->samples;

  // recurse along the structure of rExpr and evaluate...
  if (isNumeric(rExpr)) { // numeric constant...
#pragma omp parallel for
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

    //Rprintf("arity: %i\n", context->arity);
    //for (int i = 0; i < context->arity; i++) Rprintf("formal %i: %s\n", i, CHAR(STRING_ELT(context->formalParameters, i)));
    for (int i = 0; i < context->arity; i++) { // find matching actualParameter vector and return it...
      if (!strcmp(rSymbol, CHAR(STRING_ELT(context->formalParameters, i)))) {
#pragma omp parallel for
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
      evalVectorizedOmpRecursive(CADR(rExpr), context, lhs);
      evalVectorizedOmpRecursive(CADDR(rExpr), context, rhs);
#pragma omp parallel for
      for (int i = 0; i < samples; i++) {
        resultOut[i] = lhs[i] + rhs[i];
      }
      return;
    }
    else if (!strcmp(rChar, "-")) {
      if (!isNull(CADDR(rExpr))) { // support for "handmade" (parsed) functions, GP created function dont need this exception
        double lhs[samples], rhs[samples];
        evalVectorizedOmpRecursive(CADR(rExpr), context, lhs);
        evalVectorizedOmpRecursive(CADDR(rExpr), context, rhs);
#pragma omp parallel for
        for (int i = 0; i < samples; i++) {
          resultOut[i] = lhs[i] - rhs[i];
        }
      } else {
        double lhs[samples];
        evalVectorizedOmpRecursive(CADR(rExpr), context, lhs);
#pragma omp parallel for
        for (int i = 0; i < samples; i++) {
          resultOut[i] = -lhs[i];
        }
      }
      return;
    }
    else if (!strcmp(rChar, "*")) {
      double lhs[samples], rhs[samples];
      evalVectorizedOmpRecursive(CADR(rExpr), context, lhs);
      evalVectorizedOmpRecursive(CADDR(rExpr), context, rhs);
#pragma omp parallel for
      for (int i = 0; i < samples; i++) {
        resultOut[i] = lhs[i] * rhs[i];
      }
      return;
    }
    else if (!strcmp(rChar, "/")) {
      double lhs[samples], rhs[samples];
      evalVectorizedOmpRecursive(CADR(rExpr), context, lhs);
      evalVectorizedOmpRecursive(CADDR(rExpr), context, rhs);
#pragma omp parallel for
      for (int i = 0; i < samples; i++) {
        if (rhs[i]) {
          resultOut[i] =  lhs[i] / rhs[i];
        } else error("evalVectorizedOmpRecursive: division by zero");
      }
      return;
    }
    else if (!strcmp(rChar, "sin")) {
      double lhs[samples];
      evalVectorizedOmpRecursive(CADR(rExpr), context, lhs);
#pragma omp parallel for
      for (int i = 0; i < samples; i++) {
        resultOut[i] = sin(lhs[i]);
      }
      return;
    }
    else if (!strcmp(rChar, "cos")) {
      double lhs[samples];
      evalVectorizedOmpRecursive(CADR(rExpr), context, lhs);
#pragma omp parallel for
      for (int i = 0; i < samples; i++) {
        resultOut[i] = cos(lhs[i]);
      }
      return;
    }
    else if (!strcmp(rChar, "tan")) {
      double lhs[samples];
      evalVectorizedOmpRecursive(CADR(rExpr), context, lhs);
#pragma omp parallel for
      for (int i = 0; i < samples; i++) {
        resultOut[i] = tan(lhs[i]);
      }
      return;
    }
    else if (!strcmp(rChar, "(")) { // just skip parenthesis...
      evalVectorizedOmpRecursive(CADR(rExpr), context, resultOut);
      return;
    }
    else error("evalVectorizedOmpRecursive: unsupported composite R expression");
  }
}

void makeEvalVectorizedOmpContext(SEXP rFunction, SEXP actualParameters, struct EvalVectorizedOmpContext *contextOut) {
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

SEXP evalVectorizedOmp(SEXP rFunction, SEXP actualParameters) {
  struct EvalVectorizedOmpContext context;
  makeEvalVectorizedOmpContext(rFunction, actualParameters, &context);

  double result[context.samples];
  evalVectorizedOmpRecursive(BODY(rFunction), &context, result);

  SEXP rResult;
  PROTECT(rResult = allocVector(REALSXP, context.samples));
  for (int i= 0; i < context.samples; i++) {
    //Rprintf("%f\n", result[i]);
    REAL(rResult)[i] = result[i];
  }
  UNPROTECT(1);
  return rResult;
}

SEXP evalVectorizedOmpRmse(SEXP rFunction, SEXP actualParameters, SEXP targetValues) {
  struct EvalVectorizedOmpContext context;
  makeEvalVectorizedOmpContext(rFunction, actualParameters, &context);

  double result[context.samples];
  evalVectorizedOmpRecursive(BODY(rFunction), &context, result);

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
