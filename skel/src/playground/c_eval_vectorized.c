/* c_eval_vectorized.c
 *
 */

#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <math.h>


struct CevalVectorizedContext {
  int arity;
  int samples;
  const char **formalParameters;
  double *actualParameters;
};

void CevalVectorizedRecursive(SEXP rExpr, struct CevalVectorizedContext *context, double *result) {
  SEXP coercedRExpr;
  const char *rChar;
  const char *rSymbol;
  const int samples = context->samples;

  // Recurse along the structure of rExpr and evaluate...
  if (isNumeric(rExpr)) { // numeric constant...
    for (int i = 0; i < samples; i++) { // return a vector "c(a, a, ..., a)" where "a" is the constant
      result[i] = REAL(rExpr)[0];
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
          result[j] = context->actualParameters[i * samples + j];
        }
        return;
      }
    }
    error("CevalVectorizedRecursive: undefined symbol");
  }
  else { // composite R expression....
    PROTECT(coercedRExpr = coerceVector(CAR(rExpr), STRSXP));
    rChar = CHAR(STRING_ELT(coercedRExpr, 0));
    UNPROTECT(1);
    //Rprintf("COMPOSITE %s\n", rChar);

    if (!strcmp(rChar, "+")) {
      double lhs[samples], rhs[samples];
      CevalVectorizedRecursive(CADR(rExpr), context, lhs);
      CevalVectorizedRecursive(CADDR(rExpr), context, rhs);
      for (int i = 0; i < samples; i++) {
        result[i] = lhs[i] + rhs[i];
      }
      return;
    }
    else if (!strcmp(rChar, "-")) {
      if (!isNull(CADDR(rExpr))) { // support for "handmade" (parsed) functions, GP created function dont need this exception
        double lhs[samples], rhs[samples];
        CevalVectorizedRecursive(CADR(rExpr), context, lhs);
        CevalVectorizedRecursive(CADDR(rExpr), context, rhs);
        for (int i = 0; i < samples; i++) {
          result[i] = lhs[i] - rhs[i];
        } 
      } else {
        double lhs[samples];
        CevalVectorizedRecursive(CADR(rExpr), context, lhs);
        for (int i = 0; i < samples; i++) {
          result[i] = -lhs[i];
        }
      }
      return;
    }
    else if (!strcmp(rChar, "*")) {
      double lhs[samples], rhs[samples];
      CevalVectorizedRecursive(CADR(rExpr), context, lhs);
      CevalVectorizedRecursive(CADDR(rExpr), context, rhs);
      for (int i = 0; i < samples; i++) {
        result[i] = lhs[i] * rhs[i];
      }
      return;
    }
    else if (!strcmp(rChar, "/")) {
      double lhs[samples], rhs[samples];
      CevalVectorizedRecursive(CADR(rExpr), context, lhs);
      CevalVectorizedRecursive(CADDR(rExpr), context, rhs);
      for (int i = 0; i < samples; i++) {
        if (rhs[i]) {
          result[i] =  lhs[i] / rhs[i];
        } else error("CevalVectorizedRecursive: division by zero");
      }
      return;
    }
    else if (!strcmp(rChar, "sin")) {
      double lhs[samples];
      CevalVectorizedRecursive(CADR(rExpr), context, lhs);
      for (int i = 0; i < samples; i++) {
        result[i] = sin(lhs[i]);
      }
      return;
    }
    else if (!strcmp(rChar, "cos")) {
      double lhs[samples];
      CevalVectorizedRecursive(CADR(rExpr), context, lhs);
      for (int i = 0; i < samples; i++) {
        result[i] = cos(lhs[i]);
      }
      return;
    }
    else if (!strcmp(rChar, "tan")) {
      double lhs[samples];
      CevalVectorizedRecursive(CADR(rExpr), context, lhs);
      for (int i = 0; i < samples; i++) {
        result[i] = tan(lhs[i]);
      }
      return;
    }
    else if (!strcmp(rChar, "(")) { // just skip parenthesis...
      CevalVectorizedRecursive(CADR(rExpr), context, result);
      return;
    }
  }
}

SEXP CevalVectorized(SEXP rFunction, SEXP actualParameters) {
  SEXP rResult, rFormals, rFormalNames, strFormals;
  struct CevalVectorizedContext context;

  rFormals = FORMALS(rFunction);
  context.arity = LENGTH(coerceVector(rFormals, VECSXP));
  const char *formalParameters[context.arity];

  PROTECT(rFormalNames = getAttrib(rFormals, R_NamesSymbol));
  PROTECT(strFormals = coerceVector(rFormalNames, STRSXP));
  for(int i = 0; i < context.arity; i++) {
    formalParameters[i] = CHAR(STRING_ELT(strFormals,i));
    //Rprintf("formals no %d: %s \n", i + 1, formalParameters[i]);
  }
  context.formalParameters = formalParameters;

  if(context.arity) {
    context.samples = LENGTH(actualParameters) / context.arity;
  } else {
    context.samples = 1;
  }
  //Rprintf("samples: %i \n", context.samples);

  actualParameters = coerceVector(actualParameters, REALSXP);
  context.actualParameters = REAL(actualParameters);

  double result[context.samples];
  CevalVectorizedRecursive(BODY(rFunction), &context, result);

  PROTECT(rResult = allocVector(REALSXP, context.samples));
  for(int i= 0; i < context.samples; i++) {
    //Rprintf("%f\n", result[i]);
    REAL(rResult)[i] = result[i];
  }
  UNPROTECT(3);
  return rResult;
}

SEXP CevalVectorizedRmse(SEXP rExpr, SEXP actualParameters, SEXP targetValues) {
  SEXP rResult, rFormals, rFormalNames, strFormals;
  struct CevalVectorizedContext context;

  rFormals = FORMALS(rExpr);
  context.arity = LENGTH(coerceVector(rFormals, VECSXP));
  //Rprintf("arity: %i\n", context.arity); // TODO
  const char *formalParameters[context.arity];
  
  //PROTECT(strFormals = coerceVector(rFormalParameters, STRSXP));
  PROTECT(rFormalNames = getAttrib(rFormals, R_NamesSymbol));
  PROTECT(strFormals = coerceVector(rFormalNames, STRSXP));
  for(int i = 0; i < context.arity; i++) {
    formalParameters[i] = CHAR(STRING_ELT(strFormals,i));
    //Rprintf("formals no %d: %s \n",i+1,formalParameters[i]);
  }

  if(context.arity) {
    context.samples = LENGTH(actualParameters) / context.arity;
  } else {
    context.samples = 1;
  }
  //Rprintf("samples: %i \n", context.samples);

  double result[context.samples];

  double actualArray[context.arity * context.samples];
  double targetArray[context.samples];

  actualParameters = coerceVector(actualParameters, REALSXP);
  targetValues = coerceVector(targetValues, REALSXP);

  for(int i= 0; i < context.arity * context.samples; i++) {
    actualArray[i]= REAL(actualParameters)[i];
  }
  for(int i= 0; i < context.samples; i++) {
    targetArray[i]= REAL(targetValues)[i];
  }

  context.actualParameters = actualArray;
  context.formalParameters = formalParameters;

  CevalVectorizedRecursive(BODY(rExpr), &context, result);

  // calculate RMSE...
  double diff, total;

  for(int i = 0; i < context.samples; i++) {
    diff = result[i] - targetArray[i];
    total += diff * diff;
    //Rprintf(" Result: %f  Target: %f Diff: %f Total: %f RMSE: %f \n", result, targetArray[i], diff, total, sqrt(total / (double)(context.step + 1)));
  }
  PROTECT(rResult = allocVector(REALSXP, 1));
  REAL(rResult)[0] = sqrt(total / (double) context.samples);
  UNPROTECT(3);
  return rResult;
}
