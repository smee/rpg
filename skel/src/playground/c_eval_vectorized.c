/* c_eval_vectorized.c
 *
 */

#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <math.h>


struct CevalContext {
  int arity;
  int samples;
  const char **formalParameters;
  double *actualParameters;
};

void CevalVectorized(SEXP rExpr, struct CevalContext *context, double *result) {
  SEXP rfun, rtemp;
  const char *Rchar;
  const char *Rsymbol;
  const int samples = context->samples;

  // Recurse along the structure of rExpr and evaluate...
  if (isNumeric(rExpr)) { // numeric constant...
    for (int i = 0; i < samples; i++) { // return a vector "c(a, a, ..., a)" where "a" is the constant
      result[i] = REAL(rExpr)[0];
    }
    return;
  }
  else if (isSymbol(rExpr)) { // input variable...
    PROTECT(rfun = coerceVector(rExpr,STRSXP));
    Rsymbol = CHAR(STRING_ELT(rfun,0));
    UNPROTECT(1);

    for (int i; i < context->arity; i++) { // find matching actualParameter vector and return it...
      if (!strcmp(Rsymbol,context->formalParameters[i])) {
        //Rprintf(" step: %i, formal: %s, value: %f \n", context->step+1, context->formalParameters[i], context->actualParameters[i * context->samples + context->step]);
        //return context->actualParameters[i * context->samples + context->step];
        for (int j = 0; j < samples; j++) {
          result[j] = context->actualParameters[i * context->samples + j];
        }
        return;
      }
    }
  }
  else { // composite R expression....
    PROTECT(rfun = coerceVector(CAR(rExpr), STRSXP));
    Rchar = CHAR(STRING_ELT(rfun, 0));
    UNPROTECT(1);

    if (!strcmp(Rchar, "+")) {
      double lhs[samples], rhs[samples];
      CevalVectorized(CADR(rExpr), context, lhs);
      CevalVectorized(CADDR(rExpr), context, rhs);
      for (int i = 0; i < samples; i++) {
        result[i] = lhs[i] + rhs[i];
      }
      return;
    }
    else if (!strcmp(Rchar, "-")) {
      // TODO
      //if (!CADDR(rExpr)) { // For use of handmade functions, GP created function dont need this exception
      double lhs[samples], rhs[samples];
      CevalVectorized(CADR(rExpr), context, lhs);
      CevalVectorized(CADDR(rExpr), context, rhs);
      for (int i = 0; i < samples; i++) {
        result[i] = lhs[i] - rhs[i];
      }
      //} else {
      //  double lhs[samples];
      //  CevalVectorized(CADR(rExpr), context, lhs);
      //  for (int i = 0; i < samples; i++) {
      //    result[i] = -lhs[i];
      //  }
      //}
      return;
    }
    else if (!strcmp(Rchar, "*")) {
      double lhs[samples], rhs[samples];
      CevalVectorized(CADR(rExpr), context, lhs);
      CevalVectorized(CADDR(rExpr), context, rhs);
      for (int i = 0; i < samples; i++) {
        result[i] = lhs[i] * rhs[i];
      }
      return;
    }
    else if (!strcmp(Rchar, "/")) {
      double lhs[samples], rhs[samples];
      CevalVectorized(CADR(rExpr), context, lhs);
      CevalVectorized(CADDR(rExpr), context, rhs);
      for (int i = 0; i < samples; i++) {
        if (rhs[i]) {
          result[i] =  lhs[i] / rhs[i];
        } else error("CevalVectorized: division by zero");
      }
      return;
    }
    else if (!strcmp(Rchar, "sin")) {
      double lhs[samples];
      CevalVectorized(CADR(rExpr), context, lhs);
      for (int i = 0; i < samples; i++) {
        result[i] = sin(lhs[i]);
      }
      return;
    }
    else if (!strcmp(Rchar, "cos")) {
      double lhs[samples];
      CevalVectorized(CADR(rExpr), context, lhs);
      for (int i = 0; i < samples; i++) {
        result[i] = cos(lhs[i]);
      }
      return;
    }
    else if (!strcmp(Rchar, "tan")) {
      double lhs[samples];
      CevalVectorized(CADR(rExpr), context, lhs);
      for (int i = 0; i < samples; i++) {
        result[i] = tan(lhs[i]);
      }
      return;
    }
    else if (!strcmp(Rchar, "(")) { // just skip parenthesis...
      CevalVectorized(CADR(rExpr), context, result);
      return;
    }
  }
}

SEXP CevalVectorizedWrapper(SEXP rExpr, SEXP RformalParameters, SEXP FormalValues, SEXP TargetValues) {
  SEXP Rresult, Rformals, StrFormals;
  struct CevalContext context;

  Rformals = FORMALS(rExpr);
  context.arity = LENGTH(coerceVector(Rformals, VECSXP));
  //Rprintf("arity: %i\n", context.arity); // TODO
  const char *formalParameters[context.arity];
  PROTECT(StrFormals = coerceVector(RformalParameters, STRSXP));

  for(int i = 0; i < context.arity; i++) {
    formalParameters[i] = CHAR(STRING_ELT(StrFormals,i));
    //Rprintf("formals no %d: %s \n",i+1,formalParameters[i]);
  }
  if(context.arity) {
    context.samples = LENGTH(FormalValues)/context.arity;
  } else {
    context.samples = 1;
  }
  //Rprintf("samples: %i \n", context.samples);
  double result[context.samples];

  double Farray[context.arity * context.samples];
  double Tarray[context.samples];

  FormalValues = coerceVector(FormalValues, REALSXP);
  TargetValues = coerceVector(TargetValues, REALSXP);

  for(int i= 0; i < context.arity * context.samples; i++) {
    Farray[i]= REAL(FormalValues)[i];
  }
  for(int i= 0; i < context.samples; i++) {
    Tarray[i]= REAL(TargetValues)[i];
  }

  context.actualParameters = Farray;
  context.formalParameters = formalParameters;

  double diff, total;

  CevalVectorized(BODY(rExpr), &context, result);

  for(int i = 0; i < context.samples; i++) {
    diff = result[i] - Tarray[i];
    total += diff * diff;
    //Rprintf(" Result: %f  Target: %f Diff: %f Total: %f RMSE: %f \n", result, Tarray[i], diff, total, sqrt(total / (double)(context.step + 1)));
  }
  PROTECT(Rresult = allocVector(REALSXP, 1));
  REAL(Rresult)[0] = sqrt(total / (double) context.samples);
  UNPROTECT(2);
  return Rresult;
}
