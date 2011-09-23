dyn.load("eval_vectorized.so")

c_function <- function(symbol_name, ...) {
  symbol <- getNativeSymbolInfo(symbol_name, ...)
  function(...)
    .Call(symbol, ...)
}

c_eval <- c_function("evalVectorized", PACKAGE="eval_vectorized")

## List of functions used to test c_eval:
functions <- list(negate=function(x) -x,
                  n_mult_s=function(x) 2*x,
                  s_mult_s=function(x, y) x * y,
                  s_div_s=function(x, y) x / y,
                  sign=function(x) sign(x),
                  abs=function(x) abs(x),
                  trig_p1=function(x, y) sin(x)^2 + cos(y)^2,
                  trig_p2=function(x, y) sin(x) * sin(x) + cos(y) * cos(y),
                  ndens1=function(x) 1 / sqrt(2 * 3.1415) * exp(-(x*x)/2),
                  ndens2=function(x) 1 / sqrt(2 * 3.1415) * exp(-0.5 * (x*x))
                  )

#functions <- functions[names(functions) %in% "ndens1"]
number_of_samples <- 10000
for (function_name in names(functions)) {
  fun <- functions[[function_name]]

  ## Generate suitable input:
  input <- lapply(formals(fun),
                  function(x) {
                    ## Make sure we have some of the 'nasty' types of
                    ## input values in the test vectors:
                    sample(c(0, Inf, -Inf,  NA, NaN, rnorm(number_of_samples - 5)))
                  })
  matrix_input <- do.call(cbind, input)
  
  r_result <- do.call(fun, input)
  c_result <- c_eval(fun, matrix_input)
  delta <- c_result - r_result
  ## print(cbind(r_result, c_result, delta))
  if (any(delta != 1))
    message(sprintf("%-10s: %s",
                    function_name,
                    paste(sprintf("%5.3f", fivenum(delta)), collapse=" ")))
}
