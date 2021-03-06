library("microbenchmark")
library("plyr")
library("reshape2")

dyn.load("eval_vectorized_orig.so")
dyn.load("eval_vectorized.so")

c_function <- function(symbol_name, ...) {
  symbol <- getNativeSymbolInfo(symbol_name, ...)
  function(...)
    .Call(symbol, ...)
}

c_eval <- c_function("evalVectorized", PACKAGE="eval_vectorized")
c_eval_orig <- c_function("evalVectorized", PACKAGE="eval_vectorized_orig")

fun <- function(x1, x2)  
  (x1 + x2) * 0.2 * cos(x1 + 0.1) + sin(0.2 * x2) + 0.05 * x1 - 1.5 + cos(x1) * sin(2.0)

run_benchmark <- function(name, fun, sizes=10**(1:5), nruns=250) {
  ## Run benchmark for differently sized arguments:
  result <- NULL
  for (size in sizes) {
    input <- lapply(formals(fun), function(x) rnorm(size))
    matrix_input <- do.call(cbind, input)

    b <- if (size < 20000) {
      microbenchmark(r=do.call(fun, input),
                     c=c_eval(fun, matrix_input),
                     c_orig=c_eval_orig(fun, matrix_input),
                     times=nruns)
    } else {
      microbenchmark(r=do.call(fun, input),
                     c=c_eval(fun, matrix_input),
                     times=nruns)
    }
    b$size <- size
    result <- rbind(result, b)
  }
  result$fun <- name
  result
}

benchPlot <- function(bData, func = "symreg", maxSize = 10000)
  boxplot(value ~ variable, data = melt(bData[bData$fun == func & bData$size <= maxSize,], c("fun", "size")),
          main = func, xlab = "evaluator", ylab = "runtime (ns)")

run_benchmarks <- function() {
  ## List of benchmark functions:
  functions <- list(negate_s=function(x) -x,
                    n_mult_s=function(x) 2.192 * x,
                    s_mult_s=function(x, y) x * y,
                    s_div_s=function(x, y) x / y,
                    trig_p1=function(x, y) sin(x)^2 + cos(y)^2,
                    trig_p2=function(x, y) sin(x) * sin(x) + cos(y) * cos(y),
                    ndens1=function(x) 1 / sqrt(2 * 3.1415) * exp(-(x*x)/2),
                    ndens2=function(x) 1 / sqrt(2 * 3.1415) * exp(-0.5 * (x*x)),
                    parens1=function(x) (2.1 * 0.3) * x,
                    parens2=function(x) 2.1 * (0.3 * x),
                    sum10=function(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
                      x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10,
                    symreg=function(x1, x2)
                      0.2 * cos(x1 + 0.1) + sin(0.2 * x2) + 0.05 * x1 - 1.5
                    )

  result <- NULL
  for (function_name in names(functions)) {
    fun <- functions[[function_name]]
    tmp <- run_benchmark(function_name, fun)
    result <- rbind(result, tmp)
  }

  absTimes <- ddply(result, .(fun, expr, size), summarize, median_runtime=median(time))
  tmp <- ddply(absTimes, .(fun, size), transform,
               median_speedup=1/(median_runtime / max(median_runtime)))
  tblRel <- dcast(tmp, fun + size ~ expr, value_var = "median_speedup")
  tblAbs <- dcast(absTimes, fun + size ~ expr, value_var = "median_runtime")

  cat("Absolute median runtimes (ns):\n")
  print(tblAbs)

  cat("Median relative speedup:\n")
  print(tblRel)

  message("Improvement over R     : ", mean(tblRel$c / tblRel$r))
  message("Improvement over old C : ", mean(tblRel$c / tblRel$c_orig, na.rm=TRUE))

  list(absoulteMedianRuntimes = tblAbs, relativeMedianSpeedup = tblRel)
}
