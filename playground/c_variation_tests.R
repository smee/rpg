# c_variation_tests.R
#
# 2012 Oliver Flasch
#

dyn.load("random_utils.so"); e1 <- .Call("initialize_expression_grow_R", list("+", "-", "*", "/", "log"), c(2L, 2L, 2L, 2L, 1L), list("x1", "x2", "x3"), -1, 1, 1.0, 0.5, 1L); e1

dyn.load("random_utils.so"); e1 <- .Call("mutate_functions_R", quote(2*x1+log(3.14)), 0.5, list("+", "-", "*", "/", "log", "exp"), c(2L, 2L, 2L, 2L, 1L, 1L)); e1

dyn.load("random_utils.so"); e1 <- .Call("mutate_subtrees_R", quote(2*x1+log(3.14)), 1.0, 0.5, list("+", "-", "*", "/", "log"), c(2L, 2L, 2L, 2L, 1L), list("x1", "x2", "x3"), -1, 1, 1.0, 0.5, 2L); e1

dyn.load("sexp_utils.so"); f1 <- .Call("make_closure", quote(2*x1+log(x2)), list("x1", "x2")); f1

dyn.load("sexp_utils.so"); e1 <- quote(2 * sin(x + 1)); print(.Call("sexp_size_R", e1)); .Call("get_sexp_subtree_R", e1, 0L)
dyn.load("sexp_utils.so"); e1 <- quote(2 * sin(x + ifelse(2 < x, 3.14, y))); print(.Call("sexp_size_R", e1)); .Call("get_sexp_subtree_R", e1, 0L)

dyn.load("sexp_utils.so"); e1 <- quote(2 * sin(x + ifelse(2 < x, 3.14, y))); e2 <- quote(z / 5); .Call("replace_sexp_subtree_R", e1, 10L, e2)

dyn.load("random_utils.so"); r1 <- .Call("crossover_single_point_R", quote(2 * sin(x)), quote(y + 1))

