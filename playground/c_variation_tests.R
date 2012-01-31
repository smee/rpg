# c_variation_tests.R
#
# 2012 Oliver Flasch
#

dyn.load("random_utils.so"); e1 <- .Call("initialize_expression_grow_R", list("+", "-", "*", "/", "log"), c(2L, 2L, 2L, 2L, 1L), list("x1", "x2", "x3"), -1, 1, 1.0, 0.5, 1L); e1

dyn.load("random_utils.so"); e1 <- .Call("mutate_functions_R", quote(2*x1+log(3.14)), 0.5, list("+", "-", "*", "/", "log", "exp"), c(2L, 2L, 2L, 2L, 1L, 1L)); e1

dyn.load("random_utils.so"); e1 <- .Call("mutate_subtrees_R", quote(2*x1+log(3.14)), 1.0, 0.5, list("+", "-", "*", "/", "log"), c(2L, 2L, 2L, 2L, 1L), list("x1", "x2", "x3"), -1, 1, 1.0, 0.5, 2L); e1

