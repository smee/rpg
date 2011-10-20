## simple_code_gen_test.r
##   - Functions for testing the simple x86 code generator
##
## RGP - a GP system for R
## 2010-2011 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

dyn.load("simple_code_gen.so")


x86CodeGen <- function(doubleConstant) .Call("x86CodeGen", doubleConstant)

executeX86Code <- function(x86Code, argument) .Call("executeX86Code", x86Code, argument)
