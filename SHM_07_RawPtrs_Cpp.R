#' This tutorial explores how C++ pointers intereact with R objects via Rcpp.
#' Two main principles under investigation:
#'  1. How modifications in C++ are returned to R.
#'  2. The dangers of manual memory allocation ('new') in C++.
#'
#' Main insight: Returning a raw pointer to R fails, as it is no SEXP 
#'  (S expression), and such code does not compile!
#' Either you dereference the pointer or you make an XPtr and return it to R.
# ------------------------------------------------------------------------------
#'
library(Rcpp)
sourceCpp("SHM_07_RawPtrs_Cpp.cpp")

#' Setup
#' Create our original matrix in R
set.seed(42)
A_orig <- matrix(rnorm(12), nrow = 4, ncol = 3)
print(A_orig)



#' Experiment 1: Modifying by reference and returning a copy
# ------------------------------------------------------------------------
cat("\nExperiment 1:\n")
#' Call the C++ function that modifies the object it receives per reference
#' and returns the result
B_mod <- simulate_pointer_behavior(A_orig, 1)

#' Result:
cat("Original 'A_orig' is UNCHANGED:\n")
print(A_orig)
cat("The returned matrix 'B_mod' contains the new data:\n")
print(B_mod)
#' The C++ code created a copy and changed the copied version.
#' The output is a completely new object, the original data 'A_orig' remain.



#' Experiment 2: Modifying via a reference pointer
# ------------------------------------------------------------------------
cat("\n Experiment 2:\n")
#' Call the C++ function that copies `A_orig` data to a NEW matrix
#'  on the heap, modifies it, and returns the copied and modified content.
C_mod <- simulate_pointer_behavior(A_orig, 2)

#' Result:
cat("Original 'A_orig' is UNCHANGED:\n")
print(A_orig)
cat("The returned matrix 'C_mod' contains the modified data:\n")
print(C_mod)
#' Our C++ function simulate_pointer_behavior returns a *copy* of
#'  the modified data as a new R object. The original R matrix is
#'  not modified in place.
