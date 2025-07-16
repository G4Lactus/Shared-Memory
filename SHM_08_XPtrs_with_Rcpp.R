#' Illustration of XPtrs and Armadillo matrices arma::mat
# -----------------------------------------------------------------------------
#' C++ raw pointers cannot be returned to R directly.
#' Instead, we rely on Rcpp's External pointers (XPtr).
#' Those allow to export C++ heap-allocated structures (e.g., matrices/vectors)
#' to R as opaque objects. They persist in the R session and circumvent R's
#' garbage collector for as long as the reference exist.
#'
#' This tutorial demonstrates how to use Rcpp's external pointers (`XPtr`) to
#' create C++ objects on the heap and safely reference them from R.
#' This allows data to persist throughout an R session without being
#' managed by R's garbage collector.
#'
#' The full story is here:
#' https://lists.r-forge.r-project.org/pipermail/rcpp-devel/2015-June/008806.html
#'
#' The documentation of XPtr constructs:
#' https://dirk.eddelbuettel.com/code/rcpp/html/classRcpp_1_1XPtr.html
#'
#' Also see:
#' http://arma.sourceforge.net/docs.html
#' for constructor information of Armadillo matrices from pointers
# -----------------------------------------------------------------------------

library(Rcpp)
sourceCpp(file = "SHM_08_XPtrs_with_Rcpp.cpp")


#' Create some toy data
set.seed(42)
A <- matrix(rnorm(50), nrow = 5)
print(A)


#' Demo 1: The Safe Way - Creating an "Owning" XPtr
# -----------------------------------------------------------------------------
#' The best practice is to create an XPtr that points to a self-contained C++
#' object that OWNS its own memory, using `aux_mem = TRUE`.
cat("\n--- Demo 1: Creating a safe, self-contained XPtr ---\n")

# We use the recommended C++ function signature (`arma::mat&`).
# `aux_mem = TRUE` tells Armadillo to allocate new memory and COPY the data.
safe_xptr <- export_mat_to_XPtr_reference(A, aux_mem = TRUE)

# The `safe_xptr` can now be used reliably throughout our R session.
# We can retrieve the data to confirm it's a perfect copy of the original.
retrieved_data <- retrieve_mat_from_XPtr(safe_xptr)
cat("Data retrieved from safe_xptr is identical to original A:",
    identical(A, retrieved_data), "\n")



#' Demo 2: The Dangerous Way - Creating a "Viewing" XPtr
# ------------------------------------------------------------------------------
#' If we use `aux_mem = FALSE`, the C++ object does NOT own the data. Instead,
#' it just holds a raw pointer to the memory of the original R object `A`.
cat("\n--- Demo 2: Creating a dangerous, non-owning XPtr ---\n")
dangerous_xptr <- export_mat_to_XPtr_reference(A, aux_mem = FALSE)

#' **WARNING:** `dangerous_xptr` is a time bomb. It works only as long as the
#' original R object `A` exists and hasn't been moved in memory by R's
#' garbage collector. If `A` is removed, `dangerous_xptr` becomes a
#' "dangling pointer," and using it will crash your R session.
#'
#' DO NOT RUN THIS UNLESS YOU WANT TO CRASH R:
if (FALSE) {
  rm(A)
  gc()
  retrieve_mat_from_XPtr(dangerous_xptr) # This can cause a fatal error.
}



#' Demo 3: Using a Safe XPtr to Modify Data in C++
# -----------------------------------------------------------------------------
#' The real gain of XPtrs is passing them back to C++ to work with persistent
#'  data.
cat("\n--- Demo 3: Passing the safe XPtr back to C++ for modification ---\n")
cat("Data in safe_xptr before modification:\n")
print(retrieve_mat_from_XPtr(safe_xptr))

# Pass the safe pointer to a function that modifies the C++ object
#  in-place.
work_with_xptr_stored_data(safe_xptr)

# Retrieve the data again to see the changes.
cat("\nData in safe_xptr AFTER modification in C++:\n")
modified_data <- retrieve_mat_from_XPtr(safe_xptr)
print(modified_data)



#' Demo 4: Creating an XPtr from a C++ Native Object
# -----------------------------------------------------------------------------
#' We can also create data entirely within C++ and return an XPtr to it,
#' keeping it alive in R without ever creating a full R object.
cat("\n--- Demo 4: Creating a C++ std::vector and returning an XPtr ---\n")

# This C++ function allocates a `std::vector` on the heap and returns an XPtr.
cpp_vec_xptr <- export_Cpp_vec_to_R()

# Retrieve its contents
retrieved_vec <- retrieve_vec_from_xptr(cpp_vec_xptr)
print(retrieved_vec)
