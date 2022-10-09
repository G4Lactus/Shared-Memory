library(Rcpp)
sourceCpp("SHM_25_Class_with_XPtr_and_Rcereal.cpp")


# create external pointer: the process takes 5 seconds (heavy work)
system.time(xptr <- create_XPtr_to_Primebase(42))

# retrieve data from external pointer
dereference_xptr(xptr)

# Based on retrieved data the class instance can be reconstructed. But be aware,
# the class construction may take again some time.
create_XPtr_to_Primebase(dereference_xptr(xptr)) # takes ... time ...

# a better approach is to serialize the object
path_to_file <- paste0(getwd(), "/Backend/")
if (!dir.exists(path_to_file)) { dir.create(path_to_file) }
backing_file <- "Primebase_with_smartPointer.cereal"
full_path <- paste0(path_to_file, backing_file)