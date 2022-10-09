# When we introduced XPtrs we made the statement, that they are bound to their
# current session.
# Based on:
# https://stackoverflow.com/questions/53132155/keep-xptr-for-multiple-sessions
# we investigate now, how to prevail XPtr for multiple sesssions.
# 
# A C++ object managed by Rcpp::Xptr is destroyed when the R session ends. If
# you want to save the object, you have to `serialize` it.
# 
# One approach is is offered by the Rcereal package.
# The advantage of serialization and the reverse procedure, deserialization, is
# a much cheaper processing than invoking a new construction based on attributes.
# -----------------------------------------------------------------------------

# NOTE: install `Rcereal` and `boost` to get access to their C++ libraries
# check: library(Rcereal), library(boost)
library(Rcpp)
sourceCpp("SHM_22_XPtr_Serialization_for_multiple_sessions.cpp")


## ------------------------------------
# create external pointer: the process takes 5 seconds, as heavy work is
# ssimulated
system.time(xptr <- create_XPtr_to_Primebase(42))

# retrieve data from external pointer
dereference_xptr(xptr)

# Based on retrieved data the class instance can be reconstructed. But be aware,
# the class construction may take again some time.
create_XPtr_to_Primebase(dereference_xptr(xptr)) # takes ... time ...

# a better approach is to serialize the object
path_to_file <- paste0(getwd(), "/Backend/")
if (!dir.exists(path_to_file)) { dir.create(path_to_file) }
backing_file <- "Primebase.cereal"
full_path <- paste0(path_to_file, backing_file)


# now we serialize the obj
serialize_Obj(xptr, full_path)
rm(xptr)
exists("xptr")
gc()

# deserialize object
xptr <- deserialize_Obj(full_path)
exists("xptr")
dereference_xptr(xptr)
## ------------------------------------


## Open another R session and continue with this part
## ------------------------------------------------------------
library(Rcpp)
sourceCpp("SHM_22_XPtr_Serialization_for_multiple_sessions.cpp")
xptr <- deserialize_Obj("C:/Users/fabia/Documents/R/Shared Memory/Backend/Primebase.cereal")
# we have access to the pointer from another session
exists("xptr")
# we have access to the data from another session
dereference_xptr(xptr)
q()
## ------------------------------------------------------------

# And thanks to the file backing, we can restore the object anytime, even after
# closing all involved R sessions and open again.


