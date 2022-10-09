# Big Memory
# -----------------------------------------------------------------------------
# Multi-gigabyte data sets challenge and frustrate users, even on well-equipped
# hardware. Use of C/C++ can provide efficiency, but is cumbersome for
# interactive data analysis and lacks the flexibility and power of R’s rich
# statistical programming environment.
# 
# The big.memory data structure may be allocated to shared memory, allowing
# separate processes on the same computer to share access to a single copy of
# the data set. 
# The data structures may also be file-backed, allowing users to easily
# manage and analyze data sets larger than available RAM and share them across
# nodes of a cluster.
# 
# These features of the big.memory Project open the door for powerful and
# memory-efficient parallel analyses and data mining of massive data sets.
# 
# big.memory and its associated packages are still actively developed, but
# can be viewed as "stable".
# 
# Associated packages:
# --------------------
# - biganalytics
# - synchronicity
# - bigtabulate
# - bigalgebra
# - bigX ....

# -----------------------------------------------------------------------------


# Memory considerations
# -----------------------------------------------------------------------------
# The big.matrix is managed outside the R memory pool available to the garbage
# collector and the memory occupied by the big.matrix is not visible to R.
# 
# - Memory usage is not visible via general R functions (s.a., gc())
# 
# - Garbage collector is mislead by the very small memory footprint of the
#   big.matrix object, which can result in much less eagerness to 
#   garbage-collect to unused big.memory objects.
# 
# - after removing a last reference to a big big.matrix, the user should run
#   gc() to reclaim the memory
#
# - attaching the description of already finalized big.matrix and accessing this
#   object will result in undefined behavior, it will crash the current R
#   session with no hope of saving the data in it.
# 
# - To prevent this the user should keep at least one big.memory somewhere in
#   R memory in at least one R session on the current machine
# 
# - Abruptly closing R (e.g., using Task Manager) will not have a chance to 
#   finalize the big.matrix objects, which will result in a memory leak, as the
#   big.matrices will remain in the memory
#
#
# Available options
# ----------------------------------------
# - options(bigmemory.typecast.warning()) can be set to avoid warnings, for
#   example, you assign object (typically type double)
# 
# - assign objects to char, short or integer, double big.matrix objs
# 
# - options (bigmemory.print.warning) protects against extracting and printing
#   a massive matrix
# 
# - options (bigmemory.allow.dimnames) prevents the setting of dimnames
#   attributes, because they aren't allocated to shared memory and changes will
#   not be visible across processes.
# 
# - options (bigmemory.default.type) is double by default, up to change by user
# ------------------------------



# Demo 1: Scenario, one R-Session running
# -----------------------------------------------------------------------------
library(bigmemory)


# creating a big.matrix obj in R on the heap [dynamic RAM] not on the disk
# ------------------------------------------------------------------------
big_mtx <- big.matrix(nrow = 5, ncol = 2, 
                      type = "integer", # double, bool alternatives, int default
                      init = 0,
                      dimnames = list(c("1", "2", "b", "c", "_"), # rownames
                                      c("alpha", "beta")          # colnames
                                      )
                     )
# NOTE: this is a temporary obj and will disappear when the R session is closed

# big_mtx is a pointer to the memory address
big_mtx

# de-referencing the pointer and working with it
big_mtx[1:2,]
big_mtx[1:5,]
big_mtx[, 1]
big_mtx[, "alpha"]
big_mtx[, "beta"]
big_mtx[, "alpha"] <- 1:5
big_mtx[,]
colnames(big_mtx)
rownames(big_mtx)
options(bigmemory.allow.dimnames = TRUE) # default
colnames(big_mtx) <- NULL
big_mtx[,]


# Create a big.matrix from a matrix or vector or data.frame
# -------------------------------------------------------------------------
# - a vector will result in a big.matrix with one column
# - a data frame will have character vectors converted to factors
# - all factors converted to numeric factor levels
# - all labels or character values will be lost
# 
signature(big_mtx = "matrix")
signature(big_mtx = "vector")
signature(big_mtx = "data.frame")



# Convert back to R matrix: as.matrix, big.matrix-method
# -------------------------------------------------------------------------
# Extract values from a big.matrix object and convert to a base R object
# NOTE: this has the potential to kill your RAM, if your big matrix is enormous
# 
# S4 method for signature "big.matrix"
x_Rmat <- as.matrix(big_mtx)
class(big_mtx)
class(x_Rmat)
showClass("big.matrix")


# big.matrix: The core operations
# -----------------------------------------------------------------------------
# create new big.matrix obj
# -------------------------------
# big.matrix(
#   nrow,
#   ncol,
#   type = options()$bigmemory.default.type,
#   init = NULL,
#   dimnames = NULL,
#   separated = FALSE,
#   backingfile = NULL,
#   backingpath = NULL,
#   descriptorfile = NULL,
#   binarydescriptor = FALSE,
#   shared = options()$bigmemory.default.shared
# )
# 
# 
# convert to big.matrix obj
# -------------------------------
# as.big.matrix(
#   x,
#   type = NULL,
#   separted = FALSE,
#   backingfile = NULL,
#   backingpath = NULL,
#   descriptorfile = NULL,
#   binarydescriptor = FALSE,
#   shared = options()$bigmemory.default.shared
# )
# 
# 
# load filebacked big.matrix obj
# -------------------------------
# filebacked.big.matrix(
#   nrow,
#   ncol,
#   type = options()$bigmemory.default.type,
#   init = NULL,
#   dimnames = NULL,
#   separeted = FALSE,
#   backingfile = NULL,
#   backingpath = NULL,
#   descriptorfile = NULL,
#   binarydescriptor = FALSE
# )
# 


# checker methods
# -------------------------------
is.big.matrix(big_mtx)                  # check data structure 
is.separated(big_mtx)                   # separated column organization?
shared.name(big_mtx)
is.shared(big_mtx)                      # TRUE default, TRUE when backend file
is.readonly(big_mtx)                    # for not shared big mtx objs
is.nil(address = big_mtx@address)
GetMatrixSize(big_mtx)                  # bytes
bigmemory::head(big_mtx)
bigmemory::tail(big_mtx, n = 2)

# sub-setting alternative that create a new big matrix obj, i.e., pointer to a
# sub matrix
# --------------
sub.big.matrix(
  big_mtx,
  firstRow = 1,
  lastRow = 3,
  firstCol = 1,
  lastCol = NULL,
  backingpath = NULL
)

# assign pointer
big_sub_mtx <- sub.big.matrix(
  big_mtx,
  firstRow = 1,
  lastRow = 3,
  firstCol = 1,
  lastCol = NULL,
  backingpath = NULL
)

# de-reference pointer
big_sub_mtx[,]

# dimensions of a big.matrix obj
dim(big_mtx)
dim(big_sub_mtx)

dimnames(big_mtx)
dimnames(big_sub_mtx)

# extract or replace big.matrix elements
big_sub_mtx[,]
big_sub_mtx[1, 1]
big_sub_mtx[1, , drop=TRUE]
big_sub_mtx[,]

# for more queries -> see demo 4 in this file


# check if big.matrix obj is file backed -> see demo 3 for more information
if (is.filebacked(big_mtx)) {
  file.name(big_mtx)  
  dir.name(big_mtx)
} else { message("Your big.matrix obj is not file backed and lives in the heap.") }
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------




# Demo 2: Scenario, two R-Sessions running
# -----------------------------------------------------------------------------
# We create another big.matrix obj big_mtx_z, and pass its "description" to
# another R Session via SNOW, foreach, or even a simple file read/write.
# Then "attach.big.matrix()" within the second R session grants access.
# -------------------------
library(bigmemory)
big_mtx_z <- big.matrix(nrow = 3, ncol = 3, type = 'double', init = 42)
big_mtx_z[,]
is.filebacked(big_mtx_z)

# describe: The basic "big.matrix" operations for sharing and re-attaching
# ----------------------------------------------------
# returns the information needed by attach.big.matrix to reference a shared or
# file-backed big.matrix object.
# The attach.big.matrix and attach.resource functions create a new big.matrix
# object based on the descriptor information referencing previously allocated
# shared-memory or file-backed matrices.
#------------------------------------------
big_mtx_z_description <- describe(big_mtx_z)


path_to_backend <- paste0(getwd(), "/Backend/")
if (!dir.exists(path_to_backend)) {
  dir.create(path_to_backend)
}
dput(big_mtx_z_description, file = paste0(path_to_backend, "big_mtx_z.desc"))

# Our matrix lives now in shared heap memory as long as it is attached with
# attach.big.matrix in at least one session (shared pointer mechanics).


####
# Open another R session and execute this code snippet. The other R session
# needs access to a network node's RAM, in the case of HPC computing.
##### -------------------------------------------------------------------------
library(bigmemory)
# my computer: "C:/Users/fabia/Documents/R/Shared Memory/Backend/big_mtx_z_description_signature.desc"
path_to_desc_file <- "C:/Users/fabia/Documents/R/Shared Memory/Backend/big_mtx_z_description_signature.desc"
big_mtx_y <- attach.big.matrix(obj = path_to_desc_file)
big_mtx_y    # the pointer to the obj
big_mtx_y[,] # the de-referenced pointer
shared.name(big_mtx_y)


# make a deep copy and create a new big.matrix obj
# ----------------------------------------------------
# deepcopy(
#   x,
#   cols = NULL,
#   rows = NULL,
#   y = NULL,
#   type = NULL,
#   separated = NULL,
#   backingfile = NULL,
#   backingpath = NULL,
#   descriptorfile = NULL,
#   binarydescriptor = FALSE,
#   shared = options()$bigmemory.default.shared
# )
# ----------
big_mtx_yy <- deepcopy(big_mtx_y,
                       cols = -1  # exclude first column
                       )

# compare the pointers and see that the obj are different
big_mtx_y
big_mtx_yy


# try a change
# ---------------
# be careful with data type castings, when you assign new values. If the mtx
# obj is integer, a new value is type casted.
big_mtx_y[1,1] <- -100.0 
big_mtx_y[,]

q() # end session
##### -------------------------------------------------------------------------


# Back in our original session
# ----------------------------
big_mtx_z
big_mtx_z[,] # the matrix has changed
q()          # NOTE: the ".desc" file remains on hard drive, except you place
             # it into a temporary directory that is self destroyed by closing
             # the R session.
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------




# Demo 3: File-backing
# -----------------------------------------------------------------------------
# Having big.matrix objs in shared memory is nice, but some data files exceed
# the RAM. Here file backing comes to the rescue and even allow exceeding the
# virtual memory in size.
# -----------------------
library(bigmemory)

# create a directory for your file-backing
temp_dir <- tempdir() # create a temporary directory for file-backing on system

if (!dir.exists(path.expand(temp_dir))) { dir.create(path.expand(temp_dir)) }

# create a file backed big.matrix obj
big_mtx_q <- big.matrix(
                nrow = 3,
                ncol = 3,
                init = 42,
                type = "integer",
                dimnames = list(c("row_nm1", "row_nm2", "row_nm3"),
                                c("col_nm1", "col_nm2", "col_nm3")
                            ),
                separated = FALSE,
                backingfile = "big_mtx_q_flushtest.bk",
                backingpath = temp_dir,
                descriptorfile = "big_mtx_q_flushtest.desc",
                binarydescriptor = FALSE,
               )


# modify big.matrix obj
big_mtx_q[1, 1] <- 0L
big_mtx_q[,]

# flush, Updating a big.matrix file-backing
# ------------------------------------------
# For a file-backed big.matrix object, flush() forces any modified information
# to be written to the file-backing.
flush(con = big_mtx_q)
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------




# Demo 4: Operations on big matrix
# -----------------------------------------------------------------------------
library(bigmemory)
big_mtx_x <- big.matrix(nrow = 10, 
                        ncol = 5,
                        init = 0, 
                        type ="double",
                       )
big_mtx_x[,] = 1:50
big_mtx_x[,]
# NOTE: big matrices are column major oriented
big_mtx_y <- sub.big.matrix(big_mtx_x, 
                            firstRow = 2, 
                            lastRow  = 9, 
                            firstCol = 2, 
                            lastCol  = 3
                            )
big_mtx_y[,]


# is.sub.big.matrix
# ------------------------------------------
# doesn't create a copy, it just provides a new version of the class which
# provides behavior for a contiguous submatrix of the big.matrix.
# Non-continguous submatrices are not supported.
# --------------------------
is.sub.big.matrix(big_mtx_x)  # FALSE
is.sub.big.matrix(big_mtx_y)  # TRUE

# note: the following drops an error -> use sub.big.matrix() 
is.sub.big.matrix(big_mtx_x[1:2, ]) # ERROR: no big.matrix
is.sub.big.matrix(sub.big.matrix(big_mtx_x, firstRow = 1, lastRow = 2))

# length of a big.matrix object
length(big_mtx_x)
length(big_mtx_y)
length(big_mtx_x[,1])


# morder (multi-column ordering))
# ------------------------------------------
# ordering and permuting functions for 'big.matrix' and matrix objects
# to rearrange an object according to the values in the specified columns
m <- matrix(data = as.double(as.matrix(iris)),
            nrow = nrow(iris)
           )
head(m)

# classic ordering in R
order(m[, 1]) # row indices to order according to first column
head(m[order(m[, 1]), ])

# ordering with bigmemory morder
head(m[morder(m, 1), ])


# permutation with mpermute: pipeline for upper ordering
# NOTE: operation is pass-by-reference passed and overwrite original obj
# ------------------------------------------------------
mpermute(m, cols = 1)
head(m)


# mwhich (multi-col/row which)
# ------------------------------------------
# implements which-like functionality for a big.matrix, with additional options
# for efficient comparisons based on C++ code.
# -------------------------
big_mtx_x <-  as.big.matrix(matrix(1:30, 10, 3))
big_mtx_x[,]
options(bigmemory.allow.dimnames = TRUE)
colnames(big_mtx_x) <- c("A", "B", "C")
big_mtx_x[,]

# query big.matrix obj: returns the row indices satisfying the comparison
big_mtx_ids <- big_mtx_x[mwhich(big_mtx_x, 
                 cols = 1:2,  # set columns for query
                 vals = list( # set limits for operation
                          c(2, 3), 
                          c(11, 17)
                         ), 
         comps = list(c("ge", "le"),  # set ops: greater, less
                      c("gt", "lt")   # set ops: greater than, less than
                      ),
         op = "OR"                    # set fuse operation
         )]
big_mtx_x[big_mtx_ids,]
is.big.matrix(big_mtx_x[big_mtx_ids,])
big_sub_mtx_x <- sub.big.matrix(big_mtx_x, 
               firstRow = big_mtx_ids[1], lastRow = big_mtx_ids[5])
is.big.matrix(big_sub_mtx_x)               
big_sub_mtx_x[,]


# try more queries
y[mwhich(y, 1:2, list(c(2, 3), c(11, 17)),
         list(c("ge", "le"), c("gt", "lt")), "OR"),
  ]

y[mwhich(y, -3, list(c(2, 3), c(11, 17)),
         list(c("ge", "le"), c("gt", "lt")), "AND"),
  ]


big_mtx_x[,]
big_mtx_x[1, 1] <- NA
big_mtx_x[c(5, 10), c(2, 3)] <- NA
big_mtx_x[,]

# return row indices where NA occurs
mwhich(big_mtx_x, 1:2, NA, "eq", "OR")    # rows with NA
mwhich(big_mtx_x, 1:2, NA, "neq", "AND")  # rows without NA

# Column 1 equal to 4 and/or column 2 less than or equal to 16:
mwhich(big_mtx_x, cols = 1:2, list(4, 16), list('eq', 'le'), 'OR')
mwhich(big_mtx_x, cols = 1:2, list(4, 16), list('eq', 'le'), 'AND')

# Column 2 less than or equal to 15:
mwhich(big_mtx_x, 2, 15, 'le')

# No NAs in either column 1 and 2, and column 2 strictly less than 15:
mwhich(big_mtx_x, cols = c(1:2, 2), 
       list(NA, NA, 15), 
       list('neq', 'neq', 'lt'), 'AND')


big_mtx_x <- big.matrix(4, 2, init=1, type="double")
big_mtx_x[1,1] <- Inf   # NOTE: Inf is a double literal
mwhich(big_mtx_x, 1, Inf, 'eq')
mwhich(big_mtx_x, 1, 1, 'gt')
mwhich(big_mtx_x, 1, 1, 'le')

# ncol
bigmemory::ncol(big_mtx_x)
big_mtx_x[,]

# print
print(big_mtx_x)

# typeof
typeof(big_mtx_x)
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------




# Demo 5: Write and Read big.matrix objs to/from hard drive memory
# -----------------------------------------------------------------------------
library(bigmemory)
big_mtx_x <- as.big.matrix(matrix(1:10, 5, 2))
big_mtx_x[2, 2] <- NA
big_mtx_x[,]

temp_dir <- path.expand(tempdir()) 
if (!dir.exists(temp_dir)) {dir.create(temp_dir)}

# write.big.matrix
# ------------------------------------------
# Create a big.matrix by reading from a suitable-formatted ASCII file, or 
# write the contents of a big.matrix to a file.
write.big.matrix(big_mtx_x, file.path(temp_dir, "foo.txt"))

# Just for fun, read it back in
# --------------------------------------------------
# read.big.matrix(
#   filename,
#   sep = ",",
#   header = FALSE,
#   col.names = NULL,
#   row.name = NULL,
#   has.row.names = FALSE,
#   ignore.row.names = FALSE,
#   type = NA,
#   skip = 0,
#   separated = FALSE,
#   backingfile = NULL,
#   backingpath = NULL,
#   descriptorfile = NULL,
#   binarydescriptor = FALSE,
#   extraCols = NULL,
#   shared = options()$bigmemory.default.shared
# )
# --------------------------------------------------
big_mtx_y <- read.big.matrix(file.path(temp_dir, "foo.txt"), type = "char")
big_mtx_y[,]


# other examples
# --------------------------------------------------
big_mtx_w <- as.big.matrix(matrix(1:10, 5, 2), type = "double")
big_mtx_w[,]
big_mtx_w[1, 2] <- NA
big_mtx_w[2, 2] <- -Inf
big_mtx_w[3, 2] <- Inf
big_mtx_w[4, 2] <- NaN
big_mtx_w[,]
write.big.matrix(big_mtx_w, file.path(temp_dir, "bar.txt"))

big_mtx_w <- read.big.matrix(file.path(temp_dir, "bar.txt"), type="double")
big_mtx_w[,]

big_mtx_w <- read.big.matrix(file.path(temp_dir, "bar.txt"), type="short")
big_mtx_w[,]


# Another example using row names
# -------------------------------------------------------
big_mtx_w <- as.big.matrix(as.matrix(iris), type = "double")
rownames(big_mtx_w) <- as.character(1:nrow(big_mtx_w))
head(big_mtx_w)

write.big.matrix(big_mtx_w, file.path(temp_dir, "IrisData.txt"), 
                 col.names = TRUE, 
                 row.names = TRUE
                 )

big_mtx_r <- read.big.matrix(file.path(temp_dir, "IrisData.txt"),
                     header=TRUE,
                     has.row.names=TRUE)
# check for identity
identical(big_mtx_w, big_mtx_r)        # FALSE: ptr are of course not the same!
identical(big_mtx_w[,], big_mtx_r[,])  # TRUE: data are of course the same!
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------