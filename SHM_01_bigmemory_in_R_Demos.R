#' Big Memory Tutorial
#' -----------------------------------------------------------------------------
#' Multi-gigabyte data sets challenge and frustrate users, even on well-equipped
#' hardware. Use of C/C++ can provide efficiency, but is cumbersome for
#' interactive data analysis and lacks the flexibility and power of R’s rich
#' statistical programming environment.
#'
#' The `bigmemory` package and its `big.matrix` data structure may be allocated
#' to shared memory, allowing separate processes on the same computer to share
#' access to a single copy of the data set.
#' The data structures may also be file-backed, allowing users to easily
#' manage and analyze data sets larger than available RAM and share them across
#' nodes of a cluster.
#'
#' These features of the `bigmemory` project open the door for powerful and
#' memory-efficient parallel analyses and data mining of massive data sets.
#'
#' `bigmemory` and its associated packages are still actively developed, but
#' can be viewed as "stable".
#'
#' **Associated packages**:
#' ------------------------
#' - `biganalytics`
#' - `synchronicity`
#' - `bigtabulate`
#' - `bigalgebra`



#' Memory Management with `big.matrix`
#' -----------------------------------------------------------------------------
#' A `big.matrix` object as provided by the `bigmemory` package is managed
#' outside the R memory pool available to the garbage collector, and the memory
#' occupied by the `big.matrix` is not visible to R.
#'
#' - Memory usage is not visible via general R functions (for example for gc()).
#'
#' - The Garbage collector is misled by the very small memory footprint of the
#'   `big.matrix` object, which can result in much less eagerness to
#'   garbage-collect to unused `big.memory` objects.
#'
#' - After removing a last reference to a `big.matrix`, the user should run
#'   gc() to reclaim the memory.
#'
#' - Attaching the description of an already-finalized `big.matrix` and
#'   accessing this object will result in undefined behavior; it will crash the
#'   current R session with no hope of saving the data in it.
#'
#' - To prevent this the user should keep at least one `big.matrix` somewhere in
#'   R memory in at least one R session on the current machine
#'
#' - Abruptly closing R (e.g., using the Task Manager) will not allow for the
#'   finalization of `big.matrix` objects, resulting in a memory leak as the
#'   matrices will remain in memory
#'
#'
#' Available options
#' ----------------------------------------
#' - `options(bigmemory.typecast.warning = FALSE)` can suppress warnings when
#'   assigning data that requires typecasting (e.g., assigning a double to an
#'   integer `big.matrix`).
#'
#' - `options(bigmemory.print.warning = TRUE)` protects against accidentally
#'   printing a massive matrix, which would create a second large copy in
#'   memory.
#'
#' - `options(bigmemory.allow.dimnames = FALSE)` prevents setting `dimnames`
#'   attributes. This is because they are not allocated to shared memory, so
#'   changes will not be visible across different processes.
#'
#' - `options(bigmemory.default.type = "double")` sets the default data type
#'   for new matrices. This can be changed by the user to "integer", "short",
#'   or "char".


#' ** Demo 1: Scenario, set up a bigmatrix object in one R-Session running **
# ---------------------------------------------------------------------------
library(bigmemory)

# Create an in-memory `big.matrix` (i.e., on the heap, not file-backed)
big_mtx <- bigmemory::big.matrix(
  nrow = 5,
  ncol = 2,
  type = "integer",
  init = 0,
  dimnames = list(c("1", "2", "b", "c", "_"), # rownames
    c("alpha", "beta")          # colnames
  )
)
# NOTE: This is a temporary object and will be destroyed when the R session
#  is closed.

# The `big_mtx` is a pointer to the memory address
big_mtx

#' ** Access and Modify Data **
#' Dereference the pointer to access and modify the data at the R level
big_mtx[1:2, ]
big_mtx[1:5, ]
big_mtx[, 1]
big_mtx[, "alpha"]
big_mtx[, "beta"]
big_mtx[, "alpha"] <- 1:5
big_mtx[, ]


#' ** Understanding Dimnames **
#' Dimnames are an attribute of the R pointer object, not the C++ data
#' structure. Changes are local to the current R session and will not be
#' seen by other processes sharing the same memory.
colnames(big_mtx)
rownames(big_mtx)
#' Enable dimnames
options(bigmemory.allow.dimnames = TRUE) # default
colnames(big_mtx) <- NULL
big_mtx[, ]


#' ** Create a `big.matrix` from a matrix or vector or data.frame **
#' - a vector will result in a `big.matrix` with one column
#' - a data frame will have character vectors converted to factors
#' - all factors converted to numeric factor levels
#' - all labels or character values will be lost
signature(big_mtx = "matrix")
signature(big_mtx = "vector")
signature(big_mtx = "data.frame")


#' ** Convert back to R matrix: as.matrix, big.matrix-method **
#' Extract values from a big.matrix object and convert to a base R object
#' NOTE: this has the potential to kill your RAM, if your big matrix is enormous
class(big_mtx)
x_mat <- bigmemory::as.matrix(big_mtx)
class(x_mat)
showClass("big.matrix")


#' ** big.matrix: The core operations **
#'
#' showcasing the `big.matrix` constructor
print(args(bigmemory::big.matrix))

#' convert to big.matrix obj
print(args(bigmemory::as.big.matrix))

#' load filebacked big.matrix obj
print(args(bigmemory::filebacked.big.matrix))

#' checker methods
bigmemory::is.big.matrix(big_mtx)  # check data structure
bigmemory::is.separated(big_mtx)   # separated column organization?
bigmemory::shared.name(big_mtx)
bigmemory::is.shared(big_mtx)       # TRUE default, TRUE when backend file
bigmemory::is.readonly(big_mtx)     # for not shared big mtx objs
bigmemory::is.nil(address = big_mtx@address)
bigmemory::GetMatrixSize(big_mtx)   # bytes
bigmemory::head(big_mtx)
bigmemory::tail(big_mtx, n = 2)


#' sub-setting alternative creates a new big matrix obj
big_sub_mtx <- bigmemory::sub.big.matrix(
  big_mtx,
  firstRow = 1,
  lastRow = 3,
  firstCol = 1,
  lastCol = NULL,
  backingpath = NULL
)

#' de-reference pointer
big_sub_mtx[, ]

#' dimensions of a big.matrix obj
dim(big_mtx)
dim(big_sub_mtx)
dimnames(big_mtx)
dimnames(big_sub_mtx)

#' extract or replace big.matrix elements
big_sub_mtx[, ]
big_sub_mtx[1, 1]
big_sub_mtx_1strow <- big_sub_mtx[1, , drop = TRUE]
# for more queries -> see demo 4 in this file


#' check if big.matrix obj is file backed -> see demo 3 for more information
if (is.filebacked(big_mtx)) {
  file.name(big_mtx)
  dir.name(big_mtx)
} else {
  message("Your big.matrix obj is not file backed and lives in the heap.")
}

#' ** Create a `big.matrix` from other R objects **
#' The `as.big.matrix()` function can convert standard R matrices, vectors, 
#' and data frames.
#' - A **vector** will create a `big.matrix` with a single column.
#' - A **data.frame** requires character columns to be converted to
#'   factors, and then all columns are coerced to a single numeric type.

#' Example with a standard R matrix
regular_mtx <- matrix(1:10, 5, 2)
bigm_from_mtx <- as.big.matrix(regular_mtx)
bigm_from_mtx[, ]

#' Example with a data.frame
regular_df <- data.frame(a = 1:5, b = letters[1:5])
bigm_from_df <- as.big.matrix(regular_df)
bigm_from_df[, ] # the column 'b' has been converted to numbers

# -----------------------------------------------------------------------------



#' ** Demo 2: Scenario, two R-Sessions running **
# -----------------------------------------------------------------------------
#' To access a `big.matrix` object in another R session, we first need to
#'  create a "description" file. This file contains the metadata that allows
#'  another process to find and attach to the matrix in shared memory.
#'
library(bigmemory)

#' Create the `big.matrix` in the first R session.
big_mtx_z <- big.matrix(
  nrow = 3,
  ncol = 3,
  type = "double",
  init = 42
)
big_mtx_z[, ]
is.filebacked(big_mtx_z) # FALSE, still no file backing, in-memory heap object


#' ** Create and Save the Descriptor **
#' The `describe()` function captures the information necessary to re-attach.
big_mtx_z_description <- describe(big_mtx_z)

#' ** Save the descriptor object ot a file on disk
path_to_backend <- file.path(getwd(), "Backend")
if (!dir.exists(path_to_backend)) {
  dir.create(path_to_backend)
}
dput(big_mtx_z_description, file = file.path(path_to_backend, "big_mtx_z.desc"))

#' NOTE: we are storing the *description* file on disk, not the matrix itself.
#' Currently, our matrix lives in shared heap memory, and endures as long as
#' at least one R session holds a connection to it (shared pointer mechanics).
#' In other sessions it can be attached with "attach.big.matrix".


# ----------------------------------------
#' ####
#' # Open another R session and execute this code snippet. The other R session
#' # needs access to a network node's RAM, in the case of HPC computing.
#' #####
library(bigmemory)

#' In the new session, attach the existing `big.matrix` object.
path_to_desc_file <- file.path(getwd(), "Backend", "big_mtx_z.desc")
big_mtx_y <- attach.big.matrix(obj = path_to_desc_file)
big_mtx_y       # the pointer to the obj
big_mtx_y[, ]   # the de-referenced pointer
shared.name(big_mtx_y)

#' make a deep copy and create a new big.matrix obj in heap memory
print(args(bigmemory::deepcopy))
big_mtx_yy <- deepcopy(
  big_mtx_y,
  type = "double",
  cols = -1  # exclude first column
)

#' compare the pointers and see that the obj are different
big_mtx_y
big_mtx_yy


#' make a change
#' be careful with data type castings, when you assign new values. If the mtx
#' obj is integer, and you assing a double the new value is casted.
big_mtx_y[1, 1] <- -100L # int to double
big_mtx_y[, ]
q() # end session
##### -------------------------------------------------------------------------


#' Back in our original session
big_mtx_z
big_mtx_z[, ] # the matrix has changed
q()
#' NOTE: the descriptor file ("big_mtx_z.desc") remains on disk.
#' For true temporary sharing, consider writing descriptor files to the
#' directory provided by `tempdir()`, which iis automatically cleaned up by R.
#'
# -----------------------------------------------------------------------------



#' ** Demo 3: File-backing **
#' -----------------------------------------------------------------------------
#' While in-memory shared objects are fast and inter-process operable, some
#' datasets are simply too large to fit in RAM.
#' File-backing is the solution, allowing `bigmemory` objects to be stored on
#' disk as form of virtual memory.
#' The physical boundary is now set by hard drive space.
#'
library(bigmemory)

#' ** Management of Backing Files **
#' When a file-backed matrix is created, two physical files are created on disk.
#' To keep track of them is crucial to prevent "memory leaks" on the hard drive.
#' Using a temporary directory via `tempdir()` is a safe approach for example,
#' as R cleans it up upon quitting the session properly.
temp_dir <- file.path(tempdir(), "bm_backing")
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir)
}

#' ** Create a File-Backed `big.matrix` **
#' By providing the `backingfile` and `backingpath` arguments,
#' the `big.matrix` function creates a disk-based object.
big_mtx_q <- bigmemory::big.matrix(
  nrow = 3,
  ncol = 3,
  init = 42,
  type = "integer",
  dimnames = list(
    c("row_nm1", "row_nm2", "row_nm3"),
    c("col_nm1", "col_nm2", "col_nm3")
  ),
  separated = FALSE,
  backingfile = "big_mtx_q.bin",
  backingpath = temp_dir,
  descriptorfile = "big_mtx_q.desc",
  binarydescriptor = FALSE,
)

#' Modify the data in memory
big_mtx_q[1, 1] <- 0L
big_mtx_q[, ]

#' ** `flush`: Saving Changes to Disk **
#' For a file-backed `big.matrix` object, flush() forces any modified
#'  information to be written to the file-backing.
bigmemory::flush(con = big_mtx_q)
# -----------------------------------------------------------------------------




#' Demo 4: Operations on big matrix
# -----------------------------------------------------------------------------
#' Beyond basic subsetting, `bigmemory` provides a suite of highly efficient,
#' C++ backed functions for common data manipulation tasks like creating
#' sub-matrices, ordering, and filtering.
library(bigmemory)

#' Create `big.matrix`
big_mtx_x <- bigmemory::big.matrix(
  nrow = 10,
  ncol = 5,
  init = 0,
  type = "double"
)
big_mtx_x[, ] <- 1:50
big_mtx_x[, ]

#' ** `sub.big.matrix`: Creating Views without Copying **
#' A `sub.big.matrix` creates a new `big.matrix` object that is a view
#' into a contiguous block of the original matrix. It does NOT copy the data,
#' making it an extremely fast and memory-efficient operation.
big_mtx_y <- bigmemory::sub.big.matrix(
  big_mtx_x,
  firstRow = 2, lastRow  = 9,
  firstCol = 2, lastCol  = 3
)
big_mtx_y[, ]


#' check if an object is a sub-matrix view
bigmemory::is.sub.big.matrix(big_mtx_x)  # FALSE
bigmemory::is.sub.big.matrix(big_mtx_y)  # TRUE

#' NOTE: Standard R subsetting `[]` creates a regular R matrix (a copy),
#' but not a `big.matrix` view.
#' The following correctly throws an error.
bigmemory::is.sub.big.matrix(big_mtx_x[1:2, ])
# ERROR: no big.matrix, its an R matrix

#' Contrary
bigmemory::is.sub.big.matrix(
  bigmemory::sub.big.matrix(big_mtx_x, firstRow = 1, lastRow = 2)
)

#' **`morder` and `mpermute`: Fast Multi-Column Ordering**
#' These functions provide high-speed ordering, operating directly on
#' the matrix data.
big_iris <- bigmemory::as.big.matrix(as.matrix(iris[, -5]))
big_iris[, ]

#' `morder()` returns the row indices to sort the matrix, similar to
#' R's `order()`
head(big_iris[bigmemory::morder(big_iris, cols = 1), ])

#' `mpermute()` is even more direct: it permutes the matrix **in-place**
#' (pass-by-reference), overwriting the original object.
#' No new memory is allocated
bigmemory::mpermute(big_iris, cols = 1) # This permanently reorders big_iris
head(big_iris[, ])

#' ** `mwhich`: Advanced, Multi-Condition Filtering **
#' A `which`-like function that can test for multiple conditions across
#' multiple columns simultaneously.
big_mtx_z <- bigmemory::as.big.matrix(matrix(1:30, 10, 3))
colnames(big_mtx_z) <- c("A", "B", "C")
big_mtx_z[c(1, 5, 10), c(1, 3)] <- NA
big_mtx_z[, ]

#' Example 1: Find rows where column A is between 2 and 4 (inclusive)
#'  OR column B is between 11 and 17 (exclusive)
row_indices <- bigmemory::mwhich(big_mtx_z,
  cols = 1:2,
  vals = list(c(2, 4), c(11, 17)),
  comps = list(c("ge", "le"), c("gt", "lt")),
  op = "OR"
)
big_mtx_z[row_indices, ]

#' Example 2: Find rows with no NAs in both columns A and B
na_free_inds <- bigmemory::mwhich(big_mtx_z,
  cols = 1:2,
  vals = NA,
  comps = "neq",
  op = "AND"
)
big_mtx_z[na_free_inds, ]

#' Example 3: Find rows where col A is 4 AND col B is less than or
#' equal to 16
complex_inds <- bigmemory::mwhich(big_mtx_z,
  cols = 1:2,
  vals = list(4, 16),
  comps = list("eq", "le"),
  op = "AND"
)
big_mtx_z[complex_inds, ]

# -----------------------------------------------------------------------------




#' ** Demo 5: Write and Read big.matrix objs to/from hard drive memory **
# -----------------------------------------------------------------------------
#' The `bigmemory` package provides functions to write a `big.matrix` to a
#' text file (.txt) and to create a `big.matrix` by from one.
#' This is useful for importing data or sharing with non-R users.
library(bigmemory)

#' Use a temporary directory for our example files
temp_dir <- tempdir()


#' ** Read a `big.matrix` **
big_mtx_a <- bigmemory::as.big.matrix(matrix(1:10, 5, 2))
big_mtx_a[2, 2] <- NA

#' Write to disk
bigmemory::write.big.matrix(big_mtx_a, file.path(temp_dir, "file_a.txt"))

#' When reading, the data type can be specified. For example "char",
#' which will convert the numbers to their character representations.
big_mtx_b <- bigmemory::read.big.matrix(
  filename = file.path(temp_dir, "file_a.txt"),
  type = "char"
)
big_mtx_b[, ]


#' ** Handling Special Values and Type Conversions **
#' In the following we demonstrate how special numeric values are handled
#' and what happens during aggressive type conversion.
big_mtx_w <- bigmemory::as.big.matrix(matrix(1:10, 5, 2), type = "double")
big_mtx_w[1, 2] <- NA
big_mtx_w[2, 2] <- -Inf
big_mtx_w[3, 2] <- Inf
big_mtx_w[4, 2] <- NaN
big_mtx_w[, ]

#' Write the `big.matrix` to disk, as `.txt` file
bigmemory::write.big.matrix(big_mtx_w, file.path(temp_dir, "file_w.txt"))

#' Reading back as "short", which forces conversion.
#' `Inf`, `-Inf`, and `NaN` cannot be represented as short integers and become
#' `NA`.
big_mtx_w_short <- bigmemory::read.big.matrix(
  file.path(temp_dir, "file_w.txt"),
  type = "short"
)
big_mtx_w_short[, ]


#' ** Working with Headers and Row Names **
#' The `write.big.matrix` and `read.big.matrix` functions can handle standard
#' file formats with column headers and row names.
iris_numeric <- as.matrix(iris[, 1:4])
big_iris_a <- bigmemory::as.big.matrix(iris_numeric)

bigmemory::write.big.matrix(
  big_iris_a,
  file.path(temp_dir, "IrisHeaders.txt"),
  col.names = TRUE
)

#' With the `header = TRUE` and `has.row.names = TRUE` arguments files are read
#' in correctly.
big_iris_read_a <- bigmemory::read.big.matrix(
  file.path(temp_dir, "IrisHeaders.txt"),
  type = "double",
  header = TRUE
)

#' Identity check
identical(big_iris_a[, ], big_iris_read_a[, ])  # TRUE

# -----------------------------------------------------------------------------




#' Demo 6: Working with Massive Data
# -----------------------------------------------------------------------------
#' In the following we are creating a object out of RAM.
#' ** WARNING: DO NOT RUN UNLESS YOU INTEND TO CREATE A VERY LARGE FILE 
#' (~218 GB) on your HARD DRIVE.
#'
library(bigmemory)

#' Create a dedicated temporary directory
backing_dir <- file.path(tempdir(), "big_mtx_demo")
if (!dir.exists(backing_dir)) {
  dir.create(backing_dir)
}
cat("Backing files for this demo are stored in:", backing_dir, "\n")

#' Create file-backed `big.matrix`
the_big_mat <- bigmemory::filebacked.big.matrix(
  nrow = 10e3,
  ncol = 1e3,
  type = "double",
  init = 42,
  backingfile = "large_mtx.bin",
  backingpath = backing_dir,
  descriptorfile = "large_mtx.desc"
)

#' Object inspection
cat("Dimension: ", nrow(the_big_mat), "x", ncol(the_big_mat), "\n")
cat("File-backed?", bigmemory::is.filebacked(the_big_mat), "\n")
cat(
  "Backing file location:",
  file.path(dir.name(the_big_mat), file.name(the_big_mat)),
  "\n"
)

#' Cleanup and Conclusion
#'  Because we created the backing files inside a subdirectory of `tempdir()`,
#'  R will automatically delete the `backing_dir` and its contents (the .bin
#'  and .desc files) when you close your R session properly (e.g., using `q()`).
#'
q()

