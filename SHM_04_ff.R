#' Tutorial 4: Out-of-Memory Data with the `ff` Package
# -----------------------------------------------------------------------------
#' In the following we explore the `ff` package ("fast-access files"), a
#'  powerful alternative to `bigmemory` for handling large datasets in R.
#'
#' Similar to `bigmemory`, the `ff` package also uses memory-mapped files
#'  on disk. This offers several unique advantages:
#'
#'  - **Support for complex data stractures:** `ff`natively supports data frames
#'    (`ffdf`), factors, and dates, not just matrices. A richer ecosystem than
#'    `bigmemory`s `big.matrix`.
#'
#'  - **Memory Efficiency:** Provides a rich set of low-level data types
#'    (e.g., 4-bit integers) to minimize disk footprint.
#'
#'  - **Advanced Tooling:** In combination with the `bit` package, `ff` offers
#'    extremely fast filtering and subsetting capabilities.
#'
#' `ff` uses a pointer as well, but to a flat binary stored in the disk,
#'  that is sequentially accessed, and it can be shared across different
#'  sessions.
#'
#' The trade-off is, that `ff` is "very technical" and has a steep learning
#'  curve.
#'
#' In combination with package `bit` we get access to:
#'  - scaling statistical methods to large data problems
#'  - virtual objects by reference and thus avoid copy and RAM duplication
#'  - disk based objects and thus manage temporary and permanent objects on disk
#'  - memory efficient data types and thus minimize storage requirements of data
#'  - memory efficient subscript types and thus minimize storage requirements of
#'     subscripts
#'
#'  - fast chunk access method -> all fast (random) access to disk
#'  - fast index (pre)processing
#'  - chunk processing infrastructure
#'  - large csv import/export
#'  - large data management
#'  - parallel processing and parallel access to large data sets without locking
#'
#' more and very technical information:
#' http://ff.r-forge.r-project.org/ff&bit_UseR!2009.pdf
#' http://ff.r-forge.r-project.org/bit&ff2.1-2_WU_Vienna2010.pd
#'
#' **NOTE:** The companion package `ffbase` is no longer on CRAN, but the
#' core `ff` and `bit` packages provide the primary functionality.
#'
#' https://cran.microsoft.com/snapshot/2020-04-20/web/packages/ffbase/index.html
#' https://cran.microsoft.com/snapshot/2020-04-20/web/packages/ffbase/ffbase.pdf
#'
#' ffbase: functions for ff() and ffdf() objs in the areas of
#'   - mathematics: abs(), sign(), sqrt(), ceiling(), floor(), log(), exp(),
#'                  cos(), cosh(), sin(), sinh(), gamma()
#'   - summaries: all(), any(), max(), min(), cumsum(), cummin()
#'   - uniqueness: duplicated(), unique()
#'   - apply: ffdfdply()
#'

library(ff)
library(bit)

#' Setup: Create a temporary directory for ff files
#' `ff` objects are file-backed. It is CRITICAL to manage these files.
#' Using a temporary directory is a safe way to ensure cleanup.
options(fftempdir = file.path(tempdir(), "ff_tutorial"))
dir.create(getOption("fftempdir"), showWarnings = FALSE)

#' All `ff` related options can be inspected by running `options()` and
#' looking for keys starting with "ff", such as `fftempdir` and `ffcaching`.


#' Demo 1: The Building Blocks - `ff` Atomic Vectors
# ---------------------------------------------------------------------
#' **Creating `ff` objects with different `vmode`'s**
#' The `vmode` argument controls the underlying data type on disk.
#' Choosing the smallest appropriate type can save enormous amounts of
#' disk space.
# -----------------------------------
#' Sub-byte types for compact but limited storage
ff::ff(vmode = "boolean", length = 10)  # 1 bit
ff::ff(vmode = "logical", length = 10)  # 2 bits
ff::ff(vmode = "quad", length = 10)     # 2 bits
ff::ff(vmode = "nibble", length = 10)   # 4 bits

#' Single-byte integers
ff::ff(vmode = "byte", length = 10)   # 8-bit signed
ff::ff(vmode = "ubyte", length = 10)  # 8-bit unsigned

#' Standard types
ff::ff(vmode = "short", length = 10)   # 2-byte integer
ff::ff(vmode = "ushort", length = 10)  # 4-byte unsigned
ff::ff(vmode = "integer", length = 10) # 4-byte integer
ff::ff(vmode = "double", length = 10)  # 8-byte double

# **ERROR and fatal crash** ff(vmode = "raw", length = 10)
# **NOTE**: raws are usually reserved for very low-level byte manipulation

#' get an overview of memory consumption
cbind(.rambytes, .ffbytes)[c("integer", "byte"), ]

#' **Initializing `ff` objects with data**:
#' A double-precision vector, initialized with data from `iris`
ff_dbl <- ff::ff(iris$Sepal.Width[1:10], length = 10)
ff_dbl[1:5]

#' A matrix (physically stored, column-wise on disk)
ff_mat <- ff::ff(rep(0, 150), dim = c(10, 15))
ff_mat[, 1] <- 1:10
print(ff_mat)

#' **Working with Factors**
#' `ff` correctly handles factors, storing them efficiently as integers.
r_fac <- factor(sample(c("A", "T", "G", "C"), 10, replace = TRUE))
ff_fac <- ff::ff(r_fac)
levels(ff_fac)
cbind(.rambytes, .ffbytes)[c(r_fac, ff_fac), ]

# ordered factor
r_fac2 <- factor(levels = c("A", "B", "F", "C"), ordered = TRUE)
length(r_fac2) <- 10
r_fac2[] <- sample(levels(r_fac2), size = 10, replace = TRUE)

ff_fac2 <- ff(initdata = r_fac2, ordered = TRUE)
length(ff_fac2) <- 1e2
ff_fac2[11:1e2] <- NA

cbind(.rambytes, .ffbytes)[c(r_fac2, ff_fac2), ]

# factors as quad
# unordered
ff(vmode = "quad", length = 10, levels = c("A", "B", "C", "D"), ordered = FALSE)
# ordered
ff(vmode = "quad", length = 10, levels = c("A", "B", "C", "D"), ordered = TRUE)


# Date and posixct
# ------------------------
# store dates as integers
ff(seq.int(0, 9), ramclass = "Date")

# store dates as double
ff(Sys.Date() + seq.int(0, 9), length = 10, ramclass = "Date")

# store time stamps
ff(seq.int(0, 9), ramclass = c("POSIXt", "POSIXct"))
ff(Sys.Date() + seq.int(0, 9), length = 10, ramclass = c("POSIXt", "POSIXct"))

# Complex: currently can't construct complex
vmode(complex(length.out = 2, real = c(4, 2), imaginary = c(2, 4)))

# Use ff to partition data
data(trees)
ff_trees <- ffdf(
  Girth  = ff(trees$Girth, vmode = "double"),
  Height = ff(trees$Height, vmode = "double"),
  Volume = ff(trees$Volume, vmode = "double")
)


#' **Chunk-wise Processing**
#' A key pattern for `ff` is "chunking": processing a large object in smaller
#' manageable pieces to conserve RAM.
large_ff_vec <- ff::ff(vmode = "double", length = 1e7)

#' Fill the vector in chunks
for (chunk_x in bit::chunk(large_ff_vec)) {
  print(chunk_x)
  large_ff_vec[chunk_x] <- stats::rnorm(sum(chunk_x))
}

#' Process the vector in chunks (e.g., calculate quantiles for each chunk)
chunk_quantiles <- lapply(chunk(large_ff_vec), function(i) {
  return(
    stats::quantile(large_ff_vec[i], c(0.05, 0.95))
  )
})

#' Combine results
ff::crbind(chunk_quantiles)

#' **CRITICAL: File Management**
#' `rm(my_ff_obj)` only removes the R pointer, NOT the file on disk.
#' To properly delete an `ff` object and its backing file during a
#' session, the file handle must be closed, and the removed:
#' close(my_ff_obj); file.remove(filename(my_ff_obj))



#' Demo 2: High-Speed Filtering with `bit` and `hi`
# -----------------------------------------------------------------------------
#' The `bit` package provides highly memory-efficient boolean vectors, perfect
#' for creating filters for large `ff` objects.
filter_vec <- ff::ff(vmode = "double", length = 1e3, initdata = runif(1e3))
object.size(filter_vec) / (1024^2)

#' Create a `bit` vector to store a filter condition
filt_bit_vec <- bit::bit(length(filter_vec))

#' Apply the filter condition in chunks
for (chunk_x in bit::chunk(filter_vec)) {
  filt_bit_vec[chunk_x] <- filter_vec[chunk_x] > 0.99
}

#' Use the `bit` vector to subset the `ff` object
filtered_data <- filter_vec[filt_bit_vec]
head(filtered_data)
object.size(filtered_data) / (1024^2)

#' `as.hi` (hybrid index) can provide even faster subsetting in some cases
hp <- as.hi(filt_bit_vec)
head(filter_vec[hp])
object.size(hp) / (1024^2)



#' Demo 3: Working with `ff` Matrices and Arrays
# -----------------------------------------------------------------------------
#' `ff` objects are physically stored in column-major order on disk.
#' This has important performance implications.
ff_perf_mat <- ff::ff(vmode = "double", dim = c(1e4, 1e4))

#' Column wise access is extremely fast as it reads contiguous data from disk
system.time(ff_perf_mat[, 1] <- 1)

#' Row-wise access is much slower as it requires reading from many different
#' locations on the disk
system.time(ff_perf_mat[1, ] <- 1)

#' ff example: physically stored by column
r_mat <- matrix(1:12, 3, 4, byrow = TRUE) # read by row
ff(r_mat, dim = c(3, 4), dimorder = c(1, 2))
ff(r_mat, dim = c(3, 4), dimorder = c(1, 2), bydim = c(1, 2))

#' columnwise
ff(r_mat, dim = c(4, 3), dimorder = c(1, 2), bydim = c(2, 1))
ff(r_mat, dim = c(3, 4), dimorder = c(2, 1))

ff_mat <- ff(r_mat, dim = c(3, 4), dimorder = c(2, 1))
get.ff(ff_mat, 1:12) # note the physical order
ff_mat[1:12]         # [. exhibits standard R behavior]

#' array (tensor)
r_array <- array(1:12, dim = c(3, 4, 2))  # read by column
ff(r_array, dim = dim(r_array))
ff(1:12, dim = c(2, 2, 3))

#' Hybrid copying semantics: physical and virtual attributes
ff_mat <- ff(initdata = 1:12, dim = c(3, 4))
str(physical(ff_mat))
str(virtual(ff_mat))

#' Virtual transpose
vt(ff_mat) # short-cut for t()
t(ff_mat)


# Demo 4: Working with `ffdf` Data Frame
# -----------------------------------------------------------------------------
#' An `ffdf` is a data frame where each column is a separate `ff` object on
#' disk. This allows for the creation of massive data frames that far exceed
#' RAM.

#' Create an `ffdf` from existing R vectors
ff_df1 <- ff::ffdf(
  id = ff::ff(1:150),
  species = ff::ff(iris$Species),
  sepal_len = ff::ff(iris$Sepal.Length)
)
head(ff_df1)
class(ff_df1)
ff::vmode(ff_df1)

#' Create a very large `ffdf` from scratch
n_elems <- 1e6
large_ff_df <- ff::ffdf(
  id = ff::ff(1:n_elems, vmode = "integer"),
  val = ff::ff(vmode = "double", length = n_elems)
)

# Populate it chunk by chunk
for (chunk_x in bit::chunk(large_ff_df)) {
  large_ff_df$val[chunk_x] <- stats::runif(length(chunk_x))
}
head(large_ff_df)
nrow(large_ff_df)

# Resize an ffdf instantly by chaning the `nrow` attribute
nrow(large_ff_df) <- 2e6
nrow(large_ff_df) # now with 2e6 rows
nrow(large_ff_df) <- 1e6  # restore
nrow(large_ff_df)



#' Demo 5: Reading and Writing CSVs with `ffdf`
# -----------------------------------------------------------------------------
#' Setup a sample R data frame to work with:
r_df <- data.frame(
  id = 1:150,
  species = iris$Species,
  sepal_len = iris$Sepal.Length
)

#' Define the path for our output file in the temporary directory
sample_csv_path <- file.path(getOption("fftempdir"), "sample_r_df.csv")

#' Use base R's `write.csv` to save this data frame to disk
write.csv(r_df, file = sample_csv_path, row.names = FALSE)

#' ** Reading a CSV into an `ffdf` **
#' The `read.csv.ffdf` function reads a text file and immediately stores its
#' contents in a new, disk-based `ffdf` object.
ff_from_csv <- ff::read.csv.ffdf(file = sample_csv_path, header = TRUE)

#' The resulting object is an ffdf
class(ff_from_csv)
head(ff_from_csv)

#' ** Writing an `ffdf` to a CSV **
#' The `write.csv.ffdf`function performs the reverse operation, writing the
#' contents of an `ffdf` to a new text file.
output_csv_path <- file.path(getOption("fftempdir"), "output_ff_df.csv")
write.csv.ffdf(x = ff_from_csv, file = output_csv_path)

#' ** Verifying the Round-Trip **
#' Finally, we can read both CSV files into standard R data frames
#' to confirm that the data was preserved perfectly during `ff`'s I/O process.
r_df_readback <- read.csv(sample_csv_path)
ff_df_readback <- read.csv(output_csv_path)

#' The data is identical
identical(r_df_readback, ff_df_readback)



#' Demo 6: File Management and Persistence
# -----------------------------------------------------------------------------
#' This is the most important concept for safe `ff` usage. Unlike R objects,
#' `ff` files can be **permanent**.
rm(ff_from_csv)
# Even after removing the R object, the underlying `.ff` files still
#  exist on disk!

# You can see them in your ff temporary directory:
list.files(getOption("fftempdir"), pattern = ".ff$")

#' **How to clean up:**
#' By default, `ff` objects created in `getOption("fftempdir")` are deleted
#' when you close R properly with `q()`.
#'
#' To manually delete an `ff` object and its backing file during a session:
# 1. Get the physical properties (which include the filename).
p <- physical(ff_df1)
# 2. Close the file handles.
sapply(p, close)
# 3. Delete the files.
sapply(p, function(x) file.remove(filename(x)))

# The files associated with ff_df1 are now gone.
list.files(getOption("fftempdir"), pattern = ".ff$")


#' Demo 7: Parallel Processing with `ff`
# -----------------------------------------------------------------------------
#' The `snowfall` package, while older, was designed to work well with `ff`.
#' This example demonstrates a parallel bootstrap, a task that would require
#' enormous memory without `ff`'s "copy-on-modify" efficiency.
library(snowfall)

# Setup: Create a large ffdf for bootstrapping
n_elems <- 1e6 # 1 million rows
ffdf_obj <- ff::ffdf(
  age = ff::ff(sample(18:65, n_elems, replace = TRUE)),
  income = ff::ff(runif(n_elems, 20000, 200000))
)

# Define the bootstrap wrapper function
# This function will be executed by each worker.
bootstrap_wrapper <- function(sample_size) {
  # The worker takes a random sample WITH replacement from the ffdf rows
  # and calculates the means for the "age" and "income" columns.
  sample_indices <- sample(nrow(ffdf_obj), sample_size, replace = TRUE)
  colMeans(ffdf_obj[sample_indices, c("age", "income")])
}

#' Run the parallel bootstrap
#' Initialize the cluster
snowfall::sfInit(parallel = TRUE, cpus = 2, type = "SOCK")
#' Export the required library and data to the workers
snowfall::sfLibrary(ff)
snowfall::sfExport("ffdf_obj")

#' Run the bootstrap 100 times
bootstrap_results <- sfLapply(rep(1e4, 100), bootstrap_wrapper)

# 'Stop cluster
snowfall::sfStop()

#' The result is a list containing the mean age and income for each replicate.
head(do.call(rbind, bootstrap_results))
