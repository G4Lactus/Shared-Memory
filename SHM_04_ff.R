# The "fast-access-files" [ff] package:
# -----------------------------------------------------------------------------
# Deals with large data sets similar to [bigmemory]. It uses a pointer as well
# but to a flat binary stored in the disk, that is sequentially accessed, and it
# can be shared across different sessions.
# 
# One advantage [ff] has over [bigmemory] is that it supports data class types
# in the data set, i.e., you can create data.frames not just matrices.
# Also it supports large complex read and write operations (e.g., smoothing).
# 
# [ff] is "very technical" but can be tailored to be extremely-memory efficient.
# 
# 
# Usefulness: in combination with package [bit]
# ------------------------------------------------
# - scale statistical methods to large data problems
# - complexities in scope of [ff] and [bit]
#   - virtual objs by reference -> avoid copy and RAM duplication
#   - disk based objs -> manage temporary and permanent objs on disk
#   - memory efficient data types -> minimize storage requirements of data
#   - memory efficient subscript types -> minimize storage requirements of
#                                         subscripts
#   - fast chunk access method -> all fast (random) access to disk
#   - fast index (pre)processing
#   - chunk processing infrastructure
#   - large csv import/export
#   - large data management
#   - parallel processing and parallel access to large data sets without locking
# 
# more information: very technical
# --------------------------------
# http://ff.r-forge.r-project.org/ff&bit_UseR!2009.pdf
# http://ff.r-forge.r-project.org/bit&ff2.1-2_WU_Vienna2010.pdf
# 
# 
# 
# Companion package of [ff] is [ffbase] no longer on CRAN (2022):
# --------------------------------------------------------
# but snapshots of older versions are available:
# ----------------------------------------------
# https://cran.microsoft.com/snapshot/2020-04-20/web/packages/ffbase/index.html
# https://cran.microsoft.com/snapshot/2020-04-20/web/packages/ffbase/ffbase.pdf
# 
# 
# ffbase: functions for ff() and ffdf() objs in the areas of
#   - mathematics: abs(), sign(), sqrt(), ceiling(), floor(), log(), exp(),
#                  cos(), cosh(), sin(), sinh(), gamma()
#   - summaries: all(), any(), max(), min(), cumsum(), cummin()
#   - uniqueness: duplicated(), unique()
#   - apply: ffdfdply()
# 
# -----------------------------------------------------------------------------


# Generate some toy data
# -----------------------------------------------------------------------------
test_data <- matrix(rnorm(2e1), nrow = 5)
temp_dir <- path.expand(tempdir())
if (!dir.exists(temp_dir)) { dir.create(path = temp_dir) }
test_file <- "test.csv"
path_to_open_file <- paste0(temp_dir, test_file)
write.csv(test_data, file = path_to_open_file, row.names = FALSE)
rm(list = setdiff(ls(), "path_to_open_file"))
gc()



# Demo 1: Getting started with [ff]
# -----------------------------------------------------------------------------
#
# Creating atomic vectors
# -----------------------------------------------------
# Atomic data types supported by [ff] 
# [1 byte = 8 bit]
# -------------------------------------------------
# - boolean (1 bit without NA) 
# - logical (2 bit with NA)
# - quad (2 bit unsigned integer without NA)
# - nibble (4 bit unsigned integer without NA)
# - byte (1 byte signed integer with NA)
# - ubyte (1 byte unsigned integer without NA)
# - short (2 byte signed integer with NA) 
# - ushort (2 byte unsigned integer without NA)
# - integer (32-bit signed integer with NA)
# - single (4 byte float with NA)
# - double (64 bit)
# - compex (2*64 bit float with NA)           -> not implemented
# - raw (hold raw bytes, 8-bit unsiged char)
# - factor(factor levels as integers)
# - ordered (ordered factor stored as integer)
# - POSIXct (stored as double)
# - Date (stored as double)
# ---------------------

# Comparison: [base] R vs [ff]
# --------------------------------
r_int <- 10L            # define integer 10
r_int1 <- integer(10)   # define array wth integer length 10 of 0s

class(r_int)
class(r_int1)


library(ff)

# Get load options on call
# ---------------------------
# - getOption("fftempdir")=="YOUR/LOCAL/PATH/ff" -> ff objs are stored here default wise
# - getOption("ffextension")=="ff"               -> default file extension
# - getOption("ffdrop")==TRUE
# - getOption("fffinonexit")==TRUE
# - getOption("ffpagesize")==65536
# - getOption("ffcaching")=="mmnoflush"  -- consider "ffeachflush" if your system stalls on large writes
# - getOption("ffbatchbytes")==16777216 -- consider a different value for tuning your system
# - getOption("ffmaxbytes")==536870912 -- consider a different value for tuning your system


# NOTE: rm(list=ls()) cleans all your R workspace, but [ff] objs prevail!
# 
# Explicit remove file with binding during R session:
# ---------------------------------------------------
# Calling filename(x) -> file.remove(.) won't solve the problem
# How to fix? -> base::close(x) -> file.remove(filename(x))
# 
# BE AWARE OF THIS: OTHERWISE YOU FILL UP YOUR HARD DRIVE OVER TIME!
# By closing the R session temporary folders usually disappear, but be weary.


# create some atomic data types
# --------------------------------
# the basic types
# ----------------
ff(vmode = "boolean", length=10)
ff(vmode = "logical", length=10)
ff(vmode = "quad", length=10)
ff(vmode = "nibble", length=10)
ff(vmode = "byte", length=10)
ff(vmode = "ubyte", length=10)
ff(vmode = "short", length=10)
ff(vmode = "ushort", length=10)
ff(vmode = "integer", length=10)
ff(vmode = "single", length=10)
ff(vmode = "double", length=10)
ff(vmode = "raw", length=10)

# get type method from [ff]
# --------------------------------
ff_int1 <- ff(vmode = "integer", length=10)
ff_int2 <- ff(vmode = "byte", length=10)

vmode(r_int1)
vmode(ff_int1)
vmode(ff_int2)


# get an overview of memory consumption
# --------------------------------------
cbind(.rambytes, .ffbytes)[c("integer", "byte"),]


ff_dbl1 <- ff(vmode = "double", length = 10)
# modifying the file by assigning some data
ff_dbl1[1:10] <- iris$Sepal.Width[1:10]
vmode(ff_dbl1)

# alternative
ff_dbl2 <- ff(
  vmode = "double",
  length = 10,
  initdata = iris$Sepal.Width[1:10]
)

# creating a multidimensional file
ff_dbl3_multi <- ff(vmode = "double", dim = c(10, 3))
ff_dbl3_multi[1:10, 1] <- iris[1:10, 1]


# more complex data types with [ff], also compared to [bigmemory]
# ----------------------------------------------------------------
# factors
# --------------------
# factor unordered
# -----------------
r_fac1 <- factor(levels = c("A", "T", "G", "C"))
length(r_fac1) <- 10
r_fac1[1:10] <- sample(levels(r_fac1), size = 10, replace = TRUE)

ff_fac1 <- ff(initdata = r_fac1)
length(ff_fac1) <- 1e2
ff_fac1[11:1e2] <- NA

cbind(.rambytes, .ffbytes)[c(r_fac1, ff_fac1),]


# ordered factor
# -----------------
r_fac2 <- factor(levels = c("A", "B", "F", "C"), ordered = TRUE)
length(r_fac2) <- 10
r_fac2[] <- sample(levels(r_fac2), size = 10, replace = TRUE)

ff_fac2 <- ff(initdata = r_fac2, ordered = TRUE)
length(ff_fac2) <- 1e2
ff_fac2[11:1e2] <- NA

cbind(.rambytes, .ffbytes)[c(r_fac2, ff_fac2),]


# factors as quad
# ------------------------
# unordered
ff(vmode="quad", length=1e2, levels=c("A","T","G","C"))
# ordered
ff(vmode="quad", length=10, levels=c("A", "B", "C", "D"), ordered = TRUE)


# Date and posixct
# ------------------------
# store dates as integers
ff(0:9, ramclass = "Date")

# store dates as double
ff(Sys.Date()+0:9, length=10, ramclass = "Date")

# store time stamps
ff(0:9, ramclass = c("POSIXt", "POSIXct"))
ff(Sys.time()+(0:9), length = 10, ramclass=c("POSIXt", "POSIXct"))

str(ff(as.POSIXct(as.POSIXlt(Sys.time(), "GMT")), length=12))


# Complex: currently can't construct complex
# ------------------------------------------
vmode(complex(length.out = 2, real = c(4, 2), imaginary = c(2, 4)))


# chunk wise processing
# ------------------------------------------
ff_dbl4 <- ff(vmode="double", length=1e8) # lives in your temporary dir ~ 780 MB

# chunk processing part 1: fill ff obj
for (chunkX in chunk(ff_dbl4)) { ff_dbl4[chunkX] <- runif(sum(chunkX)) }

# chunk processing part 2: apply function chunk wise
chunk_lst_quantiles <- lapply(chunk(ff_dbl4), function(i) { quantile(ff_dbl4[i], c(0.05, 0.95))})

# collapse functions for batch processing
ff::crbind(chunk_lst_quantiles)
# ------------------------------------------


# Use ff to partition data
# -----------------------------------------
data(trees)
ff_trees <- ffdf(
  Girth  = ff(trees$Girth, vmode = "double"),
  Height = ff(trees$Height, vmode = "double"),
  Volume = ff(trees$Volume, vmode = "double")
)


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------


# Demo 2: working with bit filters
# -----------------------------------------------------------------------------
library(ff)
ff_dbl5 <- ff(vmode="double", length = 1e3, initdata = runif(1e3))

# ff example
# ---------------
b1 <- b2 <- bit(length(ff_dbl5))

# super fast assignment
# ----------------------
system.time( b1[] <- c(FALSE, TRUE) )
system.time( for (chunkX in chunk(ff_dbl5)) { b2[chunkX] <- ff_dbl5[chunkX] > 0.99})
system.time( b <- b1 & b2)

object.size(b)/(1024^2)
system.time(x <- ff_dbl5[b])
x[1:10]

sum(b)/length(b)
w <- as.bitwhich(b)
sum(w)/length(w)
object.size(w)/(1024)^2
system.time((x <- ff_dbl5[w]))
x[1:10]
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------



# Demo 3: working with hybrid indexing
# -----------------------------------------------------------------------------
hp <- as.hi(b)
object.size(hp)/(1024^2)
system.time(x <- ff_dbl5[hp])
x[1:10]

hu <- as.hi(w, pack=FALSE)
object.size(hu)/(1024^2)
system.time(x <- ff_dbl5[hu])
x[1:10]
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------



# Demo 4: Working with arrays and matrices
# -----------------------------------------------------------------------------
# [base] R
# ----------------------
r_mat <- matrix(1:12, 3, 4, byrow=TRUE) # read by row
r_array <- array(1:12, dim=c(3, 4, 2))       # read by column


# ff example: physically stored by column
# -------------------------------------------
# matrix
# ----------
# row wise
ff(r_mat, dim = c(3, 4), dimorder = c(1, 2))
ff(r_mat, dim = c(3, 4), dimorder = c(1, 2), bydim = c(1, 2)) 

# column wise
ff(r_mat, dim = c(4, 3), dimorder = c(1, 2), bydim = c(2, 1))
ff(r_mat, dim = c(3, 4), dimorder = c(2, 1))

ff_mat <- ff(r_mat, dim = c(3, 4), dimorder = c(2, 1))
get.ff(ff_mat, 1:12) # note the physical order
ff_mat[1:12]         # [. exhibits standard R behavior]
ncol(ff_mat) <- 1e8  # not possible with this dimorder
nrow(ff_mat) <- 1e8  # possible with this dimorder

ff_mat <- ff(vmode="double", dim = c(1e4, 1e4))
system.time(ff_mat[1,]  <- 1) # row store: slow
system.time(ff_mat[, 1] <- 1) # column store: fast
# -> column wise processing beats row wise processing

# The following two commands are only available for supports of the package :)
# symmetric matrix with free diag
ff(1:6, dim = c(3, 3), symmetric = TRUE, fixdiag = NULL)

# symmetric matrix with fixed diag
ff(1:3, dim = c(3, 3), symmetric = TRUE, fixdiag = 0)


# array (tensor)
# ---------------
ff(r_array, dim = dim(r_array))
ff(1:12, dim = c(2, 2, 3))

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------



# Demo 5: Hybrid copying semantics: physical and virtual attributes
# -----------------------------------------------------------------------------
ff_mat <- ff(initdata = 1:12, dim = c(3, 4))
str(physical(ff_mat))
str(virtual(ff_mat))

# virtual transpose
vt(ff_mat) # short-cut for t()
t(ff_mat)
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------



# Demo 6:Working with data.frames
# -----------------------------------------------------------------------------
# The difference between R data.frames and ff ffds is that the former one take
# data and copy them to vectors.
# ffdfs by default do not physically copy, but virtually map them.
# -----------------
library(ff)

# R example for creating a data frame
# ------------------------------------------
r_df <- local({
  id <- 1:12
  gender <- sample(factor(c("male", "female", "unknown")), size = 12, replace = TRUE)
  rating <- matrix(sample(1:6, 12*10, TRUE), 12, 10)
  colnames(rating) <- paste("r", 1:10, sep="")
  df <- data.frame(id, gender, rating)
  return(df)
})
r_df[1:3, ]


# ff example for creating a data frame with ffdf()
# -------------------------------------------------------
ff_df <- local({
  
  # NOTE: inputs must be atomic: scalar, vector, matrix
  # -----------------------------------------------------
  ff_id <- as.ff(r_df$id)
  ff_gender <- as.ff(r_df$gender)
  ff_rating <- as.ff(as.matrix(r_df[3:ncol(r_df)]))
  ff_df <- ffdf(id = ff_id, gender = ff_gender, ff_rating)
  
  return(ff_df)
})


# check for identity between r_df and ff_df
# ------------------------------------------
identical(r_df, ff_df[,])

# in the R data frame "rating" was a matrix,
# but data.frame has copied to cols
# unless we use I(rating) operation
# 
# -> convert efficiently to ff objs
# --------------------------------------
physical(ff_df) # does not copy anything

# ff quantities reside inside temporary directory if not specified
# -----------------------------------------------------------------
ff::filename(ff_df$id)
ff::filename(ff_df$gender)
ff::filename(ff_df$r1)
ff::filename(ff_df$r2)
ff::filename(ff_df$r3)
ff::filename(ff_df$r9)

# ff obj with specified directory
# ---------------------------------
# note we specify the file path here explicitly to be in the temporary directory
# , you can change that, but be aware, that files stores at other locations
# won't disappear as the R session is closed.
ff_obj1 <- ff(FALSE, dim=c(50, 100), filename = file.path(tempdir(), "ff_obj1.ff"))

# create some toy data to create a data.frame
ff_obj_i <- ff(1:1e6, filename = file.path(tempdir(), "ff_obji.ff"))
ff_obj_q <- ff(sample(0:3, 1e6, TRUE), vmode = "quad", filename = file.path(tempdir(), "ff_objq.ff"))
ff_obj_z <- ff(sample(0:3, 1e6, TRUE), vmode = "quad", filename = file.path(tempdir(), "ff_objq.ff"))

# NOTE: you can only initialize the data once due to file backing. If you want
# to construct an obj again, follow this chain
close(ff_obj_q)
file.remove(filename(ff_obj_q))

# create another data frame with ffdf() using the integer and quad ff vectors
ff_df2 <- ffdf(ff_obj_i, ff_obj_z)
class(ff_df2)         # ffdf
ff_df2
ff_df2[1:5,]
class(ff_df2[1:5, ])  # df
vmode(ff_df2)
str(ff_df2)


# chunk ffdf() obj
# -----------------
chunk(ff_df2)

# set the maximum size in bytes using BATCHBYTES
# chunk recommends the splitting accordingly
chks <- chunk(ff_df2, BATCHBYTES = 2e6)

# indices returned by chunk can be used to index the rows of an ffdf() or ff()
total <- numeric(2)
quadtable <- integer(length(chks))
names(quadtable) <- c("A", "B", "C")
for (idx in chks) {
  total <- total + colSums(ff_df2[idx, ])
  quadtable <- quadtable + table(ff_obj_q[idx])
}
total
quadtable


# Example: create ff vectors with 800 Mio elements as input to df
# --------------------------------------------------------------------
options(fffinalizer="close") # let snowfall not delete on remove
N <- 8e7                     # sample size
n <- 1e6                     # chunk size

# generate some toy data out of RAM
# ------------------------------------
genders <- factor(c("male", "female"))
countries <- factor(c("New York", "California", "Florida", "Washington D.C."))
years <- factor(c(1990, 1991, 1992, 1993, 1994, 1995, 1996))
ages <- c(18, 29, 49, 59, 29, 59, 29)
incomes <- c(120000, 45000, 90000, 67900, 760000)

gender <- ff(genders, vmode = "quad", length = N, update = FALSE)
country <- ff(countries, vmode = "integer", length = N, update = FALSE)
year <- ff(years, vmode = "integer", length = N, update = FALSE)
age <- ff(ages, vmode = "integer", length = N, update = FALSE)
income <- ff(incomes, vmode = "double", length = N, update = FALSE)

# to realize the chunking, we use the bit package now:
# bit::chunks()
for (idx in bit::chunks(from = 1, to = N, by = n)) {
  print(idx)
  gender[idx] <- sample(genders, size = n, replace =  TRUE)
  country[idx] <- sample(countries, size = n, replace = TRUE)
  year[idx] <- sample(years, size = n, replace = TRUE)
  age[idx] <- sample(ages, size = n, replace = TRUE)
  income[idx] <- sample(incomes, size = n, replace = TRUE)
}

ffdf_obj <- ffdf(country=country, year=year, gender=gender, 
                 age=age, income=income)
vmode(ffdf_obj)

# query ffdf like R df
ffdf_obj$country
ffdf_obj[["country"]]
ffdf_obj[c("country", "year")]
ffdf_obj[1:10, c("country", "year")]

ffdf_obj[1:10,]
class(ffdf_obj[1:10,])

ffdf_obj[1,,drop=TRUE]                # return 1 row as list
class(ffdf_obj[1,,drop=TRUE])

nrow(ffdf_obj)

# now we increase the size of the data frame:
system.time(nrow(ffdf_obj) <- 1e8) # after negligible time we have 100 Mio rows
ffdf_obj
# restore original dimensionality
nrow(ffdf_obj) <- 8e7
ffdf_obj

# The combination of [ff] and [bit] supports a variety of important data access
# scenarios:
# --------------------------
#           | random access | sequential access | unpredictable search condition | BI drill down
# -----------------------------------------------------------------------------------------------
# R         | fast          | fast              | fast                           | combine
#           | fits-in-memory| if fits in-memory | if small data                  | logicals
# -----------------------------------------------------------------------------------------------
# bigmemory | fast as poss. | as fast as poss.  | -                              | _2
#           |fits in memory | if fits in memory |                                |
# -----------------------------------------------------------------------------------------------
# ff        | fast as poss   | fast as possible | -                              | combnie bit
#           | if large chunks| if chunked       |                                | filters
# -----------------------------------------------------------------------------------------------
# MonetDB   | -              | fast as possible | as fast as possible            | -
#           |                | if many rows     | if many rows                   |
# -----------------------------------------------------------------------------------------------
# row DB    | -              | -                | b*-tree, bitmap                | combine
#           |                |                  |                                | bitmaps
# ------------------------------------------------------------------------------------------------

# Now we create, combine and coerce filters with 80 Mio bits
# -----------------------------------------------------------
fcountry <- bit(N) # create bit obj
class(fcountry)
fyear <- bit(N)

# process chunks and write to bit obj
system.time(
  for (i in bit::chunks(1, N, n)) {
    fcountry[i] <- ffdf_obj$country[i] == "California"
  }
)
system.time(
  for (i in bit::chunks(1, N, n)) {
    fyear[i] <- ffdf_obj$year[i] %in% c(1991, 1992)
  }
)
# combine with boolean operator
system.time(filter <- fcountry & fyear)
summary(filter)
summary(filter, range = c(1, 8e6))
summary(ffdf_obj[filter & bit::ri(from = 1, to = 8e6, maxindex = N), ])

# coercing
h <- as.hi(filter) # coerce chunk: as.hi(filter, range=c(1, 8e6))
as.bit(h)
f <- as.ff(filter)
as.bit(f)
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------



# Demo 6: Reading and writing csv
# -----------------------------------------------------------------------------
# read data and create an ffdf() obj as virtual data frame
# ---------------------------------------------------------
ff_mtx <- read.csv.ffdf(file = path_to_open_file)
class(ff_mtx)
str(ff_mtx)
head(ff_mtx)

# ff df object can be treated as any other R object
sum(ff_mtx[,])
sum(ff_mtx[, 3])
sum(ff_mtx[1:2, 1:3])

# write df objs from demo 5
# write file to csv with [base] R
write.csv(r_df, file = paste0(getwd(), "/Backend/df.csv"), row.names = FALSE)

# write file to csv with [ff]
write.csv.ffdf(ff_df, file = paste0(getwd(), "/Backend/ff_df.csv"))

# read in and check for identity
r_df_test <- read.csv(paste0(getwd(), "/Backend/df.csv"), row.names = NULL)
ff_df_test <- read.csv(paste0(getwd(), "/Backend/ff_df.csv"))
identical(r_df_test, ff_df_test)
# -> check function specifications for more advanced I/O operations

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------



# Demo 7: File locations and file survival
# -----------------------------------------------------------------------------
# R
# ------
rm(r_df_test) # obj is gone

# NOT RUN
q()   # everything R workspace obj is gone


# ff
# ------------------------
# Obj data in files are potentially permanent , rm(ff_df_test) removes the R obj
# but not the .ff file
rm(ff_df_test)

# the next gc() triggers a finalizer which acts on the file when closing r with
# q()
# The attribute finonexit decides whether the finalizer is called as fftempdir
# is unlinked
# -------------------------
physical(ff_mtx)

# get overview over temporal directory files
dir(getOption("fftempdir"))

# changing file locations and finalizers
# ----------------------------------------
sapply(physical(ff_df2), finalizer)
filename(ff_df2)
pattern(ff_df2) <- "./cwdpat_"
filename(ff_df2)
sapply(physical(ff_df2), finalizer)
pattern(ff_df2) <- "temppat_"
filename(ff_df2)
sapply(physical(ff_df2), finalizer)
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------



# Demo 8: [ff] in parallel environment
# -----------------------------------------------------------------------------
# Advantage: negligible RAM duplication for parallel execution of ff objs
# -----------------------------------------------------------------------------
library(ff)
library(snowfall)

# create ff obj and prepare finalizer for processing
# ---------------------------------------------------
ff_double <- ff(vmode="double", length=1e8)

# finalizer: task to free ressources no longer needed, for example remove the
# ff file or free the C++ RAM associated with an open ff file.
finalizer(ff_double)

# let slaves not delete ff_double on shutdown
finalizer(ff_double) <- "close"

# set up snowfall cluster
# -------------------------
sfInit(parallel = TRUE, cpus=10, type = "SOCK") # setup cluster
sfLibrary(ff) # export [ff] library to cluster
sfExport("ff_double") # avoid multiple exports by exporting "ff_double"
sfClusterEval(open(ff_double)) # explicitly opening avoids a garbage collection

system.time(
  sfLapply(chunk(ff_double), function(x) {
    ff_double[x] <- runif(sum(x)) + 1
    return(invisible())
  })
)

system.time(
  s <- sfLapply(chunk(ff_double), function(x) {
    return(quantile(ff_double[x], c(0.05, 0.95)))
  })
)

# close cluster
# ---------------------
sfClusterEval(close(ff_double))
csummary(s)
sfStop()
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------



# Demo 9: Parallel Bootstrap with [snowfall]
# -----------------------------------------------------------------------------
# Usual approach with [snowfall]: set up a cluster with one master and several
# slaves, e.g., 10.
# The master takes the RAM for data, the slaves take RAM for copy. Therefore
# 11 times RAM requirement => max dataset size is 1/11
# The usage of the [ff] package results into negligible RAM duplication for 
# parallel execution.
# ---------------------
library(snowfall)
wrapper <- function(n) {
  colMeans(ffdf_obj[sample(nrow(ffdf_obj), n, TRUE), c("age", "income")])
}
sfInit(parallel = TRUE, cpu = 10, type = "SOCK")
sfLibrary(ff)
sfExport("ffdf_obj")
# sfClusterSetupRNG()
system.time(y <- sfLapply(rep(1e4, 200), wrapper))
sfStop()
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------



# Demo 10: Rare collisions in parallel bagging with [ff] and [snowfall]
# -----------------------------------------------------------------------------
library(ff)
library(snowfall)
N <- 1e8                     # sample size
n <- 1e5                     # sub-sample size
r <- 10                      # number of sub-samples
ff_obj <- ff(0L, length=N)   # worst case: all votings are collected in the same col
wrapper <- function(i) {
  ff_obj[sample(N, n), add=TRUE] <- 1L
  return(NULL)
}
sfInit(parallel=TRUE, cpus=10, type="SOCK")
sfLibrary(ff)
sfExport("ff_obj")
sfExport("N")
sfExport("n")
system.time(sfLapply(1:r, wrapper))
sfStop()

e <- r*n
m <- e - sum(ff_obj[])
cat("Expected votes", e, "absolute votes lost", m, " votes lost% =",
    100*m/e, "=1/", e/m, "\n")
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

