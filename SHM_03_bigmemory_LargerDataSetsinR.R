# bigmemory in data administration
# -----------------------------------------------------------------------------
# How big is a large data set?
# -----------------------------------
# - Medium sized: files that can be loaded in R (within memory limit but
#   processing is cumbersome (typically in the 1-2 GB range))
#   
# - Large files that cannot be loaded in R due to R/OS limitations
#   - Large files: 2-10 GB, can still be processed locally using some work
#                  around solutions
#                  
# - Very Large files: > 10 GB, needs distributed large scale computing
## ----------------------------------------------------------------------------


# Medium sized datasets (< 2GB)
# -----------------------------------
# Try to reduce size of the file before loading it into R
# --------------------------------------------------------
# - for .xls files, you can select specific column that is required for analysis
#   instead of selecting the entire data set
# - You can't select specific columns if you are loading .csv or .txt files
# - preprocess data in command line using: cat or awk  
# ---------------------------

# create toy data
test_data <- matrix(rnorm(1e5), nrow = 100)
data_dir <- paste(getwd(), "Backend/", sep = "/")
if (!dir.exists(data_dir)) { dir.create(path = data_dir) }
path_to_open_file <- paste0(data_dir, "test.csv")
write.csv(test_data, file = path_to_open_file, row.names = FALSE)
rm(list = setdiff(ls(), "path_to_open_file"))
gc()


# Pre-allocated number of rows and pre-define column classes
# -----------------------------------------------------------
# read optimization example:
# -------------------------
# 1. read in a few records of the input file, identify the classes of the
#    input file and assign that column class to the input file while reading
#    the entire data set
# 
# 2. calculate approximate row count of the data set based on the size of the
#    file , number of fields in the column (or using wc in command line) and
#    define nrow parameter
# 
# 3. define comment char parameter
# -----------------------------------
bigfile <- read.csv(file = path_to_open_file, stringsAsFactors = FALSE,
                    header = TRUE, nrows = 20)
nrow(bigfile)
ncol(bigfile)

# fread
# -------
system.time(read.csv(file = path_to_open_file, 
                     stringsAsFactors = FALSE, 
                     header = TRUE)
            )
system.time(data.table::fread(input = path_to_open_file))
# fread has a superior backend implementation in C++


# The [bigmemory] package
# -------------------------------------
# - is part of the "big" family which consists of several packages that perform
#   that perform analysis on large data sets.
# 
# - bigmemory uses several matrix object we will focus on big matrix.
# 
# - big matrix is a R object that uses a pointer to a C++ data structure, and
#   the location of the pointer can be saved to the disk or RAM and shared with
#   other users in different sessions
# 
# - loading the pointer object, users can access the data set without reading
#   the entire set into r
# 
# ----------------------------
library(bigmemory)
big_mtx <- read.big.matrix(filename = path_to_open_file, type = "double",
                           header = TRUE, backingfile = "tst_data.bin",
                           descriptorfile = "tst_data.desc",
                           extraCols = NULL)

# process big matrix in active session
colsum_session <- sum(as.numeric(big_mtx[, 3]))
colsums_session <- colSums(big_mtx[, 3:10])

# Get the location of the pointer 
desc_to_ptr <- describe(big_mtx)

# WORKS FINE, but there is one limitation:
# C++ matrices allow only one type of data, therefore the data set has to be
# only one class of data.



# Shared memory
# ------------------------------------------------------------------------
# - SNOW: package snow, for "small network of workstations"
# 
# - used for parallel computing using a shared big.matrix
# 
# - future performance gains in statistical computing may depend more on
#   software design and algorithms than on further advances in hardware
#   
# - opportunities for concurrent processing of large data sets deserve
#   similar attention
# ------------------------------------------------------------------------

# specify a "worker" function: its job is to attach the shared matrix and return
# the range of values in the column(s) specified by i:
# ----------------------------------------------------
worker <- function(i, descr_of_bm) {

  require(bigmemory)
  require(biganalytics)
  big <- attach.big.matrix(descr_of_bm)

  return(colrange(big, cols = i))
}
# ------------------------------------------

# Shared memory via SNOW
# ---------------------------------------------------------
# In preparing snow, SSH keys were used to avoid having to enter a password for
# each of the workers, and sockets were used rather than MPI or PVM, but
# snow offers several choices for the underlying technology.
library(snow)
cl <- makeSOCKcluster(c("localhost", "localhost", "localhost"))
parSapply(cl, 1:5, worker, desc_to_ptr)
stopCluster(cl)

# Interactive shared memory
# --------------------------------------------
# Poor man's shared memory: open multiple R sessions, master and slaves


# A quick look at applications with big memory technology
# -----------------------------------------------------------
# kMeans and shared memory
# --------------------------------------------
big_random_mtx <- big.matrix(nrow = 3e6, ncol = 5, init = 0, type = "double")
big_random_mtx[seq(1, 3e6, by = 3), ] <- rnorm(5e6)
big_random_mtx[seq(2, 3e6, by = 3), ] <- rnorm(5e6, 1, 1)
big_random_mtx[seq(3, 3e6, by = 3), ] <- rnorm(5e6, -1, 1)
biganalytics::bigkmeans(big_random_mtx, centers = 3, nstart = 3, iter.max = 10)


# big linear model (LM): 
# ----------------------
data(trees)
log_mod <- log(Volume) ~ log(Girth) + log(Height)

chunk1 <- trees[1:10,]
chunk2 <- trees[11:20,]
chunk3 <- trees[21:31,]


library(biglm)
biglm_mod_a <- biglm(log_mod, chunk1)
biglm_mod_a <- update(biglm_mod_a, chunk2)
biglm_mod_a <- update(biglm_mod_a, chunk3)

coef(biglm_mod_a)

# NOTE:
# ------
# - data has been chunked along rows
# - initial fit is done with biglm
# - model is updated with further chunks using update fct
# -------------------
lm_mod <- lm(log_mod, data = trees)
rbind(coef(biglm_mod_a), coef(lm_mod))


