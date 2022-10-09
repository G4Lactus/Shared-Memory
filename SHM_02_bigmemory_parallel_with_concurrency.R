# Demo: Parallel access to a matrix with independent and concurrent writing,
#       data race and two ways to solve it!
# -----------------------------------------------------------------------------
library(foreach)
library(iterators)
library(parallel)
library(doParallel)
library(bigmemory)
# -------------------

# work with a temporary folder to store the toy data, the advantage is that is
# will destroy itself as soon as the R session is closed
temp_dir <- tempdir()


# Set up cluster
# ---------------
cl <- parallel::makeCluster(parallel::detectCores() - 2)
doParallel::registerDoParallel(cl)
foreach::getDoParWorkers()


# Scenario 1: Concurrent access and data race problem
# -----------------------------------------------------------------------------
# define big matrix and store it
# -------------------------------
big_mtx <- as.big.matrix(
              x = matrix(0, 10, 10),
              type = "double",
              backingfile = "big_mtx_description.bk",
              backingpath = temp_dir,
              descriptorfile = "big_mtx_description.desc",
              shared = TRUE
            )
big_mtx_description <- describe(big_mtx)
rm(big_mtx)
gc()

# Parallel access
# -----------------------------------------------------------------------------
foreach(i = 1:10, .packages = "bigmemory", .combine = cbind) %dopar% {

  m <- attach.big.matrix(paste0(temp_dir, "\\big_mtx_description.desc"))

  # independent concurrent reading and writing on the same obj no problem
  # -------------------------------------------------------------------------
  m[1, i] <- m[1, i] + 5
  m[3, i] <- m[3, i] + 42
  m[8, i] <- m[8, i] + exp(1)

  
  # concurrent access and writing on the same obj triggers a data race problem!
  # Which worker is first (?), which input to use (?) -> serious problem
  # in all multi-threading applications -> the output is definitely wrong
  # -------------------------------------------------------------------------
  m[5, 5] <- m[5, 5] + 5
  
  return(0.)
}
# -----------------------------------------------------------------------------
m_tst <- attach.big.matrix(paste0(temp_dir, "\\big_mtx_description.desc"))
m_tst[,]  # the results at m_tst[5,5] illustrates the data race outcome!
          # expect are 50, but you obtain some weird other value
rm(list = setdiff(ls(), "cl"))
gc()
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------




# Scenario 2: Simulation that illustrates the success rate with concurrent
#             access. In my test trials I achieved success rates from 40-90%.
#             Too volatile to rely on.
#             Success depends on speed to access data. If we use no file backing
#             such as .desc or .bk files, the success rate is higher. Otherwise
#             it deteriorates.
# -----------------------------------------------------------------------------
# define big matrix and store it
# -------------------------------
ntrials <- 1e2
scenario_outcome <- rep(0, 2)
path_to_file <- tempdir()
back_file <- "big_mtx"
for (scenarioX in 1:2) {
  outcome <- rep(0, times = ntrials)
  for (trial in 1:ntrials) {
    cat(paste0("Trial: ", trial, "/", ntrials, "\n"))
    
    if (scenarioX == 1) {
      big_mtx <- as.big.matrix(
        x = matrix(0, 10, 10),
        type = "double",
        shared = TRUE
      )
      big_mtx_description <- describe(big_mtx)      
    } else {
      if (!file.exists(paste0(path_to_file, "\\", back_file, ".bk"))) 
      {
        big_mtx <- big.matrix(nrow = 10, ncol = 10, init = 0, 
                              backingfile = paste0(back_file, ".bk"),
                              backingpath = path_to_file,
                              descriptorfile = paste0(back_file, ".desc"))
        big_mtx_description <- describe(big_mtx)
        rm(big_mtx)        
      }
      else 
      {
        big_mtx <- attach.big.matrix(obj = paste0(path_to_file, "\\", back_file, ".desc"))
        big_mtx[,] <- 0
        big_mtx_description <- describe(big_mtx)
        rm(big_mtx)
      }
    }

  
    # ---------------------------------------------------------------------------
    foreach(i = 1:10, .packages = "bigmemory") %dopar% {
      
      m <- attach.big.matrix(big_mtx_description)
      
      # independent concurrent reading and writing on the same obj no problem
      # -------------------------------------------------------------------------
      m[1, i] <- m[1, i] + 5
      m[3, i] <- m[3, i] + 42
      m[8, i] <- m[8, i] + exp(1)
      
      # concurrent access and writing on the same obj triggers a data race!
      # -------------------------------------------------------------------------
      m[5, 5] <- m[5, 5] + 5
      
      return(0.)
    }
    # ---------------------------------------------------------------------------
    m_tst <- attach.big.matrix(big_mtx_description)
    if (m_tst[5,5] == 50) { outcome[trial] <- 1 } else { outcome[trial] <- 0}
    rm(list = setdiff(ls(), c("cl", "outcome", "ntrials", "scenarioX", 
                              "scenario_outcome",  "path_to_file", "back_file")))
    gc()
  }
  scenario_outcome[scenarioX] <- mean(outcome)
}
# -----------------------------------------------------------------------------




# Scenario 3: Concurrent access and manually solve the data race for 100%
# -----------------------------------------------------------------------------
# Here we use a file-backed matrix for illustration. It releases the RAM 
# limitation, but has a little speed penalty -> SSD drive recommended
big_mtx <- as.big.matrix(
  x = matrix(0, 10, 10),
  type = "double",
  backingfile = "big_mtx_description_2.bk",
  backingpath = tempdir(),
  descriptorfile = "big_mtx_description_2.desc",
  shared = TRUE
)
# big_mtx_description <- describe(big_mtx)
rm(big_mtx)
gc()
# -----------------------------------------------------------------------------
out <- foreach(i = 1:10, .packages = "bigmemory", .combine = sum) %dopar% {
  
  
  m <- attach.big.matrix(obj = paste0(path_to_file, "\\", "big_mtx_description_2.desc"))
  
  # independent concurrent reading and writing on the same obj no problem
  # -------------------------------------------------------------------------
  m[1, i] <- m[1, i] + 5
  m[3, i] <- m[3, i] + 42
  m[8, i] <- m[8, i] + exp(1)
  
  
  # resolve race by writing third party variable and output
  # -------------------------------------------------------------------------
  out <- m[5, 5] + 5
  
  return(out)
}
# -----------------------------------------------------------------------------
m_tst <- attach.big.matrix(paste0(path_to_file, "\\", "big_mtx_description_2.desc"))
m_tst[,]
m_tst[5,5] <- out  # manually outmaneuver the data race!
m_tst[,]

rm(list = setdiff(ls(), "cl"))
gc()
# -----------------------------------------------------------------------------




# Scenario 4: Mutex, lock and unlock
# -----------------------------------------------------------------------------
# Another solution to the race problem is to enforce atomic execution,
# i.e., only one thread changes data at a time, also called synchronization.
# The guarding mechanism is called mutex (mutual exclusive access).
# It ensures that a thread locks the mutex or wait until it is lockable again.
# 
# The synchronicity package provides support for synchronization via mutexes and
# may eventually support interprocess communication (ipc) and message passing.
# 
# NOTE: UNFORTUNATELY NOT WORKING YET!!!
# ---------------------------------------
path_to_data <- tempdir()
big_mtx <- as.big.matrix(
  x = matrix(0, 10, 10),
  type = "double",
  shared = TRUE,
  backingfile = "big_mtx_description_3.bk",
  backingpath = path_to_data,
  descriptorfile = "big_mtx_description_3.desc",
)


# ---------------------------------------------------------------------------
foreach(i = 1:10, .packages = c("bigmemory", "flock")) %dopar% {
  
  m <- attach.big.matrix(paste0(path_to_data, "\\", "big_mtx_description_3.desc"))
  
  # independent concurrent reading and writing on the same obj no problem
  # -------------------------------------------------------------------------
  m[1, i] <- m[1, i] + 5
  m[3, i] <- m[3, i] + 42
  m[8, i] <- m[8, i] + exp(1)
  
  # concurrent access and writing on the same obj triggers a data race!
  # -------------------------------------------------------------------------
  locked <- flock::lock(paste0(path_to_data, "\\", "big_mtx_description_3.desc"))
  m[5, 5] <- m[5, 5] + 5
  flock::unlock(locked)
  return(0.)
}
# ---------------------------------------------------------------------------
m_tst <- attach.big.matrix(paste0(path_to_data, "\\", "big_mtx_description_3.desc"))
m_tst[,]
rm(list = setdiff(ls(), "cl"))


# Simulation with mutex
# ---------------------------
path_to_data <- tempdir()
back_file <- "big_mtx4"
ntrials <- 1e3
outcome <- rep(0, times = ntrials)
for (trial in 1:ntrials) {
  cat(paste0("Trial: ", trial, "/", ntrials, "\n"))
  
  if (!file.exists(paste0(path_to_data, "\\", back_file, ".bk"))) 
  {
    big_mtx <- big.matrix(nrow = 10, ncol = 10, init = 0, 
                          backingfile = paste0(back_file, ".bk"),
                          backingpath = path_to_data,
                          descriptorfile = paste0(back_file, ".desc")
                         )
    big_mtx_description <- describe(big_mtx)
    rm(big_mtx)        
  }
  else 
  {
    big_mtx <- attach.big.matrix(obj = paste0(path_to_data, "\\", paste0(back_file, ".desc")))
    big_mtx[,] <- 0
    big_mtx_description <- describe(big_mtx)
    rm(big_mtx)
  }


  # ---------------------------------------------------------------------------
  foreach(i = 1:10, .packages = c("bigmemory", "flock", "synchronicity")) %dopar% {


    m <- attach.big.matrix(paste0(path_to_data, "\\", paste0(back_file, ".desc")))

    # independent concurrent reading and writing on the same obj no problem
    # -------------------------------------------------------------------------
    m[1, i] <- m[1, i] + 5
    m[3, i] <- m[3, i] + 42
    m[8, i] <- m[8, i] + exp(1)
    
    
    # concurrent access and writing
    # -------------------------------------------------------------------------
    # lock <- synchronicity::lock(paste0(path_to_data, "\\", paste0(back_file, ".desc")))
    locked <- flock::lock()
    m[5, 5] <- m[5, 5] + 5
    flock::unlock(locked)
    # synchronicity::unlock(lock)
    
    return(0.)
  }
  # ---------------------------------------------------------------------------
  m_tst <- attach.big.matrix(big_mtx_description)
  if (m_tst[5,5] == 50) { outcome[trial] <- 1 } else { outcome[trial] <- 0}
  
  rm(list = setdiff(ls(), c("cl", "outcome", "ntrials", "path_to_data",
                            "back_file")))
  gc()
}
mean(outcome)
# -----------------------------------------------------------------------------