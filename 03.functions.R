# FUNCTIONS 
# -> defines functions that are needed in other scripts
# -> functions are accessed through base::source() 

# 01.R: Package checking 
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# ------------------------------------------------------------------------------
# 10.R: lastest version of processed data loading 
# Get last processed genotype data 
get_latest_gen <- function(path) {
  files <- list.files(
    path,
    pattern = "data.gen_\\d{4}-\\d{2}-\\d{2}\\.RData$",
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    stop("No file found.")
  }
  
  dates <- as.Date(
    sub(".*data.gen_(\\d{4}-\\d{2}-\\d{2})\\.RData", "\\1", basename(files))
  )
  
  files[which.max(dates)]
}

# Get last processed environnemental data 
get_latest_env <- function(path) {
  files <- list.files(
    path,
    pattern = "data.env_\\d{4}-\\d{2}-\\d{2}\\.RData$",
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    stop("No file found.")
  }
  
  dates <- as.Date(
    sub(".*data.env_(\\d{4}-\\d{2}-\\d{2})\\.RData", "\\1", basename(files))
  )
  
  files[which.max(dates)]
}
# ------------------------------------------------------------------------------