# FUNCTIONS 
# -> defines functions that are needed in other scripts
# -> functions are accessed through base::source() 

# 1. Package checking 
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
