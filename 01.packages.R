# PACKAGES LOADING 
# -> loads libraries for all scripts 
# -> defines paths to locate input data and write output files 
# -> defines date variable for automatic dating of files 

# Path--------------------------------------------------------------------------
dir <- getwd()
p.raw.data <- file.path(paste0(dir, "/data/raw"))
p.proc.data <- file.path(paste0(dir, "/data/processed"))

# Libraries---------------------------------------------------------------------
packages = c("tidyverse",
             "leaflet"# packages collection for data manipulation 
             # ""
) 

source(file = file.path(paste0(dir, "/03.functions.R")))

# Time stamps for files' name---------------------------------------------------
date <- today()
