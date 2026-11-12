# PACKAGES LOADING 
# -> loads libraries for all scripts 
# -> defines paths to locate input data and write output files 
# -> defines date variable for automatic dating of files 

# Path--------------------------------------------------------------------------
dir <- getwd()
p.raw.data <- file.path(paste0(dir, "/data/raw")) # path to raw data
p.proc.data <- file.path(paste0(dir, "/data/processed")) # path to processed data 
p.pca <- file.path(paste0(dir, "/PCA results")) # path to outputs of PCA analysis done in 10.data_vizualisation.R
# Libraries---------------------------------------------------------------------
packages = c(
             # data manipulation
             "tidyverse",  
             # beautiful maps
             "leaflet",
             "leaflegend",
             "vegan3d", # 3D CCA plotting 
             "psych", # pairs.panels()
             "matrixCorr", # spearman_rho
             "ade4", # PCA (dudi.pca())
             "adegenet" # multivariate analysis on genetic data
) 

source(file = file.path(paste0(dir, "/03.functions.R")))

# Time stamps for files' name---------------------------------------------------
date <- today()

# Processed data loading -------------------------------------------------------
# Call to latest processed data function 
# -> maybe we do it here to not have to do it in every script 
latest.gen <- get_latest_gen(p.proc.data) 
latest.env <- get_latest_env(p.proc.data) 

data.gen <- readRDS(file = file.path(latest.gen))
data.env <- readRDS(file = file.path(latest.env))