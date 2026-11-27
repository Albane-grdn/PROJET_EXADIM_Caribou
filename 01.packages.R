# PACKAGES LOADING 
# -> loads libraries for all scripts 
# -> defines paths to locate input data and write output files 
# -> defines date variable for automatic dating of files 

# Path--------------------------------------------------------------------------
dir <- getwd()
p.raw.data <- file.path(paste0(dir, "/data/raw")) # path to raw data

dir.create(file.path(paste0(dir, "/data/processed"))) # create dir for processed data
p.proc.data <- file.path(paste0(dir, "/data/processed")) # path to processed data 

dir.create(file.path(paste0(dir, "/PCA results"))) # create dir for PCA results
p.pca <- file.path(paste0(dir, "/PCA results")) # path to outputs of PCA analysis done in 10.data_vizualisation.R

dir.create(file.path(paste0(dir, "/figures"))) # create dir for report's figures
p.figures <- file.path(paste0(dir, "/figures")) # path to figures done in 12.R

dir.create(file.path(paste0(dir, "/tab"))) # create dir for report's figures
p.tab <- file.path(paste0(dir, "/tab")) # path to figures done in 12.R

# Libraries---------------------------------------------------------------------
packages = c(
             # data manipulation
             "tidyverse",
             # graphics
             "ggplot2",
             "gridExtra",
             
             # Tabs 
             "writexl",
             
             # beautiful maps
             "leaflet",
             "leaflegend",
             # analysis
             "vegan", # adonis2()
             "pairwiseAdonis", # pairwise comparison for permanova
             "vegan3d", # 3D CCA plotting 
             "rgl", # add legend in 3D plots
             "psych", # pairs.panels()
             "matrixCorr", # spearman_rho
             "ade4", # PCA (dudi.pca())
             "adegenet", # multivariate analysis on genetic data
             "adegraphics", # control of labels for optimal representation of the PCA 
             "ggrepel" # for easier representation of PCA
             ) 

source(file = file.path(paste0(dir, "/03.functions.R")))

# #NB with the new R version 4.6.2 --> "aucun package nommé ‘pairwiseAdonis’ n'est trouvé"
# install.packages('devtools')
# library(devtools)
# install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")

# Time stamps for files' name---------------------------------------------------
date <- today()

# Processed data loading -------------------------------------------------------
# Call to latest processed data function 
# -> maybe we do it here to not have to do it in every script 
latest.gen <- get_latest_gen(p.proc.data) 
latest.env <- get_latest_env(p.proc.data) 

data.gen <- readRDS(file = file.path(latest.gen))
data.env <- readRDS(file = file.path(latest.env))