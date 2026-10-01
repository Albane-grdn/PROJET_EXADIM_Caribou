# DATA PROCESSING : 
# -> reads raw data and makes modifications to create processed data  
# -> saves processed data in /data/processed 
# ------------------------------------------------------------------------------
# Data ENVIRONMENT
# Values 
data.env <- read.csv(file = file.path(paste0(p.raw.data, "/environnement_caribou.csv")), # reading the data file 
                        sep = ",") # setting column separator 

# Names
lgd.env <- read.csv(file = file.path(paste0(p.raw.data, "/legende_variables_environnementales.csv")), 
                     sep = ",") 

# Data GENOTYPE
data.gen <- read.csv(file = file.path(paste0(p.raw.data, "/genotypes_caribou.csv")), 
                     sep = ",") 

# ------------------------------------------------------------------------------
# PROCESSED DATA  
# Saving data 
write.csv(data.env, file = file.path(paste0(p.proc.data, "/data.env_", date,".csv")))
saveRDS(data.env, file = file.path(paste0(p.proc.data, "/data.env_", date,".RData")))

write.csv(data.gen, file = file.path(paste0(p.proc.data, "/data.gen_", date,".csv")))
saveRDS(data.gen, file = file.path(paste0(p.proc.data, "/data.gen_", date,".RData")))
