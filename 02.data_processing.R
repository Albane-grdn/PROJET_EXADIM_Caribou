# DATA PROCESSING : 
# -> reads raw data and makes modifications to create processed data  
# -> saves processed data in /DATA/PROCESSED 
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

data_coypus <- data_coypus %>% 
  # hamornizing the columns' names in between df 
  rename(human_disturbance = HD) %>% 
  # changing , to . to avoid NA and changing the columnn type 
  mutate(human_disturbance = as.numeric(gsub(",", ".", human_disturbance)))

data_coypus$specie <- "Myocastor coypus"
  
# Data Lutra lutra 
data_lutra <- read.csv(file = file.path(paste0(p.raw.data, "/lutra_presence.csv")), # reading the data file 
                        sep = "," # setting column separator 
                       , skip = 1 # skipping line one un-useful 
                       ) 
data_lutra$specie <- "Lutra lutra"


# Saving data 
write.csv(data_lutra, file = file.path(paste0(p.proc.data, "/lutra_processed_", date,".csv")))
# saveRDS(data_lutra, file = file.path(paste0(p.proc.data, "/lutra_processed_", date,".RData")))

write.csv(data_coypus, file = file.path(paste0(p.proc.data, "/coypus_processed_", date,".csv")))
# save(data_coypus, file = file.path(paste0(p.proc.data, "/coypus_processed_", date,".RData")))
