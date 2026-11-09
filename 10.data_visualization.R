# DATA MINING : Visualization 
# -> Charts and graphs of the data 

# Call to latest processed data function 
latest.gen <- get_latest_gen(p.proc.data) 
latest.env <- get_latest_env(p.proc.data) 

data.gen <- readRDS(file = file.path(latest.gen))
data.env <- readRDS(file = file.path(latest.env))

# Données environnementales 
# Histogramme des fq d'individus pour les variables environnementales numériques
data.env_num <- select_if(data.env, is.numeric)
# par( mfrow= c(4,4) )
for (x in 1:ncol(data.env_num)) {
  # par( mfrow= c(4,4) )
  hist((data.env_num[,x]), main = colnames(data.env_num[x]))
  
}

# Histogramme des fq d'individus selon les écotypes 
hist(str_count(data.env$ecotype), main = (colnames(data.env$ecotype)))

# # ------------------------------------------------------------------------------
# # Données génotypiques 
# # Histogramme des fq alléliques de la population 
# for (x in 2:ncol(data.gen)) {
#   hist((data.gen[,x]), main = colnames(data.gen[x]))
# }


