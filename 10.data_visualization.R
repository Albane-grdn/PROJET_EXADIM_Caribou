# DATA MINING : Visualization 

# Données environnementales 
# Histogramme des fq d'individus pour les variables environnementales numériques
data.env_num <- select_if(data.env, is.numeric)
for (x in 1:ncol(data.env_num)) {
  hist((data.env_num[,x]), main = colnames(data.env_num[x]))
}

# Histogramme des fq d'individus selon les écotypes 
hist(str_count(data.env$ecotype), main = (colnames(data.env$ecotype)))

# ------------------------------------------------------------------------------
# Données génotypiques 
# Histogramme des fq alléliques de la population 
for (x in 2:ncol(data.gen)) {
  hist((data.gen[,x]), main = colnames(data.gen[x]))
}


