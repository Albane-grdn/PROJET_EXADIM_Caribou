# DATA MINING : Visualization 
# -> Charts and graphs of the data 

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

#call the function: 
latest.gen <- get_latest_gen(p.proc.data) 
latest.env <- get_latest_env(p.proc.data) 

data.gen <- readRDS(file = file.path(latest.gen))
data.env <- readRDS(file = file.path(latest.env))

# Données environnementales 
# Histogramme des fq d'individus pour les variables environnementales numériques
data.env_num <- select_if(data.env, is.numeric)
par( mfrow= c(4,4) )
for (x in 1:ncol(data.env_num)) {
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


