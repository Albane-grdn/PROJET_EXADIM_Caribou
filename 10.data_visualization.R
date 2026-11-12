# DATA MINING : Visualization 
# -> Charts and graphs of the data 

# # Call to latest processed data function 
# latest.gen <- get_latest_gen(p.proc.data) 
# latest.env <- get_latest_env(p.proc.data) 
# 
# data.gen <- readRDS(file = file.path(latest.gen))
# data.env <- readRDS(file = file.path(latest.env))

# Environnemental data ---------------------------------------------------------
# Histogramme des fq d'individus pour les variables environnementales numériques
data.env_num <- select_if(data.env, is.numeric)
# par( mfrow= c(4,4) )
for (x in 1:ncol(data.env_num)) {
  # par( mfrow= c(4,4) )
  hist((data.env_num[,x]), main = colnames(data.env_num[x]))
  
}

# Histogramme des fq d'individus selon les écotypes 
hist(str_count(data.env$ecotype), main = (colnames(data.env$ecotype)))

# Genotypic data ---------------------------------------------------------
# PCA based on Novembre et al., 2008 and Leslie et al., 2015

X11()
pairs.panels(data.gen[,2:15], # ne peut pas prendre en charge simultanément 
             # les 6000 SNP 
             method = "spearman", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)

# Spearman pairwise correlation 
# -> expected time for spearman pairwise corr : t  = 120 sec

# NOT WORKING RN : need to be implemented implemented directly into the function 
# if (withCallingHandlers(get_latest_spear(p.pca))==T) { # to run the code if there is no pair.spear.corr.RData
#   # t <- system.time(pair.spears <- spearman_rho(
#   #   data.gen[,2:6001],
#   #   na_method = "pairwise",
#   #   ci = FALSE,
#   #   conf_level = 0.95,
#   #   n_threads = getOption("matrixCorr.threads", 1L),
#   #   output = c("matrix"),
#   #   diag = TRUE))
#   # saveRDS(pair.spears, file = file.path(paste0(p.pca, "/pair.spear.corr_", date,".RData")))
# }

if (exists("pair.spears")==F) {
  latest.spear <- get_latest_spear(p.pca) 
  pair.spears <- readRDS(file = file.path(latest.spear))
}

# NOT WORKING : too much data 
# plot(
#   pair.spears,
#   title = "Spearman's rank correlation heatmap",
#   low_color = "indianred1",
#   high_color = "steelblue1",
#   mid_color = "white",
#   value_text_size = 4,
#   ci_text_size = 3,
#   show_value = TRUE
# )

## PCA ------------------------------------------------------

pca <-dudi.pca(data.gen[,2:1001], scale=T, center=T)
pca$eig # variance portée par les axes de l'ACP,
pc<-round(pca$eig/sum(pca$eig)*100,2) # % d'inertie associée aux axes 

pca$li # coordonnees des individus sur chaque composante factorielle
s.label(pca$li[,1:2], clabel=0.5) # representation sur les axes 1 et 2

## Plotting of PCA results ------------------------------------------------------
scores.ind <- as.data.frame(pca$li)
scores.ind[,ncol(pca$li)+1] <- data.env$ecotype
colnames(scores.ind)[ncol(scores.ind)] <- "Ecotypes"

# Axes principaux retenus : 1 et 2 
ggplot(scores.ind, aes(Axis1, Axis2, colour = Ecotypes)) +
  geom_point()
# Axes principaux retenus : 2 et 3 
ggplot(scores.ind, aes(Axis2, Axis3, colour = Ecotypes)) +
  geom_point()
# Axes principaux retenus : 1 et 3
ggplot(scores.ind, aes(Axis1, Axis3, colour = Ecotypes)) +
  geom_point()

# 3D plot og the PCA results but couldn't figure out how to put the colors 
ordirgl(pca, type = "p") # position des sites qui sont issues de la moyenne pondérées des sp qu'ils contiennent 

