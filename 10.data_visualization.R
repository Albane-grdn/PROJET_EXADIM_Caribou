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
# par( mfrow= c(4,4))
for (x in 1:ncol(data.env_num)) {
  # par( mfrow= c(4,4) )
  hist((data.env_num[,x]), main = colnames(data.env_num[x]))
  
}

# Histogramme des fq d'individus selon les écotypes 
hist(str_count(data.env$ecotype), main = (colnames(data.env$ecotype)))

# Couvert arboré et écotypes :
# -> voir à quel écotype, décrit selon leur écologie par Mallory et Hillis, 1998, 
# pourrait correpsondre nos modalités d'écotypes

bxplt_couv <- data.env%>%
  ggplot( aes(x=ecotype, y=couverture_arboree, fill=ecotype)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Couvert arboré") +
  xlab("Ecotype")+
  ylab("Indice de couvert arboré")

bxplt_ndvi <- data.env%>%
  ggplot( aes(x=ecotype, y=ndvi, fill=ecotype)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Indice de végétation par différence normalisée (NDVI) ") +
  xlab("Ecotype")+
  ylab("NDVI")

bxplt_alt <- data.env%>%
  ggplot( aes(x=ecotype, y=altitude, fill=ecotype)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Altitude") +
  xlab("Ecotype")+
ylab("m")


# Genotypic data ---------------------------------------------------------------
data.gen[c(1:4), c(1:8)]
unique(data.gen$snp_00001) #0= homozygote commun, 1= hétérozygote, 2= homozygote rare

## PCA based on Novembre et al., 2008 and Leslie et al., 2015 ------------------
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

if (exists("pair.spears")==F) {
  latest.spear <- get_latest_spear(p.pca) 
  pair.spears <- readRDS(file = file.path(latest.spear))
}

## PCA using genind object ------------------------------------------------------
### data  ------------------------------------------------------
data.gen.perm <- readRDS(file = file.path(latest.gen))# load latest version of genetic data
rownames(data.gen.perm)<- data.gen.perm$individu # Turn individuals's ids into rownames 
data.gen.perm <- data.gen.perm[, -c(1)] # Suppress the individual column 
gen_ind <- as.genind(data.gen.perm, pop = data.env$ecotype) # Turn data into genind object

### PCA analysis ------------------------------------------------------
pca_3axis <- dudi.pca(gen_ind,cent=T,scale=T, scannf = FALSE, nf = 3) # PCA used for 3D plotting but only 2 axis should be retained for analysis 
barplot(pca_3axis$eig[1:50],main="PCA eigenvalues", col=heat.colors(50))

pca_3axis$eig # variance portée par les axes de l'ACP,
pc<-round(pca_3axis$eig/sum(pca_3axis$eig)*100,2) # % d'inertie associée aux axes 

### Plotting of PCA results ------------------------------------------------------
pca_3axis$li # coordonnees des individus sur chaque composante factorielle
s.label(pca_3axis$li[,1:2], clabel=0.5) # representation sur les axes 1 et 2

scores.ind <- as.data.frame(pca_3axis$li)
scores.ind[,ncol(pca_3axis$li)+1] <- data.env$ecotype
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

# Used Claude to set up different colors for each ecotypes in ordirgl 
ecot <- as.factor(data.env$ecotype)
# col.ecot <- c("#4DAF4A", "#FF7F00", "#377EB8","#E41A1C", "#984EA3")
col.ecot <- c("#FF7F00", "#984EA3","#4DAF4A", "#377EB8","#E41A1C")
col.ecot.ind <- col.ecot[as.integer(ecot)]  

# 3D plot og the PCA results but couldn't figure out how to put the colors 
ordirgl(pca_3axis, type = "p", col = col.ecot.ind, 
        ax.col = "darkgrey") # position des sites qui sont issues de la moyenne pondérées des sp qu'ils contiennent 
legend3d("topright", legend = levels(ecot),
         pch = 1, col = col.ecot, cex = 0.8)
