# DATA ANALYSIS : PERMANOVA   
# -> is there a significant difference between ecotypes 
#    based on there genotypes or environnemental conditions 

# Already done in 01.R
# # Call to latest processed data function 
# latest.gen <- get_latest_gen(p.proc.data) 
# latest.env <- get_latest_env(p.proc.data) 

data.gen.perm <- readRDS(file = file.path(latest.gen))# load latest version of genetic data
rownames(data.gen.perm)<- data.gen.perm$individu # Turn individuals's ids into rownames 
data.gen.perm <- data.gen.perm[, -c(1)] # Suppress the individual column 
# Already done in 01.R
# data.env <- readRDS(file = file.path(latest.env)) # load latest version of environmental data

# data.gen.perm <- as.data.frame(t(data.gen[,2:6001]))
# colnames(data.gen.perm) <- data.gen$individu
# data.env.perm <- as.data.frame(t(data.env[,2:12]))
# colnames(data.env.perm) <- data.env$individu

# ADEGENET ---------------------------------------------------------------------
## Data management  ------------------------------------------------------------
gen_ind <- as.genind(data.gen.perm, pop = data.env$ecotype) # Turn data into genind object
pop(gen_ind) # nb of ecotypes included in the analysis 

gen_pop <- genind2genpop(gen_ind) # Converting to genpop object

## PCA ------------------------------------------------------------
pca1 <- dudi.pca(gen_ind,cent=T,scale=T, scannf = FALSE, nf = 2)
barplot(pca1$eig[1:50],main="PCA eigenvalues", col=heat.colors(50))

# Labels overlap, prefer adegraphics
# ade4::s.class(pca1$li, pop(gen_ind), 
#         clabel = 0.5)

df  <- data.frame(pca1$li[, 1:2], pop = pop(gen_ind))
cen <- aggregate(cbind(Axis1, Axis2) ~ pop, data = df, FUN = mean)

### Plotting PCA results ------------------------------------------------------------
s.class(as.data.frame(pca1$li), pop(gen_ind),
        plabels = list(cex = 0.5,
                       col = "black",
                       optim = TRUE,
                       boxes = list(draw = TRUE)
                       
                       )
        )

## Permanova ------------------------------------------------------------
### Test ecotypes effect  ------------------------------------------------------------

car.mod <- adonis2(gen_ind ~ ecotype, # gene sequence comparison between ecotypes 
                    data = data.env, # explanatory covariate data
                    permutations = 999, # Need to increase permutation ? 
                   # creates the object x object distance matrix
                    method = "euclidean", # Need to justify the distance used
                    by = "terms") # assess significance for each term (sequentially from first to last)

# Pairwise comparison of ecotypes, significance assessed through Pr
pairwise.adonis2(gen_ind ~ ecotype, data = data.env, nperm = 999)

### Clustering   ------------------------------------------------------------
distgo = daisy(data.gen.perm, metric = "euclidean") # A marché une fois avec les id des ind mais lpus maintenant AAAAAAAAAAAAH 

plot.new()
hgo = hclust(distgo) ; plot(hgo, hang = -1) # Clustering hiérarchique basique
cut3 = cutree(hgo, h = 72.2) ; abline(h = 72.2, col = "blue") # Découpage de l'arbre 
# à 0.8 = on obtient 3 groupes, permet de créer un vecteur d'intersection décrivant à quel groupe appartient l'sp
adonis2(distgo ~ cut3) # Utilisation directe des résultats du clustering à 3 groupes pour une PERMANOVA

cut5 = cutree(hgo, h = 0.6) ; abline(h = 0.6, col = "red") # Découpage de l'arbre à 0.8 = on obtient 6 groupes
adonis2(distgo ~ cut5) # Utilisation directe des résultats du clustering à 5 groupes pour une PERMANOVA


