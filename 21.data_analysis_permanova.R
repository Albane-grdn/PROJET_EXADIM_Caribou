# DATA ANALYSIS : PERMANOVA   
# -> is there a significant difference between ecotypes 
#    based on there genotypes or environnemental conditions 

# Already done in 01.R
# # Call to latest processed data function 
# latest.gen <- get_latest_gen(p.proc.data) 
# latest.env <- get_latest_env(p.proc.data) 

# Already done in 01.R
# data.gen <- readRDS(file = file.path(latest.gen))# load latest version of genetic data
rownames(data.gen)<- data.gen$individu # Turn individuals's ids into rownames 
data.gen <- data.gen[, -c(1)] # Suppress the individual column 
# Already done in 01.R
# data.env <- readRDS(file = file.path(latest.env)) # load latest version of environmental data

# data.gen.perm <- as.data.frame(t(data.gen[,2:6001]))
# colnames(data.gen.perm) <- data.gen$individu
# data.env.perm <- as.data.frame(t(data.env[,2:12]))
# colnames(data.env.perm) <- data.env$individu

# ADEGENET ---------------------------------------------------------------------
## Data management  ------------------------------------------------------------
gen_ind <- as.genind(data.gen, pop = data.env$ecotype) # Turn data into genind object
pop(gen_ind) # nb of ecotypes included in the analysis 

gen_pop <- genind2genpop(gen_ind) # Converting to genpop object

## PCA ------------------------------------------------------------
pca1 <- dudi.pca(gen_ind,cent=T,scale=T)
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








