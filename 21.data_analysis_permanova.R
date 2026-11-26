# DATA ANALYSIS : PERMANOVA   
# -> is there a significant difference between ecotypes 
#    based on there genotypes or environnemental conditions 

data.gen.perm <- readRDS(file = file.path(latest.gen))# load latest version of genetic data
rownames(data.gen.perm)<- data.gen.perm$individu # Turn individuals's ids into rownames 
data.gen.perm <- data.gen.perm[, -c(1)] # Suppress the individual column 

# data.gen.perm <- as.data.frame(t(data.gen[,2:6001]))
# colnames(data.gen.perm) <- data.gen$individu
# data.env.perm <- as.data.frame(t(data.env[,2:12]))
# colnames(data.env.perm) <- data.env$individu

# ADEGENET ---------------------------------------------------------------------
## Data management  ------------------------------------------------------------
gen_ind <- as.genind(data.gen.perm, pop = data.env$ecotype) # Turn data into genind object
pop(gen_ind) # nb of ecotypes included in the analysis 

# gen_pop <- genind2genpop(gen_ind) # Converting to genpop object

# Permanova --------------------------------------------------------------------
### Test ecotypes effect  ------------------------------------------------------
set.seed(444)
car.mod <- adonis2(gen_ind ~ ecotype, # gene sequence comparison between ecotypes 
                   data = data.env, # explanatory covariate data
                   permutations = 9999, 
                   # creates the object x object distance matrix
                   method = "euclidean", # Need to justify the distance used
                   by = "terms") # assess significance for each term (sequentially from first to last)

# Pairwise comparison of ecotypes, significance assessed through Pr  -----------
pair.result <- pairwise.adonis2(gen_ind ~ ecotype, data = data.env, nperm = 9999)

# Heatmap for pairwise comparison of ecotypes  ---------------------------------
## data  -----------------------------------------------------------------------
data.pairs <- data.frame(Foret_boreale = c(NA, pair.result$Foret_boreale_vs_Arctique$R2[1],
                                           pair.result$Foret_boreale_vs_Foret_Ouest$R2[1],
                                           pair.result$Foret_boreale_vs_Haut_Arctique$R2[1],
                                           pair.result$Foret_boreale_vs_Foret_atlantique$R2[1]),
                         
                         Arctique = c(pair.result$Foret_boreale_vs_Arctique$R2[1], NA,
                                        pair.result$Arctique_vs_Foret_Ouest$R2[1],
                                        pair.result$Arctique_vs_Haut_Arctique$R2[1],
                                        pair.result$Arctique_vs_Foret_atlantique$R2[1]),
                         
                         Foret_Ouest = c(pair.result$Foret_boreale_vs_Foret_Ouest$R2[1],
                                         pair.result$Arctique_vs_Foret_Ouest$R2[1],
                                         NA,pair.result$Foret_Ouest_vs_Haut_Arctique$R2[1],
                                         pair.result$Foret_Ouest_vs_Foret_atlantique$R2[1]),
                         
                         Haut_Arctique = c(pair.result$Foret_boreale_vs_Haut_Arctique$R2[1],
                                           pair.result$Arctique_vs_Haut_Arctique$R2[1],
                                           pair.result$Foret_Ouest_vs_Haut_Arctique$R2[1],
                                           NA, pair.result$Haut_Arctique_vs_Foret_atlantique$R2[1]),
                         
                         Foret_atlantique = c(pair.result$Foret_boreale_vs_Foret_atlantique$R2[1],
                                              pair.result$Arctique_vs_Foret_atlantique$R2[1],
                                              pair.result$Foret_Ouest_vs_Foret_atlantique$R2[1],
                                              pair.result$Haut_Arctique_vs_Foret_atlantique$R2[1], NA)
                         )

data.pairs <- data.frame( X = levels(pop(gen_ind)),
                          Y = c(levels(pop(gen_ind))[2:5],levels(pop(gen_ind))[1]),
                          Z = c(pair.result$Foret_boreale_vs_Arctique$R2[1],
                                pair.result$Arctique_vs_Foret_Ouest$R2[1],
                                pair.result$Foret_Ouest_vs_Haut_Arctique$R2[1], 
                                pair.result$Haut_Arctique_vs_Foret_atlantique$R2[1], 
                                pair.result$Foret_boreale_vs_Foret_atlantique$R2[1])
                          )
  
# data.pairs <- data.frame(  Foret_boreale = c(NA, pair.result$Foret_boreale_vs_Arctique$R2[1],
#                                            pair.result$Foret_boreale_vs_Foret_Ouest$R2[1],
#                                            pair.result$Foret_boreale_vs_Haut_Arctique$R2[1],
#                                            pair.result$Foret_boreale_vs_Foret_atlantique$R2[1]),
#                          
#                          Arctique = c(pair.result$Foret_boreale_vs_Arctique$R2[1], NA,
#                                       pair.result$Arctique_vs_Foret_Ouest$R2[1],
#                                       pair.result$Arctique_vs_Haut_Arctique$R2[1],
#                                       pair.result$Arctique_vs_Foret_atlantique$R2[1]),
#                          
#                          Foret_Ouest = c(pair.result$Foret_boreale_vs_Foret_Ouest$R2[1],
#                                          pair.result$Arctique_vs_Foret_Ouest$R2[1],
#                                          NA,pair.result$Foret_Ouest_vs_Haut_Arctique$R2[1],
#                                          pair.result$Foret_Ouest_vs_Foret_atlantique$R2[1]),
#                          
#                          Haut_Arctique = c(pair.result$Foret_boreale_vs_Haut_Arctique$R2[1],
#                                            pair.result$Arctique_vs_Haut_Arctique$R2[1],
#                                            pair.result$Foret_Ouest_vs_Haut_Arctique$R2[1],
#                                            NA, pair.result$Haut_Arctique_vs_Foret_atlantique$R2[1]),
#                          
#                          Foret_atlantique = c(pair.result$Foret_boreale_vs_Foret_atlantique$R2[1],
#                                               pair.result$Arctique_vs_Foret_atlantique$R2[1],
#                                               pair.result$Foret_Ouest_vs_Foret_atlantique$R2[1],
#                                               pair.result$Haut_Arctique_vs_Foret_atlantique$R2[1], NA)
# )


rownames(data.pairs) <- colnames(data.pairs)
data.pairs <- as.matrix(data.pairs)

## Plot  -----------------------------------------------------------------------
heatmap(data.pairs, 
        Colv = NA, Rowv = NA)

