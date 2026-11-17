# DATA ANALYSIS : PERMANOVA   
# -> is there a significant difference between ecotypes 
#    based on there genotypes or environnemental conditions 

# Call to latest processed data function 
latest.gen <- get_latest_gen(p.proc.data) 
latest.env <- get_latest_env(p.proc.data) 

data.gen <- readRDS(file = file.path(latest.gen))# load latest version of genetic data
rownames(data.gen)<- data.gen$individu # Turn individuals's ids into rownames 
data.gen <- data.gen[, -c(1)] # Suppress the individual column 
data.env <- readRDS(file = file.path(latest.env)) # load latest version of environmental data

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

ade4::s.class(pca1$li, pop(gen_ind), 
        clabel = 0.5)

df  <- data.frame(pca1$li[, 1:2], pop = pop(gen_ind))
cen <- aggregate(cbind(Axis1, Axis2) ~ pop, data = df, FUN = mean)

s.class(as.data.frame(pca1$li), pop(gen_ind),
        plabels = list(cex = 0.5,
                       col = "black",
                       optim = TRUE,
                       boxes = list(draw = TRUE)
                       
                       )
        )


# df  <- data.frame(pca1$li[, 1:2], pop = pop(gen_ind))
# cen <- aggregate(cbind(Axis1, Axis2) ~ pop, data = df, FUN = mean)
# ggplot(df, aes(Axis1, Axis2, colour = pop)) +
#   geom_point(alpha = 0.5) +
#   stat_ellipse(mapping = df)+
#   # barycentres
#   geom_point(data = cen, size = 3, shape = 4, stroke = 1.2) +
#   # étiquettes repoussées par le barycentre
#   geom_text_repel(data = cen, aes(label = pop),
#                   colour = "black", size = 3,
#                   point.padding = 0.8,      # distance minimale au barycentre
#                   box.padding   = 0.5,      # distance entre étiquettes
#                   force         = 2,        # intensité de la répulsion
#                   min.segment.length = 0,   # trait vers le barycentre toujours affiché
#                   segment.colour = "grey40",
#                   max.overlaps = Inf,
#                   seed = 1) +
#   theme_minimal() +
#   theme(legend.position = "none")

## Permanova ------------------------------------------------------------
# Test ecotypes effect 

car.mod <- adonis2(gen_ind ~ ecotype, 
                    data = data.env, 
                    permutations = 999,
                    method = "euclidean", # Need to justify the method 
                    by = "terms") 

# Pairwise comparison of ecotypes, significance assessed through Pr
pairwise.adonis2(gen_ind ~ ecotype, data = data.env, nperm = 999)








