# CCA based on subset of SNP data 
# 

# to be erased from21.R or create a file dedicated to data definition 
data.gen.perm <- readRDS(file = file.path(latest.gen))# load latest version of genetic data
rownames(data.gen.perm)<- data.gen.perm$individu # Turn individuals's ids into rownames 
data.gen.perm <- data.gen.perm[, -c(1)] # Suppress the individual column 

prop.selected.SNP <- c(".10.90", 
                       ".5.95",
                       ".2.98") 
for (p in prop.selected.SNP) {
  
  df <- get(paste0("SNP.interest",p,".all"))
  df.subset <- data.gen.perm[, names(data.gen.perm) %in% df$SNP,
                      drop = FALSE]
  assign(paste0("data.gen.perm", p), df.subset)
  
  gen_ind <- as.genind(df.subset, pop = data.env$ecotype) # Turn data into genind object
  assign(paste0("gen_ind", p), gen_ind)
  
}
rm(df,
   df.subset, 
   gen_ind)

# Permanova on subset of SNP ---------------------------------------------------
## Test ecotypes effect  ------------------------------------------------------
for (p in prop.selected.SNP) {
  set.seed(444)
  gen_ind <- get(paste0("gen_ind", p))
  
  car.mod <- adonis2(gen_ind ~ ecotype, # gene sequence comparison between ecotypes 
                     data = data.env, # explanatory covariate data
                     permutations = 9999, 
                     # creates the object x object distance matrix
                     method = "euclidean", # Need to justify the distance used
                     by = "terms") # assess significance for each term (sequentially from first to last)
  pair.result <- pairwise.adonis2(gen_ind ~ ecotype, data = data.env, nperm = 9999)
  assign(paste0("car.mod", p), car.mod)
  assign(paste0("pair.result", p), pair.result)
}

# On augmente le R² mais les écotypes sont tjrs significativement différents même quand 
# on retire plus de la moitié des SNP 

# CCA on subset of SNP ---------------------------------------------------------
##  Analysis and cov. selection ------------------------------------------------
for (p in prop.selected.SNP) {
  matrix <- get(paste0("data.gen.perm",p))
  cca.subset <-cca(matrix ~ ., data = matrix.env.log)
  vif <- vif.cca(cca.subset)
  
  assign(paste0("cca",p), cca.subset)
  assign(paste0("vif",p), vif)
  
}

# vif : lat > 10 pour les 3 subset et prec annuelle > 10 pour subset 10.90
#  on suppr ces covariables 

for (p in prop.selected.SNP) {
  if (p == ".10.90") {
    matrix.env.sub <- matrix.env.log %>%
      select(-latitude, -precip_annuelles)
  }else{
    matrix.env.sub <- matrix.env.log %>%
      select(-latitude)
  }
  
  matrix <- get(paste0("data.gen.perm",p))
  cca.subset <-cca(matrix ~ ., data = matrix.env.sub)
  vif <- vif.cca(cca.subset)
  # Valeur du R² : corrélation sp-env. 
  r.sq <- RsquareAdj(cca.select)
  
  assign(paste0("cca2",p), cca.subset)
  assign(paste0("vif2",p), vif)
  assign(paste0("r.sq",p), r.sq)
  
}
## Results ---------------------------------------------------------------------

summary(cca.subset)

## Plot ------------------------------------------------------------------------

# DONE IN 25.R
# col_ecotype <- c(
#   "Arctique" = "white",
#   "Foret_atlantique" = "#40BDB9",
#   "Foret_boreale" = "darkgreen",
#   "Foret_Ouest" = "forestgreen",
#   "Haut_Arctique" = "darkblue"
# )
# col_ind <- col_ecotype[as.character(data.env$ecotype)]

for (p in prop.selected.SNP) {
  
  cca.subset <- get(paste0("cca2", p))
  bp <- scores(cca.subset, display = "bp")
  
  
  png(file.path(paste0(p.figures,"/CCA_plot_subset",p,".png")),
      width = 800,
      height = 800,
      res = 110,
      units = "px")
  plot(cca.subset, type = "n")
  text(cca.subset, display = "species", col = "lightblue", cex = 0.8)
  ordiellipse(
    cca.subset,
    groups = data.env$ecotype,
    display = "sites",
    kind = "sd",
    conf = 0.95,
    col = col_ecotype,
    lwd = 1,
    draw = "polygon",
    alpha = 80
  )
  points(
    cca.subset,
    display = "sites",
    pch = 21,
    col = "black",
    bg = col_ind,
    cex = 0.8
  )
  
  arrows(
    0, 0,
    bp[,1], bp[,2],
    length = 0.1,
    col = "red",
    lwd = 1.5
  )
  
  text(
    bp[,1], bp[,2],
    labels = rownames(bp),
    col = "red",
    pos = 4
  )
  
  legend(
    "topright",
    legend = names(col_ecotype),
    pt.bg = col_ecotype,
    pch = 21,
    col = "black",
    title = "Ecotype"
  )
  dev.off()
  
}


